// A control-only subscription for readers without a canonical catalogue socket.
// Tickets and capability URLs stay inside this transport; Elm sees bounded hints.
export function createSessionActivityStream({fetchTicket,wsUrl,WebSocketImpl=WebSocket,onFrame,setTimer=setTimeout,clearTimer=clearTimeout,ticketTimeoutMs=15000,readyTimeoutMs=15000}) {
  let owner=null,generation=0,pending=null,socket=null,timer=null,attempt=0
  const current=(stamp,epoch)=>owner===stamp&&generation===epoch
  const uuid=value=>typeof value==='string'&&/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(value)
  function cancelTimer(){if(timer!==null){clearTimer(timer);timer=null}}
  function retry(stamp,epoch){if(!current(stamp,epoch)||timer!==null)return;timer=setTimer(()=>{timer=null;pump()},Math.min(30000,500*2**Math.min(attempt++,6)))}
  async function pump(){
    if(!owner||pending||socket||timer!==null)return
    const stamp=owner,epoch=generation,controller=new AbortController(),operation={controller}
    pending=operation
    timer=setTimer(()=>{timer=null;if(current(stamp,epoch))controller.abort()},ticketTimeoutMs)
    try{
      const response=await fetchTicket({signal:controller.signal})
      if(!current(stamp,epoch))return
      if(controller.signal.aborted)throw Error('activity_ticket_cancelled')
      if(!response.ok)throw Error('activity_ticket_failed')
      const ticket=await response.json()
      if(!current(stamp,epoch))return
      if(controller.signal.aborted)throw Error('activity_ticket_cancelled')
      if(!uuid(ticket.ticket)||typeof ticket.expires_at!=='string'||!Number.isFinite(Date.parse(ticket.expires_at)))throw Error('invalid_activity_ticket')
      cancelTimer()
      const url=new URL(wsUrl);url.searchParams.set('ticket',ticket.ticket)
      const active=new WebSocketImpl(url.toString());socket=active
      let live=true
      const owns=()=>live&&current(stamp,epoch)&&socket===active
      const retire=()=>{live=false;active.close()}
      timer=setTimer(()=>{timer=null;if(owns())retire()},readyTimeoutMs)
      active.onmessage=event=>{
        if(!owns())return
        try{
          const frame=JSON.parse(event.data)
          if(frame?.schema_version!==1)throw Error('invalid_activity_frame')
          if(frame.type==='session_activity_ready'){cancelTimer();attempt=0}
          else if(frame.type==='session_activity_expired'){retire();return}
          else if(frame.type==='session_activity_changed'||frame.type==='access_granted'){if(!uuid(frame.workspace_id))throw Error('invalid_activity_workspace')}
          else if(frame.type==='access_revoked'){if(frame.workspace_id!==undefined&&frame.workspace_id!==null&&!uuid(frame.workspace_id))throw Error('invalid_activity_workspace')}
          else throw Error('invalid_activity_frame')
          onFrame({owner:stamp,frame})
        }catch{retire()}
      }
      active.onclose=()=>{
        if(socket!==active)return
        socket=null
        if(current(stamp,epoch)){cancelTimer();retry(stamp,epoch)}else pump()
      }
      active.onerror=()=>{if(owns())retire()}
    }catch{if(current(stamp,epoch)){cancelTimer();retry(stamp,epoch)}}
    finally{if(pending===operation)pending=null;if(!current(stamp,epoch))pump()}
  }
  return {
    configure(next){
      next=typeof next==='string'&&next?next:null
      if(next===owner)return
      owner=next;generation++;attempt=0;cancelTimer()
      // Closing is still physical occupancy. Only that exact socket's close
      // callback admits the latest desired owner, even across rapid switches.
      if(pending)pending.controller.abort()
      if(socket)socket.close()
      pump()
    },
    close(){this.configure(null)}
  }
}
