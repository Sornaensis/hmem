import {createRawPayloadReader,payloadTokens} from './session-viewer.js'

// The lazy pager retains its compact source, iterator and two output parts.
// Deep indentation is produced on demand rather than materialized in the DOM.
export function createEventPayloadPager(source,units=16384) {
  let reader=createRawPayloadReader(source,units),next=0,recent=[],position=0
  const readBounded=(part=0,budget=32)=>{
    if(!Number.isSafeInteger(part)||part<0)throw Error('invalid_body_part')
    const cached=recent.find(item=>item.part===part)
    if(cached)return {done:true,value:cached}
    if(part<next){reader=createRawPayloadReader(source,units);next=0;recent=[];position=0}
    let result
    for(let steps=0;next<=part&&steps<budget;steps++){result=reader.read();result.start=position;position+=result.text.length;next++;if(!result.more && result.part<part)throw Error('invalid_body_part')}
    if(next<=part)return {done:false}
    result.tokens=payloadTokens(result.text)
    recent=[...recent.slice(-1),result]
    return {done:true,value:result}
  }
  return {read(part=0){return readBounded(part,Infinity).value},readBounded,get retainedParts(){return recent.length}}
}

export function installEventPayload(window=globalThis.window) {
  if(!window?.customElements || window.customElements.get('session-event-json'))return
  window.customElements.define('session-event-json',class extends window.HTMLElement {
    constructor(){super();this._source=null;this._version=0;this._intent=0;this._parts=new Map();this._direction=1;this._end=null;this._known=-1;this._scheduled=false;this._pending=false}
    static get observedAttributes(){return ['payload']}
    get payload(){return this._source}
    set payload(value){
      if(value===this._source)return
      this._version++;this._wake?.();this._source=String(value ?? 'null');this.pager=null;this._parts.clear();this._before=null;this._after=null;this._end=null;this._known=-1;this._seek=0
      if(this.isConnected){this.textContent='';this._pre=null;this._code=null;this._schedule()}
    }
    attributeChangedCallback(_,before,after){if(before!==after)this.payload=after}
    connectedCallback(){
      this.tabIndex=0;this._root=this.closest('#session-records') || this.parentElement
      if(this._source===null&&this.hasAttribute('payload'))this.payload=this.getAttribute('payload')
      this._scroll=()=>this._schedule()
      this._input=event=>{
        if(event.isTrusted===false || !this._root.contains(event.target))return
        const box=this._root.getBoundingClientRect(),rect=this.getBoundingClientRect();if(rect.bottom<box.top||rect.top>box.bottom)return
        let direction=event.type==='wheel'?Math.sign(event.deltaY):event.type==='keydown'?(['Home','PageUp','ArrowUp'].includes(event.key)?-1:['End','PageDown','ArrowDown',' '].includes(event.key)?1:0):0
        const navigation=event.type!=='keydown'||direction!==0
        if(navigation){this._intent++;this._seek=null;this._wake?.()}
        if(direction)this._direction=direction
        if(event.type==='keydown'&&this.contains(event.target)&&['Home','End'].includes(event.key)){event.preventDefault();this._seek=event.key==='Home'?0:'end'}
        this._schedule()
      }
      this._root?.addEventListener('scroll',this._scroll)
      for(const name of ['wheel','keydown','touchmove','pointerdown'])this._root?.addEventListener(name,this._input,true)
      this._resize=window.ResizeObserver?new window.ResizeObserver(()=>this._schedule()):null;this._resize?.observe(this._root)
      this._owner=window.MutationObserver?new window.MutationObserver(()=>this._schedule()):null;this._owner?.observe(this._root,{attributes:true,attributeFilter:['data-viewer-owner','data-viewer-generation']})
      this._schedule()
    }
    disconnectedCallback(){
      this._root?.removeEventListener('scroll',this._scroll)
      for(const name of ['wheel','keydown','touchmove','pointerdown'])this._root?.removeEventListener(name,this._input,true)
      this._resize?.disconnect();this._owner?.disconnect()
      // Keyed moves can reconnect the same host before custom-element reactions
      // run. True retirement releases private source and every pending pump.
      if(!this.isConnected){this._version++;this._wake?.();this._source=null;this.pager=null;this._parts.clear();this.textContent='';this._pre=null;this._code=null;this._before=null;this._after=null}
    }
    _stamp(){return this._root?.getAttribute('data-viewer-owner')+':'+this._root?.getAttribute('data-viewer-generation')}
    _valid(token){return this.isConnected&&this._source!==null&&token.version===this._version&&token.intent===this._intent&&token.owner===this._stamp()}
    _yield(){return new Promise(resolve=>{const id=window.setTimeout(()=>{this._wake=null;resolve()},0);this._wake=()=>{window.clearTimeout(id);this._wake=null;resolve()}})}
    async _read(part,token){
      this.pager ??= createEventPayloadPager(this._source,1024)
      const pager=this.pager
      while(this._valid(token)){
        const result=pager.readBounded(part,32)
        if(result.done){this._known=Math.max(this._known,part);if(!result.value.more)this._end=part;return result.value}
        await this._yield()
      }
      return null
    }
    _schedule(){if(this._scheduled||!this.isConnected||this._source===null)return;this._scheduled=true;window.requestAnimationFrame(()=>{this._scheduled=false;void this._inspect()})}
    _visible(){
      const box=this._root.getBoundingClientRect()
      return [...(this._code?.querySelectorAll('[data-event-json-part] > span') || [])].find(node=>{const r=node.getBoundingClientRect();return r.height>0&&r.bottom>box.top&&r.top<box.bottom})
    }
    _part(body){
      const node=this.ownerDocument.createElement('span');node.dataset.eventJsonPart=String(body.part)
      let position=body.start
      for(const token of body.tokens){const span=this.ownerDocument.createElement('span');span.className='json-'+token.kind;span.dataset.payloadStart=String(position);span.textContent=token.text;position+=token.text.length;node.append(span)}
      return {body,node,cost:new window.TextEncoder().encode(node.outerHTML).length}
    }
    _commit(bodies,token,seek){
      if(!this._valid(token))return
      const root=this._root,box=root.getBoundingClientRect(),anchor=this._visible(),anchorTop=anchor?.getBoundingClientRect().top-box.top,oldTop=root.scrollTop
      const incoming=new Set(bodies.map(body=>body.part)),all=new Map(this._parts)
      if(bodies.length>1||bodies.some(body=>all.size&&!all.has(body.part-1)&&!all.has(body.part+1)&&!all.has(body.part)))all.clear()
      for(const body of bodies)if(!all.has(body.part))all.set(body.part,this._part(body))
      const protectedParts=new Set([...this._parts].filter(([,part])=>{const r=part.node.getBoundingClientRect();return r.bottom>box.top-96&&r.top<box.bottom+96}).map(([index])=>index))
      const protectedAt=Number(anchor?.parentElement?.dataset.eventJsonPart ?? bodies[0].part)
      while(all.size>256||[...all.values()].reduce((sum,part)=>sum+part.cost,0)>262144-4096){
        const victims=[...all.keys()].filter(index=>!incoming.has(index)&&!protectedParts.has(index)).sort((a,b)=>Math.abs(b-protectedAt)-Math.abs(a-protectedAt))
        if(!victims.length)return
        all.delete(victims[0])
      }
      // Only contiguous admitted ranges are represented by the two edge gaps.
      const ordered=[...all].sort((a,b)=>a[0]-b[0]);if(ordered.some(([index],at)=>at>0&&index!==ordered[at-1][0]+1))return
      this._parts=all
      if(!this._code){this._pre=this.ownerDocument.createElement('pre');this._pre.className='session-tool-content';this._pre.style.maxHeight='none';this._pre.style.overflow='visible';this._code=this.ownerDocument.createElement('code');this._code.className='language-json';this._pre.append(this._code);this.append(this._pre)}
      const average=Math.max(18,Math.min(2000,[...all.values()].reduce((sum,part)=>sum+(part.node.getBoundingClientRect().height || 80),0)/all.size))
      const gap=(side,first,last,height)=>{let node=side==='before'?this._before:this._after;if(!node){node=this.ownerDocument.createElement('span');node.style.display='block';node.setAttribute('aria-hidden','true');if(side==='before')this._before=node;else this._after=node}node.dataset.eventJsonGap=side;node.dataset.eventJsonFrom=String(first);node.dataset.eventJsonTo=String(last);node.style.height=Math.min(10000000,Math.max(1,height))+'px';return node}
      const first=ordered[0][0],last=ordered.at(-1)[0],more=ordered.at(-1)[1].body.more
      const desired=[...(first>0?[gap('before',0,first-1,first*average)]:[]),...ordered.map(([,part])=>part.node),...(more?[gap('after',last+1,this._end ?? last+1,this._end===null?Math.max(256,root.clientHeight):Math.max(1,this._end-last)*average)]:[])]
      const keep=new Set(desired);for(let index=0;index<desired.length;index++)if(this._code.children[index]!==desired[index])this._code.insertBefore(desired[index],this._code.children[index] || null)
      for(const node of [...this._code.children])if(!keep.has(node))node.remove()
      this.dataset.eventJsonFirst=String(first);this.dataset.eventJsonLast=String(last);this.dataset.eventJsonMore=String(more)
      if(seek===0)root.scrollTop+=this._pre.getBoundingClientRect().top-box.top
      else if(seek==='end')root.scrollTop+=this._pre.getBoundingClientRect().bottom-box.bottom
      else if(anchor?.isConnected)root.scrollTop+=anchor.getBoundingClientRect().top-box.top-anchorTop
      else root.scrollTop=oldTop
    }
    async _inspect(){
      if(this._pending||!this._root||this._source===null)return
      const box=this._root.getBoundingClientRect(),rect=this.getBoundingClientRect();if(rect.bottom<box.top-192||rect.top>box.bottom+192)return
      const seek=this._seek;this._seek=null
      const direction=Number(this._root.getAttribute('data-reader-direction')) || this._direction
      const first=Math.min(...this._parts.keys()),last=Math.max(...this._parts.keys()),visible=this._visible()
      const near=node=>{const r=node?.getBoundingClientRect();return r&&r.bottom>box.top-128&&r.top<box.bottom+128}
      let target=seek===0?0:!this._parts.size?0:null
      if(target===null&&seek!=='end'){
        const marker=direction<0&&this._before?.isConnected&&near(this._before)?this._before:direction>=0&&this._after?.isConnected&&near(this._after)?this._after:null
        if(!marker)return
        const r=marker.getBoundingClientRect(),from=Number(marker.dataset.eventJsonFrom),to=Number(marker.dataset.eventJsonTo)
        target=visible?(direction<0?first-1:last+1):Math.round(from+(to-from)*Math.max(0,Math.min(1,(box.top+box.height/2-r.top)/Math.max(1,r.height))))
      }
      const attempt=[this._version,first,last,target,seek,this._root.scrollTop].join(':');if(attempt===this._attempt)return;this._attempt=attempt
      const token={version:this._version,intent:this._intent,owner:this._stamp()};this._pending=true;this.setAttribute('aria-busy','true')
      try{
        if(seek==='end'){
          let part=this._end ?? Math.max(0,this._known),tail=[]
          while(this._valid(token)){const body=await this._read(part,token);if(!body)return;tail=[...tail.slice(-2),body];if(!body.more){this._commit(tail,token,'end');break}part++;if(part%32===0)await this._yield()}
        }else {const body=await this._read(target,token);if(body)this._commit([body],token,seek)}
      }catch{if(this._valid(token))this.textContent='Event payload unavailable.'}
      finally{this._pending=false;if(!this._valid(token))this._attempt=null;this.setAttribute('aria-busy','false');if(this.isConnected)this._schedule()}
    }
  })
}
