import {createRawPayloadReader,payloadTokens} from './session-viewer.js'

// A mounted event owns only its compact payload, iterator and two output parts.
// Deep indentation is produced on demand rather than materialized in the DOM.
export function createEventPayloadPager(source) {
  let reader=createRawPayloadReader(source),next=0,recent=[]
  return {read(part=0){
    if(!Number.isSafeInteger(part)||part<0)throw Error('invalid_body_part')
    const cached=recent.find(item=>item.part===part)
    if(cached)return cached
    if(part<next){reader=createRawPayloadReader(source);next=0;recent=[]}
    let result
    while(next<=part){result=reader.read();next++;if(!result.more && result.part<part)throw Error('invalid_body_part')}
    result.tokens=payloadTokens(result.text)
    recent=[...recent.slice(-1),result]
    return result
  },get retainedParts(){return recent.length}}
}

export function installEventPayload(window=globalThis.window) {
  if(!window?.customElements || window.customElements.get('session-event-json'))return
  window.customElements.define('session-event-json',class extends window.HTMLElement {
    static get observedAttributes(){return ['payload']}
    connectedCallback(){this.render(this.payloadPart || 0)}
    attributeChangedCallback(_,before,after){if(before!==after){this.pager=null;this.payloadPart=0;if(this.isConnected)this.render(0)}}
    disconnectedCallback(){this.pager=null}
    render(part){
      const document=this.ownerDocument
      try{
        this.pager ??= createEventPayloadPager(this.getAttribute('payload') || 'null')
        const body=this.pager.read(part),pre=document.createElement('pre'),code=document.createElement('code')
        this.payloadPart=part;pre.className='session-tool-content';code.className='language-json'
        for(const token of body.tokens){const span=document.createElement('span');span.className='json-'+token.kind;span.textContent=token.text;code.append(span)}
        pre.append(code)
        const controls=document.createElement('div')
        const button=(label,target)=>{const node=document.createElement('button');node.type='button';node.textContent=label;node.addEventListener('click',()=>this.render(target));return node}
        if(part>0)controls.append(button('Previous JSON part',part-1))
        if(body.more)controls.append(button('Continue JSON',part+1))
        this.replaceChildren(pre,controls)
      }catch{this.textContent='Event payload unavailable.'}
    }
  })
}
