// ZIP64 STORE uses data descriptors, incremental CRC32 and a streamed central
// index. Evidence SHA256/receipt verification is owned by createEvidenceWriter.
const encode = new TextEncoder()
const table = Uint32Array.from({length:256},(_,n)=>{for(let i=0;i<8;i++)n=n&1?0xedb88320^(n>>>1):n>>>1;return n>>>0})
const u32=(view,offset,value)=>view.setUint32(offset,value,true)
const u64=(view,offset,value)=>{if(!Number.isSafeInteger(value)||value<0)throw Error('zip_size_limit');view.setBigUint64(offset,BigInt(value),true)}
const record=(size,build)=>{const bytes=new Uint8Array(size);build(new DataView(bytes.buffer));return bytes}
export const evidenceEntryName = session => {
 const label=String(session.name||'session').normalize('NFKC').replace(/[^\p{L}\p{N}._ -]/gu,'-').replace(/\.{2,}/g,'-').replace(/^[. -]+|[. -]+$/g,'').slice(0,72)||'session'
 return label+'-'+session.id+'.jsonl'
}
export function createZip64Writer(output,index,isCurrent=()=>true){
 let offset=0,count=0,indexBytes=0,entry=null,retired=false,complete=false,aborting
 const pending=new Set()
 const check=()=>{if(retired||complete||!isCurrent())throw Error('retired_archive')}
 const perform=async action=>{check();const running=Promise.resolve().then(action);pending.add(running);try{return await running}finally{pending.delete(running)}}
 const write=async bytes=>{check();if(!Number.isSafeInteger(offset+bytes.length))throw Error('zip_size_limit');await perform(()=>output.write(bytes));check();offset+=bytes.length}
 async function abort(){retired=true;aborting||=(async()=>{const results=await Promise.allSettled([output.abort(),index.abort()]);await Promise.allSettled([...pending]);if(results.some(result=>result.status==='rejected'))throw Error('writer_cleanup_failed')})();return aborting}
 return {
  async entry(name,size){
   check();if(entry)throw Error('zip_entry_busy');if(!Number.isSafeInteger(size)||size<0)throw Error('invalid_receipt')
   const filename=encode.encode(name);if(filename.length>65535)throw Error('zip_name_limit')
   const start=offset,owner={crc:0xffffffff,size:0,closed:false};entry=owner
   const header=record(50,v=>{u32(v,0,0x04034b50);v.setUint16(4,45,true);v.setUint16(6,0x808,true);v.setUint16(12,0x21,true);u32(v,18,0xffffffff);u32(v,22,0xffffffff);v.setUint16(26,filename.length,true);v.setUint16(28,20,true);v.setUint16(30,1,true);v.setUint16(32,16,true);u64(v,34,size);u64(v,42,size)})
   await write(header.subarray(0,30));await write(filename);await write(header.subarray(30))
   return {
    async write(bytes){check();if(entry!==owner||owner.closed||owner.size+bytes.length>size)throw Error('zip_entry_size');await write(bytes);for(const byte of bytes)owner.crc=table[(owner.crc^byte)&255]^(owner.crc>>>8);owner.size+=bytes.length},
    async close(){check();if(entry!==owner||owner.size!==size)throw Error('zip_entry_size');const crc=(owner.crc^0xffffffff)>>>0
     await write(record(24,v=>{u32(v,0,0x08074b50);u32(v,4,crc);u64(v,8,size);u64(v,16,size)}))
     const central=record(74,v=>{u32(v,0,0x02014b50);v.setUint16(4,45,true);v.setUint16(6,45,true);v.setUint16(8,0x808,true);v.setUint16(14,0x21,true);u32(v,16,crc);u32(v,20,0xffffffff);u32(v,24,0xffffffff);v.setUint16(28,filename.length,true);v.setUint16(30,28,true);u32(v,42,0xffffffff);v.setUint16(46,1,true);v.setUint16(48,24,true);u64(v,50,size);u64(v,58,size);u64(v,66,start)})
     await perform(()=>index.write(central.subarray(0,46)));await perform(()=>index.write(filename));await perform(()=>index.write(central.subarray(46)));check();indexBytes+=central.length+filename.length;count++;owner.closed=true;entry=null
    },abort
   }
  },
  async finish(){check();if(entry)throw Error('zip_entry_busy');const centralOffset=offset
   const reader=index.readChunks()[Symbol.asyncIterator]()
   while(true){const part=await perform(()=>reader.next());if(part.done)break;await write(part.value)}
   if(offset-centralOffset!==indexBytes)throw Error('zip_index_size')
   const endOffset=offset
   await write(record(56,v=>{u32(v,0,0x06064b50);u64(v,4,44);v.setUint16(12,45,true);v.setUint16(14,45,true);u64(v,24,count);u64(v,32,count);u64(v,40,indexBytes);u64(v,48,centralOffset)}))
   await write(record(20,v=>{u32(v,0,0x07064b50);u64(v,8,endOffset);u32(v,16,1)}))
   await write(record(22,v=>{u32(v,0,0x06054b50);v.setUint16(8,65535,true);v.setUint16(10,65535,true);u32(v,12,0xffffffff);u32(v,16,0xffffffff)}))
   await perform(()=>index.abort());check();await perform(()=>output.close());complete=true;return{entries:count,bytes:offset}
  },abort,
  snapshot:()=>({offset,count,indexBytes,retired,complete,entryBytes:entry?.size||0})
 }
}
