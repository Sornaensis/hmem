import assert from 'node:assert/strict'
import test from 'node:test'
import {spawnSync} from 'node:child_process'
import {mkdtemp,writeFile,rm} from 'node:fs/promises'
import {tmpdir} from 'node:os'
import {join} from 'node:path'
import {createHash} from 'node:crypto'
import {createZip64Writer,evidenceEntryName} from '../src/session-zip.js'
import {createEvidenceWriter} from '../src/session-save.js'
const digest=bytes=>createHash('sha256').update(bytes).digest('hex')
export function verifyZip(path,expected){
 const code=`import sys,json,zipfile,hashlib\np=sys.argv[1]\nexpected=json.loads(sys.stdin.buffer.read().decode('utf-8'))\nwith zipfile.ZipFile(p) as archive:\n assert archive.testzip() is None\n names=archive.namelist()\n assert len(names)==len(set(names))==len(expected),(len(names),len(expected))\n for name,receipt in expected.items():\n  assert name in names,name\n  data=archive.read(name)\n  assert len(data)==receipt['size']\n  assert hashlib.sha256(data).hexdigest()==receipt['hash'],name\nprint('verified',len(expected),'entries')`
 const result=spawnSync('python',['-c',code,path],{input:JSON.stringify(expected),encoding:'utf8',windowsHide:true});assert.equal(result.status,0,result.stderr||result.stdout)
}
const receipt=(id,bytes)=>({id,workspace:'w',state:'terminated',revision:1,finalBytes:bytes.length,finalSha256:digest(bytes)})
test('ZIP64 STORE round trips exact Unicode, empty, CRLF and newline-free evidence with independent reader',async()=>{
 const scratch=await mkdtemp(join(tmpdir(),'zip64-'));let parts=[],central=[];const expected={}
 const zip=createZip64Writer({async write(bytes){parts.push(bytes)},async close(){},async abort(){parts=[]}},{async write(bytes){central.push(bytes)},async *readChunks(){yield*central},async abort(){central=[]}})
 try{for(const[i,bytes]of[Buffer.alloc(0),Buffer.from('{"λ":"café"}\r\n'),Buffer.from('[true,null]'),Buffer.from(JSON.stringify('x'.repeat(140000))+'\n')].entries()){
  const row=receipt('20000000-0000-4000-8000-'+String(i).padStart(12,'0'),bytes),name=evidenceEntryName({id:row.id,name:'../Same / café:name'});assert.ok(!name.includes('/')&&!name.startsWith('.'));expected[name]={size:bytes.length,hash:digest(bytes)}
  const writer=createEvidenceWriter(await zip.entry(name,bytes.length),row)
  if(!bytes.length)await writer.write({session:row,offset:0,nextOffset:0,dataBase64:''})
  for(let offset=0;offset<bytes.length;offset+=65536){const part=bytes.subarray(offset,offset+65536);await writer.write({session:row,offset,nextOffset:offset+part.length,dataBase64:part.toString('base64')})}
 }
 await zip.finish();const target=join(scratch,'evidence.zip');await writeFile(target,Buffer.concat(parts));verifyZip(target,expected)
 }finally{await rm(scratch,{recursive:true,force:true})}
})
test('archive abort retires writes and propagates staging cleanup failure',async()=>{
 let aborted=0
 const zip=createZip64Writer({async write(){},async close(){assert.fail('emitted partial archive')},async abort(){aborted++}},{async write(){},async *readChunks(){},async abort(){throw Error('quota cleanup')}})
 await assert.rejects(zip.abort(),/writer_cleanup_failed/);assert.equal(aborted,1);await assert.rejects(zip.entry('x',0),/retired_archive/)
})
