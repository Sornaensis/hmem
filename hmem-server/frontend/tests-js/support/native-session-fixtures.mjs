// Synthetic non-secret values in native persisted event envelopes. Schema
// provenance and supported input boundaries: docs/reference/session-explorer.md.
export const copilotRecords = [
  {id:'c0',parentId:null,type:'session.start',timestamp:'2026-10-30T10:00:00.000Z',data:{sessionId:'synthetic',version:1,context:{cwd:'/example'}}},
  {id:'c1',parentId:'c0',type:'user.message',timestamp:'2026-10-30T10:00:01.000Z',data:{content:'Inspect café λ😀'}},
  {id:'c2',parentId:'c1',type:'assistant.message',timestamp:'2026-10-30T10:00:02.000Z',data:{messageId:'m1',content:'I will inspect it.',reasoningText:'Check the source first.',toolRequests:[{toolCallId:'call-1',name:'read_file',arguments:{path:'café.txt'}}]}},
  {id:'c3',parentId:'c2',type:'tool.execution_start',timestamp:'2026-10-30T10:00:03.000Z',data:{toolCallId:'call-1',toolName:'read_file',arguments:{path:'café.txt'}}},
  {id:'c4',parentId:'c3',type:'tool.execution_complete',timestamp:'2026-10-30T10:00:04.000Z',data:{toolCallId:'call-1',success:true,result:{content:'hello λ😀',detailedContent:'complete hello λ😀'}}},
  {id:'c5',parentId:'c4',type:'tool.execution_complete',timestamp:'2026-10-30T10:00:05.000Z',data:{toolCallId:'call-error',success:false,error:{message:'Missing file',code:'ENOENT'}}},
  {id:'c6',parentId:'c5',type:'assistant.message',timestamp:'2026-10-30T10:00:06.000Z',data:{messageId:'m2',content:'Inspection complete.'}},
]

// pi v1.4.0, persisted session schema v3. Deliberate alternate parents and a
// later root exercise source-history semantics without selecting a branch.
export const piRecords = [
  {type:'session',version:3,id:'synthetic-pi',timestamp:'2026-10-30T10:10:00.000Z',cwd:'/example'},
  {type:'message',id:'p1',parentId:null,timestamp:'2026-10-30T10:10:01.000Z',message:{role:'user',content:'Inspect pi café λ😀',timestamp:1793355001000}},
  {type:'model_change',id:'p2',parentId:'p1',timestamp:'2026-10-30T10:10:02.000Z',provider:'synthetic',modelId:'not-authoritative'},
  {type:'message',id:'p3',parentId:'p2',timestamp:'2026-10-30T10:10:03.000Z',message:{role:'assistant',content:[{type:'thinking',thinking:'Check pi input.'},{type:'thinking',thinking:'redacted-secret',redacted:true,thinkingSignature:'opaque-secret'},{type:'toolCall',id:'pi-call',name:'read',arguments:{path:'pi.txt'}},{type:'text',text:'Reading pi input.'}],timestamp:1793355003000}},
  {type:'message',id:'p4',parentId:'p3',timestamp:'2026-10-30T10:10:04.000Z',message:{role:'toolResult',toolCallId:'pi-call',toolName:'read',content:[{type:'text',text:'pi tool output λ😀'}],isError:false,timestamp:1793355004000}},
  {type:'message',id:'p5',parentId:'p1',timestamp:'2026-10-30T10:10:05.000Z',message:{role:'assistant',content:[{type:'text',text:'Alternate pi answer.'}],timestamp:1793355005000}},
  {type:'compaction',id:'p6',parentId:'p5',timestamp:'2026-10-30T10:10:06.000Z',summary:'Earlier pi context summarized.',firstKeptEntryId:'p3',tokensBefore:100},
  {type:'branch_summary',id:'p7',parentId:'p6',timestamp:'2026-10-30T10:10:07.000Z',summary:'Abandoned branch summarized.',fromId:'p4'},
  {type:'custom',id:'p8',parentId:'p7',timestamp:'2026-10-30T10:10:08.000Z',customType:'extension-state',data:{secret:'hidden-extension-state'}},
  {type:'custom_message',id:'p9',parentId:'p8',timestamp:'2026-10-30T10:10:09.000Z',customType:'visible-extension',display:true,content:'Visible pi custom message.'},
  {type:'custom_message',id:'p10',parentId:'p9',timestamp:'2026-10-30T10:10:10.000Z',customType:'hidden-extension',display:false,content:'hidden-custom-message'},
  {type:'message',id:'p11',parentId:'p10',timestamp:'2026-10-30T10:10:11.000Z',message:{role:'bashExecution',command:'printf hello',output:'visible bash output',exitCode:0,cancelled:false,truncated:false,excludeFromContext:true,timestamp:1793355011000}},
  {type:'context_edit',id:'p12',parentId:'p11',timestamp:'2026-10-30T10:10:12.000Z',targetId:'p3',replacement:null},
  {type:'message',id:'p13',parentId:null,timestamp:'2026-10-30T10:10:13.000Z',message:{role:'assistant',content:[{type:'text',text:'Later pi root.'}],timestamp:1793355013000}},
]
