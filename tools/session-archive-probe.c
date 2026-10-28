/* Standalone codec feasibility probe; production database/browser gates are separate. */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <time.h>
#include <zlib.h>
#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#define seek64 _fseeki64
#define tell64 _ftelli64
#else
#include <sys/resource.h>
#define seek64 fseeko
#define tell64 ftello
#endif

#define BUF 65536u
#define DICT 32768u
#define SPAN UINT64_C(1048576)
typedef struct { uint64_t raw, compressed; uint32_t bits, length; unsigned char dictionary[DICT]; } Checkpoint;
static const char *prefix="{\"event\":\"tool_result\",\"text\":\"Transcript UTF-8: \xc3\xa6\xc3\xb8\xc3\xa5; CRLF follows. ";
static int kind;
static void die(const char *why) { fprintf(stderr,"probe failed: %s\n",why); exit(2); }
static void check(int truth,const char *why) { if(!truth)die(why); }
static uint64_t mix(uint64_t value) { value^=value>>30;value*=UINT64_C(0xbf58476d1ce4e5b9);value^=value>>27;value*=UINT64_C(0x94d049bb133111eb);return value^(value>>31); }
static unsigned char byte_at(uint64_t offset) {
  if(kind) return (unsigned char)(mix(offset/8) >> ((offset%8)*8));
  unsigned position=(unsigned)(offset%1024), length=(unsigned)strlen(prefix);
  if(position<length)return (unsigned char)prefix[position];
  if(position==1020)return '"';if(position==1021)return '}';if(position==1022)return '\r';if(position==1023)return '\n';
  /* Each 32-record group shares recent history; record identifiers vary. */
  if(position<length+16)return "0123456789abcdef"[(mix(offset/1024) >> ((position-length)%16)*4)&15];
  return "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789"[(mix((offset/32768)*128+(position/8)) >> ((position%8)*8))%64];
}
static void fill(unsigned char *bytes,size_t count,uint64_t offset) { for(size_t n=0;n<count;n++)bytes[n]=byte_at(offset+n); }
static int matches(const unsigned char *bytes,size_t count,uint64_t offset) { for(size_t n=0;n<count;n++)if(bytes[n]!=byte_at(offset+n))return 0;return 1; }
static double now(void) {
#ifdef _WIN32
  LARGE_INTEGER counter,frequency;QueryPerformanceCounter(&counter);QueryPerformanceFrequency(&frequency);return(double)counter.QuadPart/(double)frequency.QuadPart;
#else
  struct timespec value;timespec_get(&value,TIME_UTC);return(double)value.tv_sec+value.tv_nsec/1e9;
#endif
}
static uint64_t peak_memory(void) {
#ifdef _WIN32
  PROCESS_MEMORY_COUNTERS counters;check(GetProcessMemoryInfo(GetCurrentProcess(),&counters,sizeof counters),"memory probe");return(uint64_t)counters.PeakWorkingSetSize;
#else
  struct rusage usage;check(getrusage(RUSAGE_SELF,&usage)==0,"memory probe");return(uint64_t)usage.ru_maxrss*1024;
#endif
}
static FILE *open_file(const char *name,const char *mode) { FILE *file=fopen(name,mode);check(file!=NULL,"open task scratch file");return file; }
static void write_bytes(FILE *file,const void *bytes,size_t length) { check(fwrite(bytes,1,length,file)==length,"write task scratch file"); }
static uint64_t compress_file(const char *name,uint64_t length,unsigned source_chunk,int block) {
  FILE *file=open_file(name,"wb");z_stream stream={0};unsigned char input[BUF],output[BUF];uint64_t at=0,written=0;
  check(deflateInit2(&stream,6,Z_DEFLATED,31,8,Z_DEFAULT_STRATEGY)==Z_OK,"gzip compressor init");
  gz_header header={0};header.os=255;check(deflateSetHeader(&stream,&header)==Z_OK,"fixed gzip metadata");
  for(;;) {
    unsigned take=(unsigned)((length-at)<source_chunk?(length-at):source_chunk);if(take>BUF)take=BUF;
    uint64_t until=SPAN-at%SPAN;if(block && (uint64_t)take>until)take=(unsigned)until;
    fill(input,take,at);stream.next_in=input;stream.avail_in=take;at+=take;
    int flush=at==length?Z_FINISH:(block && at%SPAN==0?Z_BLOCK:Z_NO_FLUSH),result;
    do {stream.next_out=output;stream.avail_out=BUF;result=deflate(&stream,flush);check(result==Z_OK||result==Z_STREAM_END,"deflate step");unsigned produced=BUF-stream.avail_out;write_bytes(file,output,produced);written+=produced;}while(stream.avail_in || stream.avail_out==0 || (flush==Z_FINISH && result!=Z_STREAM_END));
    if(result==Z_STREAM_END)break;
  }
  check(at==length,"compressor input count");check(deflateEnd(&stream)==Z_OK,"compressor end");check(fclose(file)==0,"compress file close");return written;
}
typedef struct { uint64_t count, nonaligned, maximum_gap, bytes, dictionary_bytes; } IndexStats;
static IndexStats index_file(const char *name,const char *index_name,uint64_t expected,unsigned compressed_chunk) {
  FILE *file=open_file(name,"rb"),*index=open_file(index_name,"wb");z_stream stream={0};unsigned char input[BUF],output[BUF];uint64_t read=0,raw=0,last=0;IndexStats stats={0};int result=Z_OK;
  check(inflateInit2(&stream,31)==Z_OK,"gzip decoder init");
  while(result!=Z_STREAM_END) {
    if(stream.avail_in==0) {unsigned count=(unsigned)fread(input,1,compressed_chunk,file);check(count>0,"truncated gzip");stream.next_in=input;stream.avail_in=count;read+=count;}
    stream.next_out=output;stream.avail_out=BUF;unsigned before=stream.avail_in;result=inflate(&stream,Z_BLOCK);unsigned produced=BUF-stream.avail_out;
    check(result==Z_OK||result==Z_STREAM_END,"gzip full verification");check(raw+produced<=expected && matches(output,produced,raw),"full decoded bytes differ");raw+=produced;
    if((stream.data_type&128) && !(stream.data_type&64) && (stats.count==0||raw-last>=SPAN)) {
      Checkpoint checkpoint={0};checkpoint.raw=raw;checkpoint.compressed=read-stream.avail_in;checkpoint.bits=(uint32_t)(stream.data_type&7);uInt dictionary_length=0;
      check(inflateGetDictionary(&stream,checkpoint.dictionary,&dictionary_length)==Z_OK,"get restart dictionary");checkpoint.length=dictionary_length;check(dictionary_length<=DICT,"dictionary bound");
      check(stats.count==0 ? raw==0 : raw>last,"checkpoint order");if(raw-last>stats.maximum_gap)stats.maximum_gap=raw-last;last=raw;
      if(checkpoint.bits)stats.nonaligned++;stats.dictionary_bytes+=checkpoint.length;write_bytes(index,&checkpoint,sizeof checkpoint);stats.count++;
    }
    check(produced||stream.avail_in!=before||(stream.data_type&128)||result==Z_STREAM_END,"gzip decoder no progress");
  }
  check(raw==expected,"full decoded length");check(stream.avail_in==0 && fgetc(file)==EOF,"gzip has extra member/trailing bytes");
  if(raw-last>stats.maximum_gap)stats.maximum_gap=raw-last;check(stats.maximum_gap<=SPAN,"checkpoint raw gap exceeds bound");
  check(inflateEnd(&stream)==Z_OK,"gzip decoder end");stats.bytes=stats.count*sizeof(Checkpoint);check(fclose(file)==0&&fclose(index)==0,"index files close");return stats;
}
static Checkpoint read_checkpoint(FILE *index,uint64_t n) {Checkpoint checkpoint;check(n<=INT64_MAX/sizeof checkpoint,"checkpoint position bound");check(seek64(index,(int64_t)(n*sizeof checkpoint),SEEK_SET)==0,"index seek");check(fread(&checkpoint,sizeof checkpoint,1,index)==1,"index read");return checkpoint;}
/* A raw stream restarts after the gzip header; absolute byte and bit offsets are retained. */
static int read_range(const char *name,const Checkpoint *checkpoint,uint64_t start,uint64_t length,int bad_dictionary,uint64_t *work) {
  FILE *file=open_file(name,"rb");z_stream stream={0};unsigned char input[BUF],output[BUF],dictionary[DICT];uint64_t raw=checkpoint->raw,remaining=length;int result=Z_OK,correct=1;
  check(start>=raw && checkpoint->bits<=7 && checkpoint->length<=DICT,"range checkpoint validity");check(checkpoint->compressed<=INT64_MAX,"compressed offset bound");
  check(inflateInit2(&stream,-15)==Z_OK,"raw restart init");check(seek64(file,(int64_t)checkpoint->compressed-(checkpoint->bits?1:0),SEEK_SET)==0,"compressed seek");
  if(checkpoint->bits){int previous=fgetc(file);check(previous!=EOF,"prime byte");check(inflatePrime(&stream,(int)checkpoint->bits,previous>>(8-checkpoint->bits))==Z_OK,"prime restart bits");}
  memcpy(dictionary,checkpoint->dictionary,checkpoint->length);if(bad_dictionary)memset(dictionary,0,checkpoint->length);
  if(checkpoint->length)check(inflateSetDictionary(&stream,dictionary,checkpoint->length)==Z_OK,"install dictionary");
  while(remaining && result!=Z_STREAM_END) {
    if(stream.avail_in==0){unsigned count=(unsigned)fread(input,1,BUF-13,file);if(count==0){correct=0;break;}stream.next_in=input;stream.avail_in=count;}
    uint64_t needed=raw<start?(start-raw)+remaining:remaining;unsigned capacity=(unsigned)(needed<BUF?needed:BUF);stream.next_out=output;stream.avail_out=capacity;result=inflate(&stream,Z_NO_FLUSH);unsigned produced=capacity-stream.avail_out;
    if(result!=Z_OK&&result!=Z_STREAM_END){correct=0;break;}
    *work+=produced;uint64_t skip=start>raw?start-raw:0;if(skip>produced)skip=produced;unsigned visible=produced-(unsigned)skip;
    if(!matches(output+skip,visible,raw+skip))correct=0;remaining-=visible;raw+=produced;
  }
  if(remaining)correct=0;check(inflateEnd(&stream)==Z_OK,"raw decoder end");check(fclose(file)==0,"range close");return correct;
}
/* Consecutive tiny pages keep one decoder state. Output never exceeds one requested page. */
static uint64_t sequential_pages(const char *name,uint64_t expected) {
  FILE *file=open_file(name,"rb");z_stream stream={0};unsigned char input[BUF],output[4096];uint64_t raw=0,pages=0;int result=Z_OK;check(inflateInit2(&stream,31)==Z_OK,"sequential init");
  while(result!=Z_STREAM_END){if(stream.avail_in==0){unsigned count=(unsigned)fread(input,1,BUF,file);check(count>0,"sequential truncated");stream.next_in=input;stream.avail_in=count;}stream.next_out=output;stream.avail_out=sizeof output;result=inflate(&stream,Z_NO_FLUSH);unsigned produced=(unsigned)sizeof output-stream.avail_out;check(result==Z_OK||result==Z_STREAM_END,"sequential decode");check(raw+produced<=expected&&matches(output,produced,raw),"sequential page differs");raw+=produced;if(produced)pages++;}
  check(raw==expected&&stream.avail_in==0&&fgetc(file)==EOF,"sequential boundary");check(inflateEnd(&stream)==Z_OK&&fclose(file)==0,"sequential cleanup");return pages;
}
static int same_files(const char *left,const char *right) {
  FILE *a=open_file(left,"rb"),*b=open_file(right,"rb");unsigned char x[BUF],y[BUF];int equal=1;for(;;){size_t n=fread(x,1,BUF,a),m=fread(y,1,BUF,b);if(n!=m||memcmp(x,y,n)){equal=0;break;}if(n==0)break;}check(fclose(a)==0&&fclose(b)==0,"comparison close");return equal;
}
int main(int argc,char **argv) {
  check(argc==5,"usage: codec_probe size source_chunk jsonl|random output_prefix");uint64_t length=strtoull(argv[1],NULL,10);unsigned chunk=(unsigned)strtoul(argv[2],NULL,10);check(chunk>0&&chunk<=BUF&&length<=UINT64_C(1073741824),"probe input bounds");kind=strcmp(argv[3],"random")==0;
  char archive[1024],index_name[1024],alternate[1024];int path_length=snprintf(archive,sizeof archive,"%s.gz",argv[4]);check(path_length>0&&(size_t)path_length<sizeof archive,"archive path");path_length=snprintf(index_name,sizeof index_name,"%s.idx",argv[4]);check(path_length>0&&(size_t)path_length<sizeof index_name,"index path");path_length=snprintf(alternate,sizeof alternate,"%s.alternate.gz",argv[4]);check(path_length>0&&(size_t)path_length<sizeof alternate,"alternate path");
  double started=now(),compress_started=started;uint64_t compressed=compress_file(archive,length,chunk,1);double compress_seconds=now()-compress_started;
  double verify_started=now();IndexStats stats=index_file(archive,index_name,length,length<UINT64_C(1048576)?17:BUF-15);double verify_seconds=now()-verify_started;
  FILE *index=open_file(index_name,"rb");uint64_t seek_work=0,seeks=0,bad_rejected=0,bad_bits_rejected=0;double seek_started=now();
  for(uint64_t n=0;n<stats.count;n++){Checkpoint checkpoint=read_checkpoint(index,n);uint64_t available=length-checkpoint.raw;if(!available)continue;uint64_t start=checkpoint.raw+(available>513?513:0),count=length-start;if(count>8193)count=8193;check(read_range(archive,&checkpoint,start,count,0,&seek_work),"checkpoint seek differs");seeks++;if(checkpoint.length&&!kind&&bad_rejected==0){uint64_t ignored=0;if(!read_range(archive,&checkpoint,start,count,1,&ignored))bad_rejected++;}if(checkpoint.bits&&bad_bits_rejected==0){Checkpoint bad=checkpoint;uint64_t ignored=0;bad.bits^=1;if(!read_range(archive,&bad,start,count,0,&ignored))bad_bits_rejected++;}}
  check(fclose(index)==0,"checkpoint index close");if(length>SPAN&&!kind){check(stats.nonaligned>0,"no non-byte aligned restart covered");check(bad_rejected>0,"dictionary corruption never detected");check(bad_bits_rejected>0,"wrong restart bits never detected");}double seek_seconds=now()-seek_started;
  double sequential_started=now();uint64_t pages=sequential_pages(archive,length);double sequential_seconds=now()-sequential_started;
  /* Arithmetic-only guards: Windows zlib uLong may be32bit; authoritative offsets are64bit. */
  uint64_t beyond=UINT64_C(4294967296)+SPAN+17;check((uint32_t)beyond==SPAN+17,"gzip ISIZE modulo test");check((int64_t)beyond>INT32_MAX,"64bit cursor arithmetic");
  int source_invariant=-1;if(length<=2*SPAN+17){uint64_t other=compress_file(alternate,length,chunk==65521?1021:65521,1);check(other==compressed&&same_files(archive,alternate),"source chunk boundaries changed gzip bytes");check(remove(alternate)==0,"alternate gzip cleanup");source_invariant=1;}
  printf("{\"kind\":\"%s\",\"rawBytes\":%"PRIu64",\"sourceChunkBytes\":%u,\"compressedBytes\":%"PRIu64",\"checkpointCount\":%"PRIu64",\"checkpointBytes\":%"PRIu64",\"checkpointDictionaryBytes\":%"PRIu64",\"nonalignedCheckpoints\":%"PRIu64",\"maxRawCheckpointGap\":%"PRIu64",\"seekCount\":%"PRIu64",\"seekDecodedBytes\":%"PRIu64",\"wrongDictionaryRejected\":%"PRIu64",\"wrongBitsRejected\":%"PRIu64",\"sourceChunkInvariant\":%s,\"sequentialPages\":%"PRIu64",\"sequentialDecoderInitializations\":1,\"compressSeconds\":%.6f,\"verifyIndexSeconds\":%.6f,\"seekSeconds\":%.6f,\"sequentialSeconds\":%.6f,\"totalSeconds\":%.6f,\"peakWorkingSetBytes\":%"PRIu64",\"headerVersion\":\"%s\",\"runtimeVersion\":\"%s\",\"zStreamSize\":%zu,\"uLongSize\":%zu,\"arithmeticAbove4GiB\":true,\"actualAbove4GiBFixture\":false}\n",kind?"random":"jsonl",length,chunk,compressed,stats.count,stats.bytes,stats.dictionary_bytes,stats.nonaligned,stats.maximum_gap,seeks,seek_work,bad_rejected,bad_bits_rejected,source_invariant==1?"true":"null",pages,compress_seconds,verify_seconds,seek_seconds,sequential_seconds,now()-started,peak_memory(),ZLIB_VERSION,zlibVersion(),sizeof(z_stream),sizeof(uLong));
  check(remove(archive)==0&&remove(index_name)==0,"owned temporary archive cleanup");return 0;
}
