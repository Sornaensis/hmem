/* MPL-2.0. Owned helper: no shell, private pipes, death-on-owner. */
#ifndef _WIN32
#define _GNU_SOURCE
#else
#define _WIN32_WINNT 0x0A00
#endif
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpragma-pack"
#endif
#include <windows.h>
#include <tlhelp32.h>
#ifdef __clang__
#pragma clang diagnostic pop
#endif
#ifndef PROC_THREAD_ATTRIBUTE_JOB_LIST
#define PROC_THREAD_ATTRIBUTE_JOB_LIST 0x0002000D
#endif
typedef struct { HANDLE process, job, input, output; DWORD pid; int waited; } session_process;
static volatile LONG owners = 0;
int hmem_session_process_count(void) { return InterlockedCompareExchange(&owners,0,0); }
static void close_handle(HANDLE *h) { if (*h && *h != INVALID_HANDLE_VALUE) CloseHandle(*h); *h=NULL; }
int hmem_session_process_wait(session_process *p, int ms) {
  if (p->waited) return 1;
  DWORD result=WaitForSingleObject(p->process,(DWORD)ms);
  if (result==WAIT_OBJECT_0) { p->waited=1; return 1; }
  return result==WAIT_TIMEOUT ? 0 : -1;
}
int hmem_session_process_kill(session_process *p) {
  return p->waited || TerminateJobObject(p->job,1) || GetLastError()==ERROR_ACCESS_DENIED;
}
int hmem_session_process_destroy(session_process *p) {
  if (!p || !p->waited) return 0;
  close_handle(&p->input); close_handle(&p->output);
  close_handle(&p->process); close_handle(&p->job); free(p);
  InterlockedDecrement(&owners); return 1;
}
static int spawn_impl(const char *path, session_process **out, int fail_attributes) {
  HANDLE child_in=NULL,child_out=NULL,child_err=NULL,parent_in=NULL,parent_out=NULL,job=NULL;
  PROCESS_INFORMATION pi={0}; SECURITY_ATTRIBUTES sa={sizeof(sa),NULL,TRUE};
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits={0}; STARTUPINFOEXW startup={0};
  LPPROC_THREAD_ATTRIBUTE_LIST attrs=NULL; SIZE_T attr_size=0;
  session_process *p=NULL; wchar_t *wide=NULL,*command=NULL; int result=0,attrs_initialized=0;
  *out=NULL;
  int n=MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,path,-1,NULL,0);
  if (!n) goto done;
  wide=calloc((size_t)n,sizeof(wchar_t)); command=calloc((size_t)n+64,sizeof(wchar_t));
  p=calloc(1,sizeof(*p));
  if (!wide || !command || !p || !MultiByteToWideChar(CP_UTF8,MB_ERR_INVALID_CHARS,path,-1,wide,n)) goto done;
  command[0]=L'"'; memcpy(command+1,wide,((size_t)n-1)*sizeof(wchar_t)); command[n]=L'"';
  wcscpy(command+n+1,L" --session-http-helper +RTS -N1 -RTS");
  if (!CreatePipe(&child_in,&parent_in,&sa,0) || !CreatePipe(&parent_out,&child_out,&sa,0)) goto done;
  child_err=CreateFileW(L"NUL",GENERIC_WRITE,FILE_SHARE_READ|FILE_SHARE_WRITE,&sa,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
  if (child_err==INVALID_HANDLE_VALUE || !SetHandleInformation(parent_in,HANDLE_FLAG_INHERIT,0)
      || !SetHandleInformation(parent_out,HANDLE_FLAG_INHERIT,0)) goto done;
  job=CreateJobObjectW(NULL,NULL); if (!job) goto done;
  limits.BasicLimitInformation.LimitFlags=JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE|JOB_OBJECT_LIMIT_ACTIVE_PROCESS;
  limits.BasicLimitInformation.ActiveProcessLimit=1;
  if (!SetInformationJobObject(job,JobObjectExtendedLimitInformation,&limits,sizeof(limits))) goto done;
  InitializeProcThreadAttributeList(NULL,2,0,&attr_size); attrs=malloc(attr_size);
  if (!attrs || fail_attributes || !InitializeProcThreadAttributeList(attrs,2,0,&attr_size)) goto done;
  attrs_initialized=1;
  HANDLE inherited[3]={child_in,child_out,child_err};
  if (!UpdateProcThreadAttribute(attrs,0,PROC_THREAD_ATTRIBUTE_HANDLE_LIST,inherited,sizeof(inherited),NULL,NULL)
      || !UpdateProcThreadAttribute(attrs,0,PROC_THREAD_ATTRIBUTE_JOB_LIST,&job,sizeof(job),NULL,NULL)) goto done;
  startup.StartupInfo.cb=sizeof(startup); startup.StartupInfo.dwFlags=STARTF_USESTDHANDLES;
  startup.StartupInfo.hStdInput=child_in; startup.StartupInfo.hStdOutput=child_out; startup.StartupInfo.hStdError=child_err;
  startup.lpAttributeList=attrs;
  if (!CreateProcessW(wide,command,NULL,NULL,TRUE,EXTENDED_STARTUPINFO_PRESENT|CREATE_SUSPENDED|CREATE_NO_WINDOW,
                      NULL,NULL,&startup.StartupInfo,&pi)) goto done;
  p->process=pi.hProcess; pi.hProcess=NULL; p->job=job; job=NULL;
  p->input=parent_in; parent_in=NULL; p->output=parent_out; parent_out=NULL; p->pid=pi.dwProcessId;
  InterlockedIncrement(&owners);
  if (ResumeThread(pi.hThread)==(DWORD)-1) {
    hmem_session_process_kill(p);
    if (hmem_session_process_wait(p,2000)==1) { hmem_session_process_destroy(p); p=NULL; }
    else { *out=p; p=NULL; result=-1; }
  } else { *out=p; p=NULL; result=1; }
done:
  free(p); close_handle(&pi.hThread); close_handle(&pi.hProcess);
  close_handle(&child_in); close_handle(&child_out); close_handle(&child_err);
  close_handle(&parent_in); close_handle(&parent_out); close_handle(&job);
  if (attrs_initialized) DeleteProcThreadAttributeList(attrs);
  free(attrs);
  free(wide); free(command); return result;
}
int hmem_session_process_spawn(const char *path, session_process **out) { return spawn_impl(path,out,0); }
int hmem_session_process_attribute_probe(const char *path) {
  DWORD before=0,after=0; session_process *out=NULL;
  if (!GetProcessHandleCount(GetCurrentProcess(),&before)) return 0;
  int result=spawn_impl(path,&out,1);
  return result==0 && out==NULL && GetProcessHandleCount(GetCurrentProcess(),&after) && after==before;
}
int hmem_session_process_write(session_process *p,const unsigned char *bytes,int size) {
  DWORD n=0; return WriteFile(p->input,bytes,(DWORD)size,&n,NULL) ? (int)n : -1;
}
int hmem_session_process_read(session_process *p,unsigned char *bytes,int size) {
  DWORD n=0; if (ReadFile(p->output,bytes,(DWORD)size,&n,NULL)) return (int)n;
  return GetLastError()==ERROR_BROKEN_PIPE ? 0 : -1;
}
int hmem_session_process_close_input(session_process *p) { close_handle(&p->input); return 1; }
int hmem_session_process_pid(session_process *p) { return (int)p->pid; }
int hmem_session_process_alive(int pid) {
  HANDLE p=OpenProcess(SYNCHRONIZE,FALSE,(DWORD)pid); if (!p) return 0;
  DWORD state=WaitForSingleObject(p,0); CloseHandle(p); return state==WAIT_TIMEOUT;
}
int hmem_session_process_kill_pid(int pid) {
  HANDLE p=OpenProcess(PROCESS_TERMINATE,FALSE,(DWORD)pid); if (!p) return 0;
  int ok=TerminateProcess(p,1)!=0; CloseHandle(p); return ok;
}
int hmem_session_process_children(int parent,int *ids,int cap) {
  HANDLE snapshot=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0); if (snapshot==INVALID_HANDLE_VALUE) return -1;
  PROCESSENTRY32 entry={0}; entry.dwSize=sizeof(entry); int n=0;
  if (Process32First(snapshot,&entry)) do {
    if (entry.th32ParentProcessID==(DWORD)parent && n<cap) ids[n++]=(int)entry.th32ProcessID;
  } while (Process32Next(snapshot,&entry));
  CloseHandle(snapshot); return n;
}
int hmem_session_process_image(int pid,char *bytes,int cap) {
  HANDLE process=OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION,FALSE,(DWORD)pid);
  if (!process) return 0;
  DWORD size=32768; wchar_t *wide=malloc((size_t)size*sizeof(wchar_t)); int result=0;
  if (wide && QueryFullProcessImageNameW(process,0,wide,&size))
    result=WideCharToMultiByte(CP_UTF8,WC_ERR_INVALID_CHARS,wide,(int)size,bytes,cap-1,NULL,NULL);
  if (result>0) bytes[result]=0;
  free(wide); CloseHandle(process); return result;
}
void hmem_session_child_probe_block(void) { Sleep(30000); }
int hmem_session_child_watch_parent(void) {
  BOOL in_job=FALSE; JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits={0};
  return IsProcessInJob(GetCurrentProcess(),NULL,&in_job) && in_job
      && QueryInformationJobObject(NULL,JobObjectExtendedLimitInformation,&limits,sizeof(limits),NULL)
      && (limits.BasicLimitInformation.LimitFlags & JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE)
      && limits.BasicLimitInformation.ActiveProcessLimit==1;
}
#else
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/resource.h>
#include <sys/prctl.h>
#include <time.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
typedef struct { pid_t pid; int input,output,owner,waited; } session_process;
static volatile int owners=0;
int hmem_session_process_count(void) { return __sync_add_and_fetch(&owners,0); }
static void close_fd(int *fd) { if (*fd>=0) close(*fd); *fd=-1; }
static void close_child_fd(int fd) { if (fd>3) close(fd); }
static void close_other_fds(void) {
#ifdef SYS_close_range
  if (syscall(SYS_close_range,4u,~0u,0u)==0) return;
#endif
  struct rlimit limit; if (getrlimit(RLIMIT_NOFILE,&limit)) _exit(126);
  for (unsigned long fd=4;fd<limit.rlim_cur;fd++) close((int)fd);
}
int hmem_session_process_kill(session_process *p) {
  if (p->waited) return 1;
  int group=kill(-p->pid,SIGKILL), exact=kill(p->pid,SIGKILL);
  return group==0 || exact==0 || errno==ESRCH;
}
int hmem_session_process_wait(session_process *p,int ms) {
  if (p->waited) return 1;
  for (int elapsed=0;elapsed<=ms;elapsed++) {
    pid_t result=waitpid(p->pid,NULL,WNOHANG);
    if (result==p->pid) { p->waited=1; return 1; }
    if (result<0 && errno!=EINTR) return -1;
    struct timespec pause={0,1000000}; nanosleep(&pause,NULL);
  }
  return 0;
}
int hmem_session_process_destroy(session_process *p) {
  if (!p || !p->waited) return 0;
  close_fd(&p->input); close_fd(&p->output); close_fd(&p->owner); free(p);
  __sync_sub_and_fetch(&owners,1); return 1;
}
int hmem_session_process_spawn(const char *path,session_process **out) {
  int input[2]={-1,-1},output[2]={-1,-1},owner[2]={-1,-1},nullfd=-1;
  session_process *p=NULL; pid_t child,creator=getpid(); *out=NULL;
  if (pipe2(input,O_CLOEXEC) || pipe2(output,O_CLOEXEC) || pipe2(owner,O_CLOEXEC)) goto failure;
  nullfd=open("/dev/null",O_WRONLY|O_CLOEXEC); p=calloc(1,sizeof(*p)); if (nullfd<0 || !p) goto failure;
  child=fork(); if (child<0) goto failure;
  if (!child) {
    char *const args[]={(char *)path,"--session-http-helper","+RTS","-N1","-RTS",NULL};
    if (prctl(PR_SET_PDEATHSIG,SIGKILL)!=0 || getppid()!=creator || setpgid(0,0)!=0) _exit(126);
    if (dup2(input[0],0)<0 || dup2(output[1],1)<0 || dup2(nullfd,2)<0 || dup2(owner[0],3)<0) _exit(126);
    close_child_fd(input[0]); close_child_fd(input[1]); close_child_fd(output[0]); close_child_fd(output[1]);
    close_child_fd(owner[0]); close_child_fd(owner[1]); close_child_fd(nullfd); close_other_fds();
    execv(path,args); _exit(127);
  }
  p->pid=child; p->input=input[1]; input[1]=-1; p->output=output[0]; output[0]=-1;
  p->owner=owner[1]; owner[1]=-1; __sync_add_and_fetch(&owners,1);
  if ((setpgid(child,child)<0 && errno!=EACCES && errno!=ESRCH) || getpgid(child)!=child) {
    hmem_session_process_kill(p);
    if (hmem_session_process_wait(p,2000)==1) { hmem_session_process_destroy(p); p=NULL; }
    else { *out=p; p=NULL; }
    goto failure;
  }
  close_fd(&input[0]); close_fd(&output[1]); close_fd(&owner[0]); close_fd(&nullfd);
  *out=p; return 1;
failure:
  free(p); close_fd(&input[0]); close_fd(&input[1]); close_fd(&output[0]); close_fd(&output[1]);
  close_fd(&owner[0]); close_fd(&owner[1]); close_fd(&nullfd); return *out ? -1 : 0;
}
static int pipe_write(int fd,const unsigned char *bytes,int size) {
  sigset_t blocked,previous,pending; sigemptyset(&blocked); sigaddset(&blocked,SIGPIPE);
  if (pthread_sigmask(SIG_BLOCK,&blocked,&previous)) return -1;
  sigpending(&pending); int had=sigismember(&pending,SIGPIPE); ssize_t n=write(fd,bytes,(size_t)size);
  if (n<0 && errno==EPIPE && !had) { struct timespec zero={0,0}; sigtimedwait(&blocked,NULL,&zero); }
  pthread_sigmask(SIG_SETMASK,&previous,NULL); return n<0 ? -1 : (int)n;
}
int hmem_session_process_write(session_process *p,const unsigned char *bytes,int size) { return pipe_write(p->input,bytes,size); }
int hmem_session_process_read(session_process *p,unsigned char *bytes,int size) { ssize_t n=read(p->output,bytes,(size_t)size); return n<0 ? -1 : (int)n; }
int hmem_session_process_close_input(session_process *p) { close_fd(&p->input); return 1; }
int hmem_session_process_pid(session_process *p) { return (int)p->pid; }
int hmem_session_process_alive(int pid) { return kill((pid_t)pid,0)==0 || errno==EPERM; }
int hmem_session_process_kill_pid(int pid) { return kill((pid_t)pid,SIGKILL)==0; }
int hmem_session_process_children(int parent,int *ids,int cap) {
  DIR *directory=opendir("/proc"); if (!directory) return -1; struct dirent *entry; int n=0;
  while ((entry=readdir(directory)) && n<cap) {
    char *end; long pid=strtol(entry->d_name,&end,10); if (*end || pid<=0) continue;
    char path[128],line[4096],state; int ppid;
    snprintf(path,sizeof(path),"/proc/%ld/stat",pid); FILE *file=fopen(path,"r"); if (!file) continue;
    char *read=fgets(line,sizeof(line),file); fclose(file); if (!read) continue;
    char *tail=strrchr(line,')'); if (tail && sscanf(tail+1," %c %d",&state,&ppid)==2 && ppid==parent) ids[n++]=(int)pid;
  }
  closedir(directory); return n;
}
int hmem_session_process_image(int pid,char *bytes,int cap) {
  char path[128]; snprintf(path,sizeof(path),"/proc/%d/exe",pid);
  ssize_t size=readlink(path,bytes,(size_t)cap-1);
  if (size<=0 || size>=cap-1) return 0;
  bytes[size]=0; return (int)size;
}
void hmem_session_child_probe_block(void) { struct timespec pause={30,0}; while (nanosleep(&pause,&pause)<0 && errno==EINTR) {} }
static void *watch_owner(void *unused) {
  unsigned char byte; (void)unused;
  for (;;) { ssize_t n=read(3,&byte,1); if (!n || (n<0 && errno!=EINTR)) { kill(-getpid(),SIGKILL); _exit(125); } }
}
int hmem_session_child_watch_parent(void) {
  pthread_t thread; if (getpgrp()!=getpid() || pthread_create(&thread,NULL,watch_owner,NULL)) return 0;
  pthread_detach(thread); return 1;
}
int hmem_session_process_attribute_probe(const char *path) { (void)path; return 2; }
#endif
