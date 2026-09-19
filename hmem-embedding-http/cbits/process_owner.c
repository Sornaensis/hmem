/* MPL-2.0: the child is placed in its death-on-owner scope before readiness. */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define _WIN32_WINNT 0x0A00
#include <windows.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int test_env(const char *name, char *buffer, size_t capacity) {
#ifdef _WIN32
    DWORD size = GetEnvironmentVariableA(name, buffer, (DWORD)capacity);
    return size > 0 && size < capacity;
#else
    const char *value = getenv(name);
    if (!value || strlen(value) >= capacity) return 0;
    memcpy(buffer, value, strlen(value) + 1);
    return 1;
#endif
}

int hmem_test_spawn_knob_visible(void) {
    char value[16];
    return test_env("HMEM_HTTP_TEST_SPAWN_DELAY_MS", value, sizeof(value)) &&
           strcmp(value, "400") == 0;
}

static int test_executable(const char *path) {
    const char *slash = strrchr(path, '/');
    const char *backslash = strrchr(path, '\\');
    const char *name = slash;
    if (backslash && (!name || backslash > name)) name = backslash;
    return strstr(name ? name + 1 : path, "hmem-embedding-http-test") ==
           (name ? name + 1 : path);
}

static int test_spawn_delay(const char *path, int pid) {
    if (!test_executable(path)) return 0;
    char marker[2048], value[16];
    if (test_env("HMEM_HTTP_TEST_SPAWN_PID_FILE", marker, sizeof(marker))) {
        FILE *file = fopen(marker, "w");
        if (file) { fprintf(file, "%d\n", pid); fclose(file); }
    }
    if (!test_env("HMEM_HTTP_TEST_SPAWN_DELAY_MS", value, sizeof(value))) return 0;
    int delay = atoi(value);
    return delay > 0 && delay <= 1000 ? delay : 0;
}

#ifdef _WIN32
#include <windows.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#ifndef PROC_THREAD_ATTRIBUTE_JOB_LIST
#define PROC_THREAD_ATTRIBUTE_JOB_LIST 0x0002000D
#endif

typedef struct hmem_process {
    HANDLE process, job, input, output, errors;
    DWORD pid;
    int waited;
    int test_reap_delay_ms;
} hmem_process;

static int test_reap_delay(const char *path) {
    char value[16];
    if (!test_executable(path) ||
        !test_env("HMEM_HTTP_TEST_REAP_DELAY_MS", value, sizeof(value))) return 0;
    int delay = atoi(value);
    return delay > 0 && delay <= 1000 ? delay : 0;
}

static void close_handle(HANDLE *handle) {
    if (*handle && *handle != INVALID_HANDLE_VALUE) CloseHandle(*handle);
    *handle = NULL;
}

static wchar_t *wide_utf8(const char *text) {
    int count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, NULL, 0);
    if (!count) return NULL;
    wchar_t *wide = (wchar_t *)calloc((size_t)count, sizeof(wchar_t));
    if (!wide || !MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                     text, -1, wide, count)) {
        free(wide); return NULL;
    }
    return wide;
}

int hmem_process_spawn(const char *path, hmem_process **out) {
    HANDLE child_in = NULL, child_out = NULL, child_err = NULL;
    HANDLE parent_in = NULL, parent_out = NULL, parent_err = NULL, job = NULL;
    LPPROC_THREAD_ATTRIBUTE_LIST attributes = NULL;
    SIZE_T attribute_size = 0;
    PROCESS_INFORMATION pi = {0};
    SECURITY_ATTRIBUTES sa = {sizeof(sa), NULL, TRUE};
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = {0};
    STARTUPINFOEXW startup = {0};
    wchar_t *wide = NULL, *command = NULL;
    hmem_process *owned = NULL;
    HANDLE inherited[3];
    DWORD flags = EXTENDED_STARTUPINFO_PRESENT | CREATE_SUSPENDED | CREATE_NO_WINDOW;
    int ok = 0;
    *out = NULL;
    wide = wide_utf8(path);
    if (!wide) goto cleanup;
    size_t n = wcslen(wide);
    command = (wchar_t *)calloc(n + 3, sizeof(wchar_t));
    if (!command) goto cleanup;
    command[0] = L'"'; memcpy(command + 1, wide, n * sizeof(wchar_t));
    command[n + 1] = L'"';
    if (!CreatePipe(&child_in, &parent_in, &sa, 0) ||
        !CreatePipe(&parent_out, &child_out, &sa, 0) ||
        !CreatePipe(&parent_err, &child_err, &sa, 0)) goto cleanup;
    if (!SetHandleInformation(parent_in, HANDLE_FLAG_INHERIT, 0) ||
        !SetHandleInformation(parent_out, HANDLE_FLAG_INHERIT, 0) ||
        !SetHandleInformation(parent_err, HANDLE_FLAG_INHERIT, 0)) goto cleanup;
    job = CreateJobObjectW(NULL, NULL);
    if (!job) goto cleanup;
    limits.BasicLimitInformation.LimitFlags =
        JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE | JOB_OBJECT_LIMIT_ACTIVE_PROCESS;
    limits.BasicLimitInformation.ActiveProcessLimit = 1;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation,
                                 &limits, sizeof(limits))) goto cleanup;
    InitializeProcThreadAttributeList(NULL, 2, 0, &attribute_size);
    attributes = (LPPROC_THREAD_ATTRIBUTE_LIST)malloc(attribute_size);
    if (!attributes || !InitializeProcThreadAttributeList(attributes, 2, 0,
                                                           &attribute_size)) goto cleanup;
    inherited[0] = child_in; inherited[1] = child_out; inherited[2] = child_err;
    if (!UpdateProcThreadAttribute(attributes, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                                   inherited, sizeof(inherited), NULL, NULL) ||
        !UpdateProcThreadAttribute(attributes, 0, PROC_THREAD_ATTRIBUTE_JOB_LIST,
                                   &job, sizeof(job), NULL, NULL)) goto cleanup;
    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = child_in;
    startup.StartupInfo.hStdOutput = child_out;
    startup.StartupInfo.hStdError = child_err;
    startup.lpAttributeList = attributes;
    if (!CreateProcessW(wide, command, NULL, NULL, TRUE, flags, NULL, NULL,
                        &startup.StartupInfo, &pi)) goto cleanup;
    close_handle(&child_in); close_handle(&child_out); close_handle(&child_err);
    owned = (hmem_process *)calloc(1, sizeof(*owned));
    if (!owned) goto cleanup;
    owned->process = pi.hProcess; owned->job = job;
    owned->input = parent_in; owned->output = parent_out; owned->errors = parent_err;
    owned->pid = pi.dwProcessId;
    owned->test_reap_delay_ms = test_reap_delay(path);
    pi.hProcess = NULL; job = parent_in = parent_out = parent_err = NULL;
    if (ResumeThread(pi.hThread) == (DWORD)-1) goto cleanup;
    int test_delay = test_spawn_delay(path, (int)owned->pid);
    if (test_delay) Sleep((DWORD)test_delay);
    *out = owned; owned = NULL; ok = 1;
cleanup:
    if (owned) {
        TerminateJobObject(owned->job, 1);
        WaitForSingleObject(owned->process, INFINITE);
        close_handle(&owned->input); close_handle(&owned->output);
        close_handle(&owned->errors); close_handle(&owned->process);
        close_handle(&owned->job); free(owned);
    }
    if (pi.hProcess) {
        if (job) TerminateJobObject(job, 1);
        else TerminateProcess(pi.hProcess, 1);
        WaitForSingleObject(pi.hProcess, INFINITE);
    }
    close_handle(&pi.hThread); close_handle(&pi.hProcess);
    close_handle(&child_in); close_handle(&child_out); close_handle(&child_err);
    close_handle(&parent_in); close_handle(&parent_out); close_handle(&parent_err);
    close_handle(&job);
    if (attributes) { DeleteProcThreadAttributeList(attributes); free(attributes); }
    free(wide); free(command);
    return ok;
}

int hmem_process_write(hmem_process *p, const unsigned char *bytes, int size) {
    DWORD written = 0;
    return WriteFile(p->input, bytes, (DWORD)size, &written, NULL) ? (int)written : -1;
}
int hmem_process_read_out(hmem_process *p, unsigned char *bytes, int size) {
    DWORD read_count = 0;
    if (ReadFile(p->output, bytes, (DWORD)size, &read_count, NULL)) return (int)read_count;
    return GetLastError() == ERROR_BROKEN_PIPE ? 0 : -1;
}
int hmem_process_read_err(hmem_process *p, unsigned char *bytes, int size) {
    DWORD read_count = 0;
    if (ReadFile(p->errors, bytes, (DWORD)size, &read_count, NULL)) return (int)read_count;
    return GetLastError() == ERROR_BROKEN_PIPE ? 0 : -1;
}
int hmem_process_close_input(hmem_process *p) { close_handle(&p->input); return 1; }
int hmem_process_kill(hmem_process *p) {
    if (p->waited) return 1;
    return TerminateJobObject(p->job, 1) || GetLastError() == ERROR_ACCESS_DENIED;
}
int hmem_process_wait(hmem_process *p, int milliseconds) {
    if (p->waited) return 1;
    DWORD result = WaitForSingleObject(p->process, (DWORD)milliseconds);
    if (result == WAIT_OBJECT_0) {
        if (p->test_reap_delay_ms) Sleep((DWORD)p->test_reap_delay_ms);
        p->waited = 1; return 1;
    }
    return result == WAIT_TIMEOUT ? 0 : -1;
}
int hmem_process_pid(hmem_process *p) { return (int)p->pid; }
int hmem_process_id_alive(int pid) {
    HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!process) return 0;
    DWORD state = WaitForSingleObject(process, 0);
    CloseHandle(process);
    return state == WAIT_TIMEOUT;
}
void hmem_process_destroy(hmem_process *p) {
    if (!p) return;
    close_handle(&p->input); close_handle(&p->output); close_handle(&p->errors);
    close_handle(&p->process); close_handle(&p->job); free(p);
}
int hmem_child_watch_parent(void) { return 1; }

#else
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/resource.h>
#include <sys/prctl.h>
#include <time.h>
#include <unistd.h>

typedef struct hmem_process {
    pid_t pid;
    int input, output, errors, owner;
    int waited;
    int test_reap_delay_ms;
} hmem_process;

static int test_reap_delay(const char *path) {
    char value[16];
    if (!test_executable(path) ||
        !test_env("HMEM_HTTP_TEST_REAP_DELAY_MS", value, sizeof(value))) return 0;
    int delay = atoi(value);
    return delay > 0 && delay <= 1000 ? delay : 0;
}

static void close_fd(int *fd) { if (*fd >= 0) close(*fd); *fd = -1; }
static void close_child_fd(int fd) { if (fd > 3) close(fd); }
static void close_all_other_fds(void) {
#ifdef SYS_close_range
    if (syscall(SYS_close_range, 4u, ~0u, 0u) == 0) return;
#endif
    struct rlimit limit;
    if (getrlimit(RLIMIT_NOFILE, &limit)) _exit(126);
    for (unsigned long fd = 4; fd < limit.rlim_cur; fd++) close((int)fd);
}

int hmem_process_spawn(const char *path, hmem_process **out) {
    int input[2] = {-1,-1}, output[2] = {-1,-1};
    int errors[2] = {-1,-1}, owner[2] = {-1,-1};
    hmem_process *p = NULL;
    pid_t pid;
    pid_t creator_pid = getpid();
    const char *test_hold = getenv("HMEM_HTTP_TEST_STOP_PRE_EXEC");
    int stop_pre_exec = strstr(path, "hmem-embedding-http-test") != NULL &&
                        test_hold != NULL && strcmp(test_hold, "1") == 0;
    *out = NULL;
    if (pipe2(input, O_CLOEXEC) || pipe2(output, O_CLOEXEC) ||
        pipe2(errors, O_CLOEXEC) || pipe2(owner, O_CLOEXEC)) goto failure;
    p = (hmem_process *)calloc(1, sizeof(*p));
    if (!p) goto failure;
    pid = fork();
    if (pid < 0) goto failure;
    if (pid == 0) {
        char *const argv[] = {(char *)path, NULL};
        /* The Haskell owner runs in a bound OS thread until reap. Linux sends
         * this signal when that exact creator thread dies, even before exec.
         * The parent-PID check closes the fork-to-prctl registration race. */
        if (prctl(PR_SET_PDEATHSIG, SIGKILL) != 0 || getppid() != creator_pid ||
            setpgid(0, 0) != 0) _exit(126);
        if (stop_pre_exec) raise(SIGSTOP);
        if (dup2(input[0], 0) < 0 || dup2(output[1], 1) < 0 ||
            dup2(errors[1], 2) < 0 || dup2(owner[0], 3) < 0) _exit(126);
        close_child_fd(input[0]); close_child_fd(input[1]);
        close_child_fd(output[0]); close_child_fd(output[1]);
        close_child_fd(errors[0]); close_child_fd(errors[1]);
        close_child_fd(owner[0]); close_child_fd(owner[1]);
        close_all_other_fds();
        execv(path, argv);
        _exit(127);
    }
    if (setpgid(pid, pid) < 0 && errno != EACCES && errno != ESRCH) {
        kill(pid, SIGKILL); waitpid(pid, NULL, 0); goto failure;
    }
    if (getpgid(pid) != pid) {
        kill(pid, SIGKILL); waitpid(pid, NULL, 0); goto failure;
    }
    p->pid = pid; p->input = input[1]; p->output = output[0];
    p->errors = errors[0]; p->owner = owner[1]; p->waited = 0;
    p->test_reap_delay_ms = test_reap_delay(path);
    close(input[0]); close(output[1]); close(errors[1]); close(owner[0]);
    int test_delay = test_spawn_delay(path, (int)pid);
    if (test_delay) {
        struct timespec delay = { test_delay / 1000,
                                  (test_delay % 1000) * 1000000L };
        nanosleep(&delay, NULL);
    }
    *out = p;
    return 1;
failure:
    if (p) free(p);
    close_fd(&input[0]); close_fd(&input[1]);
    close_fd(&output[0]); close_fd(&output[1]);
    close_fd(&errors[0]); close_fd(&errors[1]);
    close_fd(&owner[0]); close_fd(&owner[1]);
    return 0;
}

static int pipe_write_no_sigpipe(int fd, const unsigned char *bytes, int size) {
    sigset_t blocked, previous, pending;
    sigemptyset(&blocked); sigaddset(&blocked, SIGPIPE);
    if (pthread_sigmask(SIG_BLOCK, &blocked, &previous)) return -1;
    sigpending(&pending);
    int already_pending = sigismember(&pending, SIGPIPE);
    ssize_t wrote = write(fd, bytes, (size_t)size);
    if (wrote < 0 && errno == EPIPE && !already_pending) {
        struct timespec zero = {0,0};
        sigtimedwait(&blocked, NULL, &zero);
    }
    pthread_sigmask(SIG_SETMASK, &previous, NULL);
    return wrote < 0 ? -1 : (int)wrote;
}
int hmem_process_write(hmem_process *p, const unsigned char *bytes, int size) {
    return pipe_write_no_sigpipe(p->input, bytes, size);
}
int hmem_process_read_out(hmem_process *p, unsigned char *bytes, int size) {
    ssize_t n = read(p->output, bytes, (size_t)size); return n < 0 ? -1 : (int)n;
}
int hmem_process_read_err(hmem_process *p, unsigned char *bytes, int size) {
    ssize_t n = read(p->errors, bytes, (size_t)size); return n < 0 ? -1 : (int)n;
}
int hmem_process_close_input(hmem_process *p) { close_fd(&p->input); return 1; }
int hmem_process_kill(hmem_process *p) {
    if (p->waited) return 1;
    int group = kill(-p->pid, SIGKILL);
    int exact = kill(p->pid, SIGKILL);
    return group == 0 || exact == 0 || errno == ESRCH;
}
int hmem_process_wait(hmem_process *p, int milliseconds) {
    if (p->waited) return 1;
    int elapsed = 0;
    while (elapsed <= milliseconds) {
        pid_t result = waitpid(p->pid, NULL, WNOHANG);
        if (result == p->pid) {
            if (p->test_reap_delay_ms) {
                struct timespec delay = { p->test_reap_delay_ms / 1000,
                                          (p->test_reap_delay_ms % 1000) * 1000000L };
                nanosleep(&delay, NULL);
            }
            p->waited = 1; return 1;
        }
        if (result < 0) return -1;
        struct timespec delay = {0, 1000000};
        nanosleep(&delay, NULL); elapsed++;
    }
    return 0;
}
int hmem_process_pid(hmem_process *p) { return (int)p->pid; }
int hmem_process_id_alive(int pid) {
    return kill((pid_t)pid, 0) == 0 || errno == EPERM;
}
void hmem_process_destroy(hmem_process *p) {
    if (!p) return;
    close_fd(&p->input); close_fd(&p->output); close_fd(&p->errors);
    close_fd(&p->owner); free(p);
}

static void *watch_owner(void *unused) {
    unsigned char byte;
    (void)unused;
    for (;;) {
        ssize_t n = read(3, &byte, 1);
        if (n == 0 || (n < 0 && errno != EINTR)) {
            kill(-getpid(), SIGKILL);
            _exit(125);
        }
    }
}
int hmem_child_watch_parent(void) {
    pthread_t thread;
    if (getpgrp() != getpid()) return 0;
    if (pthread_create(&thread, NULL, watch_owner, NULL)) return 0;
    pthread_detach(thread);
    return 1;
}
#endif
