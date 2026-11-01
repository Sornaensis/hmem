#include "audit_cancel.h"
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>

/* libpq's portable header does not mark this baseline declaration dllimport.
 * Without it, &PQlibVersion may identify an executable linker thunk. The
 * qualified import reads the loader's actual IAT binding, without code walking
 * or choosing another library. This old API exists on supported old clients. */
__declspec(dllimport) int PQlibVersion(void);

typedef int (*version_function)(void);
typedef void *(*create_function)(void *);
typedef int (*start_function)(void *);
typedef int (*poll_function)(void *);
typedef int (*socket_function)(const void *);
typedef int (*status_function)(const void *);
typedef void (*finish_function)(void *);
typedef void (*reset_function)(void *);

struct hmem_audit_cancel {
    HMODULE module;
    void *connection;
    create_function create;
    start_function start;
    poll_function poll;
    socket_function socket;
    status_function status;
    finish_function finish;
    reset_function reset;
    int phase;
    int descriptor;
    int started;
};
#ifdef HMEM_AUDIT_CANCEL_TEST
static unsigned int test_created, test_finished;
unsigned int hmem_audit_cancel_test_created(void) { return test_created; }
unsigned int hmem_audit_cancel_test_finished(void) { return test_finished; }
#endif

/* Resolve only within the module that actually implements the already-linked
 * baseline function. Holding its reference also protects every function pointer
 * until the independent cancellation object has been finished. */
static void *linked_version_address(void) {
    version_function function = PQlibVersion;
    void *address = NULL;
    if (sizeof(function) == sizeof(address)) memcpy(&address, &function, sizeof(address));
    return address;
}
#ifdef HMEM_AUDIT_CANCEL_TEST
void *hmem_audit_cancel_test_linked_address(void) { return linked_version_address(); }
#endif

static int resolve(hmem_audit_cancel *owner, int *version) {
    void *address = linked_version_address();
    FARPROC symbol;
    version_function get_version;
    if (!address || !version) return 2;
    *version = 0;
    if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                           (LPCWSTR)address, &owner->module)) return 2;
    symbol = GetProcAddress(owner->module, "PQlibVersion");
    if (!symbol || sizeof(symbol) != sizeof(address) ||
        memcmp(&symbol, &address, sizeof(address)) != 0) return 2;
    memcpy(&get_version, &symbol, sizeof(get_version));
    *version = get_version();
    if (*version < 170000) return 1;
#define RESOLVE(field, name) do { \
    symbol = GetProcAddress(owner->module, name); \
    if (!symbol || sizeof(owner->field) != sizeof(symbol)) return 3; \
    memcpy(&owner->field, &symbol, sizeof(owner->field)); \
} while (0)
    RESOLVE(create, "PQcancelCreate");
    RESOLVE(start, "PQcancelStart");
    RESOLVE(poll, "PQcancelPoll");
    RESOLVE(socket, "PQcancelSocket");
    RESOLVE(status, "PQcancelStatus");
    RESOLVE(finish, "PQcancelFinish");
    RESOLVE(reset, "PQcancelReset");
#undef RESOLVE
    return 0;
}

int hmem_audit_cancel_capability(int *version) {
    hmem_audit_cancel owner;
    int result;
    memset(&owner, 0, sizeof(owner));
    result = resolve(&owner, version);
    if (owner.module) FreeLibrary(owner.module);
    return result;
}

int hmem_audit_cancel_open(void *connection, hmem_audit_cancel **output) {
    hmem_audit_cancel *owner;
    int version, result;
    if (!output) return 2;
    *output = NULL;
    if (!connection) return 2;
    owner = (hmem_audit_cancel *)calloc(1, sizeof(*owner));
    if (!owner) return 4;
    result = resolve(owner, &version);
    if (!result) {
        owner->connection = owner->create(connection);
        if (owner->connection) {
#ifdef HMEM_AUDIT_CANCEL_TEST
            test_created++;
#endif
            /* CONNECTION_ALLOCATED is the stable libpq17+ public enum value.
             * A non-null CONNECTION_BAD object still requires PQcancelFinish. */
            if (owner->status(owner->connection) != 14) result = 4;
        } else result = 4;
    }
    if (result) { hmem_audit_cancel_close(owner); return result; }
    owner->descriptor = -1;
    *output = owner;
    return 0;
}

int hmem_audit_cancel_start(hmem_audit_cancel *owner) {
    if (!owner || !owner->connection || owner->started) return 7;
    owner->started = 1;
    if (!owner->start(owner->connection)) return 5;
    owner->phase = 2; /* PGRES_POLLING_WRITING, before the first poll. */
    owner->descriptor = owner->socket(owner->connection);
    return 0;
}

int hmem_audit_cancel_poll(hmem_audit_cancel *owner, int wait_ms) {
    WSAPOLLFD descriptor;
    int ready;
    if (!owner || !owner->connection || !owner->started ||
        wait_ms < 0 || wait_ms > 100) return -7;
    if (owner->phase == 3) return 1;
    if (owner->phase != 1 && owner->phase != 2) return -6;
    if (owner->descriptor >= 0) {
        memset(&descriptor, 0, sizeof(descriptor));
        descriptor.fd = (SOCKET)(unsigned int)owner->descriptor;
        descriptor.events = owner->phase == 1 ? POLLRDNORM : POLLWRNORM;
        ready = WSAPoll(&descriptor, 1, wait_ms);
        if (ready == 0) return 0;
        if (ready == SOCKET_ERROR) return -6;
    }
    owner->phase = owner->poll(owner->connection);
    /* libpq may replace its socket at every poll, including TLS negotiation. */
    owner->descriptor = owner->socket(owner->connection);
    if (owner->phase == 3) return 1;
    if (owner->phase == 1 || owner->phase == 2) return 0;
    return -6;
}

void hmem_audit_cancel_finish(hmem_audit_cancel *owner) {
    if (owner && owner->connection) {
        owner->finish(owner->connection);
#ifdef HMEM_AUDIT_CANCEL_TEST
        test_finished++;
#endif
        owner->connection = NULL;
        owner->descriptor = -1;
    }
}

int hmem_audit_cancel_reset(hmem_audit_cancel *owner) {
    if (!owner || !owner->connection) return 7;
    owner->reset(owner->connection);
    owner->started = 0;
    owner->phase = 0;
    owner->descriptor = -1;
    return owner->status(owner->connection) == 14 ? 0 : 8;
}

void hmem_audit_cancel_close(hmem_audit_cancel *owner) {
    if (!owner) return;
    hmem_audit_cancel_finish(owner);
    if (owner->module) FreeLibrary(owner->module);
    free(owner);
}
#else
/* Existing Unix/libpq15 builds do not link any modern cancellation symbol. */
struct hmem_audit_cancel { int unused; };
int hmem_audit_cancel_capability(int *version) {
    if (version) *version = 0; return 1;
}
int hmem_audit_cancel_open(void *connection, hmem_audit_cancel **output) {
    (void)connection; if (output) *output = NULL; return 1;
}
int hmem_audit_cancel_start(hmem_audit_cancel *owner) { (void)owner; return 1; }
int hmem_audit_cancel_poll(hmem_audit_cancel *owner, int wait_ms) {
    (void)owner; (void)wait_ms; return -1;
}
int hmem_audit_cancel_reset(hmem_audit_cancel *owner) { (void)owner; return 1; }
void hmem_audit_cancel_finish(hmem_audit_cancel *owner) { (void)owner; }
void hmem_audit_cancel_close(hmem_audit_cancel *owner) { (void)owner; }
#endif
