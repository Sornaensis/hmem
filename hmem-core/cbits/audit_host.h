#ifndef HMEM_AUDIT_HOST_H
#define HMEM_AUDIT_HOST_H

#include <stddef.h>
#include <stdint.h>

/* This bridge measures a proven local PostgreSQL 18 host. Callers must obtain
 * the proof and directory list from the fixed audit-only database diagnostic,
 * never from a command-line filename. No function launches or signals a process.
 * An owner is single-threaded and must outlive every call using it. */
enum hmem_audit_host_result {
  HMEM_AUDIT_HOST_OK = 0,
  HMEM_AUDIT_HOST_UNSUPPORTED = 1,
  HMEM_AUDIT_HOST_INVALID = 2,
  HMEM_AUDIT_HOST_ENDPOINT = 3,
  HMEM_AUDIT_HOST_PROCESS = 4,
  HMEM_AUDIT_HOST_IDENTITY = 5,
  HMEM_AUDIT_HOST_SID = 6,
  HMEM_AUDIT_HOST_PATH = 7,
  HMEM_AUDIT_HOST_VOLUME = 8,
  HMEM_AUDIT_HOST_CHANGED = 9,
  HMEM_AUDIT_HOST_MEMORY = 10,
  HMEM_AUDIT_HOST_LIMIT = 11
};

typedef struct hmem_audit_host hmem_audit_host;

/* Start times are SQL timestamps in microseconds since the Unix epoch. The
 * first two directories are always data_directory and its fixed pg_wal child;
 * audit_directories adds at most 254 fixed audit/tablespace directories.
 * Failure sets *owner to NULL and closes every capability acquired here. */
int hmem_audit_host_open(
    const char *server_address, uint16_t server_port,
    const char *client_address, uint16_t client_port,
    uint32_t backend_pid, uint32_t postmaster_pid,
    int64_t backend_start_us, int64_t postmaster_start_us,
    const char *data_directory, const char *system_identifier,
    const char *const *audit_directories, uint32_t audit_directory_count,
    hmem_audit_host **owner);

/* Revalidate the retained process/connection/path identities, then refresh
 * quota-sensitive free space. Any failure invalidates all cached readings. */
int hmem_audit_host_refresh(hmem_audit_host *owner);
uint32_t hmem_audit_host_directory_count(const hmem_audit_host *owner);
uint32_t hmem_audit_host_volume_count(const hmem_audit_host *owner);
int hmem_audit_host_directory_volume(const hmem_audit_host *owner, uint32_t directory);
int hmem_audit_host_volume_read(const hmem_audit_host *owner, uint32_t volume,
    uint64_t *available, uint64_t *total_free);
int hmem_audit_host_volume_key(const hmem_audit_host *owner, uint32_t volume,
    char *output, size_t capacity);
void hmem_audit_host_close(hmem_audit_host *owner);

#ifdef HMEM_AUDIT_HOST_TEST
int hmem_audit_host_test_same_logon(uint64_t current, uint64_t first, uint64_t second);
#endif

#endif
