/* Native audit-host evidence. This does not reserve disk space: the owning
 * runner must keep the capability, resample during work and retain ownership
 * after an unconfirmed native/database drain. */
#include "audit_host.h"

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <iphlpapi.h>
#include <tlhelp32.h>
#include <winioctl.h>
#include <winternl.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <errno.h>
#include <limits.h>

#define AUDIT_MAX_DIRECTORIES 256u
#define AUDIT_MAX_TCP_BYTES (8u * 1024u * 1024u)
#define AUDIT_MAX_PROCESS_ROWS 16384u
#define AUDIT_MAX_TOKEN_BYTES 65536u
#define AUDIT_MAX_PATH 32760u
#define AUDIT_STARTUP_US INT64_C(60000000)
#define AUDIT_MAX_LINKS 4096u

struct audit_directory {
  wchar_t *path;
  HANDLE handle;
  BY_HANDLE_FILE_INFORMATION identity;
  uint32_t volume;
};
struct audit_volume {
  wchar_t key[64];
  uint64_t available;
  uint64_t total_free;
};
struct hmem_audit_host {
  HANDLE backend;
  HANDLE postmaster;
  DWORD backend_pid;
  DWORD postmaster_pid;
  int64_t backend_start_us;
  int64_t postmaster_start_us;
  uint64_t system_identifier;
  int family;
  unsigned char server_address[16];
  unsigned char client_address[16];
  uint16_t server_port;
  uint16_t client_port;
  uint32_t directory_count;
  uint32_t volume_count;
  int valid;
  uint32_t link_count;
  HANDLE links[AUDIT_MAX_LINKS];
  BY_HANDLE_FILE_INFORMATION link_identities[AUDIT_MAX_LINKS];
  wchar_t drive_bindings[26][128];
  struct audit_directory directories[AUDIT_MAX_DIRECTORIES];
  struct audit_volume volumes[AUDIT_MAX_DIRECTORIES];
};

static wchar_t *wide_path(const char *input) {
  int count;
  wchar_t *output;
  if (!input) return NULL;
  count = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, input, -1, NULL, 0);
  if (count < 4 || count > (int)AUDIT_MAX_PATH) return NULL;
  output = malloc((size_t)count * sizeof(wchar_t));
  if (!output) return NULL;
  if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, input, -1, output, count)) {
    free(output); return NULL;
  }
  for (int i = 0; i < count; i++) if (output[i] == L'/') output[i] = L'\\';
  /* UNC/network paths cannot establish local volume/quota evidence. */
  if (!(((output[0] >= L'A' && output[0] <= L'Z') ||
         (output[0] >= L'a' && output[0] <= L'z')) &&
        output[1] == L':' && output[2] == L'\\')) {
    free(output); return NULL;
  }
  return output;
}

static wchar_t *child_path(const wchar_t *parent, const wchar_t *child) {
  size_t a = wcslen(parent), b = wcslen(child);
  wchar_t *output;
  if (a + b + 2 > AUDIT_MAX_PATH) return NULL;
  output = malloc((a + b + 2) * sizeof(wchar_t));
  if (!output) return NULL;
  memcpy(output, parent, a * sizeof(wchar_t));
  output[a] = L'\\';
  memcpy(output + a + 1, child, (b + 1) * sizeof(wchar_t));
  return output;
}

static int directory_info(HANDLE handle, BY_HANDLE_FILE_INFORMATION *identity) {
  return GetFileType(handle) == FILE_TYPE_DISK &&
    GetFileInformationByHandle(handle, identity) &&
    (identity->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
}

static int same_file(const BY_HANDLE_FILE_INFORMATION *a,
                     const BY_HANDLE_FILE_INFORMATION *b) {
  return a->dwVolumeSerialNumber == b->dwVolumeSerialNumber &&
    a->nFileIndexHigh == b->nFileIndexHigh && a->nFileIndexLow == b->nFileIndexLow;
}

struct audit_reparse_data {
  DWORD tag;
  WORD length, reserved;
  union {
    struct { WORD offset, size, print_offset, print_size; wchar_t path[1]; } mount;
    struct { WORD offset, size, print_offset, print_size; DWORD flags; wchar_t path[1]; } link;
  } value;
};

/* Resolve reparse points before following them. A local-looking pg_wal or
 * tablespace junction can otherwise enter a remote filesystem in CreateFile,
 * before a later GetDriveType check could reject it. Only fixed local drives
 * and drive-letter junction/symlink targets are supported. Unknown tags and
 * volume-GUID-only reparse targets defer rather than guessing their authority. */
typedef NTSTATUS (NTAPI *audit_nt_create_file)(PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES,
  PIO_STATUS_BLOCK, PLARGE_INTEGER, ULONG, ULONG, ULONG, ULONG, PVOID, ULONG);

static HANDLE open_relative(HANDLE parent, const wchar_t *name, int directory, int fence) {
  audit_nt_create_file create_file = (audit_nt_create_file)(void *)GetProcAddress(
    GetModuleHandleW(L"ntdll.dll"), "NtCreateFile");
  UNICODE_STRING unicode;
  OBJECT_ATTRIBUTES attributes;
  IO_STATUS_BLOCK status;
  HANDLE handle = NULL;
  size_t length = wcslen(name);
  if (!create_file || !length || length > 32766) return INVALID_HANDLE_VALUE;
  unicode.Length = (USHORT)(length * sizeof(wchar_t));
  unicode.MaximumLength = unicode.Length;
  unicode.Buffer = (PWSTR)name;
  memset(&attributes, 0, sizeof(attributes));
  attributes.Length = sizeof(attributes); attributes.RootDirectory = parent;
  attributes.ObjectName = &unicode; attributes.Attributes = 0x40; /* OBJ_CASE_INSENSITIVE */
  /* FILE_OPEN, synchronous/nonalert, OPEN_REPARSE_POINT, and DIRECTORY_FILE
   * where required. Excluding WRITE and DELETE sharing fences reparse mutation
   * and rename for the actual component, including original junction objects. */
  NTSTATUS result = create_file(&handle,
    (directory ? FILE_LIST_DIRECTORY | FILE_READ_ATTRIBUTES : GENERIC_READ) | SYNCHRONIZE,
    &attributes, &status, NULL, FILE_ATTRIBUTE_NORMAL,
    fence ? FILE_SHARE_READ : FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
    1, 0x20 | 0x00200000 | (directory ? 1 : 0), NULL, 0);
  return result >= 0 ? handle : INVALID_HANDLE_VALUE;
}

static HANDLE retain_link(hmem_audit_host *owner, HANDLE handle) {
  BY_HANDLE_FILE_INFORMATION identity;
  wchar_t filesystem[32];
  /* BY_HANDLE_FILE_INFORMATION's 64-bit file index is a qualified identity on
   * NTFS. Do not admit filesystems such as ReFS where it is not sufficient. */
  if (!GetVolumeInformationByHandleW(handle, NULL, 0, NULL, NULL, NULL, filesystem, 32) ||
      _wcsicmp(filesystem, L"NTFS")) {
    CloseHandle(handle); return INVALID_HANDLE_VALUE;
  }
  if (!GetFileInformationByHandle(handle, &identity)) {
    CloseHandle(handle); return INVALID_HANDLE_VALUE;
  }
  for (uint32_t i = 0; i < owner->link_count; i++) {
    if (same_file(&identity, &owner->link_identities[i])) {
      CloseHandle(handle); return owner->links[i];
    }
  }
  if (owner->link_count >= AUDIT_MAX_LINKS) {
    CloseHandle(handle); return INVALID_HANDLE_VALUE;
  }
  owner->links[owner->link_count] = handle;
  owner->link_identities[owner->link_count++] = identity;
  return handle;
}

static int drive_binding(hmem_audit_host *owner, wchar_t letter, wchar_t *root) {
  wchar_t drive[3] = { letter, L':', 0 }, target[128];
  size_t length;
  int index;
  if (letter >= L'a' && letter <= L'z') letter -= L'a' - L'A';
  index = letter - L'A';
  if (index < 0 || index >= 26 || !QueryDosDeviceW(drive, target, 128)) return 0;
  /* A drive alias is inspected before any filesystem open. Bypass the DOS
   * alias afterward, opening this physical local device in the NT namespace. */
  const wchar_t *prefix = L"\\Device\\HarddiskVolume";
  length = wcslen(prefix);
  if (wcsncmp(target, prefix, length) || !target[length]) return 0;
  for (size_t i = length; target[i]; i++) if (target[i] < L'0' || target[i] > L'9') return 0;
  if (owner->drive_bindings[index][0] && _wcsicmp(owner->drive_bindings[index], target)) return 0;
  wcscpy(owner->drive_bindings[index], target);
  wcscpy(root, target); wcscat(root, L"\\");
  return 1;
}

static HANDLE open_local_path(hmem_audit_host *owner, const wchar_t *input, int want_directory) {
  wchar_t path[AUDIT_MAX_PATH], component[AUDIT_MAX_PATH], replacement[AUDIT_MAX_PATH], root[128];
  DWORD length = GetFullPathNameW(input, AUDIT_MAX_PATH, path, NULL);
  uint32_t reparse_count = 0;
  if (!length || length >= AUDIT_MAX_PATH) return INVALID_HANDLE_VALUE;
restart:
  if (length < 3 || path[1] != L':' || path[2] != L'\\' ||
      !((path[0] >= L'A' && path[0] <= L'Z') || (path[0] >= L'a' && path[0] <= L'z')))
    return INVALID_HANDLE_VALUE;
  if (!drive_binding(owner, path[0], root)) return INVALID_HANDLE_VALUE;
  HANDLE parent = open_relative(NULL, root, 1, 1);
  if (parent == INVALID_HANDLE_VALUE) return parent;
  parent = retain_link(owner, parent);
  if (parent == INVALID_HANDLE_VALUE) return parent;
  if (length == 3) return want_directory ? parent : INVALID_HANDLE_VALUE;
  uint32_t components = 0;
  DWORD start = 3;
  for (DWORD end = 3; end <= length; end++) {
    HANDLE handle;
    BY_HANDLE_FILE_INFORMATION info;
    int final;
    if (end < length && path[end] != L'\\') continue;
    if (++components > 512) return INVALID_HANDLE_VALUE;
    if (end > 3 && path[end - 1] == L'\\') continue;
    if (end == start) { start = end + 1; continue; }
    memcpy(component, path + start, (end - start) * sizeof(wchar_t)); component[end - start] = 0;
    final = end == length;
    handle = open_relative(parent, component, !final || want_directory, !final || want_directory);
    if (handle == INVALID_HANDLE_VALUE) return handle;
    if (GetFileType(handle) != FILE_TYPE_DISK || !GetFileInformationByHandle(handle, &info)) {
      CloseHandle(handle); return INVALID_HANDLE_VALUE;
    }
    if (info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
      if (final && !want_directory) {
        /* The tentative no-follow file open shares writes with PostgreSQL.
         * Only an actual reparse object is reopened with a mutation fence. */
        HANDLE fenced = open_relative(parent, component, 0, 1);
        CloseHandle(handle); handle = fenced;
        if (handle == INVALID_HANDLE_VALUE) return handle;
        if (!GetFileInformationByHandle(handle, &info) || !(info.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)) {
          CloseHandle(handle); return INVALID_HANDLE_VALUE;
        }
      }
      union { unsigned char bytes[MAXIMUM_REPARSE_DATA_BUFFER_SIZE]; struct audit_reparse_data data; } buffer;
      DWORD received = 0;
      const wchar_t *target = NULL;
      WORD target_bytes = 0;
      size_t target_offset = 0;
      int relative = 0;
      if (++reparse_count > 32 || !DeviceIoControl(handle, FSCTL_GET_REPARSE_POINT, NULL, 0,
          buffer.bytes, sizeof(buffer.bytes), &received, NULL)) {
        CloseHandle(handle); return INVALID_HANDLE_VALUE;
      }
      handle = retain_link(owner, handle);
      if (handle == INVALID_HANDLE_VALUE) return handle;
      if (received < 8) return INVALID_HANDLE_VALUE;
      if (buffer.data.tag == IO_REPARSE_TAG_MOUNT_POINT) {
        if (received < offsetof(struct audit_reparse_data, value.mount.path)) return INVALID_HANDLE_VALUE;
        target_offset = offsetof(struct audit_reparse_data, value.mount.path) + buffer.data.value.mount.offset;
        target_bytes = buffer.data.value.mount.size;
      } else if (buffer.data.tag == IO_REPARSE_TAG_SYMLINK) {
        if (received < offsetof(struct audit_reparse_data, value.link.path)) return INVALID_HANDLE_VALUE;
        target_offset = offsetof(struct audit_reparse_data, value.link.path) + buffer.data.value.link.offset;
        target_bytes = buffer.data.value.link.size;
        relative = (buffer.data.value.link.flags & 1u) != 0;
      } else return INVALID_HANDLE_VALUE;
      if (!target_bytes || target_bytes % sizeof(wchar_t) || target_offset + target_bytes > received)
        return INVALID_HANDLE_VALUE;
      target = (const wchar_t *)(buffer.bytes + target_offset);
      size_t chars = target_bytes / sizeof(wchar_t), head = 0, skip = 0;
      if (relative) {
        head = end;
        while (head > 3 && path[head - 1] != L'\\') head--;
        memcpy(replacement, path, head * sizeof(wchar_t));
      } else {
        /* NT substitutes use \??\D:\...; reject \??\UNC and other devices. */
        if (chars >= 4 && !wcsncmp(target, L"\\??\\", 4)) skip = 4;
        if (chars < skip + 3 || target[skip + 1] != L':' || target[skip + 2] != L'\\')
          return INVALID_HANDLE_VALUE;
      }
      if (head + chars - skip + length - end + 1 >= AUDIT_MAX_PATH) return INVALID_HANDLE_VALUE;
      memcpy(replacement + head, target + skip, (chars - skip) * sizeof(wchar_t));
      head += chars - skip;
      memcpy(replacement + head, path + end, (length - end + 1) * sizeof(wchar_t));
      length = GetFullPathNameW(replacement, AUDIT_MAX_PATH, path, NULL);
      if (!length || length >= AUDIT_MAX_PATH) return INVALID_HANDLE_VALUE;
      goto restart;
    }
    if (!final && !(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) {
      CloseHandle(handle); return INVALID_HANDLE_VALUE;
    }
    if (final) {
      if (!!(info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != !!want_directory) {
        CloseHandle(handle); return INVALID_HANDLE_VALUE;
      }
      return want_directory ? retain_link(owner, handle) : handle;
    }
    parent = retain_link(owner, handle);
    if (parent == INVALID_HANDLE_VALUE) return parent;
    start = end + 1;
  }
  return INVALID_HANDLE_VALUE;
}

static HANDLE open_directory(hmem_audit_host *owner, const wchar_t *path) {
  /* Pin directory identity, never a relation file that maintenance renames. */
  return open_local_path(owner, path, 1);
}

static int volume_of(HANDLE directory, wchar_t *key, uint64_t *available,
                     uint64_t *total_free) {
  wchar_t resolved[AUDIT_MAX_PATH];
  ULARGE_INTEGER a, total, f;
  DWORD count = GetFinalPathNameByHandleW(directory, resolved, AUDIT_MAX_PATH,
    FILE_NAME_NORMALIZED | VOLUME_NAME_GUID);
  if (!count || count >= AUDIT_MAX_PATH || wcsncmp(resolved, L"\\\\?\\Volume{", 11)) return HMEM_AUDIT_HOST_PATH;
  wchar_t *end = wcschr(resolved, L'}');
  if (!end || end[1] != L'\\' || end - resolved + 3 > 64) return HMEM_AUDIT_HOST_VOLUME;
  size_t root_length = (size_t)(end - resolved + 2);
  memcpy(key, resolved, root_length * sizeof(wchar_t)); key[root_length] = 0;
  if (GetDriveTypeW(key) != DRIVE_FIXED) return HMEM_AUDIT_HOST_UNSUPPORTED;
  if (!GetDiskFreeSpaceExW(key, &a, &total, &f)) return HMEM_AUDIT_HOST_VOLUME;
  *available = a.QuadPart;
  *total_free = f.QuadPart;
  if (a.QuadPart > f.QuadPart || a.QuadPart > total.QuadPart) return HMEM_AUDIT_HOST_VOLUME;
  return HMEM_AUDIT_HOST_OK;
}

static int effective_token(HANDLE *token) {
  if (OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, TRUE, token)) return 1;
  if (GetLastError() != ERROR_NO_TOKEN) return 0;
  return OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, token);
}

static TOKEN_USER *token_user(HANDLE token) {
  DWORD bytes = 0;
  TOKEN_USER *user;
  GetTokenInformation(token, TokenUser, NULL, 0, &bytes);
  if (!bytes || bytes > AUDIT_MAX_TOKEN_BYTES) return NULL;
  user = malloc(bytes);
  if (!user) return NULL;
  if (!GetTokenInformation(token, TokenUser, user, bytes, &bytes)) {
    free(user); return NULL;
  }
  return user;
}

static int same_logon(LUID current, LUID first, LUID second) {
  return current.HighPart == first.HighPart && current.LowPart == first.LowPart &&
    current.HighPart == second.HighPart && current.LowPart == second.LowPart;
}

#ifdef HMEM_AUDIT_HOST_TEST
int hmem_audit_host_test_same_logon(uint64_t current, uint64_t first, uint64_t second) {
  LUID a = { (DWORD)current, (LONG)(current >> 32) };
  LUID b = { (DWORD)first, (LONG)(first >> 32) };
  LUID c = { (DWORD)second, (LONG)(second >> 32) };
  return same_logon(a,b,c);
}
#endif

static int same_effective_sid(HANDLE first, HANDLE second) {
  HANDLE current = NULL, a = NULL, b = NULL;
  TOKEN_USER *cu = NULL, *au = NULL, *bu = NULL;
  TOKEN_STATISTICS cs, as, bs;
  DWORD size;
  int result = 0;
  if (!effective_token(&current) || !OpenProcessToken(first, TOKEN_QUERY, &a) ||
      !OpenProcessToken(second, TOKEN_QUERY, &b)) goto done;
  cu = token_user(current); au = token_user(a); bu = token_user(b);
  result = cu && au && bu && IsValidSid(cu->User.Sid) && IsValidSid(au->User.Sid) &&
    IsValidSid(bu->User.Sid) && EqualSid(cu->User.Sid, au->User.Sid) &&
    EqualSid(cu->User.Sid, bu->User.Sid) &&
    GetTokenInformation(current, TokenStatistics, &cs, sizeof(cs), &size) &&
    GetTokenInformation(a, TokenStatistics, &as, sizeof(as), &size) &&
    GetTokenInformation(b, TokenStatistics, &bs, sizeof(bs), &size) &&
    same_logon(cs.AuthenticationId, as.AuthenticationId, bs.AuthenticationId);
done:
  free(cu); free(au); free(bu);
  if (current) CloseHandle(current);
  if (a) CloseHandle(a);
  if (b) CloseHandle(b);
  return result;
}

static int parent_is(DWORD child, DWORD expected) {
  HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  PROCESSENTRY32W entry;
  int result = 0;
  uint32_t rows = 0;
  if (snapshot == INVALID_HANDLE_VALUE) return 0;
  memset(&entry, 0, sizeof(entry)); entry.dwSize = sizeof(entry);
  if (Process32FirstW(snapshot, &entry)) do {
    if (++rows > AUDIT_MAX_PROCESS_ROWS) break;
    if (entry.th32ProcessID == child) {
      result = entry.th32ParentProcessID == expected; break;
    }
  } while (Process32NextW(snapshot, &entry));
  CloseHandle(snapshot); return result;
}

static int process_start_matches(HANDLE process, int64_t sql_start) {
  FILETIME created, exited, kernel, user;
  uint64_t ticks;
  int64_t started;
  if (!GetProcessTimes(process, &created, &exited, &kernel, &user)) return 0;
  ticks = ((uint64_t)created.dwHighDateTime << 32) | created.dwLowDateTime;
  if (ticks < UINT64_C(116444736000000000)) return 0;
  started = (int64_t)((ticks - UINT64_C(116444736000000000)) / 10);
  /* PostgreSQL records its start after Windows process creation. Exact equality
   * is false on the qualified native 18.2 deployment. This is only one check;
   * retained handles, parentage, tuple, PID file and control identity bind it. */
  return sql_start >= started && sql_start - started <= AUDIT_STARTUP_US;
}

static int same_postgres_image(HANDLE a, HANDLE b) {
  wchar_t first[AUDIT_MAX_PATH], second[AUDIT_MAX_PATH];
  DWORD na = AUDIT_MAX_PATH, nb = AUDIT_MAX_PATH;
  wchar_t *base;
  if (!QueryFullProcessImageNameW(a, 0, first, &na) ||
      !QueryFullProcessImageNameW(b, 0, second, &nb) || _wcsicmp(first, second)) return 0;
  base = wcsrchr(first, L'\\');
  return base && !_wcsicmp(base + 1, L"postgres.exe");
}

static int process_identity(const hmem_audit_host *owner) {
  if (WaitForSingleObject(owner->backend, 0) != WAIT_TIMEOUT ||
      WaitForSingleObject(owner->postmaster, 0) != WAIT_TIMEOUT ||
      GetProcessId(owner->backend) != owner->backend_pid ||
      GetProcessId(owner->postmaster) != owner->postmaster_pid) return HMEM_AUDIT_HOST_CHANGED;
  if (!parent_is(owner->backend_pid, owner->postmaster_pid) ||
      !same_postgres_image(owner->backend, owner->postmaster) ||
      !process_start_matches(owner->backend, owner->backend_start_us) ||
      !process_start_matches(owner->postmaster, owner->postmaster_start_us)) return HMEM_AUDIT_HOST_IDENTITY;
  if (!same_effective_sid(owner->backend, owner->postmaster)) return HMEM_AUDIT_HOST_SID;
  return HMEM_AUDIT_HOST_OK;
}

static int endpoint_matches(const hmem_audit_host *owner) {
  ULONG bytes = 0;
  void *table = NULL;
  DWORD result;
  uint32_t matches = 0;
  DWORD matched_pid = 0;
  result = GetExtendedTcpTable(NULL, &bytes, FALSE, (ULONG)owner->family,
    TCP_TABLE_OWNER_PID_ALL, 0);
  if (result != ERROR_INSUFFICIENT_BUFFER && result != NO_ERROR) return HMEM_AUDIT_HOST_ENDPOINT;
  for (int attempt = 0; attempt < 2; attempt++) {
    void *next;
    if (!bytes || bytes > AUDIT_MAX_TCP_BYTES) { free(table); return HMEM_AUDIT_HOST_LIMIT; }
    next = realloc(table, bytes);
    if (!next) { free(table); return HMEM_AUDIT_HOST_MEMORY; }
    table = next;
    result = GetExtendedTcpTable(table, &bytes, FALSE, (ULONG)owner->family,
      TCP_TABLE_OWNER_PID_ALL, 0);
    if (result != ERROR_INSUFFICIENT_BUFFER) break;
  }
  if (result != NO_ERROR) { free(table); return HMEM_AUDIT_HOST_ENDPOINT; }
  if (owner->family == AF_INET) {
    MIB_TCPTABLE_OWNER_PID *v4 = table;
    if ((uint64_t)v4->dwNumEntries * sizeof(MIB_TCPROW_OWNER_PID) +
        offsetof(MIB_TCPTABLE_OWNER_PID, table) > bytes) { free(table); return HMEM_AUDIT_HOST_LIMIT; }
    for (DWORD i = 0; i < v4->dwNumEntries; i++) {
      MIB_TCPROW_OWNER_PID *row = &v4->table[i];
      if (row->dwState == MIB_TCP_STATE_ESTAB &&
          !memcmp(&row->dwLocalAddr, owner->server_address, 4) &&
          !memcmp(&row->dwRemoteAddr, owner->client_address, 4) &&
          ntohs((uint16_t)row->dwLocalPort) == owner->server_port &&
          ntohs((uint16_t)row->dwRemotePort) == owner->client_port) {
        matches++; matched_pid = row->dwOwningPid;
      }
    }
  } else {
    MIB_TCP6TABLE_OWNER_PID *v6 = table;
    if ((uint64_t)v6->dwNumEntries * sizeof(MIB_TCP6ROW_OWNER_PID) +
        offsetof(MIB_TCP6TABLE_OWNER_PID, table) > bytes) { free(table); return HMEM_AUDIT_HOST_LIMIT; }
    for (DWORD i = 0; i < v6->dwNumEntries; i++) {
      MIB_TCP6ROW_OWNER_PID *row = &v6->table[i];
      if (row->dwState == MIB_TCP_STATE_ESTAB && row->dwLocalScopeId == 0 && row->dwRemoteScopeId == 0 &&
          !memcmp(row->ucLocalAddr, owner->server_address, 16) &&
          !memcmp(row->ucRemoteAddr, owner->client_address, 16) &&
          ntohs((uint16_t)row->dwLocalPort) == owner->server_port &&
          ntohs((uint16_t)row->dwRemotePort) == owner->client_port) {
        matches++; matched_pid = row->dwOwningPid;
      }
    }
  }
  free(table);
  /* Windows attributes the duplicated server socket to the postmaster on the
   * qualified deployment. Both modes require the same retained process chain. */
  return matches == 1 && (matched_pid == owner->backend_pid || matched_pid == owner->postmaster_pid)
    ? HMEM_AUDIT_HOST_OK : HMEM_AUDIT_HOST_ENDPOINT;
}

static int read_fixed(hmem_audit_host *owner, const wchar_t *data, const wchar_t *child, void *bytes,
                      DWORD count, DWORD *read_count) {
  wchar_t *path = child_path(data, child);
  HANDLE file;
  int success;
  if (!path) return 0;
  file = open_local_path(owner, path, 0);
  free(path);
  if (file == INVALID_HANDLE_VALUE) return 0;
  success = GetFileType(file) == FILE_TYPE_DISK && ReadFile(file, bytes, count, read_count, NULL);
  CloseHandle(file); return success;
}

static int fixed_cluster_identity(hmem_audit_host *owner) {
  unsigned char control[12];
  char pid_line[64];
  DWORD got;
  uint64_t system;
  uint32_t version;
  unsigned long pid;
  char *end;
  const wchar_t *data = owner->directories[0].path;
  if (!read_fixed(owner, data, L"global\\pg_control", control, sizeof(control), &got) ||
      got != sizeof(control)) return HMEM_AUDIT_HOST_PATH;
  memcpy(&system, control, 8); memcpy(&version, control + 8, 4);
  /* PostgreSQL 18's documented ControlFileData prefix, never a general file
   * parser. Reject an unknown physical format rather than guessing offsets. */
  if (version != 1800) return HMEM_AUDIT_HOST_UNSUPPORTED;
  if (system != owner->system_identifier) return HMEM_AUDIT_HOST_IDENTITY;
  if (!read_fixed(owner, data, L"postmaster.pid", pid_line, sizeof(pid_line) - 1, &got) || !got)
    return HMEM_AUDIT_HOST_PATH;
  pid_line[got] = 0;
  errno = 0; pid = strtoul(pid_line, &end, 10);
  if (errno || end == pid_line || (*end != '\n' && *end != '\r') ||
      pid != owner->postmaster_pid) return HMEM_AUDIT_HOST_IDENTITY;
  return HMEM_AUDIT_HOST_OK;
}

void hmem_audit_host_close(hmem_audit_host *owner) {
  if (!owner) return;
  for (uint32_t i = 0; i < owner->directory_count; i++) {
    free(owner->directories[i].path);
  }
  while (owner->link_count) CloseHandle(owner->links[--owner->link_count]);
  if (owner->backend) CloseHandle(owner->backend);
  if (owner->postmaster) CloseHandle(owner->postmaster);
  free(owner);
}

int hmem_audit_host_refresh(hmem_audit_host *owner) {
  int result;
  if (!owner) return HMEM_AUDIT_HOST_INVALID;
  owner->valid = 0;
  result = process_identity(owner);
  if (result) return result;
  result = endpoint_matches(owner);
  if (result) return result;
  for (uint32_t d = 0; d < 26; d++) if (owner->drive_bindings[d][0]) {
    wchar_t drive[3] = { (wchar_t)(L'A'+d), L':', 0 }, target[128];
    if (!QueryDosDeviceW(drive, target, 128) || _wcsicmp(target, owner->drive_bindings[d]))
      return HMEM_AUDIT_HOST_CHANGED;
  }
  for (uint32_t i = 0; i < owner->link_count; i++) {
    BY_HANDLE_FILE_INFORMATION observed;
    if (!GetFileInformationByHandle(owner->links[i], &observed) ||
        !same_file(&observed, &owner->link_identities[i]) ||
        ((observed.dwFileAttributes ^ owner->link_identities[i].dwFileAttributes) & FILE_ATTRIBUTE_REPARSE_POINT))
      return HMEM_AUDIT_HOST_CHANGED;
  }
  for (uint32_t v = 0; v < owner->volume_count; v++) {
    owner->volumes[v].available = UINT64_MAX;
    owner->volumes[v].total_free = UINT64_MAX;
  }
  for (uint32_t i = 0; i < owner->directory_count; i++) {
    struct audit_directory *directory = &owner->directories[i];
    BY_HANDLE_FILE_INFORMATION retained;
    wchar_t key[64];
    uint64_t available, total_free;
    if (!directory_info(directory->handle, &retained) ||
        !same_file(&retained, &directory->identity)) return HMEM_AUDIT_HOST_CHANGED;
    result = volume_of(directory->handle, key, &available, &total_free);
    if (result) return result;
    if (_wcsicmp(key, owner->volumes[directory->volume].key)) return HMEM_AUDIT_HOST_CHANGED;
    if (available < owner->volumes[directory->volume].available)
      owner->volumes[directory->volume].available = available;
    if (total_free < owner->volumes[directory->volume].total_free)
      owner->volumes[directory->volume].total_free = total_free;
  }
  result = fixed_cluster_identity(owner);
  if (result) return result;
  result = process_identity(owner);
  if (result) return result;
  owner->valid = 1;
  return HMEM_AUDIT_HOST_OK;
}

int hmem_audit_host_open(
    const char *server_address, uint16_t server_port,
    const char *client_address, uint16_t client_port,
    uint32_t backend_pid, uint32_t postmaster_pid,
    int64_t backend_start_us, int64_t postmaster_start_us,
    const char *data_directory, const char *system_identifier,
    const char *const *audit_directories, uint32_t audit_directory_count,
    hmem_audit_host **output) {
  hmem_audit_host *owner;
  char *end;
  int result = HMEM_AUDIT_HOST_INVALID;
  if (!output) return result;
  *output = NULL;
  if (!server_address || !client_address || !data_directory || !system_identifier ||
      !server_port || !client_port || !backend_pid || !postmaster_pid || backend_pid == postmaster_pid ||
      backend_start_us <= 0 || postmaster_start_us <= 0 || backend_start_us < postmaster_start_us ||
      audit_directory_count > AUDIT_MAX_DIRECTORIES - 2 ||
      (audit_directory_count && !audit_directories)) return result;
  owner = calloc(1, sizeof(*owner));
  if (!owner) return HMEM_AUDIT_HOST_MEMORY;
  owner->backend_pid = backend_pid; owner->postmaster_pid = postmaster_pid;
  owner->backend_start_us = backend_start_us; owner->postmaster_start_us = postmaster_start_us;
  owner->server_port = server_port; owner->client_port = client_port;
  errno = 0; owner->system_identifier = strtoull(system_identifier, &end, 10);
  if (errno || end == system_identifier || *end || !owner->system_identifier ||
      strlen(system_identifier) > 20 || system_identifier[0] == '-' || system_identifier[0] == '+') goto fail;
  owner->family = AF_INET;
  if (InetPtonA(AF_INET, server_address, owner->server_address) != 1 ||
      InetPtonA(AF_INET, client_address, owner->client_address) != 1) {
    owner->family = AF_INET6;
    if (InetPtonA(AF_INET6, server_address, owner->server_address) != 1 ||
        InetPtonA(AF_INET6, client_address, owner->client_address) != 1 ||
        !IN6_IS_ADDR_LOOPBACK((IN6_ADDR *)owner->server_address) ||
        !IN6_IS_ADDR_LOOPBACK((IN6_ADDR *)owner->client_address)) goto fail;
  } else if (owner->server_address[0] != 127 || owner->client_address[0] != 127) goto fail;
  owner->backend = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, backend_pid);
  owner->postmaster = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION | SYNCHRONIZE, FALSE, postmaster_pid);
  if (!owner->backend || !owner->postmaster) { result = HMEM_AUDIT_HOST_PROCESS; goto fail; }
  result = process_identity(owner);
  if (result) goto fail;
  result = endpoint_matches(owner);
  if (result) goto fail;
  owner->directory_count = 2 + audit_directory_count;
  owner->directories[0].path = wide_path(data_directory);
  if (!owner->directories[0].path) { result = HMEM_AUDIT_HOST_PATH; goto fail; }
  owner->directories[1].path = child_path(owner->directories[0].path, L"pg_wal");
  for (uint32_t i = 0; i < audit_directory_count; i++)
    owner->directories[i + 2].path = wide_path(audit_directories[i]);
  for (uint32_t i = 0; i < owner->directory_count; i++) {
    struct audit_directory *directory = &owner->directories[i];
    wchar_t key[64];
    uint64_t available, total_free;
    uint32_t v;
    if (!directory->path) { result = HMEM_AUDIT_HOST_PATH; goto fail; }
    directory->handle = open_directory(owner, directory->path);
    if (directory->handle == INVALID_HANDLE_VALUE || !directory_info(directory->handle, &directory->identity)) {
      result = HMEM_AUDIT_HOST_PATH; goto fail;
    }
    result = volume_of(directory->handle, key, &available, &total_free);
    if (result) goto fail;
    for (v = 0; v < owner->volume_count; v++) if (!_wcsicmp(key, owner->volumes[v].key)) break;
    if (v == owner->volume_count) {
      wcscpy(owner->volumes[v].key, key); owner->volume_count++;
    }
    directory->volume = v;
  }
  result = hmem_audit_host_refresh(owner);
  if (result) goto fail;
  *output = owner;
  return HMEM_AUDIT_HOST_OK;
fail:
  hmem_audit_host_close(owner);
  return result;
}

uint32_t hmem_audit_host_directory_count(const hmem_audit_host *owner) {
  return owner ? owner->directory_count : 0;
}
int hmem_audit_host_backend_exited(const hmem_audit_host *owner, int *exited) {
  DWORD state;
  if (!owner || !owner->backend || !exited) return HMEM_AUDIT_HOST_INVALID;
  *exited = 0;
  state = WaitForSingleObject(owner->backend, 0);
  if (state == WAIT_OBJECT_0) { *exited = 1; return HMEM_AUDIT_HOST_OK; }
  return state == WAIT_TIMEOUT ? HMEM_AUDIT_HOST_OK : HMEM_AUDIT_HOST_PROCESS;
}
uint32_t hmem_audit_host_volume_count(const hmem_audit_host *owner) {
  return owner ? owner->volume_count : 0;
}
int hmem_audit_host_directory_volume(const hmem_audit_host *owner, uint32_t directory) {
  return owner && directory < owner->directory_count ? (int)owner->directories[directory].volume : -1;
}
int hmem_audit_host_volume_read(const hmem_audit_host *owner, uint32_t volume,
    uint64_t *available, uint64_t *total_free) {
  if (!owner || !available || !total_free || volume >= owner->volume_count) return HMEM_AUDIT_HOST_INVALID;
  if (!owner->valid) return HMEM_AUDIT_HOST_CHANGED;
  *available = owner->volumes[volume].available;
  *total_free = owner->volumes[volume].total_free;
  return HMEM_AUDIT_HOST_OK;
}
int hmem_audit_host_volume_key(const hmem_audit_host *owner, uint32_t volume,
    char *output, size_t capacity) {
  if (!owner || volume >= owner->volume_count || !output || capacity < 64 || capacity > INT_MAX)
    return HMEM_AUDIT_HOST_INVALID;
  return WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, owner->volumes[volume].key,
    -1, output, (int)capacity, NULL, NULL) ? HMEM_AUDIT_HOST_OK : HMEM_AUDIT_HOST_VOLUME;
}

#else

struct hmem_audit_host { int unused; };
int hmem_audit_host_open(const char *sa, uint16_t sp, const char *ca, uint16_t cp,
    uint32_t bp, uint32_t pp, int64_t bs, int64_t ps, const char *dd, const char *si,
    const char *const *dirs, uint32_t count, hmem_audit_host **owner) {
  (void)sa; (void)sp; (void)ca; (void)cp; (void)bp; (void)pp; (void)bs; (void)ps;
  (void)dd; (void)si; (void)dirs; (void)count;
  if (owner) *owner = NULL;
  return HMEM_AUDIT_HOST_UNSUPPORTED;
}
int hmem_audit_host_refresh(hmem_audit_host *owner) { (void)owner; return HMEM_AUDIT_HOST_UNSUPPORTED; }
int hmem_audit_host_backend_exited(const hmem_audit_host *owner, int *exited) {
  (void)owner; if (exited) *exited = 0; return HMEM_AUDIT_HOST_UNSUPPORTED;
}
uint32_t hmem_audit_host_directory_count(const hmem_audit_host *owner) { (void)owner; return 0; }
uint32_t hmem_audit_host_volume_count(const hmem_audit_host *owner) { (void)owner; return 0; }
int hmem_audit_host_directory_volume(const hmem_audit_host *owner, uint32_t directory) { (void)owner; (void)directory; return -1; }
int hmem_audit_host_volume_read(const hmem_audit_host *owner, uint32_t volume, uint64_t *available, uint64_t *total_free) {
  (void)owner; (void)volume; (void)available; (void)total_free; return HMEM_AUDIT_HOST_UNSUPPORTED;
}
int hmem_audit_host_volume_key(const hmem_audit_host *owner, uint32_t volume, char *output, size_t capacity) {
  (void)owner; (void)volume; (void)output; (void)capacity; return HMEM_AUDIT_HOST_UNSUPPORTED;
}
void hmem_audit_host_close(hmem_audit_host *owner) { (void)owner; }

#endif
