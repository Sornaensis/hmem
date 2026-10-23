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
#ifdef __clang__
#pragma clang diagnostic pop
#endif
typedef struct { HANDLE handle; } hmem_file;
static volatile LONG hmem_open_sources = 0;
int hmem_session_open_count(void) { return (int)InterlockedCompareExchange(&hmem_open_sources, 0, 0); }
void *hmem_session_open(const char *path) {
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
  if (!n) return NULL;
  wchar_t *wide = malloc((size_t)n * sizeof(wchar_t));
  if (!wide) return NULL;
  if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, wide, n)) { free(wide); return NULL; }
  HANDLE handle = CreateFileW(wide, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                            NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  free(wide);
  if (handle == INVALID_HANDLE_VALUE) return NULL;
  BY_HANDLE_FILE_INFORMATION info;
  if (GetFileType(handle) != FILE_TYPE_DISK || !GetFileInformationByHandle(handle, &info)
      || (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)) { CloseHandle(handle); return NULL; }
  hmem_file *file = malloc(sizeof(hmem_file));
  if (!file) { CloseHandle(handle); return NULL; }
  file->handle = handle; InterlockedIncrement(&hmem_open_sources); return file;
}
int hmem_session_info(void *value, uint64_t *volume, unsigned char *id, int64_t *size) {
  hmem_file *file = value;
  struct { ULONGLONG volume; BYTE id[16]; } identity;
  LARGE_INTEGER length;
  /* FileIdInfo (18) supplies the full 128-bit file ID on supported volumes. */
  if (!GetFileInformationByHandleEx(file->handle, (FILE_INFO_BY_HANDLE_CLASS)18, &identity, sizeof(identity))
      || !GetFileSizeEx(file->handle, &length) || length.QuadPart < 0) return 0;
  *volume = identity.volume; memcpy(id, identity.id, 16); *size = length.QuadPart; return 1;
}
int hmem_session_read(void *value, int64_t offset, unsigned char *buffer, int count) {
  hmem_file *file = value; LARGE_INTEGER position; DWORD read_count;
  position.QuadPart = offset;
  if (!SetFilePointerEx(file->handle, position, NULL, FILE_BEGIN)
      || !ReadFile(file->handle, buffer, (DWORD)count, &read_count, NULL)) return -1;
  return (int)read_count;
}
int hmem_session_close(void *value) {
  hmem_file *file = value; int ok = CloseHandle(file->handle) != 0; if (ok) InterlockedDecrement(&hmem_open_sources); free(file); return ok;
}
#else
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
typedef struct { int fd; } hmem_file;
static volatile int hmem_open_sources = 0;
int hmem_session_open_count(void) { return __sync_add_and_fetch(&hmem_open_sources, 0); }
void *hmem_session_open(const char *path) {
  int fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
  if (fd < 0) return NULL;
  struct stat info;
  if (fstat(fd, &info) || !S_ISREG(info.st_mode)) { close(fd); return NULL; }
  hmem_file *file = malloc(sizeof(hmem_file));
  if (!file) { close(fd); return NULL; }
  file->fd = fd; __sync_add_and_fetch(&hmem_open_sources, 1); return file;
}
int hmem_session_info(void *value, uint64_t *volume, unsigned char *id, int64_t *size) {
  hmem_file *file = value; struct stat info;
  if (fstat(file->fd, &info) || !S_ISREG(info.st_mode) || info.st_size < 0) return 0;
  *volume = (uint64_t)info.st_dev; memset(id, 0, 16);
  uint64_t inode = (uint64_t)info.st_ino; memcpy(id, &inode, 8); *size = info.st_size; return 1;
}
int hmem_session_read(void *value, int64_t offset, unsigned char *buffer, int count) {
  ssize_t n = pread(((hmem_file *)value)->fd, buffer, (size_t)count, (off_t)offset);
  return n < 0 ? -1 : (int)n;
}
int hmem_session_close(void *value) {
  hmem_file *file = value; int ok = close(file->fd) == 0; if (ok) __sync_sub_and_fetch(&hmem_open_sources, 1); free(file); return ok;
}
#endif
