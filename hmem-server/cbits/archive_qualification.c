/* Test-harness-only observations of the process holding this pseudo-handle.
 * No PID lookup, peak-since-start metric, or PostgreSQL attribution. */
#include <stdint.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define PSAPI_VERSION 2
#include <windows.h>
#include <psapi.h>

static uint64_t ticks(FILETIME value) {
    return ((uint64_t)value.dwHighDateTime << 32) | value.dwLowDateTime;
}

int hmem_archive_qualification_sample(uint64_t *working_set,
                                    uint64_t *kernel_100ns,
                                    uint64_t *user_100ns) {
    PROCESS_MEMORY_COUNTERS memory;
    FILETIME created, exited, kernel, user;
    HANDLE process = GetCurrentProcess();
    memory.cb = sizeof(memory);
    if (!K32GetProcessMemoryInfo(process, &memory, sizeof(memory)) ||
        !GetProcessTimes(process, &created, &exited, &kernel, &user)) return 0;
    *working_set = (uint64_t)memory.WorkingSetSize;
    *kernel_100ns = ticks(kernel);
    *user_100ns = ticks(user);
    return 1;
}
#else
int hmem_archive_qualification_sample(uint64_t *working_set,
                                    uint64_t *kernel_100ns,
                                    uint64_t *user_100ns) {
    (void)working_set; (void)kernel_100ns; (void)user_100ns;
    return 0;
}
#endif
