#ifndef HMEM_AUDIT_CANCEL_H
#define HMEM_AUDIT_CANCEL_H

/* Optional modern libpq cancellation. A compiler-qualified baseline import
 * binds the same linked libpq used by Hasql; no new libpq symbol is imported.
 * No filename, library name, process ID or arbitrary symbol is accepted. */
typedef struct hmem_audit_cancel hmem_audit_cancel;
int hmem_audit_cancel_capability(int *version);
int hmem_audit_cancel_open(void *connection, hmem_audit_cancel **output);
int hmem_audit_cancel_start(hmem_audit_cancel *owner);
/* Returns 1 on dispatch, 0 while pending, or a negative fixed error code.
 * One poller owns the capsule. Waits are bounded to 0..100 milliseconds;
 * the caller also owns an absolute monotonic transport deadline. */
int hmem_audit_cancel_poll(hmem_audit_cancel *owner, int wait_ms);
/* Stop polling first. Resets/closes the transport for another serial attempt
 * against this same operation/backend; never reuse across operation generations. */
int hmem_audit_cancel_reset(hmem_audit_cancel *owner);
/* Stop polling before finish. Finish closes any unfinished cancel transport;
 * it never establishes completion of the original database operation. */
void hmem_audit_cancel_finish(hmem_audit_cancel *owner);
void hmem_audit_cancel_close(hmem_audit_cancel *owner);
#ifdef HMEM_AUDIT_CANCEL_TEST
void *hmem_audit_cancel_test_linked_address(void);
unsigned int hmem_audit_cancel_test_created(void);
unsigned int hmem_audit_cancel_test_finished(void);
#endif
#endif
