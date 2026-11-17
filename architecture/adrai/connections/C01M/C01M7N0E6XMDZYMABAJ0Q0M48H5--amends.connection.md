+++
schema = "adrai/connection/v1"
connection = "C01M7N0E6XMDZYMABAJ0Q0M48H5"
relation = "amends"
from_record = "R01M7N0E6XMM8R89VJ6X9PX11ZC"
subject_adr = "A01M39QPB23CYS9QHCWQSATSJE9"
to_records = ["R01M7MXQSX8CX5T2YD4K8HFPWF2"]
+++

Per the 2026-11-17 user decision, remove server-wide HTTP admission: requests are uncapped and wait first-in-first-out for a single-stripe pool connection, with a sixty-second checkout safety timeout returning 503 database_unavailable.

<!-- @adrai:eyJhIjp7ImkiOiJjbGF1ZGUtY29kZSIsImsiOiJsbG0iLCJtIjoiY2xhdWRlLW9wdXMtNS01In0sImIiOiI5MWU1MjgxOTRmYTJkMzUyZjA4YWM0ZTFmNzlhYmNhZmU0OGNlZjM0IiwiaSI6InNoYTI1Njp2MENyendLcEN6VUV4Ymo2LTBIal8yNGRzX2xPVzZUX0xPRVpOOWxDaW44IiwiayI6ImNvbm5lY3Rpb24uYW1lbmRzIiwibyI6IkMwMU03TjBFNlhNRFpZTUFCQUowUTBNNDhINSIsIm9wIjoiTzAxTTdOMEU2WE1NOFI4OVZKNlg5UFgxMVpDIiwicCI6WyJSMDFNN01YUVNYOENYNVQyWUQ0SzhIRlBXRjIiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6cl9yODRxdWRONlpUUXJDclliOF9iMmp2TWVVb3V1aF9lTHEzUjBXQXNCVSIsInQiOjE3OTQ5Mjc2OTY4MjAsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
