+++
schema = "adrai/decision/v1"
adr = "A01M51RECA7P68J92X84FGFEQPN"
record = "R01M51RECHCF2A0PY3M01EX87YF"
title = "Keep documentation concise and separate onboarding, guides and references"
summary = "Reserve READMEs for onboarding, guides for functionality, and keep the database diagram current."
domains = ["documentation"]
+++

Context: The repository owner requested these documentation rules. This decision sets policy; it does not claim that existing documentation already complies.

Decision:
- Every README, including nested READMEs, covers onboarding, a quick project explanation, setup and basic ideas only. Never put migration procedures or technical reference documentation in a README; link to a separate reference instead.
- Guides explain functionality and user workflows. Put implementation details, technical descriptions and migration procedures in clearly named reference documents.
- Keep database.md consistent with the current ordered SQL schema. Update and check its tables, columns, types, nullability, keys and declared relationships in the same change as any schema change.
- Review all affected documentation and periodically double-check the complete documentation set for AI-style boilerplate, repetition and unnecessary explanation. Keep prose direct and concise while preserving required commands, contracts and evidence. Preserve immutable decision history and legal/provenance records.

Consequences: Readers can find setup in READMEs, product use in guides and technical details in references. Documentation changes must retain working links and accurate schema diagrams.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjQ4MWY3ZmM1NzViNTExNTk5YWVlOTA0ZjgyOTdlNjg2Zjg1NjE1N2UiLCJpIjoic2hhMjU2OkdfLWFEU0k3ZmFpbERYVEpyUnQ3RHlka2ZrRGNvb3ZWVUV2VnpLTmFiLTQiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU01MVJFQ0hDRjJBMFBZM00wMUVYODdZRiIsIm9wIjoiTzAxTTUxUkVDSENGMkEwUFkzTTAxRVg4N1lGIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6LWdNcWI0bkxlWE53cjB0U2U3U002NURZX01wWG04bmhzeVhDMXdqOHk0RSIsInQiOjE3OTIxMzQyOTYxMDgsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
