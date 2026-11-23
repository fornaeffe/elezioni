## 2026-11-18 — Bounded retries for source hostname lookup failures

- Luca's complete local development and release application reports passed:
  1,382 units in each, 67 development browser checks and 74 release browser
  checks, with no skips. After commit/push, the development job for
  `fbb1ddb5f99178cf87bd51ba9a976a4e31a5ceae` stopped during preparation, before
  application acceptance. Its artifact contains `prepare.log`; ten downloads
  from `dait.interno.gov.it` succeeded before `europee-20190526` failed hostname
  lookup with `ENOTFOUND` after 4.5 seconds. That code was absent from the
  transient allowlist, so the downloader stopped at attempt 1/3.
- The preceding diagnosis confirmed successful local hostname resolution and
  HTTP HEAD 200 with the pinned 31,187,859-byte response length. This supports
  an intermittent lookup problem; it does not identify the failing resolver or
  verify a newly downloaded archive's hash. No additional logs were needed.
- Added `ENOTFOUND` to the shared retry allowlist. The existing three-attempt
  limit, 2/5-second backoff, 30-second connection cap and single 180-second total
  deadline remain. Persistent lookup errors still fail with the underlying cause.
  Source URLs, byte/hash pins, TLS validation, permanent HTTP classification,
  cache/import verification and checksum mismatch behavior are unchanged.
- Added focused fake-clock tests for wrapped lookup failure/recovery, exhaustion
  with exact backoff and diagnostics, and changed bytes after recovery. The last
  case verifies there is no third request or cached/partial file after checksum
  rejection. All **21 downloader tests passed**, without skips or TODOs.
  Reports and complete logs: `data/derived/source-dns-fix/unit.json` and `unit.log`.
- Dependency-graph inspection shows this transport change affects source/legal
  preparation cache identities, while browser compute, generation, fitting and
  profile implementation entry graphs do not include the changed downloader.
  The existing pipeline handles cache rebuilding. No production artifacts,
  manifests, authored defaults or frozen fixtures were regenerated or edited.
  Inspection evidence: `data/derived/source-dns-fix/implementation-dependencies.json`.
- Updated preparation instructions and the active blocker. `git diff --check`
  passed. No full acceptance, browser suite or preparation campaign ran here.
  Commit/push the fix and require the new hosted development job to pass, then
  manually run the release cold/warm reassessment before merge. No commit, push,
  workflow dispatch, merge or publication was performed here.
