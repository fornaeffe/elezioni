## 2026-11-17 — Bounded source-download retries

- The [second hosted cold run](https://github.com/fornaeffe/elezioni/actions/runs/44102471591)
  of `0df367812bc1e687df46bfb1979f39e75bc71713` stopped before verifying the
  first source, `camera-a1`: Node 24.22.2 reported `UND_ERR_CONNECT_TIMEOUT`
  connecting to `www.riformeistituzionali.gov.it:443` after 10 seconds. The
  existing 180-second abort signal did not extend Undici's separate connection
  timeout. The log establishes a connection failure, not source checksum drift;
  its cause on the runner (temporary outage/routing/access restriction) remains
  unproven by successful local requests.
- Shared acquisition now uses `source-download.ts` and an explicitly pinned
  Node preparation dependency, `undici@7.29.0`. Each source GET uses a scoped
  dispatcher with a **30-second connection timeout**, at most **three attempts**,
  and one **180-second deadline** covering all attempts, bodies and waits.
  No global dispatcher is changed. Dispatchers are destroyed after success or
  failure, and the deadline timer is cleared.
- Only recognized transient network errors and HTTP 408/429/500/502/503/504
  retry, with 2- then 5-second backoff. Longer numeric/date `Retry-After` values
  are honored within the deadline; excessive values cannot overflow timers or
  extend the budget. Attempts log source ID, host, elapsed time and the failure
  code. Final errors retain the original cause. Response bodies are cancelled
  before retrying HTTP errors, and interrupted body reads can retry the GET.
- Repository snapshots and explicit imports retain their existing precedence.
  TLS validation, permanent HTTP errors, corrupt caches/imports, invalid
  snapshots and checksum mismatches still fail. Byte verification remains
  outside the transport retry loop, and files are written only after successful
  verification. No alternate source, cache repair, TLS bypass or automatic
  source-pin update was introduced.
- Added deterministic fake-clock tests for retries/backoff, exhaustion, body
  interruption, HTTP status classification, `Retry-After`, shared-deadline
  expiry, contextual diagnostics and unchanged cache/checksum enforcement.
  The focused acquisition suite passed **29 tests** before full validation.
- Source manifests and authored scenario pins are unchanged. The added lockfile
  dependency and downloader code legitimately change implementation provenance.
  Regenerated the candidate: **18/19 receipt artifacts are byte-identical** to
  the previous release; only its pointer changes. Source envelope/chunks,
  national/profile/paired inputs, electorates, dates and fitted/profile/runtime
  binary columns are unchanged. State payload differences are limited to
  implementation IDs and dependent fitted/profile/composition/run references.
  The comparison also checks the new generation fingerprint against the code.
  Detailed proof is in `equivalence-report.json` and `state-diff.json`.
  Evidence and full command logs are under
  `data/derived/hosting-network-fix/`, starting with `report.json`.
- The first staged-browser pass completed all five election worker/export
  workflows. Its navigation assertions also completed, but that synthetic
  matrix's `afterEach` hook timed out awaiting transfer-size promises for
  abandoned requests. The trace places navigation completion around three
  seconds, followed by the 240-second bookkeeping timeout. Complete failed
  reports and trace remain in `browser.json`, `initial-report.json` and
  `browser-output/`. No electoral run was changed or rerolled to address it.
- The synthetic navigation matrix now records HTTP statuses and page/network
  errors with explicitly null transfer sizes and estimates. It no longer waits
  for abandoned responses' size promises. All route assertions and HTTP/error
  checks remain; the five complete election workflows still require their full
  measured-transfer receipts. `docs/hosting.md` describes this distinction.
- Windows / Node 24.20.0 verification passed: empty-source/model-cache
  preparation verified all **31 sources** in **176.3 seconds**, `data:check`
  passed in **25.5 seconds**, and warm preparation passed in **87.1 seconds**.
  All **19 cold/warm receipt artifacts are byte-identical**. Both type checks,
  all **1,373 unit tests** with zero skips, and the production build passed.
  Application types were rechecked after the navigation-receipt adjustment.
  All **six final staged browser checks** passed in **141.6 seconds**, with no
  skips or retries. The full 168-check application browser campaign was not
  repeated. Final reports are `browser-final.json` and `transfers-final/`;
  `transfer-validation.json` confirms five measured election workflows and one
  explicitly unmeasured synthetic navigation matrix. The staged site contains
  **68 files / 112,870,846 bytes**, with a **20,862,492-byte** gzip estimate.
- Standard hosted Linux acceptance still requires the next committed/pushed
  revision; local success
  cannot prove that a GitHub runner can reach every upstream host. No commit,
  push, workflow rerun, merge or deployment was performed in this correction.
