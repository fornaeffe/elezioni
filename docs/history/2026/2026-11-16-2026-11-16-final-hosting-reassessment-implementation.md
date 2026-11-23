## 2026-11-16 — Final hosting reassessment implementation

- Retained GitHub Actions plus GitHub Pages. Live repository metadata confirms
  `fornaeffe/elezioni` is public, defaults to `main`, and has no Pages site yet.
  The [hosting guide](../../hosting.md) records current primary-source limits,
  implementation choices, evidence locations and the publication procedure.
  Eligibility and local results do not establish hosted-runner acceptance.
- Added a SHA-pinned workflow on `ubuntu-24.04` / Node 24. It measures a fresh
  npm/source/model-cache pipeline and a warm replay on the same runner, runs
  the unchanged strict application gate in each pass, and requires identical
  production payload manifests. GNU time records Linux command resources;
  five-second samples retain host available RAM and disk. Sources/model-cache
  sizes and complete preparation/acceptance receipts are retained. There is no
  persistent Actions cache; large site/evidence artifacts expire after one day,
  while small status/identity and public-transfer receipts remain for 30 days. The
  combined cold/warm assessment has a 330-minute ceiling below GitHub's six-hour
  job limit, allowing both complete matrices on the standard four-CPU runner;
  this ceiling is not a measured hosted duration or a model-performance result.
- Publication requires a manual request on `main`, repository variable
  `PAGES_PUBLICATION_ENABLED=true`, and successful cold/warm gates. Pushes and
  merges alone never deploy. A request on another branch or without the switch
  fails explicitly. Separate least-permission deployment and public-smoke jobs
  verify the deployed revision, routes, actual workers, simulation and exports.
- Added a production-only staging graph: current snapshot descriptors, all
  authored politics asset pins, the selected reform candidate and its complete
  transitive source/state/column dependencies, compiled client assets and the
  four prerendered routes. Missing/corrupt dependencies, links, source maps and
  an over-budget site fail. Unreferenced historical versions and research/test
  evidence are excluded. `release.json` records the baseline revision and data
  identities; `BUILD_REVISION` makes hosted SvelteKit versioning deterministic.
- Static routes now use directory indexes. The release test server redirects
  extensionless directory requests and can exercise raw gzip delivery; existing
  transparent-HTTP-gzip acceptance remains available. National archive receipts
  can be directed to each gate's evidence directory, preserving cold/warm
  exports separately. These changes do not alter models, legal algorithms,
  approved failures, frozen fixtures or acceptance criteria.
- Local verification uses Windows / Node 24.20.0, 28 logical CPUs and about
  63.8 GiB RAM. The final production regeneration passed in 86.4 seconds, both
  type checks passed, and all **1,353 unit tests** passed in 62.8 seconds. The focused graph
  checks also passed after exercising missing/corrupt lazy binary dependencies.
  Actionlint 1.7.12 validates the complete workflow. The final application gate
  passed **168 browser checks**, without skips or retries, in 34 minutes
  33 seconds: each base passed 73 general, three editor and eight national
  Chromium/Firefox workflow checks. The final staged-site matrix passed **6/6**
  in 138.9 seconds with no page/network errors. It covers all four supported
  national law/model pairs, Emilia-Romagna, direct loads/refreshes/navigation and
  structured/CSV exports. The complete local warm hosting command passed in
  **38 minutes 26 seconds**, including regeneration, application acceptance,
  final Pages build and staged-site checks. This is not a cold installation or
  a hosted Linux timing.
- The final publishable directory contains **68 files / 112,870,858 bytes**;
  **241** accumulated build files are excluded. Summed gzip size is an estimate
  of **20,862,493 bytes**, not a CDN measurement. Two additional fresh builds,
  using the same baseline revision, matched the tested site's entire manifest:
  `6040401b4abd07e1f17189cf8b77d786e969854689b59c20342fe3747a44ec5d`.
  Retained sources occupy 503,942,038 bytes and model caches 34,054,066 bytes.
  Full local evidence excluding the site is 154,635,598 bytes for this pass,
  before artifact compression. Start with `data/derived/hosting/local/report.json`
  for combined local verification or `data/derived/hosting/warm/report.json`
  for the complete command, linked logs/reports, source receipts and archives.
  Payload reports and `site/release.json` retain exact data versions/hashes.
  The baseline is `55647ca7f53edea047bdc128947addb1063f82f7` plus these uncommitted
  changes (`workingTreeDirty=true`), not a claim that the implementation is
  contained in that commit.
- Local encoded body transfers from final raw static delivery were **21,907,131 /
  24,753,419 bytes** for Rosatellum M01/M02, **21,806,620 / 24,652,908 bytes**
  for reform M01/M02, and **6,000,704 bytes** for Emilia-Romagna. Each uses a
  fresh browser context and includes execution/exports; exports themselves are
  generated locally. The repeated direct-route matrix transferred 106,632,796
  bytes and is not a representative single visit. Public CDN compression and
  transfer remain unmeasured; the raw national workflows imply roughly
  4,000–4,600 workflows per 100 GB before compression or repeated-browser reuse.
- Development failures remain on disk: the first run caught a new test's law-ID
  type widening; initial staged checks exposed the test server's missing
  directory redirect. That incomplete run was stopped and preserved, then the
  corrected six-test matrix passed. The first broad browser run then reported
  26 passes and 47 failures: Vite preview also serves directory indexes without
  the canonical redirect, breaking relative imports on extensionless direct
  loads. General browser acceptance now uses the release static server as well;
  focused direct-route and national-execution checks passed 2/2 before the full
  rerun. Original failure reports/logs remain under
  `data/derived/hosting/failed-warm-preview/`. That rerun passed 67/73; six
  assertions encountered active import/preparation or queued chart queries.
  The missing-pin validation assertion now permits 30 seconds for streamed
  metadata; authored startup uses the existing two-minute preparation budget;
  synthetic presentation setup waits for worker readiness before asserting
  chart content. Expected errors, values, preserved drafts, legal outcomes and
  no-retry/no-skip requirements are unchanged. The failed run remains under
  `data/derived/hosting/failed-warm-async/`. All six affected cases then passed
  together before the final complete gate. Graph staging explicitly distinguishes
  binary `.u32` history/state columns from JSON metadata. No failing legal draw
  was replaced or rerolled to resolve these infrastructure issues.
- Remote work remains outstanding: this session has repository/workflow reads
  but no workflow-dispatch tool, and GitHub CLI is absent. The new workflow must
  first be committed/pushed; its migration-branch push runs the hosted gate
  without publishing. Local Windows/WSL cannot establish standard hosted Linux
  resource acceptance or Pages CDN behavior. No commit, push, merge, Pages
  setting, publication or public smoke was performed. Luca retains the merge;
  the final plan item stays open through hosted acceptance and publication.
