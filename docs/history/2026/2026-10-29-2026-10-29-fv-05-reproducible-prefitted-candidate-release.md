## 2026-10-29 — FV-05: reproducible prefitted candidate release

Completed FV-05 for the 2026 reform only. The
[release guide](../../vote-generation-fast-release.md) is the task-specific entry point;
the [small evidence snapshot](../../../test/fixtures/benchmarks/vote_share_release.json)
pins inputs, implementation hashes, artifact hashes and measured outcomes.
`data:reform:shares:release -- --download` now acquires/verifies pinned sources,
prepares original compact history and unchanged legal/preference inputs, prefits
the explicit default, builds both chamber profiles, packages immutable states,
runs application/data checks and a static build, verifies every final copied
asset and finally atomically selects the candidate pointer. Offline replay omits
the download flag. The same non-interactive Node 24 commands are available for
local and CI execution; no Linux runner measurement or public workflow deployment
was performed.

The tracked policy fixes reference/election dates 2026-10-19 / 2027-09-25, fit and
simulation seeds, all eight C/E histories, 35 selected polls and six explicit
exclusions. Three multiple-residual-answer polls cannot be represented by the
single offered-complement policy; three already had conditional-ballot exclusions.
All 41 original questions and all ten original historical chunks remain available,
including the unselected Senate-2018 25+ audit. Senate adaptation uses the original
2022 common-18+ pair. Conditional future electorates remain 46,120,143 per chamber
at the Camera-2022 municipal vintage, not a demographic forecast.

The installed filing choice set has eight ordinary nationwide Camera lists;
its other lists are geographically restricted, including opaque special-ballot
groups. Directly fitting unrestricted national margins for every minor group
would demand more votes than their physical filing permits in some territories.
The explicit candidate therefore fits nine main components plus an unfiled
historical residual, keeping current joint groups whole and Azione/Italia Viva
separate. The other 25 future components use the existing reference-supported
mechanism, with paired-2022 regional anchors conditional on physical filing and
declared fallback profile/innovation scales. Their original counts remain in
the pooled historical likelihood and source. This is a labelled candidate
baseline, not an empirical fit of each restricted party or predictive acceptance.
The resulting complete choice set retains 22 Camera / 25 Senate lists, 35 future
components, 7,904 municipalities and 15,810 physical cells. FV-08 must review these
fallbacks and sensitivity before activation.

An earlier exploratory 24-coordinate basis failed the unchanged `1e-11` solve
residual gate; an intermediate computable basis still exposed joint TAA filing
capacity failure. No ridge, relaxed tolerance, forced target repair or numerical
kernel change was retained. The final explicit coarsening and regional anchors
use the existing approved whole-group/new-support operations. The FV-02 numerical
kernel hashes and FV-01 frozen research remain unchanged.

Portable fitted/profile/runtime/scenario/catalog envelopes now preserve all
accepted joint-context Float64 draws and typed integer/fallback columns, with
explicit shapes, little-endian layout, deterministic gzip and both compressed
and decoded SHA-256. Semantic composition/run keys separate evidence-dependent
fits from future-list/reference/run changes. Cache hits validate immutable
content and parents; present corruption is a failure. Provenance-only changes
can re-envelope compatible numeric arrays. Normal candidate loading calls neither
fitting function and fetches no original source data.

The first source-only checkout exposed the old Node preparation dependency on
generated `.svelte-kit/tsconfig.json`. Added a standalone Node configuration for
these commands and preparation checking, allowing preparation before the first
app build. Browser tests then exposed transparent HTTP gzip decoding; an explicit
HTTP response wrapper now verifies decoded size/hash when compressed wire bytes
are unavailable. Disk/cache validation still requires exact compressed bytes.
Vite preview serves intermediate SvelteKit output and can retain the pointer
copied before final selection. Candidate browser acceptance therefore serves
the adapter's actual final `build/`, rather than relying on that preview copy.

Verification:

- Isolated source-only cold/warm preparation starts without research, ignored
  production/debug bundles, originals or numerical caches; fresh dependency
  installation, pinned downloads, application/data checks and final builds pass.
  Cold/warm and local artifacts match byte for byte, including the pointer.
  Missing lazy-refit chunks and corrupted fit caches fail explicitly; failed
  preparation preserves the previously selected candidates.
- On Windows with Node **24.20.0** / Intel i7-14700KF, the final cold release took
  **233.36 seconds**, including pinned downloads, historical/preference preparation,
  validation and build. The local warm release took **53.23 seconds**, with all
  four caches reused. Cold preparation-process peak RSS was **2,056,120 KiB**
  (about 1.96 GiB), including raw extraction/preference preparation and excluding
  build-child/browser memory. These are periodic preparation measurements, not
  browser parameter-fit latency or numerical-workspace acceptance.
- Generation payload including pointer/catalog is **4,402,973 bytes**; all optional
  refit metadata/history adds **4,512,456 bytes**. No Float64 quantization or
  reference-draw thinning was introduced. The accepted political and non-valid
  states each retain four chains with 2,000 draws per chain. Political maximum
  R-hat is **1.001501**, minimum bulk ESS **3,365**, minimum tail ESS **5,012**;
  non-valid diagnostics also pass.
- One complete default paired draw conserves each chamber's conditional
  electorate, has projection residual **9.6765e-10**, and passes legal scrutiny
  with all **588** seats filled. This is a package/computability check, not the
  1,000-run production seat or predictive acceptance required by FV-08.
- Chromium **148.0.7778.96** passes three actual-static-HTTP tests at each of `/`
  and `/elezioni`: Node-identical fitted buffers and legal input without refitting,
  demand-loaded original source including the 25+ audit, and explicit required
  asset failure. HTTP tests distinguish encoded Content-Length from transparently
  decoded body bytes. The main page remains responsive during the test-worker run.
- The full suite passes **92 files / 1,181 tests** with `--maxWorkers=4`.
  The first unconstrained run hit the existing five-second timeouts in two
  unrelated large legacy tests under parallel load; bounding test workers resolved
  this without changing tests, numerical tolerances or runtime behavior.
  `check:data`, `check` (zero errors/warnings), both static builds, FV-01's 1,433
  original inventory / 61 retained-evidence checks, link targets and whitespace
  checks pass. Existing bundle-size/plugin-timing advisories remain. The root
  static build was restored.

Installed parsers/defaults/catalogs/dispatch/editor and legal scrutiny algorithms
were not changed. Candidate artifacts are ignored regenerable files. Browser
refitting, cancellation/revision ownership, full input-envelope latency/combined
memory, predictive acceptance and app activation remain FV-06/07/08. Research
archive/deletion remains FV-09. No commit, push or public deployment was performed.
