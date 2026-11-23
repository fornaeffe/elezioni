## 2026-10-28 — Unique Other category across implemented elections

Implemented Luca's approved Other-list plan with the revised UI: one separate
**Includi Altre liste** switch, a reserved `__other__` identity, fixed **Altre
liste** name and grey `#9ca3af`, and no per-list type selector. Reference shares,
geographic overrides and historical correspondences remain available; reform
also retains chamber presence and editable voting coverage. Disabling removes
owned references; reform uses its existing dependency-removal preview. Ordinary
list defaults and generated bundles were not edited.

The category is explicit `kind: 'other'` metadata, never name-based inference.
Legacy projection, candidate generation, vote preparation and adaptation retain
candidate-free votes; null candidate references do not enter attribution or
candidate pools. Every national/regional/minority admission path excludes Other,
including the Senate's later regional threshold route. Regional allocation and
presidential rankings exclude it while valid-vote denominators retain its votes.
The executable R scrutiny counterparts implement the same convention.

Reform generation separates voting coverage from candidate filings through
optional `ChamberInput.otherListCoverage`. It skips ordinary, bonus and special
candidates for both candidature policies, rejects supplied affiliations, links
and preferences, and records `other-list-excluded` admission decisions. Special
valid-vote totals add unlinked Other votes exactly once; Camera TAA remains an
overlapping view. National and college statistics retain Other shares and zero
seats, including when other outcomes are unknown. Candidate probability and
bonus-contender selectors omit it. Existing legal allocation/recovery policies
remain unchanged.

Old inputs omit the optional discriminator and retain ordinary behavior. Scenario
exports with Other use politics v8/regional v2 envelopes, while ordinary exports
retain v7/v1. Reform schemas accept the additive fields without rewriting old
scenario fingerprints or candidate policies. Scenario conversion and archive
round trips preserve the category. The compact entry point for future work is
[the shared category contract](../../other-list.md), linked from the reform editor guide.

Verification and workarounds:

- New coverage includes 21 focused unit tests and three browser tests across all
  editors/workers: uniqueness, fixed metadata, persistence and archives, history
  without supplied shares, forbidden candidate/bonus associations, geography,
  vote conservation and Other shares through 100% under both reform candidate
  policies. Regional high-share tests conserve 48 list seats plus presidential
  and runner-up seats, with zero for Other.
- `scripts/check_other_lists.R` and
  `web/scripts/check-other-list-parity.ts` pass for real Camera/Senato golden
  inputs extended with Other, and constructed regional cases at 1/30/80%.
  R's candidate birth-date columns must be reconstructed as POSIXct from JSON;
  otherwise existing multi-column `order()` calls take an incompatible path.
- Re-exported both R golden suites to temporary files. Regional output equals
  the existing fixture; politics trace/output content equals the existing
  fixture after canonicalizing table row order. The host's default R locale
  differs from fixture collation. Windows R needed
  `LC_ALL=English_United States.utf8` for explicit UTF-8 sourcing. No fixture was
  overwritten.
- Official Svelte checks found no issues in the reform editor changes. Legacy
  route suggestions concern existing unkeyed loops, actions and local Map
  helpers; the existing asynchronous result-fetch effect remains intentional.
- The first unbounded browser run passed 37/39 tests but exposed timing in two
  existing statistics tests; all six statistics tests passed with one worker.
  The archive-replacement test now waits for the new result before opening its
  panels. A later unbounded unit run exceeded the existing five-second deadline
  in the serial/parallel scrutiny-comparison test. The acceptance runner now
  accepts optional `ACCEPTANCE_TEST_WORKERS` (1–64), applied to Vitest and
  Playwright. This limits simultaneous test processes, not simulated compute
  workers; runtime resource limits and assertions are unchanged.
- The four-process browser run passed 39/39 at `/`; `/elezioni` passed 38/39
  with the old-archive statistics preparation exceeding its existing timeout.
  Final full acceptance with `ACCEPTANCE_TEST_WORKERS=1` passed: production
  checksums/vertical slice, preparation and application types, 1,095 unit tests,
  39 browser tests at `/` and 39 at `/elezioni`, zero skips/retries, both static
  builds and restoration of the root build. Its report is
  `data/derived/r26-21-acceptance/report.json` (ignored generated evidence).
- Final layout polish uses a wrapping fixed-category row and a grey marker with
  normal-contrast name text. Post-polish Svelte check is clean; responsive row
  screenshots were inspected at 390px for legacy politics and regional rows.
  Post-polish focused browser tests pass at both base paths.
- Final review added a regional summary guard for ordinary standalone inputs
  with null coalition metadata: Other must not be included in that contender's
  final vote totals. The new regression and latest full unit suite pass:
  81 files / 1,096 tests. Preparation/application type checks also pass.
- All three worker benchmark tests pass. Legacy politics 10/100/1000 simulations:
  3.805/17.227/147.627 s; regional: 0.206/0.512/3.639 s. Focused production reform
  10-simulation runs with 1/2 compute workers: 10.549/7.458 s, zero errors and
  invariant outcomes/warnings. These are current-host observations, not a
  controlled comparison against older reports. Reports are temporary
  `elezioni-other-*-benchmark.json` files; tracked benchmark reports were restored
  byte-for-byte. No full reform 100/1000-worker matrix was rerun.

No commit, publication, source-data update or generated-default change was made.
