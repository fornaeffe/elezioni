## M01/M02 pre-correction restoration — 2 November 2026

Luca requested restoration of both implementations to immediately before
`4ee5a91` after reviewing the historical regression, and updated definitions with
advanced versions. The baseline is its parent,
`1c19d54d4ccd0e0f3a05940c3cd28fa5bcab0a1b`. Both laws are now **2.0.0**: the
change to specified probability laws/input meanings requires MAJOR under the
existing model-version contract. Canonical M01/M02 identities, current v5 routing,
shared legal assembly and unrelated UI fixes remain; no scrutiny rule changed.

M01 again projects historical source compositions onto future lists, preserves
historical turnout after mapping/filing, uses the original regional/equal-list
fallback, latest-publication list polling and list-level shocks, apportions
municipal counts, and reconstructs college counts afterwards. This deliberately
restores municipal filing-union, territorial-ticket, zero-support and greedy
allocation limitations documented in the revised definition. M01 scenario SDs
remain inactive; joint-list component-subset references are M02-only.

M02 again uses coarsened national observations, the original exact-group national
profile baselines, early national/local filing conditioning and locally normalized
reference fields. The default retains mass-weighted bounded pairs, scale 1.5 and
regional profile correlation 0.25. Compulsory unrestricted raking, original-strata
fitting/profile amendments, reference-backed unidentified contrasts and final
post-movement filing conditioning were removed. Pre-correction optional forecast
submethods remain explicit scenario choices. Restored defaults omit the six
model-derived missing-component anchors introduced by the superseded policy.

Updated the authoritative M01/M02 definitions and current documentation pointers,
registry labels, strict scenario versions, provenance, reference editor and release
preparation. Old 1.0.0 scenarios/aliases do not execute under 2.0.0. Preserved the
1.0.0 definitions under `docs/models/archive/` and isolated their historical Camera
kernels in `web/scripts/acceptance/archive/models-v1/`; these have no application
runtime dependencies. The historical adapter's seven checks remain in the unit
suite. Original reports/benchmark receipts are not overwritten. Historical
campaign/curation commands fail on unsupported 1.0.0 rather than mislabel output;
the report instructs reproduction at its original commit. The current smoke
script writes only `data/derived/model-rollback/`.

Verification:

- **113 unit-test files / 1,291 tests passed**, no skips. Two new regressions
  distinguish conditional from country-calibrated references and require filing
  zeros before movement; restored pre-correction reference/generation tests pass.
- `npm run check:data`, Svelte checks (zero errors/warnings), and static builds at
  `/` and `/elezioni` passed. Svelte autofixer found no issues in either changed
  editor. Local links in both current and archived definitions and
  `git diff --check` pass.
- Regenerated all release assets through the normal source-pinned pipeline.
  Final catalog:
  `fast-share-catalog-v1-0c255ade89a4f9e84edefd4e1d727c4de00496d0f2ce23778b7ebf0007ce61db`.
  It covers 7,904 municipalities, 15,810 cells, 28 fitted and 34 future components;
  preparation verifies source integrity, semantic keys, zero startup fits and a
  complete 588-filled-seat legal smoke run. The final root build is restored.
- `scripts/acceptance/model-rollback-parity.ts` extracts the pinned Git baseline
  into ignored derived storage and compares both laws on the same restored
  full-default input. Draws **1, 2 and 7 for each model** match the predecessor's
  complete generated inputs, including votes/candidatures, after excluding public
  model/parameter versions and the legacy input hashes containing the changed
  selector/version. The first strict comparison exposed only those provenance
  hashes; the normalization is explicit in the script and receipt. No votes or
  candidate data are excluded. Compact evidence:
  `test/fixtures/benchmarks/model_restoration_parity.json`.
- Seven Chromium release/refit/worker checks pass at each base path (**14**).
  Both model workflows pass in Firefox at `/` and Chromium at `/elezioni`
  (**four** more). The initial root Chromium suite passed both model workflows,
  composition editing and high-RAM worker controls; its low-RAM control timed out
  before the election selector appeared. That unchanged test passed in isolation
  on the final build. These are **23 successful focused browser checks across
  the runs**, with that original navigation failure recorded, not one pristine
  campaign. Cause of the initial timeout is not established; no wait or production
  rule was weakened to pass it.

The first release attempt caught a Windows encoding/newline mistake while
restoring the reference editor; converted it to UTF-8 with normalized newlines
and rebuilt successfully. A preparation check also caught the missing explicit
model gate during the port; both direct and dispatched generation now enforce
the supported identities. Derived release/parity reports are in
`data/derived/model-rollback/`. Existing receipts retain their historical scope.
No new 1,000-draw predictive/resource acceptance campaign, manual acceptance,
commit or public deployment is claimed.
