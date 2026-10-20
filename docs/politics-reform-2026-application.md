# Politics algorithms, worker and results (R26-20)

Use this guide for application integration. For preparation, schema versions,
model fitting, constructed inputs and explicit legacy conversion, use the
[generation guide](politics-reform-2026-generation.md). For legal logic use the
[core reading map](../web/src/lib/politics/reform-2026/README.md).

## Application behavior

- `/politics` defaults to **Legge precedente (R parity)**. Selecting **Riforma
  2026** opens a separate scenario/workspace with its own local storage. Existing
  legacy plots and tables retain their visibility. This selector does not compare
  algorithms or convert scenarios.
- Reform controls edit dates, seed, model profiles and national common/per-chamber
  shares. The complete scenario editor/import exposes coalition/filing choices,
  geographic references, candidates, supplied votes/preferences and conversion
  provenance. Shares in files are fractions; share inputs display percentages.
- Missing production data fails explicitly. The legacy worker no longer falls
  back to debug snapshots. Reform data must match the pinned compressed/decoded
  digests, schema and scenario snapshot/model versions.
- Reform execution uses global simulation IDs across 50-run chunks. Each ID
  generates both chambers before the common scrutiny entry point decides the
  joint bonus. Non-computable runs remain in the requested denominator; no
  reroll, legal-endowment rescaling or successful-run renormalization occurs.
- Reform has no arbitrary run cap. Its new viewer mounts one selected table,
  at most 50 detail rows, one selected simulation/chamber chart and at most ten
  warnings at once. Stored/exported results still grow with requested runs;
  large-run performance acceptance belongs to R26-21.
- Legacy politics retains its 1,000-run limit because its unchanged simultaneous
  scatter plots mount an SVG point per subject/run. Requests above the limit
  fail explicitly rather than silently truncating. Removing this restriction
  needs a rendering change that preserves the existing visible charts. Regional
  aggregate charts have no analogous run cap.

## Result and export contracts

`result-presentation.ts` owns reform aggregation and chart derivation. Tables
separate vote entitlements, cap effects, territorial targets, seat status,
candidate identities, original entitlement and actual election locations.
Coalition bonus ownership stays distinct from ordinary list seats and candidate
affiliations. Candidate conclusions retain missing/supplied/generated preference
conditions. Model/source/profile versions, conversion provenance, resolved
common/chamber/geographic references, census/decree notes and applied
interpretations remain inspectable. Profile sensitivity is conditional on the
selected profile pair, not calibrated uncertainty or a cross-law comparison.

Structured exports use the generic schema-v2 columnar module and include the
**executed** scenario, replay seed, algorithm/model/data identities and complete
worker presentation result. Editing a draft does not alter exported results.
Imports validate reform algorithm/seed/data agreement; old politics exports
without an algorithm ID explicitly resolve to `politiche-r-parity-v1`. Both
workspaces route imported results to their matching law. Reform scenarios are
never interpreted by the old scenario parser. CSV remains table-oriented.
Generated municipal inputs and composed legal stage objects are released after
each run; inspect the legal core directly when full internal stages are needed.

## URLs, loading and verification

Set `BASE_PATH=/elezioni` for a project-base static build (omit it for root).
Routes obtain SvelteKit's base and pass it to workers; workers must not import
`$app/paths`, which pulls unsupported client modules into their Vite bundle.
`core/runtime-data.ts` resolves data URLs and decodes verified gzip JSON. When
HTTP `Content-Encoding: gzip` makes Fetch decode the wire representation, verify
the decoded digest directly. For a raw gzip asset verify both digests. Neither
path accepts unchecked JSON or a debug fallback.

Focused checks: `core/runtime-data.test.ts`, `reform-2026/result-presentation.test.ts`,
`core/result-export.test.ts`, `politics/simulation-scrutiny.test.ts` and the
`reform-workspace` / `reform-production-worker` browser specs. The latter compiles
the actual production worker against a test-only catalog and constructed bundle,
covering chunk-boundary IDs without large production artifacts. Production UI
tests skip explicitly when their generated bundle is absent; release acceptance
must require it. The browser regression suite accepts `BASE_PATH` for root and
project-base runs. Run builds and browser tests sequentially.
