# CP-01 — Reproducible comparison evidence

Use [inventory](inventory.md) for field/control/asset ownership. Run commands
from `web/`. Source baseline is
`a700d19b04272f39fad17ddd3ceb9e48048ea623`, before any merger runtime changes.
The added collectors and this guide are test/documentation tools only.

## Retained generation and candidature

`web/tests/fixtures/common-politics-baseline.json` records the current outputs of
`web/scripts/acceptance/common-politics-baseline.ts`. The input recipe is the
tracked `originalFitFixture` and its transitive reduced fixtures, with fixed
dates, seeds, source observations, conditional electorates, fits and legal
geography. The collector sets each exact model to 2.0.0 and the registered M02
forecast method to `bounded-mass-pair-field-v2`. Its input digests cover the
scenario, numerical assets, source envelope and legal context (plus compiled M01
inputs for successful cases). No downloaded production bundle is required.

```powershell
npm test -- --maxWorkers=2 scripts/acceptance/common-politics-baseline.test.ts
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/common-politics-baseline.ts
```

The second command prints a fresh report to stdout; it never updates the stored
baseline. Compare it with the tracked JSON. For an original replay after code
changes, use an isolated checkout at the source revision and copy in the CP-01
collector from this change. Do not modify the active checkout's legal fixtures.

| Case, for both M01 and M02 | Comparison input and recorded result |
| --- | --- |
| `joint` | Simulations 1–16, complete reduced geography including VdA/TAA; per-chamber elector/valid/non-valid/list counts; first-list mean, population SD and extrema |
| `camera`, `senato` | Simulation 7, one active chamber, retained inactive declarations; actual one-chamber ballot output |
| `chamber-values` | Simulation 7, separate national Camera 55% and Senate 35% list references, each SD 1 pp; no collapse to common values |
| `restricted` | Simulation 7, remove one list from an entire ordinary Camera territory, keep Senate and special filings; M02 completes, M01 records its existing capacity error |
| `manual` | Simulation 7, explicit stable `cp01-person`, female, 1970-01-01, available; pin ordinary Camera head; report actual declared identity plus candidature hash |
| `residual-new` | Simulation 7, move one historical component to reserved candidate-free `Altre liste`; add a new component/list with labelled fallback and a common national 10%/1 pp reference |

Each successful draw records per-college valid totals and first-list valid shares,
a ballot digest, separate candidature/preference
digest, candidate count, declared candidate biography and trace warnings/model
identity. Physical totals exclude Camera TAA's derived proportional view, so its
special ballots are counted once. There are 14 cases: 13 successful cases,
43 generated draws, one recorded preparation blocker. The 16-draw summaries are
small reproducibility samples, **not** predictive accuracy or coverage acceptance.
Existing statistical tests below provide stronger invariant/moment checks.

The restricted M01 case currently throws:

```text
Reference votes conflict with filing/capacity camera/990001/A
```

This is a reduced-fixture baseline, not a claim that every restricted scenario
fails. Keep the error visible and attributable during CP-03; do not weaken
validation or reroll. If a cleaner adapter resolves this boundary without changing
the model, document the cause and changed comparison result. Exact old hashes
are supplementary RNG/ID traversal evidence; their equality does not dictate the
new architecture. Do not refresh baselines merely to hide unexplained differences.

| Retained policy / edge | Existing executable evidence under `web/src/lib/` |
| --- | --- |
| M01 latest pair, original compositions, partial Camera named polls, separate chamber membership, portable replay | `politics/reform-2026/vote-share/unified-generation.test.ts` |
| M01 turnout-preserving versus M02 all-elector conditioning; missing history/regional/equal fallback; new support; impossible overlaps and physical filing | `politics/reform-2026/vote-share/correspondence-policy-audit.test.ts` |
| Restored M02 local reference law, remove unfiled components before movement | `politics/reform-2026/vote-share/restored-models.test.ts` |
| M02 physical totals, VdA/TAA, row/chunk/simulation-order invariance, references, residual/new lists, stale state and invalid 25+ pair | `politics/reform-2026/vote-share/generation.test.ts` |
| Profile correlation and shared municipal noise; bounded forecast laws and reference feasibility | `politics/reform-2026/vote-share/{reference-noise,forecast-numerics,feasible-field,numerics}.test.ts` |
| Candidate generation/pins, preference budget, explicit missing preferences, chamber dependence, single renewal and independent special candidates | `politics/reform-2026/generation.test.ts`, `bonus-candidates.test.ts` |
| Preparation invalidation, stale/late replies, cancellation, offline checkpoint and exact fitted arrays | `politics/reform-2026/vote-share/{fit-service,fit-client,editor-session,release,release-loader}.test.ts` |

The approved pairs use these comparisons differently:

| Pair | Baseline boundary |
| --- | --- |
| Reform + M01 | Retained statistical/candidature corpus and policy tests, followed separately by reform fixed-input legal tests |
| Reform + M02 | Same separation, with retained M02 grouped-source fitting/forecast and reference tests |
| Rosatellum + M01 | Retained M01 statistical semantics; existing Rosatellum fixed-input legal/candidature evidence below. The new adapter is CP-03 work; there is no pre-existing execution to baseline or M00 distribution-parity requirement |

## Fixed-input legal and Rosatellum candidature evidence

These inputs/expected outputs predate CP-01. Keep them independent of statistical
generation, default changes and the new comparison JSON. Do not rerun a generator
to replace fixed votes/candidates, and never regenerate expectations to pass.

| Evidence | Reproduction / assertion |
| --- | --- |
| `test/fixtures/r-reference-manifest.json` | `scripts/data-preparation/workspace-preparation.test.ts`: compressed and decoded SHA-256 and exact byte counts, including frozen Camera failure |
| Rosatellum `politiche/debug_scrutinio.json.gz`, `generated_adapter.json.gz` | `src/lib/politics/{golden-fixture,adapter,simulation-scrutiny}.test.ts`: ten paired fixed-input outcomes, contexts and legally meaningful ordering/tie behavior at the existing test boundaries |
| `politiche/candidate_generation.json` | `src/lib/politics/candidate-generation.test.ts`: exact R sample-draw replay including 56 draws and both chambers' uni/pluri rows; keep known null/NA sampling behavior |
| `politiche/legacy-camera-flipper-584.json.gz` | `src/lib/workspaces/filing-generation.test.ts`: exact original legal failure, independent of revised production data/seed. A new generation seed need not encounter it |
| `test/fixtures/reform-legal-inputs-v1.manifest.json` and frozen input archive | `src/lib/politics/reform-2026/legal-regressions.test.ts`: checksum and accepted I13/I14/I16 paths using original run IDs and inputs |
| Constructed legal edge fixtures | `src/lib/politics/reform-2026/{contracts,scrutiny,ties,admission,territorial-eligibility,special-seats,special-attribution,taa-allocation,proclamations,multiple-elections,recovery}.test.ts`: fixed candidature/vote contracts, missing/zero distinctions, age/availability/external declarations, ties and explicit non-computability |

`src/lib/politics/reform-2026/vote-share/generation.test.ts` also records a tiny
generated electorate's N13 `initial-overallocation` result before scaling its
electorate for a separate successful case. Preserve both cases; this is not
permission to scale or reroll a failing real run. Known Rosatellum TODO/law-review
behavior in the plan remains unchanged.

## Defaults and pinned assets

```powershell
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/common-politics-inventory.ts
```

This prints [default-inventory.json](default-inventory.json)'s data, verifying the
compressed/decoded catalogue and scenario descriptors before reading them.
It requires the currently prepared release and fails if absent or corrupt.
It does not regenerate production assets. Compare the reported hashes/choices
before CP-04; never feed this report back into preparation.

The report confirms 154 component mappings, 126 pooled to `historical-rest`,
34 components, 53 M01 source compositions, two M01 partial named observations,
17 political references and four non-valid references. Pooling therefore stays
in CP-06 under D14. Generated defaults are unchanged by CP-01.

## Sessions, diagnostics and browser behavior

Unit evidence includes `core/{result-export,stored-diagnostics,json-stream}.test.ts`,
`workers/{reform-compute,reform-pool}.test.ts`,
`politics/reform-2026/{archive-routing,result,stored-probabilities}.test.ts` and
the portable-session tests above. Preserve committed prefixes, explicit zero
completed coverage, unknown outcomes, original simulation IDs, requested
denominators, diagnostics membership/count/context and exact checkpoint identity.

```powershell
npm run check:data
npm run check
npm test -- --maxWorkers=2
npm run test:e2e -- workspace-uniformity.spec.ts reform-production-worker.spec.ts --workers=2
```

The last command builds first, then serves/tests the production app sequentially.
`workspace-uniformity` covers separate current drafts, default filing/results/reset,
navigation cancellation, metadata-based import routing and missing/corrupt required
assets. `reform-production-worker` covers actual-worker cancellation and closed
workers, exact exported rows/diagnostics across 1/2/4 workers and a 50-run boundary,
worker crashes and quota failures with durable prefixes. Use these current
behaviors as evidence; CP-05 intentionally rejects the old formats and CP-07
replaces separate drafts with the approved common lifecycle.

For later release/performance acceptance, use CP-08's complete three-pair gates,
both URL bases and its prescribed 1,000-run campaigns. This baseline step does
not substitute for those gates or promise above-1,000-run resource acceptance.
Actual CP-01 command outcomes and environment are recorded in
[migration history](../migration-history.md#cp-01--inventory-and-preserve-behavior-2026-11-07).
