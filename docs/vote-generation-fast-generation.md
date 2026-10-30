# Fast generation and paired reform integration (FV-04)

The headless generator is implemented for the **2026 reform only**. It consumes
accepted reference/profile states and returns validated scrutiny input. Installed
schema-4 parsing, candidate defaults, dispatch and editor are available through
the opt-in fast workspace; ordinary default activation remains gated by FV-08.
For the 30 October replacement kernels, read the
[feasible model](vote-generation-feasible-model.md) and its
[research results](vote-generation-feasible-research.md). The global Beta and
projection descriptions here retain the original FV-04 comparator contract.
Start here for common physical-cell/reference/chamber assembly; use the
[numerical guide](../web/src/lib/politics/reform-2026/vote-share/README.md) for fitting
and the [source guide](vote-generation-fast-data.md) for original observations.

## Task map

| Task | Entry point under `web/src/lib/politics/reform-2026/` |
| --- | --- |
| Compile a headless scenario/assets call | `vote-share/forecast-types.ts`, `vote-share/generation.ts` |
| Reconstruct dated physical electorates | `vote-share/physical-cells.ts` |
| Calibrate/sample joint uncertain references | `vote-share/reference-family.ts` |
| Resolve common/chamber/scope precedence | `vote-share/reference-plans.ts`, `vote-share/interventions.ts` |
| Finite-step Beta movement and independent streams | `vote-share/transition.ts`, `vote-share/forecast-random.ts` |
| Check support capacity and solve global margins | `vote-share/projection.ts` |
| Prepare original paired 18+ observations | `vote-share/paired-source.ts` |
| Prepare Senate contrasts/profiles | `vote-share/senate-adapter.ts` |
| Assemble counts, people, preferences and legal input | `generation.ts` → `assembleReformSimulation` |
| Review fixed equations/acceptance | [Reference contract](vote-generation-fast-references.md), [frozen protocol](vote-generation-fast-validation.md) |
| Review measured evidence | [Benchmark snapshot](../test/fixtures/benchmarks/vote_share_generation.json), [migration history](migration-history.md) |

## Calling and ownership

```ts
const prepared = prepareFastGeneration(scenario, assets, legalContext, {
  chunkSize: 128,
  maximumWorkspaceBytes: 256 * 1024 * 1024
});
const { input, trace, audit } = prepared.run(simulationId);
const result = runPoliticsReformScrutiny(input);
```

`FastGenerationScenario` is a typed compiled boundary, not an installed JSON
parser. Scenario schema 4 identifies the selected model, reference policy, RNG,
source/fit/profile/geography identities, fit seed and separate reference/election
dates. Assets retain four accepted chains, both C/E contexts, municipal profiles,
an explicitly prepared Senate adapter, physical geography and dated conditional
18+ electorates. Missing/failed diagnostics, broken simplexes and mismatched
state identities fail. Content verification/portable state packaging and the
application revision lifecycle belong to FV-05/06/07.

Preparation owns copies of its scenario/assets/legal context. Treat the returned
prepared object as immutable; reprepare after generation-affecting edits. One
call runs one simulation; scheduling, result storage and cancellation belong to
the application workers. No simulations × municipalities × components tensor is
allocated. Reference surfaces retain one simulation's cell/component arrays;
scores are built in bounded geographic chunks and cached as one flat Float64
matrix for the current chamber/pass. Global projection reuses that matrix and
support-group capacity state. Workspace estimates include these score buffers, but exclude
decoded source objects, temporary clones and the complete browser process.

## Generation order and errors

1. Apportion each dated municipal electorate over its physical census parts
   **before** filing or sampling. Exact integer municipal totals are conserved;
   submunicipal electorates are labelled population-weight reconstructions.
   Camera TAA's proportional view is derived from its special ballots once.
2. Sum fitted components according to the current lists and actual physical
   filing. Remove globally absent components with the approved all-elector
   renormalization. Draw Camera reference coordinates from the accepted C/E fit;
   adapt Camera coordinates for Senate. European coordinates are never Senate.
3. Add reference-profile uncertainty, mask unfiled support and project the
   country reference to the entire physical geography. This baseline projection
   must itself be feasible; later references do not repair a failed projection.
4. Resolve references national → region → province → municipality. Non-valid
   targets use elector weights; political targets use valid-vote weights and
   preserve each cell's non-valid probability. Explicit chamber references replace
   that chamber's common-derived value. Derive the effective country reference
   from the final local surface; superseded national targets are not reimposed.
5. Apply the selected finite-step Beta transitions and lognormal scale mixtures.
   The conservative **1.5 × raw election-discrepancy scale** uses unit exposure,
   including zero calendar horizon. Profile uncertainty remains at zero horizon;
   calendar movement and future local innovations vanish there.
6. Add the selected 15% regional / 85% municipal local variance split, recenter
   the geographic field on the effective reference and project globally to the
   future country margin. Chunking changes traversal/storage, not the target.
7. Sample integer component counts within physical cells, sum them exactly into
   filed lists, then use the shared candidate/preference/legal assembly. Supplied
   people, vote compositions and preferences retain their existing rules. Bonus
   candidature planning uses central effective reference support, not future draws.

Capacity checking solves a transportation problem over shared support masks,
including joint capacity constraints. Residual-network support identifies exact
boundary faces before raking. Maximum margin error is `1e-9` within 120 updates;
chunked/un-chunked results agree within `1e-10`. Unavailable support, impossible
moments, empty electorates and non-convergence throw. Legal non-computability is
returned by scrutiny with its original simulation ID. Neither path rerolls or
repairs its target. Requested-run denominators remain the caller's responsibility.

New components require a reference and a labelled geographic/profile/local/
national uncertainty policy. Their reference-profile uncertainty is sampled even
when the initial fitted probability is zero. Unfiled support remains exactly
zero; no epsilon or historical component count is invented. Other retains its
existing fixed legal identity and residual-vote behavior.

Joint uncertain references use independent Gaussian log weights calibrated
**after** the simplex transform. Exact categories are eliminated first. Laplace
factorization avoids a tensor grid in production; direct normal tensor integration
checks reduced fixtures independently. Closed families use deterministic starts
at SD-scale factors 1/0.5/1.5/2 and select calibrated candidates by score
variance, then centered mean norm and stable coordinate order; `1e-7` objective
ties retain stable start order. This is a numerical root search, not a proof that
arbitrary moment combinations are representable. Mean/SD error must be `1e-6`
share units and integration refinement below `1e-7`; unsupported combinations
fail with their requested moments. Numerical v1 explicitly bounds simultaneous
uncertain categories at **16**, independently of the 128-component model limit.
Larger reference sets require a measured resource-envelope extension before use.

## Senate adapter assumptions

`paired-18plus-logratio-adapter-v1` uses the same-date Camera/Senate 2022 18+
observations. `compilePairedHistory` applies reviewed memberships/whole-component
pooling and the explicit `equal-only` electorate policy: unmatched, invalid or
different-denominator rows are excluded with reasons, never repaired. All 7,904
current production targets have matching original 18+ denominators. Senate 2018
25+ data remains audit-only.

For each observable common group, the adapter shifts Camera log probabilities
by the paired Senate/Camera valid-share log ratio, clipped to ±1. Prefer separate
component contrasts; otherwise retain observable pooled contrasts with labels.
Missing contrasts use a labelled zero gap. Non-valid support uses the analogous
paired log-odds contrast. Local residual contrasts are clipped to ±1 and shrunk
by valid exposure / (valid exposure + 500). Missing paired profile coordinates
add a declared 0.45 profile SD in quadrature; movement/filter scales inherit the
Camera budgets. These are versioned conservative adapter assumptions, not a
direct Senate fit, inferred historical splits or empirical Senate calibration.

Coupling shares Gaussian innovations/quantiles with latent correlations
0.50/0.85/0.98 from the existing dependence profiles. It allocates each chamber's
existing marginal variance budget once; it adds no old paired shocks. These
numbers are assumptions about latent innovations, not promised correlations of
Beta draws, multinomial counts, party totals or seats. Common reference anchors
share their draw; explicit chamber anchors have separate streams. Candidate,
preference, vote, fit and legal-tie namespaces remain separate. One paired
election cannot identify general future forecast-error covariance. FV-08 must
validate political/default/seat sensitivity, including sparse special domains.

## Reproduction and remaining gates

From `web/`:

```powershell
npm run data:reform:shares:generation:check
npm run data:reform:shares:generation:country -- --asset PATH_TO_SOURCE.asset.json
npx playwright test tests/e2e/fast-share-generation.spec.ts --workers=1
$env:BASE_PATH='/elezioni'
npx playwright test tests/e2e/fast-share-generation.spec.ts --workers=1
Remove-Item Env:BASE_PATH
```

The numerical command fixes 120,000 transition draws and 50,000 draws for each
reference fixture under each of the three frozen seeds. It also retains all
32 synthetic paired legal runs per dependence assumption, including failures,
bonus outcomes and seat counts. The country command requires that reduced gate
first, measures a 128-target source-backed profile pilot, then tests all physical
cells with **two explicitly pooled observable blocks and no polls**. It holds
the dated Camera-2022 electorate conditional in both chambers and uses constructed
preference settings. It neither proposes nor installs a production default.
Regenerable detailed reports stay under ignored `data/derived/fv04-generation/`.

Browser tests build a test-only worker and check Node/Chromium parity at root
and project base; they add no application route or runtime asset. FV-04 does not
claim default predictive acceptance, 1,000-run production seat acceptance,
full-envelope latency, aggregate concurrent-worker memory, or cancellation.
Those gates remain in FV-05/06/08. Archiving/deletion remains FV-09.
