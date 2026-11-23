## 2026-10-19: R26-18 composed scrutiny and simulation registry

Implemented the requested composition/registry step. Use the
[core boundary](../../../web/src/lib/politics/reform-2026/README.md#composition-and-registry-r26-18)
for direct execution, request/result types, registry availability and limitations.

### Delivered behavior and rationale

- `runPoliticsReformScrutiny()` validates the full owned envelope before draws,
  derives all stage communications within one global simulation, and composes
  N01-N27 through N28. It reuses special winners, attributes raw votes once,
  aggregates distinct national/ordinary figures, decides the paired bonus, runs
  each allocation/cap path, proclaims candidates, resolves multiple elections,
  and performs terminal initial recovery. It accepts single-chamber renewals.
- Added the simulation-level `runPoliticsSimulation()` boundary and registered
  both real algorithms. The legacy adapter calls the unchanged scrutiny and
  checks supplied `SIM` tags. Reform requests must match their envelope's
  algorithm/simulation. Unknown IDs are rejected by the common resolver; reform
  is explicitly unavailable in production mode until R26-19/20. The existing
  legacy generator/worker resolver retains its reported unknown-ID fallback,
  but now rejects a known reform request instead of executing the old law.
- Default legal streams include seed, stream version, algorithm and simulation
  ID; callers can still inject draws. Results retain source/model/seat metadata,
  conditions, applied policy versions, admission/bonus reasons, detailed stage
  communications, exact arithmetic, located allocation movements and final
  original/actual seat locations. Owning stages contribute warnings/draws once.
  Terminal ledgers check domestic entitlement totals and unique candidates.
- Recovery adapters use admissions, including zero-seat lists and local TAA
  exceptions, rather than positive targets alone. They preserve final matrix
  decimal states, Senate cap transitions, regional coalition-list quotas and TAA
  quotas with source IDs. A consumed decimal stays in the already-used tier;
  recovery does not consume it again. Coalition affiliations remain separate
  from bonus entitlements. Located transfer summaries also preserve coalition
  membership rather than relabelling member lists as standalone subjects.
- Composition exposed the need for a unique TAA proportional communication.
  Added an endowment check for exactly one Camera TAA proportional college,
  preventing selection of an arbitrary view or duplicate proportional counting
  before any draw. Legal references remain next to this validation and the
  existing scrutiny operations.

### Verification, problems and limits

- Independently worked complete fixtures use six-seat ordinary colleges,
  disjoint seven-person slates and supplied preferences. Full domestic A/B/C
  entitlements are: no bonus Camera `179/149/64`, Senate `92/73/31`; bonus
  without cap Camera `217/122/53`, Senate `111/59/26`; corrected cap Camera
  `220/121/51`, Senate `113/64/19`. Tests explain their quota/remainder arithmetic,
  count 8/7 special seats, preserve 70/35 bonuses, and fill 392/196 unique people.
- Additional composed fixtures cover mixed-affiliation coalition bonuses,
  ordinary/bonus replacement, I07 cross-region ordinary and bonus recovery,
  missing preferences, fully unavailable candidates, single renewals, zero-vote
  and zero-direct-support failures, injected/seeded replay, simulation identity,
  owned JSON output, source remainder states and raw/final vote equivalence.
- Some source stages do not calculate college/coalition or single-college
  quotients. The adapter does not invent them or substitute cap group indices.
  A constructed shortage test proves the affected recovery ends explicitly
  `unresolved` with `missing-recovery-remainder`, retaining all 588 entitlements.
  This remains a candidate-output limitation requiring reviewed source-stage
  support before claiming unrestricted candidate-level acceptance. The existing
  focused R26-17 boundary can accept independently established communications.
- The initial legacy adapter test compared R keyed-table ordering directly to
  TypeScript emission order. Corrected the test to compare keyed tables, as the
  existing golden checks do, while also requiring exact equality with the direct
  TypeScript function. All ten paired old-law simulations, including simulation
  4, pass without changing legacy legal behavior. An initial exhaustion fixture
  used low counts that correctly failed I04 integer-quota allocation; replaced
  it with a valid full allocation and explicit post-filing unavailability.
- `npm run check:data` passed; `npm run check` passed with zero errors/warnings;
  `npm run test` passed **766 tests in 48 files**, without skips. The static build
  passed. Browser verification passed **8 Chromium tests**, including politics
  and regional regressions and a new bundled module-worker test of direct reform
  execution, transferable paired results, replay and unavailable/unknown IDs.
  The browser worker is test-only and does not activate production dispatch.
  Build/e2e ran sequentially; Vite plugin timing and terminal color notices were
  informational, with successful exits.
- R26-19 generation/model fitting and R26-20 scenario/production worker/UI/export
  integration remain inactive. No generated defaults, data bundles, legal
  interpretation policy, production deployment or saved-file migration changed.
  Both plans now continue with R26-19. Changes are left uncommitted.
