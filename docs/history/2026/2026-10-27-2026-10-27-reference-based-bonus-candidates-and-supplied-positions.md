## 2026-10-27 Reference-based bonus candidates and supplied positions

Implemented Luca's approved bonus-candidate proposal, including the subsequent
requirements to leave missing territorial support to parameter estimation and
to exclude supplied ordinary people from automatic bonus selection unless their
identity also has an explicit bonus template. The dedicated
[candidate guide](../../politics-reform-2026-candidates.md) contains the policy,
constraints, replay contract and implementation map; the generation/editor
guides link to it rather than duplicating its algorithm.

- Added the optional, versioned `candidateGeneration.bonusPolicy` value
  `reference-proportional-v1`. New defaults opt in. Absence retains the original
  generator, including its original candidate streams and template semantics,
  in `legacy-candidate-generation.ts`. Its function body matches the former
  implementation after the export rename. Parsing, metadata editing and opening
  old scenarios do not silently switch policies; the candidature editor offers
  **Aggiorna selezione del premio** for an explicit draft upgrade.
- Resolve central municipal references once and project fractional vote totals
  through filing-aware census splits and electoral capacities. Normalize
  coalition member support locally for each constituency and chamber. Hamilton
  allocation and its seeded exact ties do not use simulation IDs, realized
  election votes, supplied election results or future shocks. No territorial
  support correction, arbitrary positive prior or equal-share fallback was
  introduced. Standalone lists require no coalition weighting.
- Place supplied ordinary candidates in their exact positions and reserve
  supplied bonus positions. Resolve affiliation from its explicit declaration,
  existing ordinary badge or the unpinned proportional plan. Ordinary-only
  supplied identities block their qualifying prefix, including later positions;
  they are never silently promoted. Repeated supplied identities remain one
  person. A bonus person can use an existing local witness or an additional
  compatible ordinary placement.
- Plan feasible local witness prefixes and national rounded 60% gender budgets.
  Supplied declarations, capacity and gender constraints can adapt Hamilton's
  candidate counts. Minimize absolute departures first, then squared fractional
  deviations; preserve the earliest feasible traversal before preferring closer
  national gender balance. An explicit final-review regression covers every
  production constituency so gender ties do not unnecessarily replace the
  approved heads-first traversal with deeper positions.
- Per simulation, uniformly shuffle each list's locally filed colleges using a
  separate entity-addressed stream and traverse the same order at successive
  candidate levels. Reserve distinct bonus witnesses before reuse, retain pinned
  bonus positions and interleave remaining member-list queues. Ordinary reuse
  includes bonus people, with consistent badge/gender, no duplicate within a
  slate, at most five ordinary colleges and unchanged special incompatibilities.
  Impossible combinations fail explicitly rather than changing votes or rerolling.
- Record reference shares, Hamilton and constrained counts, gender budgets and
  policy/date in the generation receipt and deduplicated result metadata.
  `BONUS_SELECTION_CONSTRAINTS` reports changed candidate allocations without
  inventing legal coalition-member seat entitlements. Bonus previews show a
  qualifying placement in the correct constituency even for people reused
  elsewhere; ordinary previews show the selected college.
- Regenerated defaults/catalog through `data:reform:generate -- --publish`:
  defaults are now `r26-defaults-v2-a0179c6c253e`. Snapshot
  `r26-generation-v1-c5c4be9fafb5`, model `r26-models-v1-f71a72020aaa`, production
  compressed bytes/checksums and estimated source parameters remain unchanged.
  Older defaults remain available. This command only published local artifacts.

Verification on Windows, Node v24.20.0 and Chromium 148.0.7778.96:

- `npm run check`: zero errors/warnings; `npm run check:data`: passed. The
  official Svelte autofixer reported no issues or suggestions for the changed
  candidature component. Production static builds passed at `/` and `/elezioni`,
  retaining the existing large-chunk advisory.
- Final `npm test`: **80 files / 1,073 tests passed**, including 14 focused
  bonus-policy regressions. Coverage includes fractional split conservation,
  filing zeros, local references, fixed allocations and replay, shuffled college
  order, every constituency's unconstrained traversal, five ordinary uses,
  declared/undeclared supplied identities, duplicate bonus prevention, fixed
  bonus order, constrained allocations, unavailable supplied people, gender
  budgets and explicit conflicts. Existing worker, result/export, old-law and
  production regressions remain green.
- Final full browser suite: **33 passed** at `/`, without retries/skips.
  Includes new explicit policy-upgrade persistence and existing production
  named-candidate preview, scenario/result roundtrip, archive replay, worker
  cancellation/failure handling and 1/2/4-worker output invariance. Focused editor,
  workspace and static-route browser checks at `/elezioni`: **12 passed**.
- `data:reform:check` verified production checksums and completed **41 stages /
  588 filled seats / zero unresolved or unfilled seats** with the new defaults.
- Final production worker benchmark, 10 simulations with one/two compute
  workers: **10.679 s / 7.858 s** elapsed, respectively, no execution errors and
  identical outcomes across worker counts. Aggregate generation times were
  5.115 s / 5.802 s. Report:
  `data/derived/r26-bonus-selection/browser-workers-final.json`; CPU i7-14700KF,
  63.79 GiB physical RAM, controlled 8-core/16-GiB browser resource hints.
  This is a focused performance check, not renewed whole-migration acceptance.
- `git diff --check`: passed.

Verification workarounds: the full browser run exposed an existing assertion
that required an alert element while checking that no unknown-table error was
present. It now checks that matching error alerts have count zero, accepting a
properly absent alert without weakening the error check. Running type checks,
the unit suite and the 14-worker browser suite together hit three existing
five-second unit-test limits. After the browser run finished, the entire unit
suite passed on its own; no time limits or product behavior were relaxed.

No legal scrutiny, vote/preference parameter estimation, R source, existing
executed scenario or public deployment was changed. No commit was made.
