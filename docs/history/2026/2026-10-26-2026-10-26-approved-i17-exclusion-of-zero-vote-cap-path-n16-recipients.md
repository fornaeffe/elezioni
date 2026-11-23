## 2026-10-26 — Approved I17 exclusion of zero-vote cap-path N16 recipients

Luca selected a provisional exclusion of zero-vote recipients for the discussed
LG06 chain, conditional on proving it resolves the observed failures. He
explicitly accepted recipient exhaustion as non-computable and required other
positive-seat zero-vote cases to remain for separate discussion.

Before modifying runtime sources, built isolated copies of the matrix, Camera
territorial and composed scrutiny modules under
`data/derived/r26-manual-tests/`, with a recipient filter enabled only in Camera
cap-path N16 corrections. Replayed all 19 original failed IDs: the eight LG06
runs completed with 588 distinct elected candidates each, and the eleven runs
already completed by the N16 I13 extension retained identical Camera territorial
allocations. All 1,254 archived national list vote counts matched. This satisfied
the user's condition; evidence is in `counterfactual-i17.json` and
`probe-i17.mts` with its isolated module copies. The earlier proposed neutral
allocation weights were not implemented.

Implemented I17-v1 through an explicit `camera-cap-positive-votes` correction
recipient policy in `allocateCameraCoalitionListsInConstituencies`, enabled only
when the national path is `bonus-with-cap`. Matrix guards reject the policy
outside Camera N16 constituency-list corrections. Before same-area donor
preference, remove zero-vote list/constituency cells from the unused recipient
pool and continue the existing same-area/cross-area search. Zero constituency
votes imply zero votes in every college there; this addresses the upstream
cause without bypassing N18. Retained list priority, I03 ties, I13 donors,
national entitlements and existing exact quotient/remainder state.

The exclusion uses actual votes, not decimal size: positive votes with zero
arithmetic decimal remain eligible. Initial N16 awards, non-cap N16, N15/N17,
N18, Senate and candidate recovery retain their existing rules. A positive
constituency total with a zero in just one college is outside this change.
Recipient exhaustion still returns `no-eligible-recipient-remainder`; it does
not relax the rule or enable I16 reuse in N16.

Emit `I17_ZERO_VOTE_CORRECTION_RECIPIENT` with severity `warning` once per matrix
that excludes recipients, including the simulation, chamber, context, legal
reference and sorted excluded cell IDs. Keep the warning if a later step fails.
Composed results record `I17-v1`; affected allocation transfers carry I17, or
retain an I13 donor's primary policy with both IDs. Added an Italian diagnostic
title; generic JSON/CSV warning and transfer exports preserve the new context.
Updated the specification, LG06 register, generation guide and active plan;
the source-generation shortcoming remains separately open.

Post-implementation replay also completes all 19 IDs with 588 seats and 588
distinct candidates each, matching the isolated counterfactual's final college
targets. Exactly the eight former zero-quota runs carry I17 warnings/metadata;
the eleven previously completed runs retain their college/list targets and have
no I17 warning. National votes, national entitlements and N15/N17 constituency
subject communications are unchanged. Every cap-path N16 correction recipient
in this replay has positive votes. Example 311 now redirects the correction
to constituency `camera:territory:11`, where Sud chiama Nord has 4,853 votes,
instead of its zero-vote Molise destination. Evidence/scripts are
`r26-risultati-2-after-i17.json`, `replay-i17.ts`, `verify-i17.py` and
`i17-verification.json` in the manual-test directory. Original archives and
earlier replay evidence were preserved. This is a 19-ID replay, not a newly
generated 1,000-run archive or a claim about every other zero-vote seat case.

Verification: added thirteen regressions covering alternate recipient search,
positive votes with zero decimals, I13/I17 combined transfers, explicit exhausted
pools and untouched donor state, unchanged initial zero-vote awards, guarded
scope, cap-only caller opt-in, JSON replay and exported transfer evidence.
All 1,057 tests in 78 files pass without skips; `npm run check` reports zero
errors/warnings; `git diff --check` passes. No generator, scenario, source bundle,
neutral allocation, browser acceptance, commit or publication change was made.
