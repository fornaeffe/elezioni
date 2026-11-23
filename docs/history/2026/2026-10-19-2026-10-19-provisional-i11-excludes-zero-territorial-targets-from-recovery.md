## 2026-10-19: provisional I11 excludes zero territorial targets from recovery

Luca explicitly resolved the last edge case with the provisional reading that a
list with zero territorial seat entitlement has no college quotient and cannot
obtain a recovery seat. This supersedes the zero-entitlement limitation recorded
in the preceding I10 follow-up. The current rule is in the
[interpretation register](../../politics-reform-2026-specification.md#interpretation-register).

- Implemented `I11-v1` before every recovery candidate/priority selection,
  including a sole destination, coalition partners and bonus replacements.
  Admitted zero-target lists also cannot supply an I10 priority witness. Targets
  are the final ordinary list allocations after territorial/cap operations and
  before recovery: Camera constituency, Senate region, and Camera TAA
  proportional allocation. A zero allocation in one college does not exclude a
  list whose territorial target is positive.
- Added explicit nonnegative `ordinarySeats` to the internal recovery admission
  communications. The coordinator obtains these from final constituency/regional
  list and TAA targets, without inferring missing targets as zero. Standalone
  recovery calls must supply the same evidence; malformed/missing counts fail
  before execution. Existing initial ordinary entitlements must have a positive
  territorial list target.
- `InitialRecoveryResult.excludedLists` records excluded territory/list pairs
  with `zero-territorial-entitlement`; recovery and composed policy metadata
  record I11-v1. Final targets remain immutable throughout recovery. Zero-target
  Senate/TAA college aliases are not created, while original territorial
  allocation traces remain available.
- Both full-envelope regressions (one and two zero-target destination colleges)
  now exclude those candidates and terminate with explicit unfilled seats when
  permitted routes exhaust, preserving all 588 domestic entitlements. They no
  longer end in `missing-recovery-remainder` or fill the formerly permitted sole
  zero-target destination. This closes the identified zero-territorial-target
  ambiguity under the authorized provisional policy; genuinely missing evidence
  and undefined arithmetic remain separate failure states.
- Added focused cases for Camera/Senate singleton exclusions despite supplied
  quotients, continuing after an excluded coalition partner, coalition bonus
  replacement, missing/invalid target rejection and TAA zero-target evidence.
  Updated prior stage fixtures to communicate their real territorial totals.
- Verification: `npm run check:data` and `npm run check` passed, with zero
  diagnostics. All **784 unit tests in 49 files** and **9 Chromium browser tests**
  passed, including browser transfer of I11 metadata and exclusions. Playwright's
  static build passed before browser execution. Plans and current API
  documentation now describe the resolved boundary. Legacy law and later
  generator/UI integration remain untouched; changes are left uncommitted.
