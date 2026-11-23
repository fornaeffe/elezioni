## 2026-10-30 — Camera TAA admitted-member allocation and exception replay

Luca reported external legal review supporting an implicit admitted-list restriction
in C93-septies(2)(b), and explicitly authorized updating the specification, algorithm
and tests, followed by replay of all 20 legacy-comparison exceptions. This
supersedes the all-locally-filed-members interpretation recorded in R26-10 and the
subsequent proposal to widen recovery eligibility. It is an adopted interpretation
of the existing provision, not a new recovery fallback or a claim of an official
adjudication.

`taa-allocation.ts` retains all locally filed coalition votes in the subject-level
allocation under (2)(a), then uses only locally filed admitted (2-bis)/(2-ter)
members for the internal quota and recipients under (2)(b). The best excluded list
admitted under (2-ter) remains eligible. Otherwise excluded members receive no list
entitlement or list remainder. National admission, I01 local minority access,
recovery admission filtering and I11 zero-entitlement exclusions are unchanged.
Positive coalition targets without admitted local recipients or admitted local
support remain explicitly non-computable; excluded lists cannot repair them.

Replaced the former all-members regression and added tests for coalition vote
conservation versus the internal quota, best-excluded admission, zero admitted
support and no admitted local slate. A full single-Camera fixture gives an excluded
member dominant local support while checking that the admitted members receive
all three TAA seats. Its available/unavailable-capolista variants both finish all
392 seats with unique identities and consistent recovery/remainder evidence.

The reproducible `web/scripts/acceptance/taa-admission-replay.ts` reads the exact
exception IDs from the original curated comparison, verifies each frozen variant
input hash, and preserves the original benchmark and diagnostic receipts. The
separate `test/fixtures/benchmarks/taa_admission_replay.json` records source/input
hashes, generated-input hashes and per-draw checks. All 20 exceptions pass:
no-refresh 131/580; poll-informed 23, 59, 125, 239, 285, 417, 495, 516, 570, 617,
637, 692, 726, 785, 825, 832, 875 and 935. Each returns a complete 588-seat result,
392 Camera plus 196 Senate, all filled by 588 distinct candidates. Draw 23's
generated-input hash matches the original; its centrosinistra TAA seat now belongs
to PD instead of `europa-piu-uno-cd`.

The separate poll-informed draw 135 control retains Senate Sicilia
`N21/impossible-residual` and 399 filled seats, with its original generated-input
hash. That investigation remains open. The targeted Node replay does not refresh
the original 1,000-draw browser timings or establish full release acceptance.

Updated the N11 specification, core reading map, comparison results/reproduction
guide and migration checklist. Verification: 52 reform-core test files / 854 tests
pass, `npm run check:data` passes, `npm run check` reports zero errors and warnings,
and `git diff --check` passes. Changes remain uncommitted; no public deployment or
default-generator switch was performed.
