# Senate Sicilia N21: draw 135 investigation

Status: **Senate extension approved, implemented and verified as I14-v2**.
Review and approval date: 30 October 2026. See the [migration task](../MIGRATION_PLAN.md)
for acceptance status. The original diagnosis and isolated proof are retained below.

## Finding and decision

The immediate cause is a **single-pass restriction in the Senate implementation**.
Sicilia 1 needs four initial remainder seats but has only three eligible lists.
The original Camera I14-v1 interpretation handled this situation by repeating
the ranking; its former scope explicitly excluded the Senate.

Luca approved extending that interpretation to S17(4)(b), including the
internal-list operation imported by S17(5)(d). It is implemented as warned
provisional I14-v2, not a newly established legal lacuna. No further interpretative
decision is pending. Votes, regional/list entitlements, geography and candidate
generation retain their existing rules.

## Frozen evidence and prerequisites

The compact [investigation receipt](../test/fixtures/benchmarks/senate_sicilia_n21_review.json)
contains source hashes, original receipt hashes, the complete minimal matrix
input, the failed trace and the isolated proposal's trace/checks.

- Frozen scenario/snapshot: `data/derived/legacy-comparison/future-polls.json`;
  SHA-256 `44433654b3cb5ff3466ce07bf3f729fc3f5582decb803154e0db176ce9251412`.
- Draw: **135**, poll-informed legacy generator.
- Regenerated legal input SHA-256:
  `efbb30bc9ff2be283b083d7d28205276e1fa1f1bc7234265df2d1dfabc7bb23d`,
  identical to the original failed receipt and the later TAA control.
- Full generated-input validation passes. Regional subject allocation and
  internal coalition-list allocation both complete. The college adapter checks
  regional/college figure conservation and regional seat capacities.
- The path is `bonus-without-cap`: centrosinistra qualifies for the joint bonus;
  Senate cap correction is not activated. Sicilia has **15 seats: three bonus
  and 12 ordinary**, with ordinary capacities **6/6**. The without-bonus 7/8
  capacities are therefore inapplicable.
- These are the existing census-based **I08 provisional endowments**, version
  `r26-seats-census-2021-i08-v1-9672c20529b2`, not an official election decree.
  Their sum and branch selection are consistent; no new territorial mismatch
  was found in this failure.

The original comparison, `diagnostic-polls-135.json`,
`poll-noncomputable-135.json` and `taa_admission_replay.json` remain unchanged.

## Event chain

1. N19 assigns Sicilia's ordinary seats to centrodestra **6**, centrosinistra
   **5**, and Futuro Nazionale **1**. Azione/LibDem gets zero. The regional
   subject quota is 190,795; the coalition remainder seats go to centrosinistra
   and centrodestra. N20 leaves these targets unchanged.
2. S17(1)(b)'s admitted-member splits give FdI **2**, FI **4**, M5S **3**,
   PD **2**, Futuro Nazionale **1**. The centrodestra internal quota is
   100,417: FdI gets two integer seats, FI three plus the remainder seat;
   Lega gets zero. The centrosinistra quota is 173,557: M5S gets three integer
   seats, PD one plus the remainder seat; AVS and +Europa get zero.
3. S17(4)(a) uses only the five lists with positive regional seat targets.
   Their college vote totals are 805,658 and 858,105. Division by six and
   truncation give college quotas **134,276** and **143,017**.

| List | Regional target | Sicilia 1 votes | Integer seats | Sicilia 2 votes | Integer seats | Integer total |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| FdI | 2 | 82,753 | 0 | 136,551 | 0 | 0 |
| FI | 4 | 203,815 | 1 | 150,730 | 1 | 2 |
| Futuro Nazionale | 1 | 130,345 | 0 | 151,370 | 1 | 1 |
| M5S | 3 | 264,998 | 1 | 306,645 | 2 | 3 |
| PD | 2 | 123,747 | 0 | 112,809 | 0 | 0 |
| Total | 12 | 805,658 | 2 | 858,105 | 4 | 6 |

4. M5S and Futuro Nazionale have reached their regional targets from integer
   seats, so S17(4)(b)'s exclusion removes them from initial remainder awards
   in both colleges. They remain in the quota calculation.
5. Sicilia 1 has **four** seats left and only **three** eligible decimals:
   PD `123747/134276`, FdI `82753/134276`, FI `69539/134276`, in that order.
   Sicilia 2 needs two seats; its eligible ranking is FdI, PD, FI.
6. `territorial-matrix.ts` preflights every college before assigning any
   remainder. Its condition is `residual > available`, unless initial repeated
   passes are enabled. `repeatRemainders` is restricted to Camera N18, so the
   **4 > 3** check returns `phase: remainders`, `reason: impossible-residual`.
   The matrix has no draws, transfers or initial remainder awards. S17(4)(c)'s
   correction has not run; I13 donor removal and I16 corrective recipient reuse
   cannot address an earlier-phase failure.
7. Senate N23/N24 proclamation/multiple-election processing and N27 recovery
   block on that prerequisite; N28 reports the propagated failure. The final
   ledger contains **392 Camera plus seven Senate special-uninominal seats**.
   The 399 total is a partial execution ledger, not a lawful allocation of only
   399 seats or evidence of 189 exhausted candidate positions.

## Legal assessment

Checked S17 as replaced by article 2(10) of law 180/2026 in both the
[original GU publication](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and the [23 October republication with notes](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26A05611&art.dataPubblicazioneGazzetta=2026-10-23&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1).
The relevant wording agrees. No contrary rule was identified in those provisions.

S17(4)(a) fixes the quota; (b) orders residual awards by descending decimals,
then college votes and a draw, excluding lists at their regional target;
(c) subsequently reconciles list totals. Paragraph (b) neither expressly
limits the ranking to one traversal nor expressly prescribes repeated passes.
Paragraph (c)'s unused-decimal condition governs correction recipients.
S17(5)(d) imports (4)(a)-(c) for internal cap-group lists.

The exhaustion is therefore an interpretative edge. Repeating the ranking is
a defensible reading consistent with the already approved
[Camera I14 policy](politics-reform-2026-specification.md#initial-camera-list-remainder-passes-i14).
It has not been established as an authoritative legal interpretation. If that
reading is rejected, the law needs a different explicit interpretation or a
documented fallback; the present code's stop alone does not prove a legal gap.

## Approved policy and initial isolated verification

I14-v2 extends to Senate **N21 list matrices**, and **N22 internal-list matrices**
through S17(5)(d). Keep group-index matrices and regional subject allocations
outside its scope. Preserve the current simultaneous first pass; before each
additional whole-matrix pass, freeze the exclusion of lists that have reached
their regional target. Reuse the established ranking and resolved ties, giving
each remaining entry at most one seat per pass. An empty pool still fails.

For draw 135, pass one assigns PD/FdI/FI in Sicilia 1 and FdI/PD in Sicilia 2.
Regional totals then are FdI 2, FI 3, Futuro Nazionale 1, M5S 3, PD 2.
Only FI remains deficient. Pass two assigns it Sicilia 1's last seat.

| List | Sicilia 1 final | Sicilia 2 final | Regional final |
| --- | ---: | ---: | ---: |
| FdI | 1 | 1 | 2 |
| FI | 3 | 1 | 4 |
| Futuro Nazionale | 0 | 1 | 1 |
| M5S | 1 | 2 | 3 |
| PD | 1 | 1 | 2 |
| Total | 6 | 6 | 12 |

An isolated esbuild source transform enabled that scope without editing any
production module. Full scrutiny on the identical generated input returned
**complete: 588 filled seats, 588 distinct candidates, 392 Camera/196 Senate**.
The regional allocation and regional-list result were byte-for-byte identical
after JSON serialization. Sicilia needed one additional-pass warning and no
corrective transfers. The minimal matrix reproduced the full result; reversing
target, area and entry arrays produced identical matrix output.

This initial feasibility replay preceded implementation. The diagnostic bundle
retains the old I14-v1 label and Camera-oriented warning wording as historical
evidence. The implemented extension records **I14-v2**, uses regional wording in
Senate warnings, preserves `provisionalSeats - integerSeats` as the count of
initial remainder awards and retains `used` until all such awards have been removed. Existing
I13/I16 and recovery rules retain their separate scope.

Do not remove the reached-target exclusions, recompute quotas after excluding
reached lists, silently resample the draw, or skip directly to correction.
Those alternatives alter the specified operation or its territorial outcome.

## Implementation acceptance

- [x] Add the frozen minimal matrix as a permanent Senate regression; require the
  exact final table above and one warned second pass.
- [x] Cover N22's imported list operation, multiple passes, reached-target
  exclusions, array permutations, college-vote ties without repeated draws,
  multiply-awarded donor removal and unchanged recovery remainder state.
- [x] Retain failures for genuinely empty eligible pools, zero quotas and initial
  overallocation. Replace the existing Senate `impossible-residual` fixture
  whose positive eligible ranking becomes repeatable; add an actual empty-pool
  fixture rather than preserving the single-pass assumption.
- [x] Require unchanged results/warnings when a single pass suffices, and unchanged
  cap-group/regional arithmetic. Update I14 metadata, specification and reading
  map without registering an unproven new legal gap.
- [x] Replay draw 135, the previous 20 TAA exceptions and the frozen 1,000-draw
  workloads, comparing all attempts and changes. Update the TAA replay script's
  hard-coded expectation that draw 135 still fails; preserve its old receipt.
- [x] Run legal-core checks and browser acceptance before marking the migration
  item complete. Functional Node replay does not refresh the original browser timings.

Local working artifacts and diagnostic scripts are in
`data/derived/n21-sicilia-review-20261030/` (ignored/regenerable). The compact
receipt retains the decisive input, traces, transform and verification evidence
without requiring that directory for reading this diagnosis.

The implementation uses I14-v2 in warnings and composed metadata, including
Camera runs using the retained policy. Archive readers still accept I14-v1.
`senate-remainder-passes.test.ts` uses the frozen minimal matrix without requiring
the large generated snapshot; `senate-territorial.test.ts` covers the imported
N22 operation and recovery state. A browser-worker regression checks exact output
against the same minimal matrix. The TAA replay now expects a complete draw 135
and writes `taa_admission_senate_i14_replay.json`, preserving its earlier receipt.

## Completed implementation verification

The [paired replay receipt](../test/fixtures/benchmarks/senate_i14_replay.json)
and its compressed detail ledger retain **all 2,000 attempts**. The baseline is
commit `b3830ec4c127ea45d1409a0eb24bacfc0fb83d6e`, after the TAA correction and
before the Senate extension. Each generated input is passed unchanged to both
versions; source/bundle/input hashes and original-receipt checks are retained.

| Frozen workload | Before complete | After complete | Allocation changes |
| --- | ---: | ---: | --- |
| No general poll refresh | 1,000/1,000 | 1,000/1,000 | None |
| Poll-informed | 999/1,000 | 1,000/1,000 | Draw 135 |

Every implemented result has 588 filled seats and 588 distinct candidates.
Of all 2,000 result objects, **1,992 are byte-identical**; seven differ only in
I14-v1/v2 warning/metadata labels; draw 135 is the sole allocation change.
Its regional/list results and original generated-input hash are unchanged.
All 20 original TAA exceptions also pass the separate targeted replay.

Verification passes: **107 unit files / 1,257 tests**, `check:data`, Svelte check
with zero errors/warnings, static builds at `/` and `/elezioni`, and **nine
browser tests at each base path**. The browser suites cover the Sicilia matrix
and production-worker replay/cancellation/chunking. An initial unrestricted
unit run hit two five-second timeouts alongside the sharded replays; the full
suite passes with `--maxWorkers=2`, without changing test deadlines.

The original comparison/diagnostic receipts and browser performance figures
remain historical evidence. These checks complete this allocation task, not
the separate publication or performance gates. Working replay scripts and
bundles are ignored under `data/derived/n21-sicilia-implementation-20261030/`.
