# Politics reform 2026: legal gaps

This register tracks situations where the strict statutory reading adopted for
this project cannot finish allocation. A diagnostic propagated from an earlier
failure, such as N28, is not a separate gap. Implementation bugs and statistical
assumptions belong elsewhere. Add gaps only after checking the controlling text
and a worked example; do not infer them from benchmark counts alone.

Source: [law 180/2026, article 1(23), amending C83-bis](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
checked again on 21 October 2026; [article 2(10), amending S17](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
checked on 21 October 2026. The simulator policies below are Luca's explicit
decisions, not established administrative practice or binding interpretations.
Keep operative contracts in the [specification](politics-reform-2026-specification.md)
and verification history in [migration history](migration-history.md).

| Gap | Rule | Simulator treatment |
| --- | --- | --- |
| LG01 | C83-bis(1)(b)(3) and S17(4)(c), including imports by C83-bis(1-bis)(d) and S17(5)(d): excess list seats with no remaining remainder-awarded donor | Warned integer-seat removal under provisional I13-v1 in Camera N18 and Senate N21/N22 |
| LG02 | C83-bis(1-bis)(c) and S17(5)(c): excess cap-group seats with no remaining remainder-awarded donor | Warned integer-seat removal under provisional I13-v1 in Camera N18 and Senate N22, within the same college |
| LG03 | C83-bis(1-bis)(b)-(c), C3: fixed college capacity differs from the constituency entitlement communicated upstream | Warned disjoined Camera cap-group corrections under provisional I15-v2 |
| LG04 | C83-bis(1)(b)(3) and S17(4)(c), including their cap-list imports: a deficient list has exhausted all unused college decimals | Warned repeated used-decimal recipient passes under provisional I16-v1 |

## LG01: ordinary list donor exhaustion

**Strict reading:** excess seats must be removed where obtained through decimal
parts. An integer-only surplus, or a surplus remaining after eligible remainder
seats have been removed, has no permitted donor.
This is the adopted reading of both ordinary list provisions; reading the
decimal as merely a donor-college ranking would yield a different interpretation.

**Example:** benchmark simulation 4, seed `r26-default-v1`, Basilicata. Four
positive-entitlement lists have votes 88,446 / 45,804 / 28,436 / 7,630 and targets
1/1/1/1. The four-seat quota is 42,579. Integer allocation 2/1/0/0 plus AVS's
remainder seat produces 2/1/1/0. M5S's excess seat is an integer seat; Forza
Italia is missing one seat. Under the strict reading the correction stops.

**Approved workaround, 20 October 2026:** after exhausting eligible remainder
donors, remove an integer seat from a surplus list in the college with the
smallest existing decimal part. Repeat only while surplus and positive seats
remain; resolve equal ranks reproducibly under I03. Apply also to list splits
inside cap groups through (1-bis)(d). Recipient eligibility remains unchanged.
Ordinary b(3) removals/additions remain independent.

**Senate extension, approved 21 October 2026:** apply the same fallback to
S17(4)(c), also imported by S17(5)(d). Ordinary Senate corrections retain their
paired subtraction/addition and independently ranked colleges; preserve regional
list targets and totals. Recipient decimals must still be unused.

**Senate example:** benchmark simulation 31, same seed, Campania. Regional
M5S/+Europa/Azione-IV/FdI/FI targets are 11/3/1/2/1. College quotas
167,254/150,885 give M5S 7+5 integer seats. After FI's remainder seat corrects
Azione-IV's deficit, M5S still has one excess and +Europa one missing seat.
I13 removes M5S's integer seat in college 150002 (decimal 892/30177), preserving
its unused state, and assigns a seat to +Europa's unused decimal in 150001.
The composed production run completes with 588 domestic seats.

## LG02: cap-group donor exhaustion

**Strict reading:** group correction expressly removes seats obtained through
decimal parts. Integer-only residual excess therefore blocks group reconciliation.

**Example:** three two-seat colleges have majority/minority votes (50,10),
(50,10), (0,480), with constituency targets 1/5. Both group quotas are 100.
The first two colleges initially give both seats to the majority; the third
gives both to the minority. Removing the majority's two remainder seats still
leaves an excess integer seat.

**Approved workaround, 20 October 2026:** use the same I13 donor fallback,
transferring each removed seat to the deficit group in the same college.
Every college capacity and the communicated group/list targets remain binding.
The example then completes the list-allocation and recovery communications.
Luca extended this workaround to Senate S17(5)(c) on 21 October 2026.
The same constructed example completes Senate group and internal list allocation;
group corrections preserve every college capacity and regional group/list target.

## LG03: C3 capacity and upstream constituency totals disagree

**Conflict:** C83-bis(1-bis)(b) distributes the fixed college seat counts supplied
under C3. Upstream national-to-constituency correction can communicate a different
constituency total. The same-college transfers required by (1-bis)(c) cannot
alter the sum, so both totals cannot be satisfied.

**Evidence:** `camera-territorial.test.ts` has a constructed case with fixed
capacities 3/3 but communicated constituency totals 2/4, conserving six seats
nationally. The full constructed N18 investigation reproduces Veneto 1 capacity
11 / entitlement 12 and Puglia capacity 21 / entitlement 20 after N16's
cross-constituency correction. These were blocked by
`constituency-capacity-target-mismatch` before I15.

**Approved workaround, 21 October 2026:** Luca authorized applying (1-bis)(c)
disjoined when the constituency's communicated group total differs from C3.
I15-v2 retains the original college capacities for initial indices and quotas,
then independently removes all group surpluses in ascending used-remainder
order and fills all group deficits in descending unused-remainder order. After
unused recipients exhaust, repeat descending passes over used decimals,
including decimals used by an earlier addition. Each eligible cell is visited
once per pass; there is no limit on reuses. Consumed decimals cannot receive seats.
Recheck group deficits after every addition. Equal correction ranks use I03.
Existing I13 integer donor fallback remains available after strict donors exhaust.
Luca removed I15-v1's one-pass limit on 21 October 2026 alongside approval of
I16; applied metadata now records I15-v2.

Final college totals may differ from C3; both upstream group targets and all
constituency/national list entitlements remain binding. The corrected group
counts become the capacities for the subsequent (1-bis)(d) internal list split.
Recovery consumes that split's existing list decimals, never group indices.
No decree table is rewritten. Balanced constituencies and Senate rules retain
paired group transfers. Arithmetic failures and exhausted recipient passes
remain explicit non-computable results.

Emit `I15_DISJOINED_GROUP_CORRECTION` with severity `warning` once per affected
constituency, including simulation, context and C3/communicated totals. Preserve
it if a later stage fails; record applied `I15-v2` in composed metadata.
Disjoined movements have a null donor or recipient and I15 trace metadata;
integer removals retain I13 metadata. Group movements remain in the detailed
group matrix, separate from candidate-seat transfers.

**Applicability:** the controlling text, rechecked on 21 October 2026, requires
same-college reassignment; neither disjoining movements nor used-decimal
additions is expressly prescribed by (1-bis)(c). This is the approved simulator
policy, not a claim about electoral-office practice. It needs no input/output
schema or downstream algorithm change; relaxing final local C3 totals is its
explicit substantive tradeoff. See the [I15 contract](politics-reform-2026-specification.md#disjoined-camera-cap-group-corrections-i15).

## LG04: ordinary list recipient exhaustion

**Strict reading:** correction awards a missing list seat in the college with
its greatest unused quotient decimal. Both C83-bis(1)(b)(3) and S17(4)(c)
expressly require that decimal to be unused; C83-bis(1-bis)(d) and S17(5)(d)
import the same rule into internal cap-group list splits. The controlling
articles were rechecked on 21 October 2026. They provide no fallback when all
of the deficient list's college decimals have already been used.

**Verified boundary:** in all six currently replayed cases, the deficient
list's target exceeds its total integer seats plus one remainder award per
college by exactly one seat. Initial college allocations are already full;
even awarding that list every available decimal once cannot reach its target.
This is not caused by correction order, ties, absent candidature or candidate
exhaustion. It is an uncovered state under the adopted single-use recipient
reading, rather than an observed arithmetic defect. This classification does
not establish a binding interpretation or electoral-office practice.

**Camera example:** production simulation 42, seed `r26-default-v1`, Calabria,
cap path. Upstream targets are majority/minority 4/6; within the minority,
M5S/PD must receive 4/2. The sole college's minority votes are 210,634/32,554.
Quota `floor(243188/6) = 40531` produces integer seats 5/0 and decimals
7,979/40,531 and 32,554/40,531. PD receives the remaining initial seat, using
its only decimal. Allocation 5/1 still needs M5S -1 and PD +1. I13 can supply
an integer donor, but PD has no unused recipient, so correction fails before
any transfer. Simulation 792 has the same structure.

**Senate example:** simulation 242, same seed, Emilia-Romagna. N19 initially
allocates the 11 ordinary seats as winning coalition/M5S/PD 10/1/0. N20's
national cap correction removes four coalition seats: its first regional
movement gives PD one seat through its unused remainder, then three subsequent
regional-vote fallback movements give M5S three seats. Final targets are 6/4/1.
N22's group split assigns minority capacities 2/3 to colleges 080001/080002.
Internal minority quotas 48,025/45,929 produce M5S/PD integer seats 0/1 and
1/1. PD already exceeds its regional target and receives no initial remainder;
M5S uses both its decimals, reaching only 1+2 = 3 seats instead of four.
PD has an I13 integer donor, but no M5S decimal remains unused. No minority
transfer occurs. N21's aggregate and N22's active cap stage both report this
one root blocker; subsequent non-computable stages propagate it.

**Other verified cases:** Camera 80 (no bonus, Lombardia 1) and 598 (bonus
without cap, Lazio 2) also exhaust PD's decimals, after independent removal
and addition movements. Senate 49 (cap path, Lombardia) needs four PD seats
across three colleges with no PD integer seats; all three PD decimals are
used initially. One Azione-IV-to-M5S correction completes before PD's remaining
deficit blocks the next movement.

**Approved workaround, 21 October 2026:** Luca confirmed LG04 and authorized
I16-v1 for Camera N18 and Senate N21/N22 ordinary/internal list correction.
Exhaust unused recipient decimals first, then make repeated descending passes
over eligible used decimals without a reuse limit. Visit each eligible cell
once per pass, exclude reached targets after every seat, and use I03 for ties.
Preserve decimal values, initial allocation evidence and final `used` state;
do not admit consumed, absent or undefined decimals. Warn per reused list seat
and record I16-v1, retaining both donor and recipient policies when I13/I14
also applies. All upstream entitlements remain binding. See the
[I16 contract](politics-reform-2026-specification.md#reused-list-correction-recipients-i16).
I13 still governs donors; I14 governs only initial Camera list passes.
I15-v2 uses unlimited used-decimal passes in mismatched Camera cap groups.
Pre-workaround inputs/results, scoped matrices and independent arithmetic/
transfer verification are retained under
`data/derived/r26-recipient-review-20261021/`. The current implementation and
isolated pre-I16 baseline comparison are reproduced by
`data/derived/r26-i16-review-20261021/replay.mjs`; see migration history for
versions and verification scope.

## I13 evidence and downstream constraints

Every integer-seat removal emits `I13_INTEGER_SEAT_REMOVAL` with severity
`warning`, simulation, chamber and college/subject context. Composed results
record `I13-v1`; list transfer exports mark the interpretation. Group movements
remain in the detailed group matrix, not fictitious candidate-seat transfers.

An integer removal preserves the decimal value and its used/unused/consumed
state. It does not consume an unused remainder or consume a used one twice.
Strict remainder removals still consume their source once. Each operation
reduces surplus by one, never takes a seat below zero, and retains original
integer/provisional counts as evidence. Candidate proclamation and recovery
consume the corrected targets and unchanged remainder evidence.

I13 applies to Camera N18 and Senate N21/N22 college list corrections, including
internal cap lists, and Camera N18/Senate N22 cap-group corrections. Camera
constituency and Senate regional/cap-stage corrections retain their existing policies.
I16 handles LG04 recipient reuse; genuinely empty recipient pools, undefined
quotas and impossible residuals remain explicit non-computable paths. LG03 is
handled by the narrowly scoped I15 policy above.
