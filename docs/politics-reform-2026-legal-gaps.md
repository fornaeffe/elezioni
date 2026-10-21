# Politics reform 2026: legal gaps

This register tracks situations where the strict statutory reading adopted for
this project cannot finish allocation. A diagnostic propagated from an earlier
failure, such as N28, is not a separate gap. Implementation bugs and statistical
assumptions belong elsewhere. Add gaps only after checking the controlling text
and a worked example; do not infer them from benchmark counts alone.

Source: [law 180/2026, article 1(23), amending C83-bis](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
checked on 20 October 2026; [article 2(10), amending S17](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1),
checked on 21 October 2026. The simulator policies below are Luca's explicit
decisions, not established administrative practice or binding interpretations.
Keep operative contracts in the [specification](politics-reform-2026-specification.md)
and verification history in [migration history](migration-history.md).

| Gap | Rule | Simulator treatment |
| --- | --- | --- |
| LG01 | C83-bis(1)(b)(3) and S17(4)(c), including imports by C83-bis(1-bis)(d) and S17(5)(d): excess list seats with no remaining remainder-awarded donor | Warned integer-seat removal under provisional I13-v1 in Camera N18 and Senate N21/N22 |
| LG02 | C83-bis(1-bis)(c) and S17(5)(c): excess cap-group seats with no remaining remainder-awarded donor | Warned integer-seat removal under provisional I13-v1 in Camera N18 and Senate N22, within the same college |
| LG03 | C83-bis(1-bis)(b)-(c), C3: fixed college capacity differs from the constituency entitlement communicated upstream | Warned disjoined Camera cap-group corrections under provisional I15-v1 |

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
I15-v1 retains the original college capacities for initial indices and quotas,
then independently removes all group surpluses in ascending used-remainder
order and fills all group deficits in descending unused-remainder order. After
unused recipients exhaust, make one descending pass over used decimals,
including decimals used by an earlier addition. Each used decimal can receive
one additional seat in that pass; consumed decimals cannot receive seats.
Recheck group deficits after every addition. Equal correction ranks use I03.
Existing I13 integer donor fallback remains available after strict donors exhaust.

Final college totals may differ from C3; both upstream group targets and all
constituency/national list entitlements remain binding. The corrected group
counts become the capacities for the subsequent (1-bis)(d) internal list split.
Recovery consumes that split's existing list decimals, never group indices.
No decree table is rewritten. Balanced constituencies and Senate rules retain
paired group transfers. Arithmetic failures and exhausted recipient passes
remain explicit non-computable results.

Emit `I15_DISJOINED_GROUP_CORRECTION` with severity `warning` once per affected
constituency, including simulation, context and C3/communicated totals. Preserve
it if a later stage fails; record applied `I15-v1` in composed metadata.
Disjoined movements have a null donor or recipient and I15 trace metadata;
integer removals retain I13 metadata. Group movements remain in the detailed
group matrix, separate from candidate-seat transfers.

**Applicability:** the controlling text, rechecked on 21 October 2026, requires
same-college reassignment; neither disjoining movements nor used-decimal
additions is expressly prescribed by (1-bis)(c). This is the approved simulator
policy, not a claim about electoral-office practice. It needs no input/output
schema or downstream algorithm change; relaxing final local C3 totals is its
explicit substantive tradeoff. See the [I15 contract](politics-reform-2026-specification.md#disjoined-camera-cap-group-corrections-i15).

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
constituency and Senate regional/cap-stage corrections retain their existing policies. Recipient
exhaustion, undefined quotas and impossible residuals remain separate
non-computable paths. LG03 is handled by the narrowly scoped I15 policy above.
