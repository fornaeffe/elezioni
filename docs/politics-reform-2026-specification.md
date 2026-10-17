# Politics reform 2026: normative specification (R26-01)

This is the legal boundary for the [implementation plan](politics-reform-2026-plan.md),
verified on **16 October 2026**. Function names and test IDs below define stage
boundaries; implementation status belongs to the plan. R26-05's
[typed contracts and input validation](../web/src/lib/politics/reform-2026/README.md)
are implemented; later scrutiny stages remain pending. The supported target is
the initial allocation and proclamation of **domestic elective seats** under
the promulgated reform. The [transcription](politics-reform-2026.md) is the
current consolidated working transcription of all and only provisions translated
within this scope. Update it when the controlling legal text changes; keep
interpretations and modelling assumptions in this specification.

Read only the relevant part:

- Current legal sources: [sources](#sources-and-version-reconciliation).
- Inputs, output scope and seat accounting: [scope](#supported-scope-and-accounting).
- Senate exhaustion and bonus shortages: [approved I07 protocol](#senate-initial-recovery-i07).
- Legal stages and their tests: [matrix](#norm-to-functiontest-matrix).
- Numerical operations and tie rules: [numerics](#numerical-and-tie-contracts).
- Provisional policies and unresolved cases: [interpretation register](#interpretation-register).

## Sources and version reconciliation

References `C` and `S` below mean articles of DPR 361/1957 and D.Lgs. 533/1993,
respectively, **as amended**, not article numbers of the amending law.

| ID | Exact source/version | Use |
| --- | --- | --- |
| L | [Law 9 October 2026, no. 180, 26G00201, GU General Series no. 235, 9 October 2026, original publication](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg) | Controlling amendment text; promulgated, with entry into force on 24 October 2026. Do not describe it as already in force on this review date. |
| L1 / L2 | [GU article index](https://www.gazzettaufficiale.it/atto/vediMenuHTML?atto.codiceRedazionale=26G00201&atto.dataPubblicazioneGazzetta=2026-10-09&tipoSerie=serie_generale&tipoVigenza=originario), articles 1 and 2, original version 1 | Camera and Senato amendments. For a matrix reference, locate the amended `C`/`S` article here and use P for unchanged surrounding provisions. |
| P | [Camera dossier AC0469g, 16 September 2026, A.C. 2822-B, comparison of consolidated texts](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf); local [testo_a_fronte.pdf](testo_a_fronte.pdf), 141 PDF pages | Right column supplies the proposed consolidated text; `Identico/a` incorporates the corresponding left-column text. Printed pages are PDF page minus two in the substantive sections. |
| E | [A.C. 2822-B errata corrige, document 19PDL0214090](https://documenti.camera.it/apps/commonServices/getDocumento.ashx?sezione=lavori&tipoDoc=testo_pdl&idlegislatura=19&codice=leg.19.pdl.camera.2822.19PDL0214090), listed with the September transmission | Corrects the bill's article 1(22)(e)(4), printed p. 18, second column: explicitly supplies C83(1)(e)(2-bis), preserving 2-ter. This is a parliamentary erratum, not a later GU correction. |
| I | [Camera legislative record, A.C. 2822-B](https://www.camera.it/leg19/126?idDocumento=2822-B) | Transmission 15 September; final approval 8 October; identifies L and E. The B-bis document is a minority report, not a replacement normative text. |
| T | [Current scoped transcription](politics-reform-2026.md) | Consolidated operative text for the matrix, including constitutional totals/allocation/age constraints and the revised Bolzano mapping. Excludes administrative procedures, individual ballot adjudication, foreign scrutiny and later supplementary elections. |
| K | [Constitution, Presidency of the Republic, current text](https://www.quirinale.it/allegati_statici/costituzione/costituzione.pdf), articles 48(1), 56, 57 and 58; Civil Code article 2(1), first sentence | Electorate/candidate age, elective chamber totals, separate foreign-seat endowments and population apportionment. Operative excerpts are in T. |
| P0 | [DPR 20 January 2023, GU 53, ordinary supplement 10, 3 March 2023](https://www.gazzettaufficiale.it/eli/gu/2023/03/03/53/so/10/sg/pdf) | Identified legal-population baseline: census at 31 December 2021. R26-07 must verify the latest legally declared census at generation time and reconcile territorial changes. Annual estimates are not automatically legal census data. |
| H07 | [R26-01a precedent review and primary sources](migration-history.md#2026-10-16-r26-01a-senate-recovery-precedents-and-approved-i07) | Senate decisions on Pavanelli (2019) and Minasi (2021), and Constitutional Court order 86/2020. Evidence for provisional I07, not an amendment to the controlling statutory text. |

Reproducible source identification (SHA-256 of the PDF bytes):

- Local P: `6c354a0e1241725fb41b7770fd33663ca9b967817a602b41e1bc16de033ad5ca`.
- Downloaded official P: `6a5a26dc2502dff6ed149f3f13b517d7afff223e9f1e22a62cdb1147f34edc23`.
- These PDFs have different bytes but identical extracted text on all 141
  corresponding pages using PyMuPDF. Their cover identifies the same dossier;
  the local metadata records creation/modification on 16 September 2026.

T consolidates C/S with L and official corrections controlling any conflict
between official sources. P supplies unchanged surrounding provisions, using its
left column where the right column says `Identico`. K supplies constitutional
inputs. L1/L2 are not duplicated as amendment instructions in T; their operative
content is represented by the amended C/S articles. For geographic input data,
use the official tables cited by T; its Bolzano mapping supplies the reform's
changed U04-U06 sections. Keep provisional numerical policies out of legal quotations.

No later GU correction was identified in the act's correction links or targeted
official-site searches for `26G00201` and errata/rettifica on this review date.
This is a dated search result, not a guarantee against later corrections.
The official publication announces republication with notes on **23 October
2026**; that future version was not available for this review. Recheck that
publication, any corrections and election-specific decrees before production
acceptance (R26-21); record changes as a new source version and reassess affected
matrix rows, updating the current transcription and its source version together.

## Supported scope and accounting

### Legal inputs and outputs

The legal core receives validated candidatures/links, geographic endowments and
**valid** votes already adjudicated by the electoral offices. It does not decide
the validity of individual marked ballots, count disputed ballots, authenticate
signatures, adjudicate appeals or infer legal eligibility from a name.

Two vote representations must remain explicit: direct list votes plus separately
counted bonus-only/candidate-only ballots, or final attributed list totals with
provenance. Only the first invokes attribution. Special candidate totals remain
available for uninominal winners; generated totals equal the sum of their linked
lists' final votes under approved R26-02. Preferences are separate valid counts;
one ballot can contribute several preferences but only one list vote. Missing
preferences differ from zero. Candidate identities, date of birth, list role,
order and affiliation must be supplied for exact proclamations.

Return stage traces, entitlement seats, actually filled seats, elected identities
when determined, and reasons for excluded/admitted subjects and absent bonuses.
Preserve ordinary, bonus and special seats separately even when recovery changes
the eventual holder or territory. Record the legal destination and the original
entitlement of a replacement. An unresolved interpretation returns the affected
stage/subjects and partial results; it must not look like a completed election.
R26-05 implements these input distinctions and result types in its isolated
versioned envelope. Result production, numerical stage traces and integration
remain the responsibility of the subsequent implementation steps.

The [approved R26-02 contract](politics-reform-2026-modelling.md#bonus-only-votes-r26-02)
selects final attributed list totals for generation, with explicit component
inputs retained, unknown ballot composition and historical-source reconciliation.
The [approved R26-03 contract](politics-reform-2026-modelling.md#preferences-r26-03)
selects historically informed synthetic preferences with supplied valid counts
taking precedence, valid-combination construction and conditional candidate/
recovery outputs. The [approved R26-04 contract](politics-reform-2026-modelling.md#camera-senato-votes-r26-04)
selects shared shocks, historical contrasts and correlated residuals. Common
national/local shares anchor derived chamber references; explicit chamber shares
take precedence at the same scope, retaining geographic specificity and
mean-mode uncertainty. Model fitting and generation remain unimplemented.
These statistical choices do not alter the legal matrix.

### Territorial and seat ledger

| Scope | Camera | Senato |
| --- | --- | --- |
| Constitutional elective total (K) | 400, including 8 Estero | 200, including 4 Estero; excludes senators for life |
| Domestic target | 392 | 196 |
| Special territories | VdA: 1 uninominal; TAA: existing A.1 Camera uninominal colleges plus its remaining proportional seats (C2) | VdA: 1; TAA: 6 uninominal seats (S1(3)-(4)); no ordinary regional proportional pool there |
| Bonus | 70, outside VdA/TAA | 35, outside VdA/TAA |
| Bonus-winner cap | Ordinary + bonus + linked special seats <= 220; Estero excluded | Ordinary + bonus + linked special seats <= 113; Estero excluded |
| Cap-corrected target | Ordinary majority = `150 - winnerSpecial`; minority ordinary = `314 - majorityOrdinary` (C83(1-ter)(a)-(b)) | Ordinary majority + winnerSpecial = 78; retain 35 bonus seats (S16-bis(1-ter)) |

The literal **314** is an ordinary-seat pool in the Camera cap path, not the
whole domestic chamber. Its consistency requires `392 - allSpecial - 70 = 314`,
thus eight total special seats. With that endowment, ordinary seats are 384
without a bonus and 314 with it. R26-07 must verify the official or provisional I08 tables give
that endowment (including TAA proportional seats); do not silently change 314
or force a conflicting decree to fit it. Senato ordinary totals follow from
`196 - 7 = 189` without bonus and `189 - 35 = 154` with bonus.

Both chambers include special-territory list votes in national admission,
first-place and 42% calculations (C83(1-sexies), S16-bis(1-sexies)). Camera
ordinary allocation uses separate figures excluding those votes (C83(1)(d-bis));
Senato ordinary allocation takes only ordinary regions. Coalitions include all
linked-list votes in their electoral figure: the reform deletes the former
sub-1% exclusion. The subsequent coalition-to-list allocation uses the lists
identified by the applicable admission rule, not all lists automatically.

Foreign votes and seats remain a separate electoral system. L6/7 change that
system, including territorial divisions; this project step supplies no foreign
scrutiny or mapping of old foreign fixtures to the reform. Exclude foreign
figures from domestic admission/bonus/cap calculations. A future domestic-only
result must be labelled 392/196 seats; an optional separately supplied foreign
result cannot be presented as computed by this algorithm. C19(5)-(6) candidature
incompatibilities still require foreign/cross-chamber declarations even though
foreign scrutiny is outside scope.

For renewal of only one chamber, C83(1-quater)/S16-bis(1-quater) require its
no-bonus path. Do not synthesize the other chamber. A missing chamber in a
declared **joint** renewal is incomplete input, not a single-chamber election.

Foreign scrutiny, further historical Camera/Senato coverage review, and
away-from-home distribution modelling are in the
[post-migration roadmap](../MIGRATION_PLAN.md#after-completion-of-the-web-migration).
Paired Senate-history preparation is required within R26-19 by approved R26-04.

L8(12) assigns an away-from-home voter to the lists/candidates of the temporary
domicile. The core can accept those correctly attributed valid aggregates;
R26-19 must disclose if historical elector/geographic projections do not model
such movement. L8 is not authority to reassign them to their residence college.
Administrative procedures in L1/2/5 (filing, signatures, ballot layout, appeals)
are outside the core and assumed satisfied by valid inputs; substantive coverage/candidature rules
in N01-N03 still apply. L2(6), amending S13, also replaces the former age-25
electorate wording with majority age; R26-19 must check the demographic basis of
any historical Senate elector data it reuses. L4 does not supply a new
college-generation formula.
Future vacancies and supplementary elections (C86(3), S21-ter) are outside the
initial simulation, even though initial multiple-election recovery uses their
applicable replacement provisions.

### Senate initial recovery (I07)

Luca approved this provisional interpretation on **16 October 2026**, following
the [R26-01a precedent review](migration-history.md#2026-10-16-r26-01a-senate-recovery-precedents-and-approved-i07).
Revise it if further legal evidence arises. The 2019 Pavanelli decision supports
residual cross-region recovery for initial exhaustion; the 2021 Minasi decision
concerns a later vacancy and does not set the priority for this initial scrutiny.
Neither case directly establishes the reform's new bonus mechanism.

For **ordinary initial allocation**, including seats released by resolving
multiple elections:

1. Fill from the original list in the same region under S17-bis(2)/C84(2),
   following the statutory college and unused-then-used remainder priorities.
2. Exhaust the same-coalition remedies within that region under C84(5): first
   the original college, then the other colleges, in their prescribed order.
3. Only after those regional remedies fail, use S19(2) -> C86(2) -> C84(4) for
   the **original list** in another region. Apply the prescribed regional and
   college ordering, unused remainders before used remainders, and eligibility
   checks. This residual import does not also enable C84(7)'s cross-region
   recovery for other coalition lists.

For a **bonus-candidate shortage**, use the separate express cross-reference
S16-bis(1-quinquies) -> C83(1-quinquies) -> C86(1), insofar as applicable:

1. Seek the prescribed replacement within the region. For a coalition, choose
   the eligible member list by C86(1)'s unused-then-used remainder order and,
   where there are several colleges, its highest college vote percentage;
   then follow the candidate ranking. Preserve the bonus-winning coalition's
   entitlement rather than allocating a fixed bonus share to each member list.
2. When the region has no available candidates, recover for that **bonus-winning
   list or coalition** in another region, by its greatest unused quotient
   remainder, descending; only after those opportunities are exhausted use
   already-used remainders, descending. Within the destination region, apply
   C86(1) again to identify the list, college and candidate.

Do not restore the abolished C84(3) recovery of unsuccessful uninominal
candidates. Do not apply the 2021 later-vacancy priority in place of the regional
steps above. Later vacancies and supplementary elections remain outside scope.

Every cross-region recovery must emit a `ScrutinyWarning` with severity
`"warning"` and policy ID I07, with a reproducible trace containing the route,
original seat identity/type and entitlement territory/list or coalition,
destination region/college/list and elected identity, candidate eligibility,
ordered alternatives, remainder values/used state and any draw context/outcome.
Use seeded statutory draws where prescribed (including C84(8)); apply warned
I03 only when the final tie criterion is missing. Resolve multiple elections
without duplicate final elected identities.

Retain entitlement and filled-seat ledgers separately: regional totals may
diverge after a cross-region recovery. Do not recompute the ordinary allocation
to conceal that movement. Retain national seat entitlement and the applicable
list/coalition allegiance, recording any permitted regional coalition transfer.
If every permitted route is exhausted, report the seat explicitly unfilled;
do not invent another redistribution.

Worked expectations for **constructed R26-17 fixtures**, not reconstructions of
the historical vote arithmetic (all candidates mentioned are otherwise eligible):

| Fixture | Inputs at the recovery boundary | Expected result |
| --- | --- | --- |
| I07-A / regional list first | List A lacks one candidate in region R, college R1. A has an unelected candidate in R2; coalition partner B has one in R1. | C84(2) elects A in R2 before considering B. No cross-region warning. |
| I07-B / regional coalition before another region | A is exhausted throughout R; B has an eligible candidate in original college R1. A also has an eligible candidate in region S with an unused remainder of 0.90. | C84(5) elects B in R1 before A in S. Record the permitted list transfer; do not import the 2021 vacancy priority. |
| I07-C / residual same-list ordering | A and its coalition are exhausted in R. A has eligible candidates in S and T with unused regional remainders 0.70 and 0.40, and in U with a used remainder 0.95. One seat is missing. | C84(4) elects A in S, applying C84(2) within S. Preserve R's original entitlement and S's filled seat; emit I07 warning. Used U cannot precede unused S/T; a separate all-used variant chooses U. |
| I07-D / exhausted routes | A has no eligible candidates anywhere; its coalition is exhausted in R, but B has a candidate in S. | The ordinary seat remains explicitly unfilled. Do not invoke C84(7) to elect B in S. |
| I07-E / coalition bonus shortage | Bonus winner G lacks one bonus candidate in R and has no regional replacement. G has unused regional remainders 0.60 in S and 0.30 in T. In S, eligible member lists A/B have unused remainders 0.20/0.50; B has college percentages 10%/12% and eligible candidates in both. | The bonus route chooses G in S, then B's first unelected candidate in its 12% college, applying the candidate ranking. Preserve G's bonus entitlement; emit I07 warning. A local-replacement variant must fill in R before considering S; an all-used variant exercises used-remainder ordering. |
| I07-F / releases and ties | A multiple election releases the missing ordinary seat; the eligible destinations have equal remainders. Repeat with permuted input order and the same seed. | Follow the same regional-first protocol, use C84(8)'s seeded statutory draw at a tied recovery rank, retain the trace and obtain the same unique final identities. Do not label an express statutory draw as an I03 fallback. |

### Provisional census-based seat tables (I08)

This protocol implements Luca's interim decision, not an assertion about an
unpublished decree. R26-07 must publish the generated tables and reproducible
calculation traces; no tables are generated in this documentation task.

1. Freeze the latest legally declared municipal census, its legal reference and
   date, and a reconciled municipality/constituency/college catalog. The identified
   baseline P0 is the 2021 legal population; verify that no newer legal census
   applies when generating. Do not substitute annual resident estimates.
2. Apportion Camera's 392 domestic seats by K56's population quotas/remainders,
   checking the statutory VdA/TAA structure. For Senato's 196 domestic seats,
   apply K57's regional/autonomous-province minima and special endowments before
   proportionally apportioning the remaining seats by quotas/remainders. Give
   the minimum where the unconstrained population allocation falls below it,
   freeze that territory, and recompute among the others until all constraints
   hold. This constrained procedure must be labelled as part of I08.
3. Provisionally distribute 70/35 bonus seats by population quotas and highest
   remainders among the respective eligible constituencies/regions only, excluding
   VdA/TAA. This is an interim model choice, not a formula quoted from C3/S1.
4. Within each eligible constituency/region, separately apportion its full
   ordinary capacity (no bonus) and its capacity less the assigned bonus seats
   (bonus) among its colleges using their census populations, quotas and highest
   remainders. Validate nonnegative capacities and all chamber/territory totals.
   Resolve unspecified exact population/remainder ties by I03 and trace the draw.
5. Verify `392 - allSpecial - 70 = 314`; if the census/endowments conflict with
   a statutory constant or fixed special structure, report the concrete conflict
   for interpretation rather than changing the statute or fabricating a table.
6. Store `provisional-census` versus `official-decree`, source/method versions and
   resulting tables with each scenario/result. Replace the provisional tables
   when the official decree is available, regenerate dependent defaults/catalogs
   through scripts and rerun affected checks; keep old results reproducible with
   their original table versions.

## Norm-to-function/test matrix

All rows use the current T, with K for constitutional rules and L controlling
amendments. Page
ranges are **printed P pages**. Each test ID is an acceptance obligation for
the named downstream step. A test must check intermediate figures and reasons,
not just a chamber total. `Ixx` refers to the interpretation register.
Check conservation at the boundary prescribed by each stage: cross-constituency
corrections and candidate recovery can change a local total. Do not impose
preservation of every intermediate local capacity merely because national
entitlements must reconcile.

### Inputs, votes and admission

| ID / step | Source (P pages) | Proposed operation: inputs -> outputs | Test boundary and ties |
| --- | --- | --- | --- |
| N01 / 05 | C14-bis(1)-(2), (3-bis), (4-bis); S8(1), (1-bis) (17-19, 104) | `validatePoliticalLinks`: list/coalition IDs, reciprocal declarations and cross-chamber bonus links -> validated political subjects and links | `N01`: inconsistent coalition links, same labels without declarations, minority exception; no inferred cross-chamber identity. |
| N02 / 05 | K48(1), K56, K58; Civil Code 2(1); C18-bis(1), (2-bis)-(3.1), C19; S9(2)-(5), S11(3-bis); C93-quinquies(4),(6), C93-octies; S20(1)(a-ter), S20-bis (21-26, 90-91, 98, 105-107, 131, 133) | `validateCandidatures`: coverage, seven ordered ordinary candidates, gender, bonus endowments, identities/affiliations and special contests -> validated slates or input errors | `N02`: half of Camera constituencies, two-thirds of their colleges and minority exceptions; Senate single-region permission; alternating ordinary gender, national 60% bonus limit with statutory rounding, special gender constraints; <=5 ordinary colleges, <=1 bonus slate, same-label and territorial links, required preceding bonus candidates, foreign/cross-chamber incompatibilities. No bonus slates in single-chamber renewal (C18-bis(2-ter), S9(2-bis)); ages 25/40 at election and the common adult electorate follow K. Input invalidity is distinct from later exhaustion. I09 covers remaining compatibility questions. |
| N03 / 07 | K56-57; C1(2)-(3), C2, C3; S1; L3 annexes 4/5 (7-10, 100-102) | `validateSeatEndowments`: versioned geography, official/provisional decree data -> domestic/special/bonus and two alternative college capacities | `N03`: complete non-overlapping joins, Bolzano boundary revision at sufficient geographic resolution; 70/35; ledger above; official decree tables when available; otherwise population-based provisional tables under I08. Test source versions, provisional labels and replacement by decree tables; reject conflicting endowments. |
| N04 / 08 | C58 third paragraph, C77(1)(c); S14(2), S16(1)(c) (44, 58, 111-112) | `distributeBonusOnlyVotes`: direct list votes `v`, bonus-only total `B`, coalition and college -> integer attributed votes with quota/remainder trace | `N04`: `q=sum(v)/B`, add `floor(v_i/q)`, residual by descending remainder; at **plurinominal college**, not constituency/nation. Single-list ballots accrue to that list; final-total representation bypasses redistribution. Test `(60,30,10), B=7 -> additions (4,2,1)`, conservation and double-attribution rejection; ties/zero-support: I03/I04. |
| N05 / 08 | C77(1)(d)-(f),(i); S16(1)(d)-(f),(i); C93-septies(1)(d)-(h) (58-59, 94-95, 112-113) | `rankPreferenceCandidates` / `calculatePercentages`: valid preferences, list order, list figures, all valid college votes -> non-capolista ranking, list/individual percentages | `N05`: preferences descending, then list order; capolista excluded; individual denominator is **all valid college votes**, not own-list votes or total preferences. TAA uses its constituency. Zero vs missing, multi-preference counts and zero denominator (I04) are separate cases. |
| N06 / 08 | C83(1)(a)-(d-bis),(1-sexies); S16-bis(1)(a)-(d),(1-sexies) (61-62, 75-76, 115-116, 122) | `aggregateElectoralFigures`: attributed ordinary and special figures/links -> distinct national admission/bonus and ordinary-allocation figures | `N06`: special votes change admission/first place/42%, not ordinary allocation; sub-1% linked votes count in coalition figures; excluded-list votes remain in valid-vote denominators; no double-counting candidate-only ballots or Estero. |
| N07 / 09 | C83(1)(e), (e-bis), C93-septies(2) (63-64, 96) | `determineCameraAdmission`: N06 figures, minority qualification, coverage and preliminary special wins -> subjects/lists admitted with separate reason codes | `N07`: exact/below/above 10%, 3%, applicable 20%; coalition must meet 10% and contain a qualifying list; standalone minority access including `ceil(uninominalColleges/4)` wins; admitted-coalition 2-bis members plus **one** best excluded 2-ter list; failed-coalition 3% list; I01 for local minority exception, I03 for tied best excluded list. Do not propagate the quarter-wins exception into every admission category. |
| N08 / 09 | S16-bis(1)(e) (116-117) | `determineSenateAdmission`: national and regional figures/links -> subjects and internal lists with reason codes | `N08`: 10% coalition plus qualifying member; 3% nationally or 20% in at least one region as specified; 2-bis plus best excluded 2-ter; failed-coalition access. The 20% route is not minority-only; admission is not limited to the qualifying region by this provision. I03 for tied 2-ter. |
| N09 / 09-10 | C93(2)-(3), C93-septies(1)(a)-(b); S21(1)-(2) (89, 94, 133) | `determineSpecialWinners`: valid candidate figures, birth dates, links -> preliminary winners and linked-seat ledger | `N09`: highest votes even if linked lists fail admission; Camera younger, Senato older on vote tie; same birth date uses a provisional warned seeded draw (I03). Reuse these winners for admission and caps, not a second draw. |
| N10 / 10 | C93(3-bis), C93-septies(1)(c)-(d),(h)-(i); S21(2-bis) (89, 94-95, 134) | `attributeSpecialVotes`: direct/candidate-only votes and candidate-list links -> list/territorial figures for N06, keeping winner figures separate | `N10`: same rational attribution as N04 but at **uninominal college**; sum only afterwards; single-list link, excluded-list winner, zero/tie cases; S cross-reference per I02. VdA cannot lose its national contribution because it has no ordinary list slate elsewhere. |
| N11 / 10 | C93-septies(2)-(3) (96-97) | `allocateCameraTaaSeats`: national admission communication plus local minority exception, local figures and proportional endowment -> subject seats, coalition-list seats, elected candidates | `N11`: non-truncated quotas; local electoral-figure tie-break then draw; capolista then preferences; totals exclude already assigned special uninominal seats. Local exception I01 does not imply nationwide admission. |

### Joint decision and ordinary allocation

| ID / step | Source (P pages) | Proposed operation: inputs -> outputs | Test boundary and ties |
| --- | --- | --- | --- |
| N12 / 11 | C83(1)(e-ter)-(e-septies),(1-quater); S16-bis(1)(e-bis)-(e-sexies),(1-quater) (64-66, 74-75, 117-118, 121) | `determineGovernabilityBonus`: both admitted-subject rankings, all domestic valid totals, declarations and renewal scope -> paired winners with 70/35, no-award reason, or unresolved status | `N12`: 42% inclusive in **each** chamber; unique first admitted subjects must be declared linked. Test 42/42, 42/below, different/unlinked winners, missing declarations, single renewal and incomplete joint input. I03 for first-place ties: provisional warned seeded draw after the statutory criteria; record the chosen subject and paired bonus effect. |
| N13 / 12 | C83(1)(f)-(g),(1-bis) (66-68, 72) | `allocateCameraNationalSeats`: admitted subjects, ordinary-only figures and bonus decision -> provisional subjects and internal coalition-list targets with traces | `N13`: no bonus uses full ordinary capacities; bonus reserves 70; integer quota then remainders/national votes/draw for subject and coalition-list allocations. Coalition denominator includes only admitted internal lists at the second stage. Zero-seat coalitions need no division. |
| N14 / 12 | C83(1)(f-bis)-(f-ter),(1-ter)(a)-(c) (67, 72-73) | `applyCameraCap`: N13 provisional seats, bonus and linked special seats -> unchanged seats or corrected majority/minority national targets | `N14`: 220 unchanged, 221 corrected; retain bonus; ordinary majority `150-special`; minority `314-majorityOrdinary`. For `special=5`, majority=145, minority=169, winner domestic=220. Reject incompatible endowment (I08); do not count Estero. |
| N15 / 13 | C83(1)(h) (68-70) | `allocateCameraConstituencies`: subject targets, constituency votes/capacities -> provisional matrix and national-target correction movements | `N15`: integer quota, decimal parts, then national votes/draw; exclude targets already met from residual allocation. Verify excess-order, eligible unused remainders, same-constituency transfer and prescribed cross-constituency fallback. Missing terminal tie-breaks: provisional warned seeded draws under I03. |
| N16 / 13 | C83(1)(i),(1-ter)(e) (70-71, 74) | `allocateCameraCoalitionListsInConstituencies`: national list targets and coalition's constituency seats -> list matrix and corrections | `N16`: integer quota; initial remainder tie uses **constituency** votes then draw; correction reuses h's ninth and following sentences. Trace the switch of criteria rather than assuming one universal national/local comparator. |
| N17 / 13 | C83(1-ter)(d) (73-74) | `allocateCameraCapConstituencies`: majority/minority national quotas, local figures/capacities -> indices, normalized local allocations and corrected matrix | `N17`: truncate majority and minority indices at six decimals under provisional I05; normalized quotas `index*capacity/sum(indices)`; remainders then national votes/draw; h correction; test values where the symmetric truncation changes a result and retain the provisional policy ID in run metadata. |
| N18 / 13 | C83-bis(1)(b),(1-bis)(a)-(d) (76-78) | `allocateCameraColleges`: constituency list targets, local votes/capacities, cap branch -> ordinary college matrix | `N18`: normal integer quota, remainder then **constituency** votes/draw, reached-target exclusion and prescribed surplus/deficit movements; cap branch first assigns majority/minority groups with both indices truncated to six decimals, then their lists. Group correction moves within the same college. I03 supplies warned seeded draws for unspecified correction ties. |
| N19 / 14 | S16-bis(1)(f),(1-bis),(1-quater) (118-120, 121) | `allocateSenateRegions`: admitted subjects, regional figures/capacities and bonus decision -> provisional region matrix and ascending national ranking of winner's **used** remainders | `N19`: full vs reduced capacities; integer quota, regional votes then draw after remainder ties; aggregate subject totals; never use national Camera-style proportional allocation for regional targets. |
| N20 / 14 | S16-bis(1)(f-bis)-(f-quater),(1-ter) (119-121) | `applySenateCap`: N19 traces, bonus, special ledger and all regional valid totals -> removals and corresponding regional reassignment | `N20`: 113 unchanged, 114 corrected; ordinary plus special target=78, bonus=35 retained; used-remainder removals, then ascending regional percentages, exclusions and repeated passes; apply the statutory one-seat protection in the percentage passes. Recipient: largest unused remainder, otherwise regional votes. Region capacities unchanged; I03/I06 for provisional seeded ties and single-use remainder state; explicit failure if no lawful progress. |
| N21 / 15 | S17(1)(b),(2)-(4) (123-125) | `allocateSenateListsAndColleges`: corrected regional subject targets, admitted internal lists, college votes/capacities -> list targets and college matrix | `N21`: regional coalition-list integer quota, remainder/regional votes/draw; single college takes regional targets; normal college allocation uses integer quota, remainder/**college** votes/draw, target exclusion and correction movements. Do not copy Camera's constituency tie-break into S17(4)(b). |
| N22 / 15 | S17(5)(a)-(d) (126-129) | `allocateSenateCapColleges`: regional majority/minority seats and group votes -> both six-decimal indices, group matrix, corrected list matrix | `N22`: truncate both indices, normalize by their sum; remainder then **college** group votes/draw; group transfers within college; apply S17(4) within each group. Zero group targets: I04; missing correction ties: warned seeded draw under I03. |

### Proclamations, recovery and composition

| ID / step | Source (P pages) | Proposed operation: inputs -> outputs | Test boundary and ties |
| --- | --- | --- | --- |
| N23 / 16 | C83-bis(1)(a), C84(1), C93-septies(2)(c); S17(1)(a), S17-bis(1) (76, 79, 97, 123, 129) | `proclaimCandidates`: ordinary/special entitlement, bonus slate and preferences -> provisional elected identities by seat category | `N23`: elect bonus slate members, ordinary capolista first then preference order; do not distribute a coalition bonus mathematically between lists. Record supplied candidate affiliation separately. Missing preferences block dependent identities, not seat entitlements. |
| N24 / 16 | C85, C93-octies(3); S17-bis(3), S20-bis(01) (82-83, 98, 129, 133) | `resolveMultipleElections`: all provisional elections, roles and percentages -> one retained seat per person and released seats | `N24`: special uninominal priority, bonus over ordinary, capolista over non-capolista; among capolista wins lowest list percentage; among other wins highest individual percentage. Exact equal percentages: warned seeded draw under I03. Prohibit incompatible bonus/special input before resolving wins. |
| N25 / 17 | C84(2),(4)-(5),(7)-(8) (79-82) | `recoverCameraOrdinarySeats`: released/unfilled ordinary seats, remaining candidates, used/unused remainders -> ordered replacements and unfilled residue | `N25`: same-list same constituency unused then used remainders; same list other constituencies; coalition in original college then other local colleges; coalition other constituencies. Exercise every level and exhausted endpoint; decimal ties draw under C84(8). Preserve movements and final filled/entitled distinction. |
| N26 / 17 | C86(1)-(2), C83(1-quinquies) (75, 83-84) | `recoverCameraBonusSeats` / `fillReleasedSeats`: missing bonus candidates or initial released seats, local slates/preferences/remainders -> replacement holders and traces | `N26`: missing bonus candidate invokes C86(1), same constituency, single list or coalition-list unused then used remainders; in multiple colleges choose list's highest college percentage. C83(1-quinquies) supplies fallback to other constituencies. C86(2) expressly invokes C84(2),(4),(5); do not replace this with all C84 levels indiscriminately. I03 for unsupplied tie-breaks. |
| N27 / 17 | S17-bis(2), S19(2), S16-bis(1-quinquies) (121-122, 129-130) | `recoverSenateSeats`: ordinary/bonus shortages, initial releases, regional slates and traces -> replacement candidates, original/actual territories, I07 warnings or unfilled residue | `N27`: apply the [approved I07 protocol and fixtures](#senate-initial-recovery-i07): regional C84(2),(5) before residual same-list C84(4) through S19(2)/C86(2); no ordinary C84(7) import. Bonus shortage separately follows S16-bis(1-quinquies)/C83(1-quinquies)/C86(1). Exercise seeded statutory draws, exhaustion, explicit unfilled seats and entitlement/filled ledgers. |
| N28 / 18 | Composition of N01-N27 | `runPoliticsReformScrutiny`: validated legal inputs, chamber renewal scope, seeded legal draws -> paired scrutiny stages, ledgers, reasons and supported output status | `N28`: complete independently worked fixtures for no bonus, bonus below cap, cap correction, special territories and recovery; deterministic injected statutory draws and warned I03 fallback draws; repeated proclamation/recovery terminates by consuming an eligible candidature/seat transition or reports no progress. No duplicate final elected identity. |
| N29 / 19-21 | K48(1), K56-58; C4(2), C58; S13(1), S14; L6-8 and scope above | `describeSupportedOutputs`: source/data/model versions and stage results -> reproducible labelled result and export | `N29`: raw ballot validity remains upstream; missing preferences/chamber/decree remain explicit; supplied/generated preference provenance and conditional candidate/recovery reporting under R26-03; common/supplied/model-derived chamber references and dependence assumptions under R26-04; domestic-only totals cannot masquerade as a whole chamber; away-from-home data limitation; no claim to simulate foreign voting or later supplementary elections. These are downstream acceptance checks, not UI implementation in R26-01. |

## Numerical and tie contracts

### Quotas and exactness (R26-06)

Use exact integer vote counts and rational comparisons for legal decisions;
any statistical fractional generation needs an explicit conversion boundary in
R26-19. Avoid an epsilon for equality and avoid binary floating-point rounding
deciding statutory ties. A later implementation may choose its representation,
but cross-products and intermediates must remain exact at supported sizes.

| Operation | Required numeric policy |
| --- | --- |
| Raw bonus/candidate-only attribution (N04/N10) | Non-truncated `q=directTotal/onlyBallots`; integer assigned additions, exact remainders. Equivalent rational form `onlyBallots*v_i/directTotal` avoids a reciprocal division without changing the rule. |
| Ordinary quotas C83(1)(f)-(i), C83-bis; S16-bis(1)(f), S17 | Discard fractional part of the **quota first**, then take the integer part of subject allocations; compare prescribed remainder/decimal values exactly. |
| Camera TAA proportional subject/list quotas (N11) | No instruction to discard the quota's fraction in C93-septies(2)(a)-(b); retain it, then take integer seat allocations. Do not reuse the ordinary integer-quota path. |
| Cap indices | C83(1-ter)(d): truncate both majority and minority indices under provisional I05; C83-bis(1-bis)(b) and S17(5)(b): both group indices expressly truncated. Truncation is `floor(x*10^6)/10^6` for nonnegative x, not rounding. |
| Thresholds/percentages | 3%, 10%, 20%, 42% are inclusive. Compare exact ratios with the proper all-valid denominator; display rounding never affects admission, ranking or cap removal. |

Independent arithmetic anchors for future tests:

- N04: `(60,30,10)` direct votes with seven bonus-only ballots gives quota
  `100/7`, additions `(4,2,1)`, final votes `(64,32,11)`, total 107.
- N13 versus N11 numeric policies: votes `(10,3,1)`, six seats. Integer quota
  `floor(14/6)=2` gives `(5,1,0)` with no residual seat; exact quota `7/3`
  gives integer seats `(4,1,0)` and the residual to the third subject, hence
  `(4,1,1)`. These are small numeric fixtures, not national election scenarios.
- Six-decimal truncation of `2/3` is `0.666666`, not `0.666667`.
- N14: `145+70+5=220`; N20 target: `78+35=113`. Count special seats only once.

### Tie catalogue

Draws must receive an injected seeded resolver and emit the tied alternatives,
stage and chosen outcome. Apply all express statutory criteria first. Where no
final criterion is specified, I03 provisionally supplies a seeded draw with a
`warning` in the affected simulation; distinguish it from a statutory draw.
Permutation invariance is required where the law provides a criterion/draw;
source list order remains material for preference ties.

| Stage | Successive criteria after the primary quantity | Terminal rule |
| --- | --- | --- |
| N04/N10 attribution remainders | No further criterion stated | I03: provisional warned seeded draw |
| N05 non-capolista preferences | Presentation order within list | Determined without draw |
| N07/N08 best otherwise excluded member | No further criterion stated | I03: provisional warned seeded draw |
| N09 special candidate votes | Camera younger; Senato older | Same birth date: provisional warned seeded draw under I03 |
| N11 subject/list remainders | Constituency electoral figure | Draw |
| N12 first admitted subject | No further criterion stated | I03: provisional warned seeded draw |
| N13 national subject/list remainders; N14 minority remainders | National electoral figure | Draw |
| N15/N17 initial constituency allocation remainders | National electoral figure | Draw |
| N15 surplus ordering; same-constituency correction recipient | Surplus count then national figure; recipient unused remainder then national figure | I03: provisional warned seeded draw |
| N16 initial internal-list remainders | Constituency electoral figure | Draw; corrections follow the referred h text |
| N18 normal or cap-group college remainders | Constituency electoral figure | Draw |
| N19/N21 regional allocation remainders | Regional electoral figure | Draw |
| N20 removal rankings; reassignment | Used remainders or regional percentage; recipient unused remainder, else regional figure | Equal ranking values: I03, with state progression I06 |
| N21/N22 college remainder allocation | College electoral figure | Draw |
| N18/N21/N22 surplus/deficit movement rankings | The specified ascending/descending decimals | I03: provisional warned seeded draw for equal movement ranks |
| N24 repeated capolista/non-capolista wins | Lowest list/highest individual percentage respectively | Equal percentages: provisional warned seeded draw under I03 |
| N25 C84 recovery decimals | None | Draw expressly required by C84(8), also when imported by S17-bis |
| N26 bonus recovery list remainders / best college percentage | As specified in C86(1) | I03: provisional warned seeded draw |

## Interpretation register

`Delimited` means implementation proceeds within the stated scope. Decisions
I03, I05, I06, I07 and I08 are **Luca's provisional interpretation/modelling decisions
of 16 October 2026**, to be corrected when further legal evidence arises. They
permit the specified execution paths and are not quotations or claims that the
law expressly supplies these choices. Persist their policy/version identifiers
in reproducible runs. I03 requires a warning whenever its fallback draw is used;
I07 requires a warning whenever its cross-region recovery is used.
Other genuinely non-computable inputs still return a structured partial result.

| ID / status | Reading or explicit boundary | Downstream effect and required test |
| --- | --- | --- |
| I01 / delimited | C93-septies(2) explicitly treats a protected minority list in a failed coalition as standalone for **TAA proportional allocation** if it reaches the local 20%, despite its reference to C83(1)(e)(2-bis). Give effect to that explicit local exception; do not rewrite nationwide admission. | R26-09/10: fixture with failed coalition, locally qualifying minority and no national 3%; eligible locally, not automatically in ordinary nationwide allocation. Preserve anomalous reference in comments. |
| I02 / delimited | S21(2-bis)(a)'s S20(1)(a-bis) reference identifies candidate-list links; candidate-only ballot validity/redistribution is described by S20(1)(c-ter). Read them together for explicit valid inputs; do not change either quotation. | R26-08/10: linked candidate-only vote fixture; unlinked/nonmatching input rejected. Ballot adjudication stays upstream. |
| I03 / provisional decision | After exhausting all express statutory tie criteria, resolve any remaining tie by an injected seeded draw. Canonically identify/order the tied alternatives so input-array order cannot bias the draw. Keep the legal draw stream reproducible. | R26-05/06/08-18/20: test each formerly unspecified tie, including first subjects, best excluded lists, equal dates/percentages and movement ranks. Record rule ID, stage, alternatives, draw context and outcome, and emit `ScrutinyWarning.severity = "warning"` in each affected simulation. Statutory preference list-order criteria still take precedence. |
| I04 / delimited numerical domain | Zero requested seats: empty assignment without division. Zero only-ballots: no additions. A single linked list receives its only-ballots directly. Multiple linked lists, positive only-ballots and zero direct support: no legal proportional weights, unresolved. Zero all-valid denominator makes percentages undefined, never 0%; zero positive-seat eligible vote sum, integer quota zero, initial integer seats exceeding capacity or impossible residual allocation: explicit non-computable stage, no invented allocation. | R26-05/06/08-17: test each case, including an entirely zero-vote election and a cap group with zero target. A zero-target group is inactive; do not evaluate its undefined quota/index. If a required positive-seat stage cannot be computed, block that result. No fallback to legacy scrutiny or equal shares. |
| I05 / provisional decision, symmetry | Interpret “Analogamente” in C83(1-ter)(d) symmetrically: truncate both the majority index and every minority subject index to six decimal places before normalizing the local allocations. | R26-06/13: test both sides at the sixth/seventh-decimal boundary and a case where truncation changes a seat; persist the provisional policy ID. This path is not blocked by the wording ambiguity. Revise if further legal evidence establishes a different reading. |
| I06 / provisional decision, Senate cap progression | Resolve equal removal ranks by the seeded draw of I03. Trace used/unused remainders and consume each remainder at most once: a used-remainder removal entry cannot be selected twice; a recipient remainder changes from unused to used on assignment and cannot win another remainder seat. Once unused recipient remainders are exhausted, use the statutory regional-vote fallback, resolving any remaining tie under I03. | R26-14: test equal ranks, several transfers in one region, single consumption and transitions to regional-vote fallback; retain ordered before/after state and movements. Apply statutory regional exclusions, repeated passes and last-seat protection; report non-computable no-progress states. Fallback tie draws emit warnings. |
| I07 / provisional decision, initial Senate recovery | Apply the [approved protocol](#senate-initial-recovery-i07), informed by the [R26-01a precedents](migration-history.md#2026-10-16-r26-01a-senate-recovery-precedents-and-approved-i07): exhaust same-region list/coalition remedies, then permit residual same-list cross-region recovery through S19(2)/C86(2)/C84(4), without C84(7). Keep the express bonus-shortfall route separate; later vacancies are outside scope. | R26-05/17/18/20: implement I07-A through I07-F expectations and every recovery level. Retain original entitlement and actual election territory, candidate/remainder state, route and draw traces; warn on each cross-region recovery. Preserve unfilled seats when permitted candidates run out. Revise on further legal evidence; do not treat historical decisions as direct proof of the new bonus rule. |
| I08 / provisional decision, census-based tables | Until the prescribed election decree is available, distribute seats from the latest legally declared population census, using the reproducible [provisional apportionment protocol](#provisional-census-based-seat-tables-i08). Record census, geography, method and table versions and mark the resulting endowments provisional. An official decree replaces these tables after validation. | R26-07/12/19-21: missing decree alone no longer blocks census-based generation or acceptance labelled provisional. Verify coverage, constitutional minima, special endowments, 70/35 totals, alternative college capacities and compatibility with Camera’s literal 314. Reject inconsistent inputs rather than silently rescaling. Recheck the latest legal census before generation. |
| I09 / delimited candidature validation | C93-quinquies(6)/S9 refer to Camera filing rules only insofar as compatible; S9 expressly permits a single region. Do not impose Camera's nationwide coverage requirement on Senate inputs, or require bonus slates in special territories/single-chamber renewals. Special ordinary candidacy restrictions and seven-slot/gender rules still need focused compatibility checks. The three-preference gender wording concerns upstream ballot validity, not a license to infer valid preference counts from totals. | R26-05/10: write separate chamber/special-territory validation fixtures and record any remaining compatibility reading before rejecting a legally supplied slate. R26-03's separate valid-combination generator must produce realizable preference aggregates under these constraints; invalid-ballot generation/adjudication remains outside scope. |

The specification is complete as a dated R26-01 mapping and delimitation.
The approved provisional decisions govern dependent work;
they do not mark those implementation steps complete. Changes to this document
must preserve source/version provenance and keep later statistical decisions in
the [modelling document](politics-reform-2026-modelling.md).
