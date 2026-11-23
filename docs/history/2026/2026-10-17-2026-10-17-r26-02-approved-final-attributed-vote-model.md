## 2026-10-17 R26-02: approved final-attributed vote model

Luca approved generation of **final attributed list totals**, retaining explicit
direct-list and bonus-only/candidate-only components for supplied scenarios and
legal tests. Recorded the active [R26-02 contract and implementation protocol](../../politics-reform-2026-modelling.md#bonus-only-votes-r26-02)
and marked the decision step complete. R26-03 is next; preferences and chamber
dependence remain undecided. No generator, input conversion or legal scrutiny
function was implemented in this documentation update.

### Decision, alternatives and limits

Scenario shares and local overrides describe final list support, including
lawfully attributed bonus-only votes. Their ballot composition remains unknown;
no default random bonus-only count or implicit zero count is introduced. Explicit
components undergo attribution once before admission. The same convention
applies separately to special candidate-only votes at uninominal level; generated
special candidate totals equal the sum of their linked lists' final totals.
The model ID is `final-attributed-list-totals-v1`; future results/exports must
retain representation and source/conversion provenance.

Alternatives assessed:

| Alternative | Decision and rationale |
| --- | --- |
| Generate final attributed totals | Selected default: understandable forecast-share semantics without an unsupported intermediate ballot-composition model; fits the existing generator's list-total boundary. |
| Generate direct votes and random bonus-only counts | Not selected: frequency, geography, coalition effects and chamber correlation lack calibration. Adding assumptions can change the forecast distribution without evidence of improved accuracy. |
| Accept supplied explicit components | Retained for controlled scenarios and independently testable legal attribution; this does not require a calibrated random composition model or a new UI control in this step. |
| Use historical candidate-only behaviour as a bonus-only proxy | Not selected as default calibration: different ballot/election context. Historical evidence may inform explicitly labelled sensitivity hypotheses. |

The algebraic identity before integer rounding is
`T[i] = D[i] * (1 + B / sum(D))` for positive direct total. It preserves
within-coalition proportions at the attribution college. Thus identical final
totals support identical downstream scrutiny, conditional on identical other
legal inputs and downstream draws. This is representation equivalence, not
empirical evidence that the forecast is unbiased. Unknown correlations between
ballot behaviour and territorial/list strength remain uncalibrated.

Use the legal [N04/N10 boundaries](../../politics-reform-2026-specification.md#inputs-votes-and-admission):
ordinary attribution at plurinominal college, special attribution at uninominal
college, with integer additions/remainders and I03/I04 tie/zero-support policies.
Do not infer preferences from the unknown composition; R26-03 decides that model.
Revisit R26-02 if calibrated composition data become available, composition is
a requested output, or implementation checks reveal a material limitation.

### Reproducible arithmetic and source-data evidence

Read-only Python exact-integer/Fraction checks, repeated when recording approval:

- Enumerated list counts 2, 3 and 4; every direct-vote vector with entries in
  `0..6`, excluding the all-zero vector; and every bonus-only count in `0..16`.
  For each, assigned `floor(B*D[i]/sum(D))`, then one addition per remaining
  highest remainder `B*D[i] mod sum(D)`. Equal remainders were ordered by list
  index only for this arithmetic enumeration, not as a proposed legal tie rule.
- All **47,430** cases conserved `sum(D)+B`. The largest absolute list deviation
  from the ideal proportional total was **3/4 vote**; every deviation was less
  than one vote. This bound concerns fixed explicit `D/B` at one attribution
  college, not decomposition of an arbitrary target total or a bound on seats.
- Worked example: direct `(60,30,10)` plus seven bonus-only ballots gives
  `(64,32,11)`. Geography counterexample: two units each with direct `(1,2)`
  and one only ballot give `(2,6)` if attributed separately and then summed,
  versus `(3,5)` when the combined `(2,4)` and two only ballots are attributed
  once. Hence municipal/national substitution cannot be presumed equivalent.

Inspected the existing [vote generator](../../../web/src/lib/politics/vote-generation.ts)
and historical loader `R/caricamento_dati.R` (retired), which selected the 2022
`VOTILISTA` field. Audited the bundled
2022 Camera CSV `dati/camera-20220925/Camera_Italia_LivComune.csv` (not retained), SHA-256
`9f4f49dd0dfde3d1ea32ccda9665d6d230959f97ce074418ef2553242aa675b3`:

- Read as UTF-8 with BOM handling and semicolon delimiter. Grouped by
  `CIRC-REG`, `COLLPLURI`, `COLLUNINOM`, `COMUNE`, `COGNOME`, `NOME`,
  `DATANASCITA`; summed list votes and counted the repeated candidate total once.
- All **70,539** groups had a consistent candidate total, never below the list
  sum; **42,740** had a positive difference. The combined excess was
  **1,027,135 votes**. This is historical candidate-only evidence, not an
  estimate of future bonus-only participation.
- Reform calibration must reconcile each source's vote categories using that
  election's attribution rules at their legal geographic level. Document any
  finer-resolution reconstruction assumption. Do not merely relabel the raw
  list field or alter the preserved legacy parity path. This conversion remains
  an R26-19 implementation obligation.

Updated R26-05 representation/provenance requirements, R26-08 equivalence checks,
R26-19 historical calibration requirements and R26-20 export semantics. The
specification now points to the approved statistical contract; the legal
transcription remains unchanged. Full seat/bonus equivalence is still a future
implementation check with exact zero-discrepancy acceptance for matched inputs
and isolated downstream draws. Any stochastic sensitivity experiment must state
hypotheses, margins and seed/replication budget before examining results.

Verification of this documentation update:

- All 73 local file/section links across the root plan, four reform documents
  and this history entry resolved. Confirmed 22 ordered steps with only
  R26-01, R26-01a and R26-02 complete, and all N01-N29/I01-I09 entries retained.
- R26-03/04 decision sections and the statutory transcription are unchanged.
  Exactly five Markdown files changed; no application/generated data changed.
- Repeated the arithmetic enumeration and CSV audit above; counts and source
  hash matched. These are decision-evidence checks, not reform application tests.
- `git diff --check` passed. Application tests/build/browser checks were not
  run for this documentation-only update; generator integration, historical
  conversion and full downstream scrutiny equivalence remain unimplemented.
