## 2026-10-16 Politics reform implementation documentation

Completed documentation only, following Luca's approved implementation proposal
and three additional requests for modelling discussions:

- Added the then-active reform implementation plan with 21 pending steps,
  dependencies, legal function boundaries, test cases, and acceptance criteria.
  The steps retained the original 18-stage scope and added three explicit
  modelling decisions before dependent production generation. The completed
  plan was later retired; current legal and application guidance lives in the
  specification and application guide.
- Added [`politics-reform-2026-modelling.md`](../../politics-reform-2026-modelling.md)
  to isolate the decisions on bonus-only ballots, preferences, and Camera-Senato
  differences. No statistical model was selected or implemented.
- Preserved Luca's proposal to interpret generated list votes as final totals
  including redistributed bonus-only votes. Required checks distinguish exact
  representation equivalence, integer/geographic attribution effects, and
  sensitivity to unobserved behavior; they cannot establish empirical absence
  of bias without calibration data. Double redistribution is explicitly excluded.
- Documented missing-preference alternatives and their effects on candidate
  identities, multiple elections, exhaustion, and final filled seats. Documented
  chamber-dependence alternatives and their effect on the joint bonus condition.
- Linked the focused plan and decisions from `MIGRATION_PLAN.md` and the reform
  transcription. Retained stable guidance in `AGENTS.md` and kept narrative here.
- Kept all reform steps unchecked. Normative concordance, modelling decisions,
  legal scrutiny, generators, worker, and UI remain future work. The quoted legal
  transcription was not changed.

Verification:

- `git diff --check`: passed; Git emitted only LF-to-CRLF conversion notices.
- A read-only Python check validated 37 local Markdown file/section links in
  the active migration plan and three reform documents, all 21 ordered step
  headings and matching unchecked entries, and exact preservation of the legal
  transcription from its original introductory paragraph onward against `HEAD`.
- Changes are limited to Markdown documentation. Application tests, builds,
  benchmarks, and normative source reconciliation were not run for this task.
