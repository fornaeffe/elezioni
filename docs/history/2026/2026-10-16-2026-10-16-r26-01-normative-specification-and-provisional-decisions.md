## 2026-10-16 R26-01: normative specification and provisional decisions

Completed the specification and updated it to Luca's requested scope:

- The [transcription](../../politics-reform-2026.md) is the current consolidated
  operative source for all and only statutory provisions translated within the
  domestic initial-scrutiny scope. Promulgated law and official corrections
  control conflicts with other official sources. Provisional interpretations
  remain separate in the [specification](../../politics-reform-2026-specification.md).
- Included constitutional elective totals, foreign-seat endowments, population
  apportionment and electorate/candidate ages; majority age; single-chamber
  candidatures; applicable territorial/list coverage; ordinary and special
  vote attribution, allocations, proclamations and initial recovery. The
  promulgated Bolzano U04-U06 mapping contains 116 municipalities.
- Kept filing/signature/symbol procedures, individual ballot adjudication,
  foreign scrutiny and later supplementary elections outside the transcription's
  operational scope. Large unchanged geography/census tables remain sourced
  input catalogs rather than duplicated algorithmic text.
- The specification contains 29 norm-to-function/test boundaries and nine
  interpretation/data policies. Test IDs are obligations for later coding,
  not application tests executed during this documentation task.
- Legal-source review uses Law 9 October 2026, no. 180 (26G00201, GU 235),
  the A.C. 2822-B dossier and parliamentary erratum, and the Presidency of the
  Republic's constitutional text. The reform enters into force on 24 October.
  The dated source register records the local/official comparison PDF hashes.

Luca's provisional decisions, recorded on 16 October 2026 and revisable when
further legal evidence arises:

- I03: after all express legal criteria, resolve an otherwise unspecified tie
  with an injected seeded draw and report a warning in every affected simulation.
  Preserve tied alternatives, draw context and outcome; do not override express
  preference-list order or age criteria.
- I05: apply symmetry, truncating both majority and minority Camera cap indices
  at six decimals before normalization.
- I06: resolve equal Senate cap-removal ranks by warned seeded draw; trace
  used/unused state and never reuse a remainder for another award/removal.
- I07: add pending R26-01a before R26-02 to research any relevant past Senate
  recovery case, establish its primary-source outcome and legal applicability,
  and record the supported provisional recovery reading and tests. No such
  precedent is asserted or researched as completed in this documentation update.
- I08: until the election decree exists, generate provisional seat tables from
  the latest legally declared population census, applying constitutional/special
  constraints and the documented interim population quota/remainder method.
  The identified baseline is DPR 20 January 2023's legal population at
  31 December 2021; recheck the applicable latest source at R26-07 generation.
  Label/version census-based tables and replace them with verified decree
  tables when available, preserving replay of prior runs.

Plan changes:

- Only R26-01 is complete. R26-01a is the next step and R26-02 depends on it;
  no modelling decision, legal implementation, generator, worker or UI step
  was implemented here. Relevant downstream directives now carry the provisional
  policies, warnings and seat-table provenance.
- After completion of all web simulator/data-preparation migration work, plan
  foreign-seat simulation, historical Senate-result modelling if not already
  delivered through R26-04 (accounting for the voter-age change), and
  away-from-home voter distribution. R26-04 first audits available source data;
  statistical choices remain pending.

Verification of the updated documents:

- Read-only checks passed for 56 local file/section links across the root plan
  and four reform documents; 22 ordered reform checklist entries with only
  R26-01 complete; R26-01a before and required by R26-02; all N01-N29 and I01-I09
  rows; and the provisional policy, warning and scope references.
- Checked 23 retained scrutiny article bodies for content consistency after
  source/typography normalization. Compared constitutional articles 56-58 and
  the added C18-bis(2-ter), S9(2-bis), S11(3-bis) excerpts directly with downloaded
  official text, normalizing publication typography only.
- Confirmed the Bolzano table reproduces the promulgated annex's 116 distinct
  municipality entries: U04 has 2, U05 has 59 and U06 has 55. The table preserves
  the source's bilingual names; geography joins still belong to R26-07.
- Exact rational checks passed for bonus-only attribution, integer versus
  non-truncated quotas, six-decimal truncation and the 314/154/220/113 seat
  identities. These are documentation checks, not implemented reform tests.
- `git diff --check`: passed. Application tests/build/browser checks were not
  run because all changes are Markdown; application behavior and generated
  data remain unchanged. Temporary source/review scripts stayed outside the
  repository. The Senate precedent search and census-table generation remain
  pending in their own steps.
