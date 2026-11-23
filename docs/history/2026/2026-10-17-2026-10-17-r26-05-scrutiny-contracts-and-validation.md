## 2026-10-17 R26-05 scrutiny contracts and validation

Implemented the isolated [reform contract boundary](../../../web/src/lib/politics/reform-2026/README.md)
and marked R26-05 complete. R26-06 is next. The new schema version and reserved
algorithm ID identify a legal input envelope; this step does not register an
executable algorithm or change saved scenario/snapshot versions.

### Delivered boundary

- Readonly, JSON/worker-safe input/result contracts cover joint/single renewal,
  chamber-specific votes and references, reciprocal political links, coverage,
  global person identities, affiliations, ordinary/bonus slates, preferences,
  special contests and sourced seat endowments. The shared simulation ID pairs
  chamber inputs. Model metadata carries supplied/generated provenance,
  versions, common anchors, resolved scopes and chamber reference origins.
- A strict structural schema rejects missing/extra fields, mixed vote variants,
  invalid dates, non-finite/negative/fractional/unsafe counts and incompatible
  envelope versions. Semantic validators report coded issues and paths. Successful
  validation returns an owned clone with a branded readonly type; callers cannot
  invalidate it by editing their original payload. No coercion or legacy migration.
- Raw ordinary components retain bonus-only ballots; attributed totals have no
  such field. Special candidate-only components remain separate and reconcile to
  candidate totals, counted once for the valid-ballot/electorate bound. Every
  college and contesting list/candidate requires a row/count, including zeros.
  Zero-vote elections remain accepted for later explicit I04 stage failures.
- Candidature checks cover seven filed slots, alternating genders, election-day
  ages 25/40, rounded national bonus/special gender limits, same-badge and multiple
  candidacy limits, local ordinary/bonus prefix links, special incompatibilities,
  single-renewal exclusions and foreign/other-chamber declarations. Subsequent
  candidate unavailability keeps the original filed slate intact and is accepted
  for later recovery, rather than misreported as a malformed filing.
- Coverage validation separates Camera and Senate: Camera's ordinary-contesting
  lists need half-constituency coverage, including declared linked special
  candidatures, subject to the asserted minority exception; Senate may contest
  one region. Both need two-thirds college coverage per filed ordinary territory.
  Special-only lists do not acquire a nationwide filing requirement. TAA
  proportional slates use the imported seven-slot/alternating rules. Camera TAA
  and Senate special uninominal gender checks are distinct, under I09.
- Missing preferences retain a reason. Supplied adjudicated counts assert upstream
  ballot validity, with identities and aggregate bounds still checked. Generated
  counts require the approved valid-combination certificate: distinct eligible
  candidates, up to three marks, mixed genders for multiple marks, correct exact
  margins and complete ballot budget. Raw components use direct list votes as
  that budget; attributed totals preserve unknown composition. Validity assertion
  and constructive certificate remain explicit alternatives, not inferred zeros.
- Endowment validation checks 392/196 domestic seats, 70/35 bonuses, special
  totals, college kinds/capacities and territorial reductions. Sources distinguish
  constructed, I08 provisional-census and official-decree tables. Provisional
  sources carry census, geography, method and table versions.
- Result types reserve stage and overall partial/non-computable status, subject
  decisions/reasons, conditional conclusions, severity-bearing warnings, policy
  versions, exact fraction strings, injected draw requests/traces, separate seat
  categories, allocation transfers and candidate recovery state. Original
  entitlement and actual election location remain separate, including I07
  cross-region recovery and explicit unfilled or
  unresolved seats; I06 remainder consumption has before/after states.

### Verification and boundaries

- Added **57 reform contract tests**, using tracked, invented geography and
  complete domestic totals independently of generated artifacts. Cases cover
  valid joint/single inputs, missing versus zero preferences, later unavailable
  candidates, malformed references/slots/links, coverage and age constraints,
  incompatible candidatures, double attribution, special candidate-only totals,
  invalid/safe numeric domains and constructive preference validity. Metadata
  fixtures cover common-only, one/both explicit chamber references, no common
  anchor, differing scopes and duplicate scope rejection. A typed JSON/structured
  clone round-trip preserves conditional partial results, I03 warning/draw links,
  I06 consumed remainders, I07 destinations and exact integers beyond JS's safe
  number range. This is contract evidence, not execution of those legal stages.
- `npm run check`: zero errors/warnings. `npm run test`: **23 files, 244 tests
  passed**, including existing legacy regressions. `npm run build` produced the
  static site successfully; Vite emitted timing advisories only. Verified all
  **107 local file/section links** across the root plan, four reform documents,
  history and contract README. `git diff --check` and a separate whitespace scan
  of the new contract files passed.
- Actual official geography, Bolzano joins, complete real regional coverage,
  constitutional regional minima and census apportionment remain R26-07;
  constructed fixtures assert internal accounting, not an official election map.
  Numerical/draw execution remains R26-06, attribution R26-08, special scrutiny
  R26-10 and initial recovery R26-17. Output JSON validation and actual result
  production are not implemented by the type declarations.
- The accepted R26-02/03/04 models remain unfitted. R26-19 must implement scope/
  supplied-share precedence, preference construction and statistical generators;
  current metadata tests establish representability and reference consistency.
  Legacy scrutiny, random streams, scenario schema, algorithm registry, workers,
  UI and generated/default data were left unchanged. Browser/performance checks
  are deferred to integration; no new application path is active in this step.
