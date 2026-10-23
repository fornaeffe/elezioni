# Poll preparation

Use this guide for the poll step in [MIGRATION_PLAN.md](../MIGRATION_PLAN.md).
For measured results and source-selection rationale, read only the
[poll pilot history](migration-history.md#2026-10-23--selected-poll-archive-and-reconciliation-pilot).

## Task map and boundaries

| Task | Entry point |
| --- | --- |
| Add/review selected observations | `data/sources/polls/archive.json` |
| Inspect survey metadata and references for manual audit | `data/sources/polls/manifest.json` |
| Add aliases, joint lists or a reviewed political proxy | `data/sources/polls/entities.json` |
| Validate/normalize the archive offline | `web/scripts/data-preparation/polls.ts` |
| Reconcile support to scenario lists | `web/src/lib/polls/reconciliation.ts` and `types.ts` |
| Generate/check preparation artifacts | `web/scripts/data-preparation/polls-cli.ts` |
| Review historical polls and official result pins | `data/sources/polls/history/` |
| Replay official historical result import | `web/scripts/data-preparation/poll-calibration-sources.ts` |
| Fit and validate historical calibration candidates | `web/scripts/data-preparation/poll-calibration.ts` |
| Regression coverage | `web/scripts/data-preparation/polls.test.ts` |

Substep 1 prepares observations and reconciliation previews. Substep 2 builds
opt-in, replayable poll-informed scenarios for the reform generator. Substep 3
provides historical pairing and calibration/backtest reports. Its acceptance gate
currently rejects automatic corrections; the active policy remains assumed.
The initial archive covers October 2026 national adult vote-intention questions;
it is not a representative historical panel or evidence of forecast accuracy.

## Offline preparation in a fresh clone

Requires Node 24 and npm. From `web/`:

```powershell
npm ci
npm run data:polls:check
npm run data:polls:generate
```

The three JSON files (`archive.json`, `manifest.json`, `entities.json`) are the
local source of truth. These commands need no network, PDFs, Python, ignored R
caches or production snapshots after dependency installation. They validate
JSON schemas, dates, references and entity/question links, normalize the questions,
and reconcile
each alternative separately against the small tracked reform defaults. `check`
writes nothing. `generate` writes immutable observation, audit and preview JSON
under ignored `data/derived/polls/`. Repeating it produces identical bytes.

Optional paths are relative to the working directory:

```powershell
npm run data:polls:generate -- --sources ../data/sources/polls --output ../data/derived/poll-check
npm run data:polls:check -- --scenario C:/Downloads/reform-scenario.json
```

The optional scenario must be a valid reform envelope. Its ordinary filed lists
and historical correspondences define preview targets; the tracked defaults
provide historical names. Custom historical IDs outside that catalog require a
richer named-history adapter. The pure helper accepts an explicit historical
catalog, baseline weights and poll mappings without depending on reform types.
The CLI deliberately uses only ordinary filed lists: special-territory ballot
groups retain their separate modelling boundary. It does not apply user share
overrides or represent the preview as a complete future election distribution.

## JSON maintenance and optional manual audit

The chosen source is [sondaggipoliticoelettorali.it](https://www.sondaggipoliticoelettorali.it/).
The [department's description](https://informazioneeditoria.gov.it/it/attivita/sondaggi-politico-elettorali/)
explains the methodological/question information pollsters submit. Maintain the
selected JSON archive in Git. No preparation command scrapes the website or
requires a captured source document.

For each addition:

1. Add survey metadata to `manifest.json`: a unique ID and government survey
   `reference` (e.g. `Sondaggio14362`), archive URL, retrieval instructions,
   title, pollster/commissioner, publication/fieldwork dates, electorate/territory,
   methods and sample information. The v2 manifest has no document path, size,
   SHA-256 or `reviewedFiles` pins. `capturedOn` records the original consultation
   or capture date, separately from the JSON archive's `reviewedOn`.
2. Transcribe the selected vote-intention question once into `archive.json`,
   following its data contract. Record page numbers, question number, response
   labels, numerical levels, exclusions and any repaired/ambiguous wording in notes. No new
   poll-specific executable helper is needed.
3. Classify response rows: `list` with canonical entity IDs; generic `residual`;
   overlapping `coalition-total`; or `nonresponse`. Keep specific grouped
   responses as one row with several entity IDs. Record conditional ballot
   labels/assumptions. All alternatives from one survey share its
   `independenceGroup`; full-survey N is not their effective decided-voter N.
4. Review aliases and any joint-list components or proxy in `entities.json`.
   Add unknown parties as entities rather than silently discarding their votes.
   Each proxy needs a note stating its modelling interpretation.
5. Run the commands/tests below and inspect the resulting estimates. Valid JSON
   edits take effect directly without repinning checksums. Update review dates
   and declared versions when accepting a source revision.

For a manual audit, open the government archive URL from the manifest, search by
pollster, title and publication/fieldwork dates, and select its survey reference.
Use the question/page numbers and notes in `archive.json` to check the selected
answers, denominator, exclusions and assumptions, including attachments. Audit
survey meaning and values, not PDF SHA-256: the website generates documents
dynamically. Optional downloaded PDFs remain ignored and never affect preparation.

The manifest enforces a 1 MiB budget for the three JSON files.
`.gitattributes` keeps JSON LF across Windows/Linux. Generated bundles record
automatic JSON content fingerprints for deterministic output identity; these
are not upstream-source verification or hashes that a maintainer must repin.

## Observation and reconciliation rules

- Source answers remain percentages; normalized support uses fractions summing
  to one. Coalition totals, reported changes, unrelated questions, seat
  simulations and nonresponse do not add party support. Nonresponse is not an
  election-turnout estimate. Unknown sample bases/confidence levels remain null.
- Up to one percentage point of total discrepancy is treated as rounding, with
  the factor recorded. A larger missing total becomes an explicitly estimated
  residual while preserving observed levels. A total above 101% requires source
  review: malformed/overlapping data is not a missing-evidence fallback.
- Explicit poll mappings take precedence. Otherwise use verified name aliases
  and joint-list components, then historical correspondences. Normalize factors
  within each historical source list. Multiple historical sources with the same
  entity have equal evidence weight when their historical totals are absent.
- Many-to-one mappings sum support. Missing within-group splits use equal
  entity weights; target splits use reviewed factors, positive baseline shares,
  or equal weights. Record all inferred splits, including their assigned mass.
- Preserve unresolved support as an estimated residual. Prefer eligible lists
  not already represented by mapped answers, then all eligible lists. Allocate
  by positive historical baseline weights where supplied, otherwise equally.
  Never send poll residuals to `astensione` or silently turn them into zero.
- The catalog declares Lega as the initial Futuro Nazionale proxy: an assumed
  geographic donor, or absent-option allocation proxy if the future scenario
  excludes FN. This is not a measured voter transfer. A present FN target keeps
  its own polled support; the proxy concerns its missing geographic prior.
- Record territorial prior recipes: reviewed historical correspondence first,
  declared donor geography next, uniform support over filed eligible units last.
  Substep 2 must materialize these estimates only over legal filing support,
  conserve target totals and expose provenance. The pilot invents no municipal
  observations. Structural filing exclusions still apply.

The output keeps raw support, assignments, estimated mass/methods and prior
recipes separately. Missing political evidence should leave reconciliation
computable. Invalid JSON/schemas, zero-total mapping factors, absent
eligible targets and infeasible legal filing remain genuine input errors.

## Poll-informed scenarios (substep 2)

From `web/`, after preparing the required reform production snapshot:

```powershell
npm run data:polls:references -- --date 2026-10-23
npm run data:polls:references -- --scenario C:/Downloads/scenario.json --date 2026-10-23 --ballot centrosinistra-conte
```

`poll-references-cli.ts` verifies the production snapshot hashes, prepares the
tracked archive, and writes an immutable report and scenario under
`data/derived/polls/`. It validates the prepared scenario through one actual
paired generation and composed scrutiny before writing. Import the printed
scenario JSON through the existing reform scenario upload/JSON interface.
Defaults and saved scenarios are never automatically overwritten. The command
requires production data: regenerate it using the reform generation guide if
missing. Substep 1 commands still work without that production bundle.

`web/src/lib/polls/forecast.ts` defines `poll-reference-v1`; the reform adapter is
`web/scripts/data-preparation/poll-references.ts`. The report retains every
selected/excluded question, effective sample/weight, reconciliation estimate,
source archive version, baseline and policy. Scenario v3 embeds references,
uncertainty, donor recipes and provenance. V1/v2 remain unchanged when polling
is absent, and presentation upgrades retain v3. Generated input provenance
carries both the polling archive and reference versions for replay/export.

Selection uses publication and fieldwork dates at/before the projection date,
with a 60-day age limit. Generic questions are the default. An explicit ballot
label replaces the generic wave from its pollster; one latest compatible wave
per house prevents alternative questions or prolific houses acquiring unlimited
weight. No usable evidence leaves historical references active.

The central convention is a probability-scale weighted mean, shrunk toward
pooled Camera/Senato historical support with 200 pseudo-observations. Poll
weights decay with a 14-day half-life. Effective N uses a reported question base,
or estimates it from full N and the reported nonresponse fraction (otherwise
50% decided); divide by design effect 2 and cap at 600. These are assumptions,
not fitted house corrections. Estimated allocations double their sampling
variance. The reference variance also retains prior uncertainty and a shared
0.08 logit-scale error floor. Party logit perturbations are normalized jointly;
their reference-date draws are shared between chambers. Future national,
regional and local movement continues in the existing separate streams. National
polls refresh the national date only; territorial uncertainty retains its
historical date, because national polling does not measure current local shares. This
is a logistic-normal approximation, not calibrated coverage or a fitted
multinomial covariance. Substep 3 must compare/calibrate these conventions.

Common anchors use the approved historical chamber contrast. Apply poll targets
only to ordinary municipal support, preserving abstention and special-territory
inputs. A newly supported party uses its declared donor geography, then uniform
filed support. If all historic mappings disappeared, recover the observed raw
historical valid total as a labelled turnout proxy. Never invent filing support.
Historical odds ratios and ordinary residual-list mass are retained through IPF.
National and then local user references apply afterward; a supplied chamber
reference wins only in that chamber. Its measurement component is removed in
that scope, while ordinary mean-mode future movement remains. Reprepare after
changing lists, filing, correspondence or the projection date; validation rejects
incompatible references rather than silently relabelling old evidence.

## Historical calibration (substep 3)

Use the focused [historical calibration guide](polls-calibration.md) for the
selected 16-survey/six-election panel, official result import, offline commands,
chronological backtests, fitted candidates and the currently rejected acceptance
gate. Normal reference preparation records this audit without changing v1
estimates. See migration history for measured outcomes.

## Verification

From `web/`, run `npm run data:polls:check`, `npm run check:data`, and
`npx vitest run scripts/data-preparation/polls.test.ts`. Tests use just the three
JSON files without networking/legacy caches, accept valid edits without repinning,
ignore optional PDFs, reject malformed JSON/metadata, and cover conditional
questions, source totals, aliases, mapping precedence/splits, residual estimates,
proxies and mass conservation. For changes
to consumers, run the consumer's full type/unit/build/browser checks as well.
