## 2026-10-22 — Reform scenario editor alignment

Implemented the scenario-editor item in `MIGRATION_PLAN.md`, with Luca's decisions:
persist presentation metadata; common editing as the initial workflow; explicit
advanced Camera/Senato scopes; supplied data stays JSON; expose the existing bonus
planner and compatible templates; defer legacy conversion UI; retain mean mode.
The result-table/plot alignment item remains open.

`ReformScenarioEditor` coordinates typed immutable edits and separate subject,
reference, historical-mapping, filing, bonus-link and candidate sections. Lists
and searches, filing/template tables and preview tables use bounded slices;
advanced panels mount only when opened. Extracted the earlier editor's percentage
input for both workflows and reused its grouped editing/search conventions.
Chamber overrides retain precedence after common edits. Structural changes show
dependent-removal counts before application; supplied ballot data requires JSON
reconciliation before structural edits. Invalid drafts do not replace the last
valid saved scenario. Stale JSON drafts cannot overwrite newer form edits.

Reform scenario v2 adds scenario title, list colors and coalition labels/colors.
The strict parser/export path retains valid v1 reform scenarios verbatim; only
editable drafts upgrade explicitly with deterministic presentation defaults.
Existing result sessions keep their executed scenario, and presentation-only
upgrades do not trigger a false changed-scenario notice. Metadata changes leave
legal inputs, generation output and RNG streams identical. No legacy conversion
wizard or alternative share model was added.

Added a compact editor catalog generated/fingerprinted alongside production
defaults, containing municipality/geography labels and historical list metadata
without municipal vote matrices. Regenerated through the existing Node pipeline:
snapshot `r26-generation-v1-c5c4be9fafb5` and model
`r26-models-v1-f71a72020aaa` remained unchanged; defaults are
`r26-defaults-v2-45ca4adeaaa8` and editor catalog is
`r26-editor-v1-72979a99ca58`. The preparation check verifies the editor projection
against the checksum-verified snapshot. Generated artifacts were not hand-edited.

An independent browser worker prepares references and one deterministic generated
input. It previews actual ordinary/bonus/special placements, including bonus
ordinary-prefix affiliations, resolved share references, weighted abstention and
normalization warnings. Templates pin compatible identities without allowing a
new bonus composition/order. Previews invalidate on edits and remain explicitly
conditional on simulation 1: they do not run scrutiny or establish computability
for all simulations.

Verification: `npm run acceptance:reform` passed in this checkout with all required
production bundles: checksum/catalog checks and a complete 41-stage/588-filled-seat
vertical slice; preparation/application type checks (zero Svelte errors/warnings);
909 unit tests, zero skipped; 25 browser tests at `/` and 25 at `/elezioni`, zero
skips/retries; both static builds and final root-build restoration. The eight
new unit tests cover v1/v2 scenario/result archives, unchanged generation, common
and chamber edits, filing/mapping dependencies and bonus-prefix templates. Four
new browser tests cover persistence/export, explicit chamber controls, dependency
review, invalid-draft retention, v1 imports, JSON supplied data, production worker
preview/template compatibility and mobile overflow. Existing result replay,
workers, missing-data handling and legacy/regional paths passed unchanged.
Acceptance evidence: `data/derived/r26-21-acceptance/report.json` and its unit/browser
reports (generated, untracked). `git diff --check` passed. Build chunk-size/plugin
timing notices remain informational; the politics page bundle is about 182 kB
gzip. No new performance claim, commit, deployment or publication is implied.

Browser-test development exposed a selector issue: nested select-label text
includes option text for `getByLabel(..., exact: true)`, even though its accessible
combobox name is correct. Use `getByRole('combobox', { name, exact: true })` for
these controls. Svelte read proxies also require copying JSON trees at editing
boundaries rather than passing proxies to `structuredClone` or worker messages.
The final browser/type/acceptance checks include these corrections.

Operational details and task-oriented code map:
[`politics-reform-2026-editor.md`](../../politics-reform-2026-editor.md).
