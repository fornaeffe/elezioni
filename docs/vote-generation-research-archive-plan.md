# Vote-generation research archive and cleanup plan

## Scope and timing

This is the FV-01 inventory/FV-09 execution plan linked from the
[2026 production plan](vote-generation-production-plan.md). No moves or deletions
are executed by this document. Inventory and preserve small evidence before
implementation; move code and remove outputs after production acceptance and
dependency separation. Keep active production instructions short and task-local.

FV-03 has promoted the required readers/correspondences into
[`fast-share-*` preparation and `data/sources/fast-share/`](vote-generation-fast-data.md).
Those active files are excluded from research archive/deletion. The original
research files remain unchanged for evidence replay; this does not execute FV-09.

FV-01's [evidence index](../research/vote-generation/README.md) now provides the
frozen exact inventory, selected evidence/omission manifest and default physical
domain. Research source locations are unchanged; this is preservation before
FV-09, not an executed archive or deletion authorization.

Archive the endpoint, six-category, fixed-outcome drift, party-level exact/
Laplace/importance/Stan and fast share-model **research** together. Keep the fast
model's maintained production contract, engine, source data and acceptance tests
active. An archive records rejected/failing candidates as well as successful
ones; it must not imply that every retained posterior is valid.

## Target layout

| Location | Responsibility |
| --- | --- |
| `docs/vote-generation-fast-model.md` | Maintained selected model equations, assumptions and scenario semantics, extracted in FV-01 |
| `docs/vote-generation-production-plan.md` | Active implementation steps until completed; evidence goes to migration history |
| `docs/politics-reform-2026-generation.md` | Current production preparation/runtime entry point, updated at cutover |
| `research/vote-generation/README.md` | Single archive index: candidate/status/date, relevant report, reproduction command and evidence manifest |
| `research/vote-generation/docs/` | Superseded proposals, model reports, protocols and reviewed research-table snapshots |
| `research/vote-generation/code/` | Research-only implementations/tests/CLI wrappers, preserving useful internal layout and source revision |
| `research/vote-generation/evidence/` | Small frozen summaries, cases/metrics, failures, source/code pins, environment/seed settings and independent reduced fixtures |
| `data/derived/vote-generation-research/` | Optional regenerated heavy research outputs, ignored; never production inputs |

The archive is outside `web/static/`, runtime imports and normal preparation/test
globs. Provide explicit archive reproduction/test commands and an isolated
dependency description. A normal app build/check must not discover or execute
archived TypeScript/Python/Stan through a broad glob. Do not duplicate a second
active implementation of the numerical production engine there.

## File-family disposition

This list identifies candidates, not permission to move everything matching a
prefix. The execution manifest must enumerate exact files after an import and
consumer audit.

| Existing files/families | Planned disposition |
| --- | --- |
| `docs/vote-generation-fast-research.md` | Extract its approved model definition into the maintained model contract; move the complete historical report, evidence/limitations and reproduction map to archive docs |
| `docs/vote-generation-research.md`, `vote-generation-methods.md` | Archive endpoint and six-category comparison documentation |
| `docs/vote-generation-drift-model.md`, `vote-generation-drift-results.md`, `vote-generation-drift-testing.md` | Archive the superseded formulations/results/protocol; retain historical dates and failed gates |
| `docs/vote-generation-party-drift-stage0.md`, `vote-generation-party-drift-restricted.md`, `vote-generation-party-memberships.md`, `correspondence-model.md` | Archive stopped/restricted inference, provisional table snapshot and original proposal; promote reviewed production memberships separately |
| `docs/polls-uncertainty.md` | Archive the endpoint uncertainty study; it is not the fast model's likelihood |
| `docs/polls-calibration.md` | Split active acquisition/result-provenance instructions from historical estimator/backtest findings; archive only research material and preserve current consumers |
| `docs/polls-preparation.md`, `docs/other-list.md`, R26 modelling/generation/application/protocol guides | Keep active; revise links/scope to the new engine without archiving legal, preference, data-source or UI instructions still used |
| `web/scripts/data-preparation/generation-*.ts` | Archive research comparison/metric/CLI code and tests after promoting required historical acquisition, attribution and normalization out of `generation-history.ts` and its dependency closure |
| `web/scripts/data-preparation/drift-*.ts`, `party-drift-*.ts` | Archive rejected/earlier inference, panels and experiment CLIs/tests; promote required membership/admissibility/coarsening, poll-view reconciliation and numerical helpers into named active modules first |
| `web/scripts/data-preparation/stan/` | Archive research Python/C++/Stan runners, models, independent checks and environment instructions, including the fast reference implementation; omit generated bytecode/binaries |
| `poll-uncertainty*.ts`, research portions of `poll-calibration*.ts` | Archive estimator/study code only after separating source readers and current production consumers |
| `polls*.ts`, `poll-references*.ts`, `web/src/lib/polls/` | Audit consumers; keep source preparation/runtime parts still needed. Remove/archive old reference-estimation entry points only when all dependent active commands are replaced |
| `data/sources/party-drift/{memberships-policy,membership-sources,poll-refinements}.json` | Promote needed reviewed originals to a named production source location, preserving provenance/content identities; archive superseded snapshots rather than deleting curated observations |
| `data/sources/polls/history/{generation-horizon-manifest,generation-horizon-observations,generation-six-category-observations}.json` | Retain/promote required reviewed early observations and pins; separate research-panel definitions. Path changes must update pins consistently without pretending source content changed |
| Other poll JSON, election/source manifests, source/result pins | Retain every authoritative observation/normalization/provenance record required by active preparation or archive reproduction |
| Existing R generation, shared `core/vote-generation.ts`, legacy politics/regional generators, R golden fixtures and exporters | Keep while their workflows consume them; they are not disposable research just because their names mention generation |
| Superseded reform runtime generator/default/model files | Retire only after new dispatch and consumer audit; preserve their repository revision for historical replay. Do not remove candidate/preference/seat assets still used |

The initial dependency audit already shows why prefix deletion is unsafe:
`generation-history.ts` uses `poll-calibration-sources.ts` and historical
attribution; the fast panel loader uses reviewed poll JSON, early six-category
answers and membership policies. `poll-references-cli.ts` still uses poll
calibration. Promote the production dependency closure before archival.

Do not sweep unrelated `R/research-scripts/` work into this archive. Include it
only if a file is actually part of these vote-generation experiments and has no
remaining workflow consumer. Keep the central migration history intact; its
past decisions are an audit trail, not a duplicate active specification.

## Preserve evidence before deleting generated output

Create a small tracked evidence manifest with original path, bytes/SHA-256,
candidate/status, source/code/repository revision, environment, dates/cutoff,
seed, command and retained summary or regeneration recipe. Preserve original
logical paths in the manifest. Relocation must not invalidate a historical hash
check silently: use a recorded old-revision replay or an explicit new path map/
runner. Verify an archive reduced fixture from a clean checkout.

At minimum retain:

- Endpoint/six-category and fixed-outcome drift comparison case metrics and
  decisions; stage-0 posterior disagreement, identifiability/runtime failures,
  stopped Stan diagnostics and the reasons those routes were not adopted.
- Fast development selection
  `fast-development-frozen-20261027.json`; failed/coarse/optimizer confirmation
  summaries and the corrected-replay qualification.
- `fast-fixtures-final-20261027/summary.json`, corrected final development and
  confirmation `summary.json`/`cases.json`, and the full-compatible
  `modular-041250483558/{manifest,benchmark}.json` evidence with required audit
  fields. Retain export identity/check results from
  `fast-export-check-20261027.json`, not two identical large exports.
- Independent reduced quadrature/martingale/projection/count fixtures, component
  and geography coverage, denominators/exclusions and known mean/sparse-event
  limitations. Retain machine/thread details for timings and memory.

Compact summaries may replace large chains/tensors only after verifying they
contain the conclusions used in reports/history. Preserve irreplaceable manual
notes, curated source corrections and failed-run diagnostic extracts. Do not
relabel an unreproducible or incomplete failure as a passing experiment.

## Disposable artifacts and execution safeguards

After the preceding evidence is secured, list these exact regenerable outputs
as deletion candidates:

- Repeated normalized history exports, case inputs, sampled truth/count arrays,
  chain CSV/NPZ/posterior files, profile matrices and duplicate research bundles
  under `data/derived/generation/`, `drift/` and `party-drift/`.
- Superseded development/corrected/intermediate experiment directories and logs
  whose relevant diagnostics/commands have been preserved in the evidence set.
- Temporary analysis scripts/drafts, benchmark scratch files, Python bytecode and
  research-owned native build products once their source/recipe is retained.

Raw pinned downloads are optional caches, not curated source records. They may
be removed separately only after a cold-cache reacquisition check and a consumer
audit; retaining them can avoid unnecessary downloads. The research environment
or compiler cache may be shared outside this repository: exclude it from this
cleanup unless ownership and obsolescence are established explicitly.

Never recursively delete `data/sources/`, all of `data/derived/`, legacy `dati/`,
production runtime bundles or other people's result archives. In particular,
`data/derived/r26-manual-tests/`, legal investigations, legislator-note evidence,
R26-21 acceptance and generated standalone scenario/results are outside this
research cleanup even when a research document references them.

Implement cleanup with an exact manifest/dry-run summary first. For each entry,
record keep/promote/archive/delete, reason, consumers, regeneration recipe,
retained evidence and expected bytes. Before deletion, validate resolved absolute
paths against the intended research directories and recheck sizes/hashes; stop
on a mismatch. Use native PowerShell path handling on Windows, without passing
enumerated paths through another shell. Remove only selected files/directories;
leave unknown or newly created files for review. Record deleted paths/bytes and
verify no production/source artifact was removed.

## Completion checks

Update `MIGRATION_PLAN.md`, `data/README.md`, active generation/model/application
guides, archive reproduction instructions and links from migration history.
Update package scripts, TypeScript/Vitest globs and code/source manifests.
Active preparation/runtime imports must not reach into the archive. An optional
archive test command must reproduce at least one independent reduced numerical
fixture without downloading heavy chains.

Run production cold-cache preparation, integrity checks, static build and
relevant browser checks after the moves; clearly separate optional archived
research suites from required production acceptance. Confirm no research/raw
source artifacts enter the deployed build. Record the archive index, manifest,
exact dispositions, verification and reclaimed space in migration history.
