# Vote-generation research archive

Research concluded on **2 November 2026** with **M01 2.0.0** and **M02 2.0.0**
selected; M02 is the default. FV-09 archived superseded research and removed
1,260 exact generated files (4,301,573,109 bytes). `docs/model-drafts/` remains
in place and is excluded from archival and deletion.

| Task | Read only |
| --- | --- |
| Implement the supported laws | [M01](../../docs/models/M01.md), [M02](../../docs/models/M02.md) |
| Prepare or validate production | [Release guide](../../docs/vote-generation-fast-release.md), [source preparation](../../docs/vote-generation-fast-data.md) |
| Review completed verification | [FV-09 receipt](fv09/verification.json) |
| Inspect cleanup scope/counts | [Execution record](fv09/execution.json), [exact dispositions and hashes](fv09/manifest.json) |
| Inspect protected/retained local files | `fv09/retained.json.gz` (JSON; intentionally outside the main manifest) |
| Verify frozen FV-01 evidence | [FV-01 manifest](fv01/evidence-manifest.json), [default domain](fv01/default-domain.json), [original inventory](fv01/inventory.jsonl) |
| Replay an experiment | Relevant report below, then the reproduction boundary below |

## Reports and evidence

| Candidate or study | Historical report and interpretation |
| --- | --- |
| Endpoint/six-category and poll uncertainty | [Methods](docs/vote-generation-methods.md), [endpoint report](docs/vote-generation-research.md), [uncertainty](docs/polls-uncertainty.md), [calibration](docs/polls-calibration.md); rejected corrections remain rejected |
| Fixed-outcome drift | [Model](docs/vote-generation-drift-model.md), [results](docs/vote-generation-drift-results.md), [protocol](docs/vote-generation-drift-testing.md); approximation limits retained |
| Party exact/Laplace/importance/Stan | [Stage-0 stop](docs/vote-generation-party-drift-stage0.md), [restricted research](docs/vote-generation-party-drift-restricted.md), [memberships](docs/vote-generation-party-memberships.md); failed/stopped runs are not valid posteriors |
| Fast share-model screening and corrected replay | [Full report](docs/vote-generation-fast-research.md); corrected confirmation is not an untouched holdout |
| Feasible-field alternatives | [Definition](docs/vote-generation-feasible-model.md), [protocol](docs/vote-generation-feasible-research.md), [results](docs/vote-generation-feasible-results.md); rejected variants retained |
| Installed-legacy comparison | [Assessment](docs/vote-generation-legacy-comparison-plan.md), [results](docs/vote-generation-legacy-comparison-results.md); legal exceptions and exact failing draw IDs retained |
| Superseded correspondence corrections and 1.0.0 comparison | [Audit](docs/correspondence-policy-audit.md), [feasibility](docs/model-comparison-feasibility.md), [Camera results](docs/current-camera-comparison-results.md), [M01 1.0.0](docs/models/M01-1.0.0.md), [M02 1.0.0](docs/models/M02-1.0.0.md) |

`fv01/evidence/` preserves the original selected summaries, failures, source/code
pins and exact omission paths. `evidence/data/derived/` adds exact gzip copies of
59 later reports, selection records, browser ledgers and diagnostics. Decompress
only the relevant file to recover its original bytes. Curated group/college
metrics and compressed details remain under `test/fixtures/benchmarks/`; they
retain their original model versions and acceptance scope.

`fv09/manifest.json` records original logical paths, sizes/hashes, dispositions,
consumers, replay instructions and relocated hashes. Source code and authored
protocols are byte-exact copies. Archived Markdown changes only links and adds
an archive notice. Old fingerprints are never recomputed as new original pins.
The FV-01 inventory remains unchanged, including `deletionAuthorized: false`;
the separate FV-09 manifest and receipt record the later authorized execution.

## Reproduction boundary

Archived source is under `code/web/` with its original internal layout, outside
normal TypeScript/Vitest/build globs. It is a historical source snapshot, not an
installed second engine. Its relative imports, CLI roots, npm command names and
report hashes refer to the **original repository layout**. Do not invoke the
moved CLI files in place. Use a separate Git worktree at the experiment's pinned
revision, install that revision's `web/package-lock.json` with `npm ci`, and run
the original report's commands there. For the exact pre-cleanup source snapshot:

```powershell
git worktree add --detach ../elezioni-research-replay cd69edd9be02c2ca5ce0bc3e133f8608fe721c83
```

The 1.0.0 Camera campaign specifically uses `8dcb830`; the FV-01 extraction uses
`6f2e23b`. These historical identities do not become current browser selectors.
Use report-specific source hashes/cutoffs/seeds and archived protocols; raw
pinned downloads can be reacquired. Deleted tensors/chains require regeneration.
Retained unknown/manual artifacts and legal replay inputs remain local, with
exact retained paths recorded in `fv09/retained.json.gz`.

Node/TypeScript tools use Node 24 and the pinned npm lockfile at the replay
revision. Python/Stan tools additionally use the isolated environment described
in `code/web/scripts/data-preparation/stan/README.md` and `requirements.txt`;
compiler/native products and shared external environments are not archived or
removed. Historical machine/thread/timing claims retain their original context.

## Small offline verification

With Node 24, from the repository root:

```powershell
node research/vote-generation/fv09/check.mjs
node research/vote-generation/fv01/freeze.mjs --check
```

Or from `web/`: `npm run test:research:archive`. No downloads, npm dependencies,
Python, Stan, heavy chains or fit are needed for the two direct Node commands.
They verify relocated/retained hashes, deletion records, frozen FV-01 numerical
extraction, worked contract arithmetic, physical coverage and an independent
reduced Gaussian/Helmert fixture. They do not claim predictive calibration or a
new browser/resource acceptance campaign. `freeze.mjs --check-sources` and
`--write` require the original pre-cleanup checkout and raw artifacts.

The manifest executors default to dry-run, reject changed bytes/escaping paths
and refuse to reapply completed cleanup. They are an execution audit, not a
routine pruning command. For future cleanup, create a fresh reviewed manifest.
