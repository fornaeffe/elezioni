# Local derived-data retention

`data/derived/` contains ignored preparation outputs and local verification
evidence. Production preparation starts from pinned `data/sources/` inputs;
runtime assets belong under `web/`. Use [production preparation](data-preparation.md)
for regeneration, not an old clean-checkout copy or research export.

## Retained outputs

The 2 November 2026 cleanup retained these groups in their existing locations:

| Need | Paths under `data/derived/` |
| --- | --- |
| Source preparation and current local candidate | `fast-share/`, the complete `fast-share-release/candidate.json` dependency graph, current preparation/candidate dependencies in `fast-share-cache/` |
| Workspace production outputs and verification | `production/`, `workspaces/`, current R26 generation/default/model/editor files, geography/seat-table artifacts |
| Numerical, source, generation, release, fit and UI evidence | `fv02-numerics/`, `fv03-acceptance/`, `fv04-generation/`, `fv05-acceptance/`, `fv06-acceptance/`, `fv07-acceptance/`, `fv08-acceptance/`, `fv08-unified/`, `filing-aware-research/` |
| Model comparison and restoration evidence | `current-camera-comparison/`, `legacy-comparison/`, compact `model-rollback/` and `correspondence-corrections/` reports |
| Application acceptance and completed result archives | `r26-21-acceptance/`, final archives/receipts in `fv08-unified/browser/`, `r26-manual-tests/` final gzip archives and compact verification |
| Legal investigations and replay inputs | `n18-investigation-20261020/`, both `n21-sicilia-*` directories, `r26-*-review-*`, `r26-bonus-selection/`, selected full cases and task scripts in `r26-manual-tests/` |
| Poll/reference and independent audit outputs | Top-level `polls/` outputs, `independent-seat-audit.json`, `correspondence-policy-audit.json`, final pruning/retirement acceptance logs |

These are historical verification records with their original model versions
and limitations. Retaining a report does not grant new acceptance to a model.
The local candidate pointer can differ from the installed runtime candidate;
the cleanup preserves its own complete dependency graph without selecting it.
Frozen regression/benchmark fixtures and the curated research archive remain
in `test/fixtures/` and `research/vote-generation/` respectively.

## Recovery of removed scratch

Automatic approval review blocked preparation of the deletion script. The
cleanup therefore used reversible moves to this local directory outside Git:

```text
C:\Users\forna\Repos\elezioni-derived-cleanup-20261102\
  files\                 Original paths relative to data/derived/
  manifest.json          Every retained/moved file, reason and identity
  execution.json         Completed move actions
  summary.json           Counts and byte totals
  verification.json      Post-move integrity checks
```

The archive holds superseded clean-clone outputs/source copies, build/toolchain
and Git-extracted checkout copies, stale candidate/cache versions, expanded CSV
exports, obsolete research/acquisition scratch, old logs and intermediate replay
or worker tables. Nothing was permanently deleted or compressed by this cleanup;
the repository footprint shrank, but total disk usage was not reclaimed.

In particular, the completed SCN archive, selected legal cases, scenario and
verification remain at `r26-manual-tests/scn-autonoma-20261028/`. Its eight
`worker-*` directories moved to the same relative path under `files/`. Restore
those directories before using the historical runner's `--export-only` mode;
normal result import needs only the retained final gzip. Large intermediate
expanded manual replays also reside in the recovery archive.

Restore only the paths needed for a specific investigation to their original
relative locations, first checking that no newer file would be overwritten.
This machine-local archive is not a test, build or production dependency.
Dependency junctions were moved as links; their live targets were not traversed
or modified. File verification uses SHA-256 for files up to 1 MiB and unchanged
Windows file identity, size and modification time for all regular files.

Future cleanup should trace actual readers and current manifest dependencies.
An output destination in a CLI or browser test is not itself an input
dependency. Preserve final campaign evidence and legal cases; do not infer
permission to remove authoritative sources or people's completed result archives.
