# Migration history split and completed-plan retirement

Split the former `docs/migration-history.md` into 149 dated task or milestone
records from its 195 top-level sections. The June 5 baseline/snapshot sections
and each day's numbered checkpoints remain grouped to preserve their shared
context. Dated entries retain their original section headings and substantive
content. `migration-history-map.json` maps every source section to its new file
and records source-section, output-file and former-document SHA-256 values;
`split_migration_history.py --check` verifies mapped files, section coverage
and recorded output hashes. The history index now directs readers to search the
task records. The bulk extraction was a one-time deterministic pass followed
by manual link refinement. A mutating rerun script was not retained because the
completed files and refined mapping are now the source of truth; retaining a
script that would overwrite this reviewed content would be misleading.

Updated in-repository links to the old history document and its section anchors,
and rebased relative evidence links inside moved entries. Removed the old
monolithic file after checking the map and links. Links to retired R sources and
datasets are retained as plain historical paths; the associated SHA-256 evidence
remains recorded, but those files are no longer available in this checkout.

Retired the completed reform implementation plan and the completed fast-model
production and research-archive plans. R26-01 through R26-21 and FV-01 through
FV-09 are complete; the current reform specification, application/generation
guides, fast-model contracts and operation guides retain maintained guidance.
FV-09 already preserves the research-archive plan as a compressed historical
source. Deferred work remains in `TODO.md`; the retirement does not close it.

Verification: the map covers all 195 sections in 149 files and records the
former source checksum; `split_migration_history.py --check` passed. All 40
links to split history records were checked for paths and anchors with zero
failures; moved entries' own history links were checked as well. `git diff
--check` passed. No application code, fixtures or production data
changed. Historical source commands remain historical and are not validated as
current workflows.
