# Project History

Record substantive completed work as one Markdown file per coherent task,
decision or milestone in `docs/history/YYYY/YYYY-MM-DD-descriptive-title.md`.
The date identifies the work; add a distinguishing suffix if needed. A task may
span multiple commits or sessions. Routine changes can use a clear Git commit
message alone.

Each entry should make its subject discoverable and concisely record what changed,
why, relevant verification and outcomes, limitations, and links to durable
evidence or current documents. Record actual outcomes and distinguish user
reports from independently checked evidence. Keep operational tasks in
[TODO](../../TODO.md), current requirements in stable documentation, and full
logs and structured reports in their appropriate evidence locations. Do not copy
large logs into entries.

Use this index for the convention and major milestones, not an exhaustive manual
catalogue. Search entry titles and content with `rg` to find a topic. Preserve
working links when moving or renaming entries.

## Archives and milestones

- Migration work is split into dated task records under this directory. Search
  titles and contents with `rg`; use
  [`migration-history-map.json`](migration-history-map.json) to map original
  headings and source hashes to their new files. Use
  [`split_migration_history.py`](split_migration_history.py) to verify the
  recorded split.
- [Publication and history convention](2026/2026-11-23-publication-and-history-convention.md).
