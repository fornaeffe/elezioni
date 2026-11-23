# Task: split migration history and retire completed plans

Intended for GPT-6 Luna. Follow [repository instructions](../../AGENTS.md) and
the [history convention](../history/README.md). This is a separate task: perform
the work; do not merely propose it.

1. Inventory `docs/migration-history.md`: its structure, dates, links, anchors,
   references and any undated or grouped sections. Design coherent task or
   milestone entries under `docs/history/YYYY/`; use judgment for interrelated
   sections and avoid inventing dates or losing context. Preserve all substantive
   decisions, outcomes, limitations and evidence. Preserve historical wording
   where it matters, and label historical commands or obsolete status as such.
2. Prefer a reproducible script for the bulk split and link mapping, followed by
   manual refinement of boundaries, titles, dates, cross-links and discoverability.
   Keep any useful script in an appropriate location or explain why it was not
   retained. Compare the source and output systematically to detect omissions,
   duplicates and changed content.
3. Update repository links to the old document and its headings, including
   relative links inside moved entries. Verify file paths and Markdown anchors,
   including duplicate headings and non-ASCII headings. Do not
   silently break evidence links. Update the history index when the split is done.
4. Inventory plan-like documentation across the repository, not just files named
   `*plan*`. Inspect actual implementation, TODO and history before deciding a
   plan is complete. Preserve every unfinished task in TODO or the appropriate
   active plan. Retire completed plans; relocate stable guidance only if still
   applicable and not already covered, into the most relevant stable document.
   Preserve decisions and evidence in history, update links and distinguish
   historical plans from current instructions. Do not retire a plan merely
   because the migration was published.
5. Validate content coverage, links and anchors, and `git diff --check`. Run
   focused checks if changes affect runnable code. Do not run long acceptance
   campaigns. Record the work, checks and any unresolved limitations in a new
   history entry and update TODO. Report results and any remaining ambiguity.

Do not alter election logic, frozen fixtures or production data as part of this
cleanup. Avoid dumping the large history file or reports into tool output.
