## Politics result colors (2026-11-15)

The common reform execution now attaches the executed scenario's list and coalition presentation metadata when producing result series. This keeps presentation outside legal and generation inputs while allowing national and territorial charts and probabilities to use the configured colors. Rosatellum already records those colors. The reform single-run seat chart now colors its bars by the executed scenario's subject, with a neutral fallback for unmatched owners. Existing stored series are not rewritten; newly generated sessions contain the corrected metadata.

Verification: `npm run check` passed with no errors or warnings. The existing reform presentation and series, common result-presentation, and common execution Vitest files passed (30 tests); after updating the existing execution assertions, the execution and presentation files passed again (23 tests). No new tests were added.

Record through: 2026-11-08

This file preserves the detailed migration record: completed implementation,
decisions and rationale, verification results, encountered problems,
workarounds, risks, and dated checkpoints. It is historical context, not the
active task list. See [`../TODO.md`](../../../TODO.md) for the
current directive-only plan and [`../AGENTS.md`](../../../AGENTS.md) for stable agent
instructions.
