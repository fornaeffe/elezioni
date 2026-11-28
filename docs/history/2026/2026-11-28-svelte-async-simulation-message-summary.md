# Svelte async adoption and simulation-message summary

Enabled Svelte 5.56 experimental async in the [web configuration](../../../web/svelte.config.js). Reviewed the scheduler change before converting a query. The application source has no `$effect.pre` or `beforeUpdate` call sites. Bits UI uses pre-effects internally, so the browser checks covered the download menu and candidate picker as well as workspace behavior.

The [simulation-message summary](../../../web/src/lib/results/SimulationMessages.svelte) now awaits one worker response inside a local boundary. Its initial placeholder gives way to the filter form and summary. During refreshes the form stays mounted, and a failed read replaces the loading message with the worker error. Svelte commits the response with its filters and page offset, and a late reply cannot replace a newer page. The remaining queries are tracked in [TODO](../../../TODO.md).

## Verification

- `npm run check` passed with zero errors and warnings. The static build completed for both focused browser runs.
- Seven flag-only Chromium tests passed across `app-status.spec.ts`, `regional-er.spec.ts`, and `workspace-uniformity.spec.ts`.
- Five Chromium tests passed after the summary conversion, covering the editor and Bits UI controls, archived diagnostics and simulation inspection, summary pagination and failure recovery, and election probabilities. The [summary regression test](../../../web/tests/e2e/reform-messages.spec.ts) held an older request until the newer page appeared, then released it and verified that the newer page remained visible.
- Verification was limited to the focused checks above.
