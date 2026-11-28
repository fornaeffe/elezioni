# Svelte async result queries

Completed the result-query group after the [simulation-message summary pilot](2026-11-28-svelte-async-simulation-message-summary.md).

- [General messages](../../../web/src/lib/results/GeneralMessages.svelte) derive the in-memory page for results without storage. For stored results, the displayed rows and offset come from the same response. The previous page stays visible during loading, and the query boundary can retry a failed page.
- [Affected simulation IDs and original messages](../../../web/src/lib/results/AffectedSimulations.svelte) use independent async boundaries. Exact membership still governs ID queries and CSV export. Legacy archives still show saved message text, and failures appear beside the query that failed.
- [Election probabilities](../../../web/src/lib/politics/reform-2026/ElectionProbabilityTable.svelte) use `{#await}` to show loading feedback on selection changes. Loading still starts when the disclosure opens, and the table preserves the unavailable-data state for older archives.

`npm run check` passed with zero errors and warnings. All four focused Chromium tests passed across `reform-messages.spec.ts` and `reform-statistics.spec.ts`. The diagnostics regression held a general-page request to verify that the prior page range stayed visible during loading and that retry loaded the intended page. It also paged IDs while context was pending, then verified that a context failure stayed local and a new selection recovered. The other checks covered exact and legacy archives, navigation, CSV export, and probabilities across selection and metric changes.
