# Non-valid reference editor grouping

The scenario's non-valid reference editor now places the national common quota
in the same responsive table as the Camera and Senato quotas, inside the
collapsible section titled "Astensione e voti non validi". The group remains
collapsed by default. When national references differ by chamber, its badge is
shown inside the disclosure.

`ReferenceFields` enables the national common table cell only for a null target,
which represents non-valid references. List and component callers keep their
existing common quota field and advanced-table behavior. Updated the existing
common-politics editor selectors and assertion for the relocated field.

`npm run check` passed with zero Svelte or TypeScript diagnostics, and
`git diff --check` found no whitespace errors. The Playwright suite was not run.
