# Politics editor tree counts

Filing and reference summaries, including their enclosing sections, show
counts only when positive. [LazySection](../../../web/src/lib/politics/editor/LazySection.svelte)
uses one smaller, lighter style for both kinds of count.

Territorial summaries include declarations or candidate pins on the node and
its descendants. The nested Candidati summary counts the pins for its own
slots. Stored pins still count when a filing becomes inactive.

`npm run check` reported zero errors and warnings. All 11
[editor browser tests](../../../web/tests/e2e/common-politics-editor.spec.ts)
passed in Chromium with controlled preparation. They verified that totals
update through the tree and disappear after the last declaration or pin is
removed.
