# Confirm filing changes

Filing checkboxes open a native confirmation dialog for additions and
removals at every scope. The dialog names the list and scope and shows how
many constituencies would change. Clicking **Annulla** or pressing Escape
discards the pending edit and returns focus to its checkbox.
[Filing tree](../../../web/src/lib/politics/editor/FilingTree.svelte)

`npm run check` reported zero errors and warnings. All 14 national editor
Chromium tests passed with controlled model
preparation. The filing tests verify cancellation for checked and mixed
states, keyboard focus, confirmed additions and removals, and preservation of
coalition-excluded filings and candidate pins.
[Editor tests](../../../web/tests/e2e/common-politics-editor.spec.ts)
