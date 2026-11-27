# Move filing checkboxes into disclosure headers

List filing checkboxes sit between the disclosure arrow and title and stay
visible when their section is closed. The three native checkbox states use
light red, yellow, and green backgrounds. Filing descriptions appear as
tooltips, and the checkbox and disclosure can be operated independently.
[Filing tree](../../../web/src/lib/politics/editor/FilingTree.svelte)

The **Depositi e candidati** checkbox reflects and edits every editable
filing location for the selected law, including chambers marked
*rinnovo inattivo*. It preserves filings in chambers excluded by the
coalition and keeps candidate pins.

`npm run check` reported zero errors and warnings. All 15 focused Chromium
tests passed across the national editor and Emilia-Romagna other-list specs.
The national editor tests used controlled model preparation and checked header
order, keyboard operation, filing colors, and preservation of saved data.
[Editor tests](../../../web/tests/e2e/common-politics-editor.spec.ts)
