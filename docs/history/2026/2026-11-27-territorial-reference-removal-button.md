# Territorial reference removal button

The [reference tree](../../../web/src/lib/politics/editor/ReferenceTree.svelte)
now uses the shared button styling and a leading trash icon for territorial
reference removal. Both labels spell out whether the action includes descendants
and wrap in narrow panels.

`npm run check` passed with zero errors and warnings. The focused
[Chromium regression](../../../web/tests/e2e/common-politics-editor.spec.ts)
verified the visible border, decorative icon, label wrapping at 320 px and
removal of a descendant reference using the editor-only preparation fixture.
