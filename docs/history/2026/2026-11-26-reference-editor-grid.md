# Responsive reference editor grid

The [reference fields](../../../web/src/lib/politics/editor/ReferenceFields.svelte)
align the common, Camera and Senato inputs by channel in each expanded
territorial node and in the advanced national section. Channels become rows
when their own grid is narrow. The basic national quota stays in its existing
field. The [territorial tree](../../../web/src/lib/politics/editor/ReferenceTree.svelte)
retains lazy disclosures, declaration counts, search and paging.

Shared toggletips explain channel scope, the quota denominator and the meaning
of an empty or zero quota. M02 shows SD for declared quotas, with default or
explicit status and a reset action beside the input. M01 hides the SD controls.
Channel removal stays beside its fields and subtree removal follows the grid.
Both use the existing scenario edit operations.

`npm run check` passed with zero errors and warnings. Four focused Chromium
tests passed on the static build. The
[editor regression](../../../web/tests/e2e/common-politics-editor.spec.ts)
checked desktop and 320px layouts, toggletips, explicit zero, SD reset, model
switching, and municipality and subtree edits. Browser tests used the
editor-only preparation fixture.
