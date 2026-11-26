# Prune empty politics filing branches

The filing tree now shows locations with filing or candidate controls and
the ancestors needed to reach them. Reform coalition editors show bonus
candidate sections without the empty college disclosures. Rosatellum coalition
editors keep the paths through plurinominal colleges to uninominal candidates.
[FilingTree](../../../web/src/lib/politics/editor/FilingTree.svelte)

Bulk checkboxes use every eligible filing location under their branch, and
candidate counts use the full underlying tree. Disabled filing checkboxes
and candidate creation in sections with zero slots count as controls.
Territories with zero reference declarations still offer inputs to add one.
[ReferenceTree](../../../web/src/lib/politics/editor/ReferenceTree.svelte)

`npm run check` reported zero errors or warnings. All 12 editor tests passed
in Chromium with controlled preparation, including the new coalition tree
regression for both laws and the existing filing, candidate and territorial
reference tests. [Editor tests](../../../web/tests/e2e/common-politics-editor.spec.ts)
