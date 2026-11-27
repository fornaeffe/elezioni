# Province names in the reference tree

The [reference tree](../../../web/src/lib/politics/editor/ReferenceTree.svelte)
uses province names from the existing generated municipality catalog. The
[editor asset loader](../../../web/src/lib/politics/editor/assets.ts) uses the
same label for the disclosure and its reference fields, so province `037`
appears as `Provincia: Bologna` and is still stored as `037`. A province code
absent from the catalog retains its numeric label.

The catalog provided a single name for each of the 107 province codes in the
installed politics geography bundles. `npm run check` reported zero errors
and warnings. The focused [Chromium test](../../../web/tests/e2e/common-politics-editor.spec.ts)
passed on the static build and checked the Varese, Como and Bologna labels,
Bologna's saved province code, and reference editing and removal.
