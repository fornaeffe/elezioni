# Coalition and list removal in the politics editor

The [shared native dialog](../../../web/src/lib/politics/editor/SubjectRemovalDialog.svelte)
names the coalition, list, or Altre liste and describes what the edit removes.
The advanced options use a trash icon and the shared danger button style.
The dialog starts on `Annulla`, restores focus after cancellation, and moves it
to the next relevant control after confirmation. Failed edits are reported in
the dialog.

The old [deletion edit](../../../web/src/lib/politics/scenario/edits.ts) removed
the list and its membership while retaining the owned fallback and its
references. M02 then received a reference whose component was absent from its
generation inputs. List deletion now removes that fallback and its references
in the same edit. Imported source mappings, polls and M01 declarations that use
the fallback block the edit and identify the dependency. Coalition deletion
makes its member lists autonomous. Recovery for previously orphaned fallbacks
and references is a separate action tracked in [TODO](../../../TODO.md).

## Verification

- `npm run check` passed with zero errors and warnings. The static build used
  by Playwright succeeded.
- The scenario and execution suites passed 63 tests. They cover targeted
  cleanup, preservation of an older orphan, blocking an imported dependency,
  and vote generation after deletion with the constructed M02 fixture.
- Four focused Chromium editor tests passed. They covered cancellation with
  Escape and `Annulla`, focus after deletion, list disclosure at 390 px,
  clearing a deleted component selection, and Altre liste deactivation.
  Browser runs used the controlled editor preparation fixture. The production
  Altre liste test selectors were updated for the new dialog.
