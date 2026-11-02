# M01/M02 1.0.0 historical Camera kernels

Used only by `../../current-camera-models.ts` and its adapter regression tests.
These files preserve the corrected historical calculation after the active
models restore the pre-`4ee5a91` laws as 2.0.0. They are not selectable in the app.

Sources are from `8dcb830`; imports are rebased and the old legacy preparation
interface is extracted into `legacy-types.ts`. `snapshot.json` pins these local
files. Unchanged RNG/linear-algebra helpers remain shared. Reproduce the original
full report, browser campaign and exact source hashes by checking out `8dcb830`;
current campaign/curation entry points reject 1.0.0. Frozen benchmark receipts
retain their original bindings and are never rewritten as 2.0.0 results.
