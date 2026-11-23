## 2026-10-28 — Confirmed reset of the reform scenario

Added **Ripristina scenario predefinito** beside the reform scenario download.
A native confirmation explains that current scenario edits will be lost and
calculated results retained. Acceptance reloads the catalog-selected bundled
defaults and remounts the editor to discard pending structural changes, JSON
drafts and previews. Cancellation leaves both the scenario and editor intact.
Reset is disabled during initialization, execution, import/export and preview
checks; simulation count and compute-worker preferences remain separate.

Initialization now preserves an existing locally saved draft when restoring
result tables. Otherwise a reload after reset would silently restore the old
executed scenario. Result scenarios, exports and explicit result restoration
retain their existing behavior. Updated the editor guide and migration checklist.

Verification: `npm run check` reports zero errors/warnings; the official Svelte
autofixer reports no issues (its suggestions concern existing asynchronous table
effects). Production build and all 12 editor/workspace Playwright tests pass,
without skips or retries. Coverage includes confirmation/cancellation, complete
default restoration, discarded JSON drafts, busy-state protection, persistence
across reload with calculated results, and restoration of the original executed
scenario. `git diff --check` passes. Existing build chunk-size/plugin-timing
warnings remain non-blocking. No generated data or scrutiny behavior changed.
