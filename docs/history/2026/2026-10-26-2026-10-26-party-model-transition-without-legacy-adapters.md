## 2026-10-26 — Party-model transition without legacy adapters

Removed the requirement to preserve old scenarios/results through adapters from
`docs/vote-generation-drift-model.md`, as approved by Luca. The app is in early
development and existing artifacts are tests; the new party-level contract may
break compatibility, with old tests replayed in the old app version if needed.
Versioning and fitting fingerprints remain required for the new contract.

Verification: checked the focused documentation diff and whitespace; model math
is unchanged. No runtime code or data changed. The active plan has no party-model
legacy-adapter requirement to remove; its pre-existing edits were preserved.
