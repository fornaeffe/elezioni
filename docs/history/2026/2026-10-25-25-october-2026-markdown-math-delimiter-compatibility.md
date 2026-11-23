## 25 October 2026 - Markdown math delimiter compatibility

Replaced LaTeX inline and display delimiters in
`docs/vote-generation-drift-model.md` with single-dollar inline delimiters and
double-dollar display blocks for GitHub and VS Code native Markdown previews.
Scanned all files under `docs/`; no other documentation used the same delimiters.
Verified all 41 inline formulas and 9 display formulas retain their exact
contents, including whitespace; only their delimiters changed. No mathematical
or model semantics changed. Rendering in the two preview applications was not
manually exercised.
