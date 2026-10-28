# Vote-generation research evidence index

FV-01 preserved evidence on **28 October 2026**. Research code, reports and
original artifacts remain at their existing locations; full archival/deletion
is FV-09. Nothing in this index selects a production bundle or authorizes cleanup.

| Task | Read only |
| --- | --- |
| Implement the selected numerical model | [Maintained equations](../../docs/vote-generation-fast-model.md) and [validation protocol](../../docs/vote-generation-fast-validation.md) |
| Implement scenarios/references/fit reuse | [Data/scenario contract](../../docs/vote-generation-fast-contracts.md) and [reference contract](../../docs/vote-generation-fast-references.md) |
| Audit retained evidence/provenance | [Evidence manifest](fv01/evidence-manifest.json); follow only the relevant `retainedPath` |
| Inspect the required default/physical domain | [Default-domain inventory](fv01/default-domain.json); proposed memberships are provisional, special ballot groups require separate review |
| Prepare FV-09 cleanup | [Archive plan](../../docs/vote-generation-research-archive-plan.md), then [exact file inventory](fv01/inventory.jsonl); do not read the entire inventory for numerical work |
| Verify this snapshot | Command below; no original ignored artifacts, network, Python, Stan or fit required |

From the repository root, with the project's Node runtime:

```powershell
node research/vote-generation/fv01/freeze.mjs --check
```

This checks every retained hash/size, inventory integrity, both chambers' complete
physical coverage against the frozen target, worked reference arithmetic and
independent reduced matrix/mode conditions, plus exact unchanged numerical-model
extraction. It does **not** run the future engine, a new posterior, quadrature,
joint-reference calibration, browser or predictive acceptance.

With the original ignored artifacts still present, `--check-sources` additionally
rehashes every inventoried original and verifies that each compact JSON extract
differs only at its explicitly omitted paths. This stronger local check is not a
requirement for verifying retained evidence in a clean clone.

## Evidence groups and status

| Candidate | Preserved evidence and interpretation |
| --- | --- |
| Poll calibration/uncertainty, endpoint and six-category | Reports/policies/case metrics under retained `polls/` and `generation/`; earlier comparisons only, not the selected production model |
| Fixed-outcome drift | Final/supplemental summaries and reduced posterior check under retained `drift/`; preserve original approximation/confirmation limitations |
| Party exact/Laplace/importance | Stage-0 and restricted summaries, independent disagreement and importance-geography failure; a converged mode was not posterior accuracy |
| Reduced/real Stan | Original summary/error records, convergence/ESS/timing/stop context; failed/interrupted runs are not valid posteriors |
| Fast screening/selection | Initial undercoverage, four-variant development and frozen selection; retain pre-repair code hashes rather than silently rewriting history |
| Fast corrected replay | Corrected development/confirmation summaries and cases; loader/optimizer defects had been seen, so these are not untouched holdouts |
| Fast full compatible domain | Complete-case manifest, fit diagnostics, compact benchmark, export checks; 7,821 future municipalities excludes VdA/unmatchable research rows and is not the full production domain |

The manifest retains original paths, bytes, SHA-256 and repository revision,
plus retained hashes and **exact omitted JSON paths** for extracts. Large
municipal scoring rows are omitted from the compact benchmark/early case
extracts; aggregate metrics, diagnostic/policy/source/seed fields and group-level
bias evidence remain. Exact copies retain identical hashes. Original source/code
hashes may differ between failed and corrected runs; both are kept as recorded.
Do not recompute old fingerprints after a file move and call them original pins.
Local Git attributes disable line-ending conversion for the frozen JSON/JSONL
bytes, so Windows checkout settings cannot invalidate their SHA-256 identities.

`inventory.jsonl` has one record per researched file with its hash/size, proposed
disposition, retained evidence and direct textual import consumers. Dynamic/file
path consumers still require FV-09 review. All entries say
`deletionAuthorized: false`. Unknown/manual analysis files remain review items.
The inventory additionally records research-owned Python bytecode and the mixed
poll output directory. Poll scenarios/reconciliation/observations remain explicit
keep/review items; only known estimator outputs are research candidates. No
shared source cache, current production asset or unrelated legal/result archive
is authorized for deletion.

## Reproduction boundary

Historical commands and precise numerical qualifications remain in the indexed
reports. The evidence manifest records the capture's base HEAD, modified tracked
sources and exact working-tree hashes; it does not claim a new commit. Historical
inputs/algorithm hashes within each retained result control its old replay. The
one-time `freeze.mjs --write` capture requires the original local ignored
artifacts and refuses to overwrite differing frozen output. It preserves existing
evidence only; it is not a model-reproduction or production preparation command.

The maintained production protocol and
[worked contract/numerical targets](fv01/contract-fixtures.json) are frozen before
the new engine. Proposed component coverage is not a reviewed historical
decomposition, a fitted Senate model or a generated default. FV-03/04 must resolve
those release requirements without manufacturing historical counts.
