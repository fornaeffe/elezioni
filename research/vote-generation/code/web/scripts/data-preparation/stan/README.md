# Vote-generation research entry points

The Python environment and binaries are external caches, not repository inputs.
Do not install a research fit into production defaults from these scripts.

## Current fast candidate (27 October 2026)

Read only [the fast research report](../../../../docs/vote-generation-fast-research.md)
for its model, approximations, source boundaries and measured validation.
The fast path needs Python/NumPy/SciPy and `g++` (C++20), **not CmdStan**.

- Prepare: from `web/`, `npm run data:forecast:share:prepare -- development all 128`.
- Mathematical gates: `check_fast_fixtures.py --cache <cache> --output <derived-summary>`.
- Functional gates: `python -m unittest test_fast_model.py` from this directory.
- One case: `run_fast.py <case-directory> --cache <cache>`.
- Study: `study_fast.py <index> --cache <cache> --output <derived-dir>`.
- Frozen refinement/confirmation: add `--selected kind-common-conservative --draws 512`.
- Complete fitting/generation benchmark: `benchmark_fast.py <case> --cache <cache> --draws 512`.
- Portable research bundle: `export_fast_bundle.py <accepted-case> --output <derived.json.gz>`.

Set `OPENBLAS_NUM_THREADS=1` and `OMP_NUM_THREADS=1` before starting Python.
Prepared manifests pin source files: re-export after algorithm edits. `main`
component scope reproduces the admissible core panel. Appending `all` requests
the wider catalog, but is rejected when future-only residual support lacks
cutoff evidence; such a scenario requires reference information. A profile
limit of `10000` includes the complete compatible future municipal domain.

## Earlier or rejected candidates

Read [the restricted report](../../../../docs/vote-generation-party-drift-restricted.md)
only when reproducing the 26 October model. It requires CmdStan 2.37.0 and
`requirements.txt`. The `restricted` model is a successful small reference;
`real` is the interrupted larger pilot. `collapsed` preserves that probability
model while changing coordinates. `modular-party-drift.stan` is a national-only
Dirichlet-multinomial experiment; it is **not** the fast candidate's likelihood.
No timeout/failed run is a valid posterior or forecast bundle.
The real-case preparation CLI retains the `original` default; its fourth
argument selects `collapsed` or the fixed-scale profiling variant `fixed`.
