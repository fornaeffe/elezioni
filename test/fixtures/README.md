# Regression evidence

These JSON fixtures and benchmark receipts are test/reference evidence, separate
from production preparation and browser assets. Some were exported from the
retired R implementation; their origin is part of their provenance, not an R
runtime dependency.

- `r-reference-manifest.json` pins compressed and uncompressed hashes for the
  frozen politics/regional outputs and Camera simulation-584 failure input.
- `reform-legal-inputs-v1.manifest.json` pins the reform legal replay cases.
- `benchmarks/` records past measurements; reports are not fresh acceptance.

Tests load tracked compressed fixtures directly. Do not regenerate expectations
to conceal a failure. A changed legal expectation requires an explicit reviewed
decision and versioned evidence. The active checkout has no R exporters.

For historical source inspection or the deferred municipal port, use revision
`9bcbe403342c2570c83f547f7ae70adddec9961f` in an isolated checkout. See the
[retirement record](../../docs/migration-history.md#2026-11-02--r-and-temporary-bridge-retirement)
for scope and verification.
