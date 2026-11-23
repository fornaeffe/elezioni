## Common execution resource fixes (2026-11-14)

Implemented Luca's approved follow-up to the
[M02 resource investigation](../../common-politics-scenario/resource-investigation-2026-11-14.md).
The [execution guide](../../common-politics-scenario/execution.md#workers-and-workspace-integration)
owns the compute contract; the [lifecycle guide](../../common-politics-scenario/lifecycle.md#sessions-and-bounded-resources)
owns resource estimates and checkpoint persistence.

- The coordinator validates pinned assets, compiles observations and prepares
  M01 references or M02 fits/profiles. Workers receive only prepared inputs,
  with no catalogue/JSON bytes, source-column buffers or fitting cache. Exact
  build/graph SHA binding covers typed columns and ordered Maps without numeric
  JSON expansion. M02 workers independently check consumed-input parents and
  the generation key; preparation identity v2 hashes the paired-input parent
  instead of retransmitting observations just to reproduce an identity.
- Worker-owned and coordinator-owned immutable M02 arrays no longer incur an
  extra defensive numeric clone. Other generator callers keep defensive copying
  by default. The retained compatibility wrapper precomputes provenance so its
  closure releases original numeric assets. The probability law, RNG, seeds,
  scrutiny logic and model versions remain unchanged.
- Automatic workers are capped at four and may be lower for resource hints or
  large inputs. Planning accounts for initialization, pending results, GC and
  retained coordinator caches, using 768 MiB coordinator scratch and 512/640/
  1536 MiB worker scratch for M02/M01/Rosatellum. Explicit CPU-based choices,
  warnings and actual-worker provenance remain available.
- The result database's forward version-3 upgrade moves old inline common
  checkpoints and retained reform fit files into an immutable per-session store
  atomically. New sessions store the payload once with the first manifest, then
  release it from the live record. Ordinary queries and commits read/write lean
  manifests; explicit archive/checkpoint access hydrates the payload. Deletion
  covers both stores, and synchronous quota errors abort queued writes as well.
  Archive/file schemas and immutable executed identities remain unchanged.

Verification from `web/`:

- `npm test -- --maxWorkers=4 --reporter=dot`: **116 files / 1,319 tests passed**,
  31.04 s, including exact CP-01 replay and all retained legal/checksum evidence.
  New checks cover owned numeric transfer, read-only generation, all three pairs,
  numeric/parent tampering, Map/buffer hashing and conservative automatic sizing.
- `npm run check`: zero errors/warnings; `npm run check:data` passed. Static
  builds passed at `/` and `/elezioni`.
- The common lifecycle, compute, files and result-store browser suites passed
  **12/12 at `/`**. The focused nested matrix including shared disk statistics
  and regional ER execution passed **14/14 at `/elezioni`**. These include real
  forward migration, zero payload rewrites across commits, attachment quota
  atomicity, immutability, scoped cleanup, JSON.gz roundtrips, reload, cancellation
  and one/two-worker outcome/row equivalence.
- An initial Rosatellum check exposed an ambiguous import-error alert locator
  once the stricter explicit-worker memory warning also rendered; the assertion
  now targets the actual import-error alert. A broader statistics command also
  ran seven stale UI cases: their unchanged helper still seeds the retired
  `elezioni:politics-reform-2026:fast-results:v1` key ignored by CP-07. Those cases
  fail looking for old charts and remain CP-08 retargeting work; the shared disk
  statistics/storage/archive case passed. They were not converted or hidden by
  these resource changes.

Added `npm run benchmark:common-politics` for explicit sequential national
Chrome resource campaigns (1–1,000 simulations), with required-production-data
checks, renderer-wide Windows memory sampling and progress/final receipts under
`data/derived/common-politics-resources/`. This does not run as part of routine
functional tests. The full CP-08 matrix, other national pairs and publication
remain separate; no commit, deployment or manual user acceptance is claimed.

The reform/M02 national resource campaign completed with the installed
**Chrome 157.0.8103.51**, both chambers, **1,000/1,000 complete legal outcomes**,
no partial/failed membership and no page errors or renderer crash. Automatic
selection used **four compute workers** on the reported 28 processors / 32 GiB
device-memory hint, retaining the authored fitting and simulation seeds. Total
browser fitting-plus-execution time was **960.40 s (16.01 min)**, including
29.55 s preparation; the stored run measured 927.49 s. This passes the one-hour
target for this pair on this machine.

Sampled renderer private memory peaked at **3.3504 GiB** (working set
**3.3115 GiB**) and repeatedly returned to roughly 2.6–2.9 GiB well beyond the
reported simulation-250 failure point. These are sampled whole-renderer process
metrics including nested workers/native allocations, not JS heap or guaranteed
hard maxima. The final **1,170,550-byte manifest** contains no inline checkpoint;
exactly **one** immutable checkpoint record is stored. The executed common code
SHA-256 is `f5befd5cb72912b1bcbd4074c9aa1e281e46efb8b35ca220cb751a62d4bc1504`.

The full local receipt is
`data/derived/common-politics-resources/M02-politiche-reform-2026-v1-1000-auto.json`
(SHA-256 `5f338a267e394824bf05d43851b3f8b75156806fa1ba0be62233ef3410077c3e`),
with progress samples, exact execution dependencies/seeds, outcome membership,
storage descriptors and actual-worker provenance. `npm run benchmark:common-politics`
passed **1/1** with retries disabled. Root static output was rebuilt for the
campaign, and final Svelte/TypeScript checks plus `git diff --check` passed.
Marked only the resource-hardening follow-up complete. CP-08's full release
matrix and the Rosatellum/M01 and reform/M01 national campaigns remain open.
