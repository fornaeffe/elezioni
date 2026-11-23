## M02 resource investigation after CP-07 (2026-11-14)

Investigated Luca's Chrome 157.0.8103.51 tab crash around simulation 250 of a
1,000-run reform/M02 request with automatic workers. The
[scoped report](../../common-politics-scenario/resource-investigation-2026-11-14.md)
contains attribution, source entry points, limitations and proposed fixes; its
linked compact JSON receipt preserves short-run measurements.

Found redundant historical compilation during common compute-worker startup,
retained numeric parents alongside the generator's cloned arrays, and an
automatic worker policy inherited from older calibration. The HTTP common
packet also carries decoded source columns, not just compressed disk bytes.
The bounded ordered pool still retains at most one pending simulation per
worker; no growing per-simulation queue was found. Rewriting the large portable
checkpoint with every result manifest is shared with the retained pre-CP path,
so it is an optimization opportunity rather than a new CP-specific leak.

Sequential 60-run probes completed: retained fast execution services with ten
workers and the pinned pre-CP M02 generation source peaked at 5.36 GiB renderer
private memory in Chromium 148. With the current CP-03 compatibility wrapper,
the same services peaked at 5.71 GiB; the current production UI with automatic
ten workers peaked at 5.78 GiB in that browser. The pinned-source run uses a
diagnostic Vite load override, not a production source edit. It matters because
the CP-03 compatibility wrapper itself retains original numeric assets for run
metadata alongside the extracted generator's clones. The roughly 8% rise over
the pinned source is consistent with added ownership/allocation pressure, but
single sampled probes do not attribute the exact difference, reproduce the
former complete editor or establish a CP throughput slowdown.
Installed Chrome 157.0.8103.51 completed 60 runs with four workers,
peaking at 3.28 GiB. Separate temporary browser profiles kept user sessions
untouched. Whole-renderer private samples include nested workers and native
allocations; they are not JavaScript heap measurements or hard maxima.

The comparison harness initially needed raw compressed fixture responses:
preview serves history/state gzip with Content-Encoding, so browser fetch
returns decoded bytes. Retrying with explicit raw-file responses preserved
the existing portable-file checksum contract. Only successful probe receipts
are included in the comparison.

Focused verification from `web/`:
`npx vitest run scripts/acceptance/common-politics-baseline.test.ts src/lib/politics/execution/execution.test.ts src/lib/politics/execution/lifecycle.test.ts src/lib/workers/reform-pool.test.ts --maxWorkers=2 --reporter=dot`
passed **4 files / 28 tests** in 23.64 s, including the exact CP-01 replay and
common-versus-retained ballot/legal-outcome checks. Added the resource follow-up
under the still-open CP-08 item. No runtime/model/scrutiny change, generated
asset modification, 1,000-run acceptance, reproduction of the reported crash,
commit or publication is claimed. Proposed implementation remains separate.
