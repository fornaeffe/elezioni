## 2026-10-21: correct N22 stage failure reporting

Luca approved the bounded N22 reporting correction. `scrutiny.ts` previously
passed `alternative-allocation-path` regardless of whether N22 was inactive or
its selected cap allocation had failed. The stage recorder retained that fixed
reason on failure, and presentation/export correctly propagated the wrong text.

N22 now records `not-run / regional-allocation-non-computable` when regional
allocation fails before path selection, and reserves `not-run /
alternative-allocation-path` for a completed regional allocation using another
path. On the selected cap path it records the actual college status, failure
reason and available affected region. Successful N22 records have no reason.
The existing N21 aggregate stage and all allocation/recovery algorithms remain
unchanged; this adds no legal interpretation or gap-register entry.

Verification: extended existing composition tests cover no bonus, bonus without
cap correction, successful cap correction and failed regional prerequisites.
The digest-verified production regression for simulation **242**, seed
`r26-default-v1`, confirms the active N22 minority-list correction still fails
with `no-eligible-recipient-remainder`, now reporting affected region **08**.
Its stage table and error message carry the correct reason, and JSON result
export/import preserves both the reason and region. Historical benchmark
artifacts were not rewritten; no new 1,000-run benchmark is claimed.

`npm run check`: zero errors/warnings. `npm test`: **61 files / 885 passed,
no skips**. `npm run build`: static build passed; plugin-timing notices were
informational. No commit, deployment or publication was performed.
