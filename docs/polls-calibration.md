# Historical poll sources and calibration commands

The reviewed poll observations, election manifest, compact official targets and
result pins remain under `data/sources/polls/history/`. Production preparation and
opt-in legacy reference scenarios still consume these records. Curated sources
are retained; optional raw government downloads remain ignored caches.

From `web/`:

```powershell
npm run data:polls:calibration:check
npm run data:polls:calibration:generate
npm run data:polls:calibration:download
npm run data:polls:calibration:verify-results
```

`check` validates the reviewed JSON and evaluates the historical audit offline.
`generate` writes its immutable report under `data/derived/polls/`; reference
preparation still embeds its rejection reasons. These estimator modules remain
active because `poll-references.ts` consumes them. The rejected corrections are
not newly accepted by FV-09.

`download` acquires hash-pinned official election archives; `verify-results`
regenerates targets and requires byte equality with the tracked table. A changed
remote archive fails. `freeze-results` is the explicit maintainer command for a
reviewed manifest change: it rewrites only `results.json` and `results-pin.json`.
Review that diff and run `verify-results` and `check`. Normal checks/generation do
not download or rewrite tracked sources.

Import only named scrutiny/list members; candidate-preference files repeat list
totals. Camera 2022 candidate-only attribution uses the original uninominal college.
The retained table records national denominators, source/member hashes and the
comparable panel. The historical archive retains its enforced 1 MiB budget.

For original selection dates, exclusions, estimator equations, backtests and
acceptance limits, read the [archived calibration report](../research/vote-generation/docs/polls-calibration.md).
For current acquisition/reference scenarios, read [poll preparation](polls-preparation.md).
