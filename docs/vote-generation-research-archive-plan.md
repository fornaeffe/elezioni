# FV-09 archive and cleanup

Executed on **2 November 2026**, after selection of **M01 2.0.0** and
**M02 2.0.0**. The [archive index](../research/vote-generation/README.md)
is the entry point for historical reports, source snapshots and reproduction.
The original proposal is retained as
`research/vote-generation/evidence/docs/vote-generation-research-archive-plan.md.gz`.

| Result | Exact record |
| --- | --- |
| 151 source/report/protocol files relocated | [Dispositions, original/relocated hashes and replay bindings](../research/vote-generation/fv09/manifest.json) |
| 1,260 generated files deleted; 4,301,573,109 bytes reclaimed | [Execution receipt](../research/vote-generation/fv09/execution.json) |
| FV-01 evidence plus 59 later compressed reports retained | [Archive verification and evidence map](../research/vote-generation/README.md#small-offline-verification) |
| Verification, rationale and limitations | [Migration history](migration-history.md#fv-09-research-archive-and-cleanup--2-november-2026) |

`docs/model-drafts/` is protected in place. Authoritative source observations,
production bundles/defaults, shared scrutiny and R workflows, legacy `dati/`,
legal investigations and people's result archives are excluded. Unknown/new/manual
scratch files are retained; no whole research directory was recursively deleted.

The production `fast-share-*` preparation closure stays active. The two
`data/legacy-comparison-*.json` policies remain because unified-default preparation
consumes them. Poll source acquisition, result pins, calibration/reference code
and reviewed historical observations remain for their active consumers. Historical
estimation findings are in the archive; active acquisition instructions are in
[poll calibration](polls-calibration.md).

Archived CLI roots/imports are historical and require the original-layout pinned
checkout. Normal preparation/tests/builds do not import archive code. Optional
archive checks are separate from required production checks. Future generated
research may use ignored `data/derived/vote-generation-research/`; it must never
become a production input or a broad deletion target.
