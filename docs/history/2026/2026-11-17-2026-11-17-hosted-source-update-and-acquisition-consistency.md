## 2026-11-17 — Hosted source update and acquisition consistency

- The first [hosted reassessment](https://github.com/fornaeffe/elezioni/actions/runs/43937023815)
  of `b09df0bceeca7447d4874d36ea8c7236dab44792` stopped at
  `municipalities-2021` on Node 24.22.2. This is source drift, not evidence of a
  Linux/Node incompatibility or insufficient runner resources. A live local
  request returned 3,410,103 bytes instead of the pinned 3,410,100 bytes.
  Comparison found exactly 58 changed `COD_COM_FISCALE` values, no added/removed
  municipalities, and identical fields consumed by the simulator. Unlike the
  earlier crosswalk case, this was actual field drift rather than row ordering.
- Luca chose the newer upstream response without tracking its raw bytes in
  Git. A fresh 17 November request confirmed the reviewed 3,410,103-byte response,
  SHA-256 `94564a573ee2a98585f530bfd0d546c4110aa6b19df634701b9e220cd2b8ceef`.
  Updated the source manifest's checksum, byte count and review date; the URL
  and ignored cache path remain unchanged. Removed the uncommitted repository
  snapshot proposal and its Git ignore/attribute exceptions. Future
  upstream drift must still fail rather than be accepted automatically.
- Acquisition now honors repository snapshots centrally, including specialized
  download commands. Explicit imports remain available; valid caches are reused,
  corrupt caches still fail, and missing/bad snapshots do not fall back to live
  data. Repository paths must stay within the checkout.
- Source-manifest fingerprints omit only the local `repositoryPath`, retaining
  checksums, sizes, URLs, headers and other provenance. Preparation-code
  fingerprints also remain enforced. Unlike the discarded snapshot-only fix,
  this reviewed source update intentionally changes data identities. The earlier
  trial's evidence remains under `data/derived/hosting-source-diagnosis/` and
  does not establish acceptance of the newly updated inputs.
- Regenerated and registered the matching reform seat table before model
  preparation, then regenerated production bundles, defaults and catalogs.
  This ordering is required by the existing geography/seat version guard; an
  initial bootstrap attempt stopped there, with evidence in
  `data/derived/municipality-source-update/bootstrap.log`. Updated six asset
  pins and 2,716 geography references in the authoritative common default by
  script, preserving electoral choices, seeds, dates and model versions. No
  election-law correction or implicit latest-asset substitution was introduced.
- Equivalence checks compare the prior accepted local data with the update:
  previous-law and Emilia-Romagna electoral data/default choices are identical;
  all ten common asset payloads differ only in the reviewed source provenance
  and dependent version references. Model parameters, populations, geography
  memberships, seat allocations and historical observation chunk descriptors
  are unchanged. Detailed evidence: `data/derived/municipality-source-update/`.
- The rebuilt candidate's national/paired inputs, electorate identity and
  generation implementation are unchanged. Fitted, profile and runtime binary
  columns are byte-identical. State differences are confined to provenance and
  dependency keys; their allowed paths are checked in
  `candidate-equivalence-report.json`, alongside `equivalence-report.json` and
  complete source/asset/state differences in the same evidence directory.
- The first unit pass found two historical-fixture couplings. The constructed
  generation fixture now explicitly reads its original tracked seat table,
  keeping the complete CP-01 M01 baseline replay unchanged, including its input,
  ballot and candidature hashes. No baseline JSON was rewritten. Workspace
  tests retain the retired M00 scenarios' historical pins instead of requiring
  those frozen inputs to reference today's descriptor. Current production
  assets remain covered by their separate strict packaging/release checks.
  The two affected files then passed all 10 tests; initial reports/differences
  remain in the source-update evidence directory.
- Saved user scenarios, checkpoints and results are not rewritten. Existing
  supported result archives retain their original results/provenance. Old
  scenarios still need their exact bundles; current-only publication does not
  retain those bundles. Explicitly rebased scenarios require checkpoints bound
  to the new identities. Luca accepted these compatibility consequences.
- Added a post-migration review of tracked sources/artifacts and their sizes,
  reproducibility, download/regeneration policy, mutable endpoints, retention
  and historical evidence. Existing artifact tracking policy otherwise remains.
- Windows / Node 24.20.0 validation: preparation from initially empty source
  and model-cache directories acquired all **31 required sources** and passed
  in **261.5 seconds**; `data:check` passed in **24.8 seconds**. Warm preparation
  passed in **137.7 seconds**, with all **19 receipt artifacts byte-identical**.
  A further data check after the historical-fixture correction retained the
  same receipt. Both type checks, all **1,355 unit tests** with zero skips,
  the production build and all **six staged-site browser checks** passed.
  Browser checks took **277.8 seconds**, with no skips or retries, covering
  project-base direct routes/refreshes/navigation, both national laws under
  M01/M02, Emilia-Romagna workers and structured/CSV exports. The full 168-check
  application browser campaign was not repeated for this source update.
  The regenerated legacy defaults/editor differ only in their snapshot-version
  references; `tracked-equivalence-report.json` records that comparison.
  The staged payload contains **68 files /
  112,870,832 bytes**, with **20,862,477 bytes** estimated gzip transfer; public
  CDN transfer remains unmeasured. Full logs and receipts are under
  `data/derived/municipality-source-update/`, starting with `report.json`.
- No further user logs were needed. Hosted Linux cold/warm acceptance, Luca's
  merge and public publication remain outstanding; no commit, push, merge or
  deployment was performed during this correction.
