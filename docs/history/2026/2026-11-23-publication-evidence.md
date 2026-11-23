# GitHub Pages publication evidence

The [publication workflow](https://github.com/fornaeffe/elezioni/actions/runs/44388444197)
completed successfully on 18 November 2026 for main revision
`f6e634e14e6be1f08a3e5a75c6adb09cc4e9498d`. Its reassess, deploy and
public-smoke jobs succeeded; development was skipped on this manual dispatch.
The public URL is https://fornaeffe.github.io/elezioni/. On 23 November an
independent request to its `release.json` returned that revision and `/elezioni`
base. This request checked the release identity, not all current site behavior.

The cold and warm hosted reports both passed on a clean checkout under Linux,
Node 24.22.2 and four CPUs. Cold took 2,007.6 seconds and warm 180.4 seconds,
excluding installation and infrastructure setup. Minimum sampled available RAM
was respectively 12.23 and 12.33 GB, and minimum sampled free disk 87.78 and
88.58 GB; these are host-wide samples, not process peaks. Warm reused cold
acceptance on the same runner and matched all 19 preparation artifacts and the
staged payload manifest. The site was 112,870,849 bytes; its gzip estimate was
20,862,492 bytes, not a measured CDN transfer.

Cold release acceptance passed 1,385 unit checks, two archive workflows, 70
browser cases, one editor cancellation and one root route/worker smoke, with no
skips; all six staged-site checks passed. Public Chromium smoke passed all six
checks without retries or skips: direct routes, refresh, navigation and revision;
four national law/model worker and export combinations; and the regional worker
and structured/CSV exports. The five measured cold-browser workflows transferred
2,053,625 and 4,903,056 encoded bytes for parity M01/M02, 2,054,001 and
4,903,311 for reform M01/M02, and 451,272 for regional. The synthetic route
matrix did not measure transfer bytes. These measurements are not traffic or
availability guarantees.

The release receipt identifies these data versions:

- Politics snapshot: `politics:3c56d00fc1acbbb2891fd553da2b0cb627f2ca988c507dafd976a7511c476445` (schema 4).
- Emilia-Romagna snapshot: `regional-er:2dfeca47bd14dbd920149e06ba74e586c5b46b2dc8f75ac318e45dcf77c091ef` (schema 1).
- Reform generation: `r26-generation-v1-8ee56e457e29`; models: `r26-models-v1-f71a72020aaa`; defaults: `r26-defaults-v2-fc8ed36693c4`; editor: `r26-editor-v1-1cf79de0a077`.
- Catalog: `fast-share-catalog-v1-7cc768acc5cf8db28393edd430ffe4cadafcc52ef798cc2baf10459187cc4dce`; authored scenario SHA-256: `f6e9738cf1c609df7a65104867704ce695a79d26138e85609e657d35f99fba9d`.

Evidence was read from the downloaded `hosting-summary-f6e634e14e6be1f08a3e5a75c6adb09cc4e9498d`
cold/warm reports and `public-site-summary-f6e634e14e6be1f08a3e5a75c6adb09cc4e9498d`
browser and transfer receipts under the ignored `data/github-artifacts/` directory.
GitHub summary artifacts have a 30-day retention; the complete one-day evidence
artifacts and their logs were not supplied here. The workflow record and the
downloaded summaries support these outcomes, but the full logs were not reviewed.
