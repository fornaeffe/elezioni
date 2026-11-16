# GitHub Pages hosting gate

Use this guide only for hosting reassessment and publication. The
[preparation guide](data-preparation.md) owns source regeneration; the
[acceptance guide](acceptance-reporting.md) owns application report inspection.
The final migration task remains open until hosted and public checks pass.

## Decision and current limits

Retain **GitHub Actions plus GitHub Pages**, using the public
`fornaeffe/elezioni` repository and `/elezioni` project base. Repository metadata
checked on **16 November 2026** reports `visibility=public`, `default_branch=main`
and `has_pages=false`. No paid service or custom domain is required.

| Constraint | Current documented allowance | Implementation |
| --- | --- | --- |
| Eligibility | Public repositories on GitHub Free | Public static simulator; no server service |
| Standard public Linux runner | 4 CPUs, 16 GB RAM, 14 GB SSD | `ubuntu-24.04`, Node 24, two acceptance test workers |
| Hosted job | 6 hours maximum | Reassessment ceiling 330 minutes for both full passes |
| Published site | 1 GB maximum | Stage fails at 1,000,000,000 bytes |
| Pages deployment | 10 minutes maximum | Separate 10-minute deployment job |
| Transfer | 100 GB/month soft limit | Per-workflow browser response-byte receipts; monitor real traffic |
| Build frequency | 10/hour soft limit does not apply to custom Actions workflows | One serialized run per branch |
| Artifact storage | GitHub Free includes 500 MB, shared with Packages | Large site/evidence artifacts retained one day; small receipts 30 days |
| Actions cache | 10 GB included per repository; default unused-cache eviction after 7 days | No persistent Actions cache; cold and warm passes use the same ephemeral runner |

Sources: [Pages limits](https://docs.github.com/en/pages/getting-started-with-github-pages/github-pages-limits),
[runner specifications](https://docs.github.com/en/actions/reference/runners/github-hosted-runners),
[Actions limits](https://docs.github.com/en/actions/reference/limits),
[billing and storage](https://docs.github.com/en/billing/concepts/product-billing/github-actions),
[cache eviction](https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching#usage-limits-and-eviction-policy).
Recheck these before a materially later release. Account-wide storage usage is
not established by repository visibility; one-day retention bounds lifetime,
not the aggregate size of concurrent artifacts.

## Reassessment workflow

[hosting.yml](../.github/workflows/hosting.yml) runs on pushes to `migrazione`
and `main`, or manual dispatch. Pushes **never deploy**. Each fresh Linux runner:

1. Installs Node 24 and dependencies using an initially empty npm cache.
   Installs Chromium and Firefox for the existing acceptance matrix.
2. Runs `npm run hosting:reassess -- cold`: acquires checksum-pinned originals,
   prepares all production data without pre-existing generated/model caches,
   then runs the complete application gate. That gate includes `data:check`,
   both type checks, every unit test and both root/project-base browser matrices,
   requiring all tests on their first attempt, without skips.
   General and dedicated browser suites use the release static server's
   directory redirects; Vite preview does not reproduce that Pages behavior.
3. Builds `/elezioni` again and stages only the current production graph.
   All four static routes use directory indexes. Snapshot descriptors, authored
   politics dependencies and the selected reform candidate's transitive
   compressed metadata/column/source chunks determine the required files.
   Missing or corrupt files fail; old immutable versions, research, fixtures,
   source maps and receipts stay outside the site. The compiled `_app` tree,
   `.nojekyll` and deterministic `release.json` complete the payload.
4. Tests that exact staged tree with raw gzip file delivery. Checks direct
   loads, refreshes and navigation; all four law/model worker combinations;
   Emilia-Romagna execution; structured and CSV exports. Each browser context
   starts fresh. Encoded response sizes and content encodings are retained.
5. Reinstalls dependencies using the warmed npm cache, then runs
   `npm run hosting:reassess -- warm`, retaining source/model caches. It repeats
   preparation, all checks and builds, and staged-site acceptance. The complete
   cold/warm site manifests must match byte for byte. `BUILD_REVISION` fixes
   SvelteKit's app version to the commit SHA instead of a build timestamp.

The local command `npm run hosting:reassess -- warm` verifies an existing
prepared checkout. `cold` requires a fresh checkout; it does not delete local
caches to manufacture one. A repeated run refuses to overwrite an existing
`report.json`: move the previous mode directory to an evidence archive first.
Keep builds and browser commands sequential.

## Evidence and interpretation

Start with `data/derived/hosting/{cold,warm}/report.json`. Inspect `status`,
`steps`, `acceptance.counts`, `payload.bytes`, `payload.manifestSha256`, `smoke`
and `error`. `workingTreeDirty` distinguishes local edits from a clean hosted
revision; `sourceBytes` and `modelCacheBytes` record retained input/cache disk use.
The payload includes per-file hashes, stored bytes, a separate
gzip estimate, excluded build files and exact data versions. Full evidence:

| Path relative to each mode directory | Content |
| --- | --- |
| `install.log`, `install-resources.txt` | npm installation output and Linux elapsed/CPU/peak-RSS/I/O |
| `logs/*` | Complete subprocess logs; Linux GNU-time resource sidecars |
| `prepare-report.json`, `check-report.json` | Production input/output receipt hashes and preparation time |
| `application/report.json` and linked reports/logs | Strict application acceptance |
| `archives/*` | National export/import/re-export archives and digests |
| `site/*` | Actual publishable directory, uploaded only for authorized publication |
| `browser.json`, `transfers/*.json` | Staged-site results and per-request encoded transfer receipts |

The runner samples system available RAM and free disk every five seconds;
these are host-wide measurements, not summed process RSS. GNU time reports
child-command peak RSS separately. npm installation precedes the gate and has
its own resource receipt. Browser installation is infrastructure setup, outside
the measured preparation/check/build commands. A warm replay uses the same VM,
not a restored Actions archive or a guaranteed browser/CDN cache hit.

Local raw-file transfer measurements conservatively omit CDN compression for
JSON/JS/CSS. Gzip estimates are explicitly estimates. Public smoke records the
CDN's actual content encodings and wire body sizes. Generated exports do not
consume site bandwidth. Dividing 100 GB by the measured workflow size gives a
rough traffic budget, not a performance or availability guarantee. The direct-route test is
a synthetic matrix of repeated loads, not a representative single visit.

Download `hosting-evidence-<sha>` and, after publication,
`public-site-evidence-<sha>` within one day. Small `hosting-summary-<sha>` and
`public-site-summary-<sha>` artifacts retain status, identities and transfer
receipts for 30 days. Preserve release evidence externally
if longer retention is needed; do not commit generated archives to the repository.

## Remaining remote work and publication

The Windows checkout can verify production behavior and payloads. It cannot
establish GitHub-hosted Linux runtime/RAM/disk or CDN behavior. The connected
GitHub tools provide repository/workflow reads but no new-workflow dispatch;
GitHub CLI is not installed. The new workflow must first be committed and pushed
to GitHub. WSL or Docker would still not establish standard hosted-runner
acceptance. No commit, push, merge, repository setting or publication is part of
the local reassessment implementation.

Before merging, push the migration branch and inspect its cold/warm hosting
reports. Fix any source-download, Linux, resource, no-skip or payload mismatch
failure before treating hosting feasibility as established. A live source whose
bytes drift must fail; do not replace the pinned manifest without review.

After Luca merges into `main`, publication is an explicit action:

1. In repository **Settings → Pages**, select **GitHub Actions** as the source.
   Keep the default project URL; configure the `github-pages` environment to
   allow deployment from `main`.
2. Set repository Actions variable **`PAGES_PUBLICATION_ENABLED=true`** only when
   ready to publish. Missing/false keeps deployment disabled even after merge.
3. In **Actions → Hosting reassessment and Pages → Run workflow**, select
   **main** and set **publish=true**. That revision must pass both cold and warm
   gates again. The upload and deployment jobs require the explicit dispatch,
   main ref, publication variable and successful reassessment.
   An explicit publication request on another branch or without the variable
   fails clearly instead of silently reporting a successful publication.
4. Confirm `deploy` and `public-smoke` pass. Public smoke verifies the deployed
   `release.json` revision, routes, real workers, simulations and both export
   formats. A smoke failure after deployment leaves a failed workflow and needs
   investigation; it does not automatically roll back the public site.
5. Record the actual URL, main SHA, `release.json` data identities, hosted times,
   memory/disk, site bytes and public transfer/outcome receipts in
   [migration history](migration-history.md). Only then close the final plan item.

Expected default URL: `https://fornaeffe.github.io/elezioni/` (**not yet published
or accepted**). If hosted measurements exceed the limits, retain the failed
receipts, identify the limiting stage, and reassess free alternatives before
selecting a replacement. Current eligibility alone is not hosted acceptance.
