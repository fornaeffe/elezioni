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
| Standard public Linux runner | 4 CPUs, 16 GB RAM, 14 GB SSD | `ubuntu-24.04`, Node 24, two unit workers and one browser context |
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

[hosting.yml](../.github/workflows/hosting.yml) has separate push and manual paths.
Pushes to `migrazione` and `main` run **development** acceptance: prepare required
assets, both type checks, all units, 66 general fixture/UI cases and one editor
cancellation case, using Chromium. Reports are in
`data/derived/development-acceptance/`, uploaded as `development-evidence-<sha>`
for one day. Pushes never deploy or start the release cold/warm campaign.

Manually dispatch **Hosting reassessment and Pages**, selecting the desired
branch with **publish=false** and **extended=false** for ordinary release
reassessment. `extended=true` explicitly requests the broader application
matrix. See [application profiles](common-politics-scenario/acceptance.md) for
selected cases, limits and local commands. Each fresh release Linux runner:

1. Installs Node 24 and dependencies using an initially empty npm cache, then
   installs Chromium and Firefox.
2. Runs `npm run hosting:reassess -- cold`: acquires pinned originals and prepares
   all production data without pre-existing generated/model caches. Release
   application acceptance requires data validation, both type checks, all units,
   two representative real archive workflows, 70 general cases and one editor
   cancellation at `/elezioni`, plus a root routing/worker smoke (74 browser
   checks). Firefox/M02 runs early. There is no duplicate preflight or full root
   matrix unless the extended profile was explicitly selected.
3. Builds `/elezioni` again and stages only the production dependency graph:
   compiled `_app`, directory-index routes, selected pinned bundles/chunks,
   `.nojekyll` and deterministic `release.json`. Missing/corrupt files fail;
   research, fixtures, source maps, receipts and unused versions stay outside.
4. Requires all six browser checks against that exact staged tree: direct
   routes/refreshes/navigation, all four real national law/model worker pairs,
   regional execution, and structured/CSV exports. Raw gzip delivery and encoded
   response receipts remain required. Every selected check must pass first time.
5. Reinstalls dependencies using the warmed npm cache and runs
   `npm run hosting:reassess -- warm`. It repeats preparation, `data:check` and
   the project build. Every preparation artifact's path, byte length and SHA-256
   must match cold, followed by exact staged payload-manifest equality.
   Warm requires a passing cold report for the same revision and policy. It
   records reused cold application/browser acceptance and does not rerun tests
   against identical bytes. `BUILD_REVISION` fixes SvelteKit's app version to
   the commit SHA instead of a build timestamp.

`cold` requires a fresh checkout; it never deletes local caches to manufacture
one. `warm` requires that checkout's successful cold evidence. Both refuse to
overwrite an existing mode `report.json`: archive the previous mode directory
before reusing it. Keep local builds and browser commands sequential.

## Evidence and interpretation

Start with `data/derived/hosting/{cold,warm}/report.json`. Inspect `status`,
`policy`, `steps`, `acceptance`, `artifactComparison`, `payload.bytes`,
`payload.manifestSha256`, `smoke`
and `error`. `workingTreeDirty` distinguishes local edits from a clean hosted
revision; `sourceBytes` and `modelCacheBytes` record retained input/cache disk use.
The payload includes per-file hashes, stored bytes, a separate
gzip estimate, excluded build files and exact data versions. Full evidence:

| Path relative to each mode directory | Content |
| --- | --- |
| `install.log`, `install-resources.txt` | npm installation output and Linux elapsed/CPU/peak-RSS/I/O |
| `logs/*` | Complete subprocess logs; Linux GNU-time resource sidecars |
| `prepare-report.json`, `check-report.json` | Production input/output receipt hashes and preparation time |
| `application/report.json` and linked reports/logs (cold only) | Named profile, exact selected membership and strict application acceptance |
| `application/*-output/` | Per-suite/base browser failure traces, screenshots and contexts |
| `archives/*` | National export/import/re-export archives and digests |
| `site/*` | Actual publishable directory, uploaded only for authorized publication |
| `browser.json`, `transfers/*.json` (cold only) | Staged-site results and per-request encoded transfer receipts |

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
a synthetic matrix of repeated loads, not a representative single visit. Its
receipts retain HTTP statuses and errors, with transfer sizes explicitly null:
navigation intentionally abandons responses whose Playwright size promises
may never settle. The five complete election workflows retain full encoded
transfer measurements and traffic estimates.

Download `hosting-evidence-<sha>` and, after publication,
`public-site-evidence-<sha>` within one day. Small `hosting-summary-<sha>` and
`public-site-summary-<sha>` artifacts retain status, identities and transfer
receipts for 30 days. Preserve release evidence externally
if longer retention is needed; do not commit generated archives to the repository.

## Remaining remote work and publication

The Windows checkout cannot establish GitHub-hosted Linux runtime/RAM/disk or
public CDN behavior. After committing/pushing on `migrazione`, first inspect the
automatic development job, then manually dispatch **Hosting reassessment and
Pages** on that revision with **publish=false**, **extended=false**. Require both
cold and warm success before merge. Fix source-download, resource, membership or
payload mismatches; source drift still requires explicit manifest review.

Hosted jobs explicitly set two unit workers and one browser context. No new
repository variables are needed. Reports reject incomplete selected matrices;
zero retries, fail-fast commands, per-suite evidence and first-failure traces
remain. Actual durations after this rationalization are not yet measured.

If a job fails, download its evidence within one day, preserving directory
structure. For development start with `development-acceptance/report.json`; for
release start with `cold/report.json` and `cold/application/report.json`, or
`warm/report.json` for byte-comparison failures. Include the failed suite JSON,
linked log and its `*-output/**/trace.zip`. Re-running an older revision does not
include a newly committed fix.

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
   [project history](history/README.md), and update the publication evidence task in
   [TODO](../TODO.md).

Public URL, as reported by Luca: `https://fornaeffe.github.io/elezioni/`.
Record the supporting publication evidence as described above. If hosted measurements exceed the limits, retain the failed
receipts, identify the limiting stage, and reassess free alternatives before
selecting a replacement. Current eligibility alone is not hosted acceptance.
