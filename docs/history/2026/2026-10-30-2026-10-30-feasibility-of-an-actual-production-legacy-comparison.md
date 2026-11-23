## 2026-10-30 — Feasibility of an actual production legacy comparison

Assessed Luca's request to compare the selected candidate against the installed
R-derived reform generator. The earlier global-Beta comparator is a predecessor
of the fast model, not that production generator. The active assessment is
[the legacy comparison plan](../../../research/vote-generation/docs/vote-generation-legacy-comparison-plan.md).
No runtime code, default, fitted asset or correspondence table was changed.

Verified the actual production catalog/default and implementation: September
2022 historical baseline, 22 Camera/25 Senate lists, no active poll references
or supplied shares, and assumed national/regional/local logit scales. Optional
poll-reference support exists in schema 3. Share overrides reset national and
territorial uncertainty dates; an unchanged historical-only scenario retains
its historical date regardless of projectionDate. Unmapped or future-unfiled
historical votes enter combined non-valid support. Factors normalize over all
mappings before future filing, so a lone 0.3 weight does not preserve only 30%.

An in-memory 24-list structural translation with correct legacy presentation
metadata passes validation. All constituent-college future filing masks agree
within each municipality. A first structural probe omitted required schema-2
presentation metadata; correcting that routine shape conversion removed its
only validation error. No numerical correspondence decisions were applied.
The source/membership audit identified joint-list split decisions (including
Azione/IV and Noi Moderati/UDC), new/unsupported support, grouped residuals and
special-territory proxies. Source event/chamber and list-versus-group IDs must
be retained when printed names coincide.

Read-only Node 24.20.0 smoke probes used the actual installed default: reference
preparation 438.6 ms, three paired municipal draws 278.6/270.9/268.1 ms with count
conservation. A separate complete default smoke measured 781.3 ms preparation,
645.0 ms generation/assembly/preferences, 384.3 ms scrutiny, complete status and
all 588 filled seats. These are not new-24-list timings or browser acceptance.

Repeating the study is feasible with an isolated adapter and cutoff-specific
historical snapshots. Reuse the earlier Camera accuracy panels and scoring,
plus the actual future 24-list paired scenario for distribution/technical/browser
checks. Counterfactual 2026 list shares are not observed historical truth.
Decisions remain explicit before benchmarking: no-poll versus controlled-input
variants, reviewed historical split ratios, zero versus scenario-assisted support
for new lists, and preservation versus controlled matching of turnout/residual
semantics. No full comparative benchmark or predictive result is claimed.
