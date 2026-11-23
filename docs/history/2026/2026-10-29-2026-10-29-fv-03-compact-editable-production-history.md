## 2026-10-29 — FV-03 compact editable production history

Completed FV-03's original-source boundary, documented in the
[source preparation guide](../../vote-generation-fast-data.md). Added separate active
history, membership, poll-view, assembly and CLI modules, portable source types,
chunk codec and explicit selection/compiler. Added non-interactive
`data:reform:shares:download/prepare/check/prefit` commands. The prefitting wrapper
requires a complete explicit selection and writes accepted C/E reference draws,
input/provenance identities, seed and request; it selects no default. Updated
the migration checklist, active reading maps and archive exclusions. FV-04
onward remain open.

The source manifest composes the existing pinned census/geography, historical
Camera/European and reform Senate archives. Promoted copies of the required
correspondence/citation/restoration JSON into `data/sources/fast-share/`, retaining
the provisional correspondence status and warnings in the runtime envelope.
Original research code/data are unchanged. Active preparation imports no
`party-drift-*`, `generation-history`, Stan, R cache or ignored research artifact.
Existing reviewed old-college attribution and geographic normalization remain
shared active dependencies.

Prepared all ten events: Camera 2008/2013/2018/2022, European
2009/2014/2019/2024 and Senate 2018/2022. Retained all VdA/TAA choices, unmatched
and invalid-denominator observations and separately identified unlocated
fuori-sede records. Direct source counts accompany attributed integer list
totals; special candidate/group outcomes remain opaque. The dictionary has
8,456 historical/target/unlocated identities and 296 source choices, representing
149 proposed historical atoms before any requested pooling. This is not a claim
that all atoms form an admissible <=128-component default basis.

The complete target contains all 7,904 municipalities, 86 physical colleges and
15,810 membership cells, conserving census population by municipality/chamber.
Camera TAA's proportional view is not a second physical ballot. Same-place
matches retain target codes; unresolved mergers/homonyms are labelled missing
crosswalks rather than guessed or deleted. Both 2022 chambers reproduce all
15,808 municipality/chamber records from the independently prepared installed
baseline exactly, including every named/special choice count and electorate.
Senate 2018 is explicitly 25+ age-confounded audit-only; it cannot enter C/E
fitting, and 2022's paired input does not itself validate the future adapter.

The archives lack pinned result-publication timestamps. Added a declared
retrospective assumption of 30 days after the final polling day, including the
second day for Camera 2008/2013. The bundle labels this as an assumption, not an
official timestamp, and the compiler enforces it at inclusive daily cutoffs.
No current clock decides data eligibility. Bundled elections end 2024-06-09;
the selected poll archive's latest publication is 2026-10-19. Extrapolation and
source vintage remain explicit.

All 41 poll questions preserve original and normalized answers, sample/decided
bases, the single design-effect adjustment, effective-sample cap, shared survey
and semester/context errors, rounding, dates and citations. Thirty-eight are
supported; three conditional questions remain excluded with reasons. Early
FDI/FI restoration refines the same conserved residual and survey, never adds
a duplicate observation. Contemporary named answers with no established history
use distinct `poll-only-*` atoms; no Lega donor or inferred party ancestry is
imported. Reviewed JSON suffices in a clean tree without optional PDFs.

The compiler rebuilds grouped national/profile information from original counts
after membership/date/selection edits. It rejects competing raw memberships,
opaque splits, unknown/incomplete pooling, duplicate surveys and unavailable or
excluded observations. Other complements are formed before pooling named and
unnamed support. Admissibility tests conditional offered-set contrasts: a
one-choice observation supplies no distinguishing information. National sums
retain unmatched resident observations; invalid source denominators produce
explicit local exclusions. Unlocated ballots remain source data without being
assigned residential denominators. Profiles sum whole groups into the national
observable partition; absent target history remains a labelled fallback.
Numerical keys exclude display/citation changes, while provenance binds both
numerical keys and the source/edit evidence.

The immutable `fast-share-source` bundle has compressed/decoded hashes and
sizes, dictionaries and one little-endian uint32 chunk per event. Explicit
offsets/lengths and row flags distinguish missing voters, unmatched/unlocated
geography and invalid denominators. All integer conversions, indices, shapes,
offered zeros and national sums are checked. Exact input/decoded row round trips
preserve **81,734 explicit zeros**. Audit files have their own content identities
so an audit correction cannot overwrite an immutable source version.

The [benchmark snapshot](../../../test/fixtures/benchmarks/vote_share_sources.json)
pins final preparation/codec/compiler source hashes, envelope/asset identities,
source range, counts, exclusions, environment and qualified measurements.
Detailed reports are ignored under `data/derived/fv03-acceptance/`. The bundle is
**4,512,430 bytes compressed / 19,823,299 bytes decoded** (4.30 / 18.91 MiB),
including 3,785,547 decoded metadata bytes. Preparation took about 17.4 seconds
on the current Windows/Node 24 host. Sequential event decode's largest live
numeric allocation is 4,494,961 bytes; this excludes metadata, hash copies and
JS row objects. Measured Node decode/check lifetime peak RSS was 268,880 KiB
(262.6 MiB), with inflate/column/row-boundary heap/buffer samples recorded.
This is not a browser process-memory claim.

Full-source worker validation passed in Chromium 148.0.7778.96 at `/` and
`/elezioni`, with one first-worker run and five warm runs per path. First runs
took about 367/371 ms; warm medians 329/336 ms, maxima 343/350 ms. These include
local intercepted asset delivery and validation, not internet transfer speed.
The reduced source fixture produces identical content/numerical keys and
compiled observations in Node and Chromium. Browser aggregate process memory,
combined retained state, maximum-envelope fitting, full default admissibility,
Senate dependence and predictive acceptance remain FV-04/05/06/08 gates.

Verification: acquisition into an initially empty source cache succeeded from
the pinned network URLs. A separate source-only tree, independently installed
with `npm ci`, regenerated offline without original ignored/derived artifacts;
all 13 final generated files match byte for byte. This clean-tree run was on
Windows; Linux release acceptance remains FV-08. Missing chunks and corrupted
compressed chunks failed CLI checks without fallback. The explicit reduced
prefit CLI passed with four accepted chains; no full-country fit/forecast ran.
Application/preparation checks passed with zero Svelte diagnostics; the full
unit suite passed **84 files / 1,133 tests**, including 13 new source tests.
Focused tests passed after final provenance/audit corrections. Static builds
and two browser tests per base passed, and the root build was restored. Existing
chunk-size build advisories remain. FV-01 evidence verification, unchanged FV-02
numerical source hashes, documentation links and diff whitespace passed.
No installed scenario/default, generator, legal behavior or authoritative old
observation changed; no research move/deletion, commit or publication occurred.
