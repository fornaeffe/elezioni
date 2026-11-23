## 2026-10-23 — Authoritative poll JSON and optional manual audits

At Luca's request, the poll archive's three JSON files are now the local source
of truth: `archive.json`, `manifest.json`, `entities.json`, in both the current
and historical selections. This supersedes the PDF retention/checksum policy in
the preceding poll entries. Survey facts, percentages, mappings and statistical
assumptions remain unchanged.

The v2 poll manifests replace document path/SHA-256/byte-size fields with a
human survey `reference` such as `Sondaggio14362`. They retain government URL,
retrieval instructions, title, pollster, dates and methodological metadata for
optional manual audits. Question/page numbers remain available. Removed
`reviewedFiles` checksum pins too: accepted JSON edits must not require repinning
a manifest that is itself an authoritative source. Strict schema, date,
uniqueness, source/question/entity linkage, sample-base and support-total checks
remain active. Missing/malformed JSON still fails; missing or invalid optional
PDF downloads cannot affect preparation, size accounting or output identity.

Preparation reads exactly three JSON files, with no network or source-document
access. Generated source/version fingerprints are computed from that JSON for
replay and immutable output naming; they are not upstream audit hashes or
maintainer-updated pins. Observation/preparation schemas and identifiers are v2;
the archive/entity question schemas and saved scenario-v3 contract remain valid.
The independent official election-result manifests, ZIP hashes and target-table
pins remain unchanged.

Removed PDF allowlists and PDF-specific Git attributes. No poll PDF was already
in the Git index, so there was nothing to remove from it. Existing local PDFs
were preserved as ignored optional downloads. Verified that Git lists no tracked
poll PDFs and ignores both current and historical downloads. Updated the active
plan and focused preparation/calibration guides to describe JSON maintenance and
optional government-reference audits.

Current poll JSON totals 45,433 bytes; historical poll JSON plus separate election
target inputs totals 106,378 bytes. Observation version is
`poll-observations-v2-ff126a8195d4`, calibration version
`poll-calibration-v1-7b7af9479543`, and production reference version
`poll-reference-v1-c13ddf5cede1` (scenario suffix `16ae1353f5fd`). Compared against
the preceding generated artifacts: every calibration fold, fitted parameter,
metric, acceptance decision and production poll-reference row is identical.
Only source representation, provenance/version metadata and archive byte counts
changed. Production paired generation/composed scrutiny remained computable.

Verification on Windows/Node 24: all 31 focused poll tests passed, then all 956
unit tests in 69 files passed (13.71 s). New coverage proves that only JSON is
needed, optional malformed PDFs are ignored, valid observation/manifest/entity
edits are consumed without repinning and get new derived identities, and missing
or malformed JSON, unusable/duplicate references and invalid links/dates fail.
Historical calibration is tested from six JSON files with no PDFs; separate
corrupt election-target pins still fail. Preparation/application type checks
passed with zero Svelte errors/warnings. Production build passed (12.77 s SSR),
as did actual poll generation and the production reference vertical slice.
The subsequent full root-base Chromium suite passed all 30 tests (46.8 s).
Git diff whitespace checks passed; build chunk/plugin/color notices remained
informational. No commit, publication, default activation or legal scrutiny
change was performed.
