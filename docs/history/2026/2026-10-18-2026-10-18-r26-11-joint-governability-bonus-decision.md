## 2026-10-18: R26-11 joint governability bonus decision

Implemented the requested N12 stage in `reform-2026/bonus.ts`, exported through
the core index. The [module boundary](../../../web/src/lib/politics/reform-2026/README.md#joint-bonus-decision-r26-11)
documents the stage contract and its remaining dependencies. Rechecked the
applicable [Camera amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=1&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
and [Senate amendments](https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26G00201&art.dataPubblicazioneGazzetta=2026-10-09&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1)
against the scoped C83/S16-bis transcription and N12/I03/I04 specification.
No normative source version changed; the announced annotated republication and
later corrections remain acceptance checks. Operative excerpts and article
references are retained beside the translated logic.

### Delivered boundary and rationale

- `determineGovernabilityBonus()` consumes the declared renewal scope, both
  admission communications, N06 national all-valid figures and explicit bonus
  links. It selects first place only among admitted coalitions/standalone lists;
  coalition members are not ranked again. Counts include VdA/TAA votes once and
  retain excluded subjects' ballots in the threshold denominator.
- Exact integer cross-products implement inclusive 42% independently in both
  chambers, ignoring display percentages. Both selected first subjects must be
  explicitly reciprocally linked. Equal IDs/names imply no link; list/coalition
  kinds remain distinct. A lower linked subject never replaces the first.
  Successful output awards 70/35 to the paired subjects, preserving coalition
  ownership without inventing shares for member lists or candidate identities.
- Results distinguish awards, computed no-award reasons and non-computable
  blockers. Threshold and linkage failures can both be reported. Single renewal
  follows the statutory no-bonus path without ranking/drawing; absent data in a
  declared joint renewal never synthesizes a chamber or implies single renewal.
  Both chambers are preflighted before consuming any legal draw.
- Equal first figures use the existing injected I03 resolver, canonical subject
  identities and fixed Camera/Senato order. Each fallback emits a warning with
  simulation/chamber/policy/context metadata; traces retain the selected subject
  and paired bonus effect. Admission events are not emitted again. Computed
  results retain counts, denominators, exact percentages and threshold decisions
  as owned JSON-safe data. Pairing stage records to the same simulation remains
  the caller's responsibility, as N06/N07/N08 records lack independent run IDs.

### Verification and limits

- Windows, Node **v24.20.0**, npm **11.19.0**, commands from `web/`.
  `npm run check`: **zero errors/warnings**. `npm run test`:
  **38 files / 468 tests passed**, 4.48 s, including **30 new bonus tests**.
  Cases cover exact/below/above 42%, differing chamber winners, reciprocal/missing
  links, single and incomplete joint renewals, blocked-stage preflight,
  special-only first subjects, coalition/list identity collisions, excluded
  subjects, large exact counts, deterministic/permutation-invariant warned draws,
  chosen-tie bonus effects, ownership/JSON transfer and validated-envelope
  composition through special attribution, figures and admission.
- Verification caught a test union-narrowing omission and two expectations that
  specified only one element of a two-chamber array; corrected the tests before
  the final passing checks. No scrutiny change was needed for those failures.
- `npm run build`: static build succeeded, 12.81 s reported by Vite.
  Subsequently `npm run test:e2e -- --reporter=line`: **7 Chromium tests passed**,
  13.3 s, covering existing politics/regional worker and import workflows.
  Build and e2e ran sequentially; existing plugin-timing/color notices were
  non-blocking. `git diff --check` and new-file whitespace scans passed.
- No R code, generated data/defaults, registry, worker or UI changed. This is
  bonus entitlement only: ordinary allocations/caps, territorial bonus seats,
  candidate proclamations/recovery, generators and application composition remain
  their later R26 steps. Both plans now continue with R26-12. Changes are left
  uncommitted.
