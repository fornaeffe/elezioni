## 2026-10-16 R26-01a: Senate recovery precedents and approved I07

Completed the requested primary-source research and, after Luca approved the
proposal, recorded the provisional [I07 protocol and worked fixture expectations](../../politics-reform-2026-specification.md#senate-initial-recovery-i07).
This completes the research decision step, not recovery implementation. It
supersedes the pending-I07 status in the preceding R26-01 entry. R26-02 is the
next discussion; all subsequent implementation steps remain open.

### Initial exhaustion: Sicily to Umbria, Pavanelli (2019)

The 2018 Senate election left a proportional M5S seat in Sicilia-02 unfilled
because the list's candidates were exhausted. Candidates already elected in
uninominal seats, including Nunzia Catalfo, could not fill that additional seat;
there were no remaining eligible regional candidates and M5S had no coalition
partner to receive it.

The Giunta delle elezioni e delle immunità parlamentari adopted its proposed
solution on 26 June 2019. The Assembly approved **Doc. XVI no. 2 on 31 July 2019**;
the Giunta identified Emma Pavanelli in Umbria and the President proclaimed her
that day. This was an initial-allocation deficiency resolved later during
verification of powers, rather than a seat vacated during the term.

Primary evidence:

- [Doc. XVI no. 2, majority report](https://www.senato.it/service/PDF/PDFServer/DF/346068.pdf),
  particularly pp. 7-9, and the [relator's explanation on 25 July 2019](https://www.senato.it/show-doc?id=1119720&idoggetto=0&leg=18&part=doc_dc-ressten_rs-gentit_ddddxn2rgeeissnars&tipodoc=Resaula):
  regional same-list recovery, then the then-existing unsuccessful-uninominal
  recovery, then regional coalition recovery; only after those failed, residual
  S19(2)/C86/C84(4) recovery for the same list in another region, selected by the
  greatest unused quotient remainder.
- [Official procedure and outcome](https://www.senato.it/legislature/18/leggi-e-documenti/attivita-non-legislative/procedure?documentoId=42318),
  [Assembly proceedings of 31 July 2019](https://www.senato.it/show-doc?id=1120170&leg=18&part=doc_dc&tipodoc=Resaula)
  and [Pavanelli's official profile](https://www.senato.it/leg/18/BGT/Schede/Attsen/00034677.htm)
  establish the actual transfer and proclamation in Umbria. Her later
  convalidation date is distinct from the 2019 proclamation.

This supplies a relevant parliamentary precedent for an initial unfilled seat
crossing a regional boundary as a residual remedy. The outcome was contested;
it is not a judicial determination that every such transfer is constitutional.

### Later vacancy: Veneto to Calabria, Minasi (2021)

Lega senator Paolo Saviane died on 20 August 2021. With Lega candidates exhausted
in Veneto, the Giunta on **11 November 2021** rejected the proposal to replace
him with the regional coalition partner's Bartolomeo Amidei (FdI), and adopted
the same-list cross-region solution of Clotilde Minasi (Lega) in Calabria.
The Assembly approved **Doc. XVI no. 7 on 2 December 2021**, and Minasi was
proclaimed that day.

Primary evidence:

- [Giunta decision of 11 November 2021](https://www.senato.it/show-doc?id=1317860&idoggetto=0&leg=18&part=doc_dc-sedetit_vdp&tipodoc=SommComm)
  records the rejected regional-coalition proposal and the adopted alternative.
- [Doc. XVI no. 7, majority report](https://www.senato.it/service/PDF/PDFServer/DF/365843.pdf),
  [official procedure and final outcome](https://www.senato.it/legislature/18/leggi-e-documenti/attivita-non-legislative/procedure?documentoId=46913)
  and [Assembly proceedings of 2 December 2021](https://www.senato.it/show-doc?id=1321518&leg=18&part=doc_dc-ressten_rs-gentit_ddddxn7rgeeipssvirv&tipodoc=Resaula)
  establish the legal argument, approval and proclamation.

This confirms that a same-list cross-region replacement actually occurred, but
in a **later vacancy**. Its priority over an available regional coalition
candidate must not replace the regional-first order for initial allocation.
Later vacancies remain outside the current simulation's scope.

### Constitutional proceedings and applicability limits

The Constitutional Court's [order 86/2020](https://www.cortecostituzionale.it/scheda-pronuncia/2020/86),
decided on 7 April and deposited on 7 May 2020, declared Gregorio De Falco's
conflict of powers concerning the Pavanelli procedure **inadmissible**. The
Court found no demonstrated substantial impairment of his individual
parliamentary powers; he could not assert the electoral offices' powers or a
generic interest in legality. It did not decide the regional-transfer issue
on its merits. The bounded research did not identify a merits judgment settling
the Minasi issue; that is a search limit, not a claim that none exists.

The historical decisions applied the pre-reform system. Their old C84(3)
unsuccessful-uninominal remedy is abolished by the reform and is excluded from
the approved protocol. Neither decision addresses the new bonus slates or
directly validates the S16-bis(1-quinquies) mechanism.

Some Senate pages/PDFs returned access challenges or HTTP 403 during direct
retrieval. Official indexed text supplied the relevant report/proceeding
passages; official procedure/profile records corroborated the outcomes. The
Court's decision text was accessible. These access limits do not amount to a
complete visual review of every linked Senate PDF.

### Approved decision and documentation changes

I07 is a provisional simulator interpretation, revisable on further legal
evidence, separate from the consolidated statutory transcription:

- Initial ordinary recovery, including releases after multiple elections,
  exhausts same-region list and coalition remedies under C84(2),(5), then uses
  S19(2)/C86(2)/C84(4) for the original list in another region. It does not
  activate C84(7)'s cross-region coalition remedy.
- Bonus shortages follow the separate express
  S16-bis(1-quinquies)/C83(1-quinquies)/C86(1) route: local recovery first, then
  cross-region recovery for the bonus-winning list or coalition, unused before
  used remainders. A coalition bonus is not split into arbitrary list shares.
  This is a provisional reading of the new cross-reference, rather than a
  result proved by the historical cases.
- Cross-region recovery requires an I07 warning and a reproducible trace,
  preserving original entitlement and actual election territories. Ordinary
  allocation is not recalculated to hide a movement; exhausted permitted
  routes leave an explicitly unfilled seat.

Updated the specification's N27 boundary, I07 register entry and six constructed
fixture expectations; carried the policy into R26-17 and result/export
requirements in R26-20. Marked only R26-01a newly complete and updated the root
plan's next-step directive. The legal transcription, modelling choices,
application code and generated inputs are unchanged.

Verification:

- Checked 66 local file/section links across the root plan, four reform
  documents and this history entry; all resolved. Confirmed 22 ordered reform
  checklist entries with only R26-01 and R26-01a complete, and R26-02 still
  dependent on R26-01a.
- Confirmed N01-N29 and I01-I09 remain present, all six I07 fixture boundaries
  are recorded, and active directives no longer describe I07 as pending.
  Only four Markdown files changed; the statutory transcription is unchanged.
- `git diff --check` passed. No application tests/build were run for this
  documentation-only change; the worked expectations are future test
  obligations, not implemented or executed tests.
