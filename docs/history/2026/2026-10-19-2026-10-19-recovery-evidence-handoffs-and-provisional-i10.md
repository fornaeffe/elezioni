## 2026-10-19: recovery evidence handoffs and provisional I10

Follow-up to R26-18, explicitly authorized by Luca: repair missing downstream
communications without new legal assumptions; research the inherited C84(5)
coalition phrase and the R implementation; use the proposed member-list maximum
provisionally if the evidence does not establish another interpretation. The
[interpretation register](../../politics-reform-2026-specification.md#interpretation-register)
contains the operational I10-v1 policy. The [core README](../../../web/src/lib/politics/reform-2026/README.md#composition-and-registry-r26-18)
describes execution and the remaining boundary.

### Source review and authorized decision

- Searched the exact phrase and C84(5) recovery terminology, including election
  instructions, candidate exhaustion, parliamentary material and interpretations.
  The [2017 Senate dossier 548 comparison](https://www.parlamento.it/show-doc?id=1046530&leg=17&part=dossier_dossier1-sezione_sezione11-h2_h21&tipodoc=DOSSIER)
  confirms that the wording predates the reform. The [Camera dossier of 26 June 2026](https://documenti.camera.it/leg19/dossier/testi/AC0469b.htm)
  discusses member-list decimals, but its summary does not conclusively resolve
  the exact college-selection operation. The [6 October 2021 Senate debate](https://www.parlamento.it/show-doc?id=1313906&leg=18&part=doc_dc-sedetit_vdp-genbl_rv-intervento_durnwalderautsvppattuv&tipodoc=SommComm)
  quotes C84(5) and debates coalition replacements; the cited intervention is not
  an authoritative algorithm specifying this maximum. No conclusive official
  ruling or operational interpretation was located. This is a scoped search
  finding, not proof that no interpretation exists.
- Inspected `R/politiche/scrutinio.R`: `cerca_accettori()` filters candidate-bearing
  list/college rows and sorts unused before used, then descending list decimal.
  `cerca_naz()` filters the requested lists and unused/used tier, then aggregates
  list decimals by constituency with `max`. It never constructs a new coalition
  quotient. This corroborates member-list maximum reasoning in a different
  territorial operation, not empirical proof of C84(5)'s college ordering.
- Crucially, `subentro()` calls the local `cerca_accettori(i)` without forwarding
  `coal`/`pluri`; this is the previously recorded legacy parity boundary. Thus
  real-election output agreement cannot establish this branch's interpretation
  without demonstrated coverage. No assertion was made that the disputed case
  actually occurred in a historical election. The R and legacy TypeScript paths
  remain unchanged, including this known forwarding behavior.
- With inconclusive exact evidence and no established contrary interpretation,
  applied Luca's authorized provisional I10-v1: rank candidate-bearing colleges
  by the largest unused decimal of any admitted member list filed there, then
  the largest already-used decimal. Preserve actual member-list source IDs and
  states; do not sum votes/decimals or invent coalition quotients. Allocation
  records of an exhausted member still contribute to that college's priority;
  eligible candidates in another member list can fill its seat. Then apply
  the normal member-list ordering inside the selected college. Equal college
  priorities use the statutory C84(8) draw. Emit an I10 warning on application,
  retain I10 in the recovery trace and composed policy metadata, and preserve
  independent I07 cross-region warnings and route ordering.

### Mechanical repairs, tests and remaining boundary

- A sole candidate-bearing alternative now bypasses a comparison that cannot
  affect the choice. No missing quotient or percentage is replaced by zero;
  existing evidence is retained and no draw is consumed. Multiple alternatives
  still require all applicable ranking evidence.
- The S17(3) single-college path now receives the existing regional list quotient
  and final state from N19 (standalone) or N21 (coalition member). New scoped IDs
  retain `sourceId` pointing to the original record; no allocation or vote ratio
  is recomputed. Tests include Senate cap states and coalition member records.
- The formerly blocked constructed coalition-shortage election now fills all
  588 domestic seats. Added tests for member maxima conflicting with artificial
  coalition decimals, exhausted member witnesses, unused versus used/consumed
  states, statutory ties and permutation replay, singleton comparisons, exact
  source aliases, warning/trace/policy retention, and direct browser-worker I10
  execution. Existing missing-evidence tests now supply multiple alternatives,
  because a single alternative no longer needs that comparison.
- One distinct problem remains, reproduced with a full valid envelope: an
  admitted list has zero regional entitlement, hence is excluded from the local
  college-allocation denominator. There is no original college/list quotient
  to communicate. Recovery can choose a sole destination (and the single-college
  regional evidence is carried), but comparison between two such destination
  colleges remains `unresolved: missing-recovery-remainder`. Establishing a
  divisor or alternative priority here requires an additional legal/modelling
  decision; this is not the now-resolved absence of a coalition quotient. The
  regression preserves this distinction and all seat entitlements. The same
  principle preserves nulls for zero-capacity allocation paths. Recorded the
  remaining comparison as a precondition for unrestricted candidate acceptance.
- Verification: `npm run check:data` and `npm run check` passed (zero diagnostics);
  `npm run test` passed **775 tests in 49 files**, with no skips; the static build
  passed; **9 Chromium browser tests** passed, including I10 recovery and policy
  warnings in a bundled module worker. Build/e2e ran sequentially. No production
  generation, UI selector, source data, generated defaults or deployment changed.
  Changes are left uncommitted.
