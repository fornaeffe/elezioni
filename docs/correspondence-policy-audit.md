# Post-FV-08 correspondence-policy audit

Completed 1 November 2026 against the current v5 implementation. **The audit is
complete; the policy is not validated as generally equivalent between models.**
Corrections below are proposals requiring Luca's approval before implementation.
Original v1–v3 archive routing and policy remain intact.

## Evidence and reproduction

- Maintained contract: [unified workflow](vote-generation-unified-workflow.md).
- Quantitative default snapshot: [audit report](../test/fixtures/benchmarks/correspondence_policy_audit.json),
  bound to the catalog, source and compiled legacy input IDs it records.
- Constructed executable cases: `web/src/lib/politics/reform-2026/vote-share/correspondence-policy-audit.test.ts`.
- Default reader: `web/scripts/acceptance/correspondence-policy-audit.ts`.

From `web/`, with the prepared production bundles installed:

```powershell
npx tsx --tsconfig tsconfig.node.json scripts/acceptance/correspondence-policy-audit.ts
npx vitest run src/lib/politics/reform-2026/vote-share/correspondence-policy-audit.test.ts src/lib/politics/reform-2026/vote-share/unified-generation.test.ts src/lib/politics/reform-2026/vote-share/forecast-numerics.test.ts
npm run check:data
```

The reader fails if required assets are absent; the constructed tests need no
production bundle. It reads the current release and recompiles its legacy inputs,
then removes polls and manual references **in memory** to isolate historical
correspondence/filing effects. The comparison applies the original archive
projection rule to these same inputs; it is not a replay of a past archived
scenario or a comparison of the two models' forecasts. Fractional list counts
are expected baseline allocations. College counts apportion those baselines with
the existing integer allocator, fixed audit rounding seed, $10^{12}$ probability
weights, and simulation ID zero. They exclude vote shocks, scrutiny and seat
effects. No full-country simulation acceptance campaign was rerun.

## Current rules and their consequences

| Case | Selected model | v5 legacy |
| --- | --- | --- |
| Incomplete future destinations / ceased components | Structural memberships define offered components; omitted coordinates are conditioned out of the full elector probability vector, including non-valid support | Normalize source composition on original components, retain mapped/filed mass, then renormalize political shares and restore historical valid fraction |
| New component | Requires explicit reference and declared fallback; absence is not an observed zero | A list without any correspondence in its chamber can use a named poll exception; unsupported points are audited. A partly new joint list is not separately classified as new |
| Unfiled support | Masked at physical cells; national absence also uses full-vector conditioning | Availability is the union of colleges touching a municipality; counts are fitted/generated there and allocated to colleges afterwards |
| No mapped political support | Fitted/profile or declared new-component assumptions govern the field; no equivalent legacy equal-list rule | Raw mapped regional donors restricted to filed lists, otherwise equal weight per filed list; positive historical valid mass with no filed list fails |
| Same component on different territorial tickets | One component field, different filed list labels per physical ballot | Historical correspondence can link to both labels; national poll point is split across destination labels without territorial weights |
| Overlapping filing capacity | Physical transportation feasibility checks joint capacity when projecting margins; conditional forecast fields derive totals from feasible cells | Reference reconciliation checks individual capacity at municipal-union level and iterates; college reconstruction can still fail |

Implementations: `vote-share/correspondences.ts`, `legacy-polls.ts`,
`legacy-adapter.ts`, `generation.ts`, `interventions.ts`, `projection.ts`, and
`../paired-vote-generation.ts`, `../generation.ts` relative to
`web/src/lib/politics/reform-2026/vote-share/`.

### 1. Preserving valid votes is a substantive transfer assumption

For historical valid mass $V$ and retained mapped/filed mass $R>0$, legacy gives
list $l$ the baseline $V r_l/R$. The multiplier $V/R$ can be arbitrarily large;
losing a destination is interpreted as proportional transfer to surviving
destinations. This avoids equating incomplete metadata with non-voting, but
does not establish where ceased-party voters would actually move.

Constructed municipality: 500,000 electors, 135,000 A votes, 165,000 B votes and
200,000 non-valid electors. Retain half of A and all B. Legacy preserves 300,000
valid votes and assigns 87,097 to A and 212,903 to B: B gains **47,903 votes**
without an authored transfer to B. The original archive rule renormalizes each
nonempty source row first, so partial A retention alone has no such effect there.
If A has no destination at all, the original rule loses its votes to non-valid
support, whereas v5 legacy preserves them through conditioning.

Selected conditioning is different. On a flat elector vector
$(0.40,0.27,0.33)$, removing the first political component yields
$(0.40/0.73,0,0.33/0.73)$. Non-valid support rises from **40% to 54.795%**;
v5 legacy instead retains **40%** and gives the surviving list all valid votes.
The selected cell score normalization likewise includes non-valid support.
Only explicit political reference interventions preserve the already constructed
cell turnout. Thus a shared choice-set representation does **not** imply shared
turnout semantics or identical municipal/college predictions. Here “non-valid”
includes abstention and invalid/blank ballots; it is not measured turnout alone.

### 2. Fallbacks are discontinuous and depend on list packaging

With no retained support, the legacy regional donor field uses raw mapped counts
before fallback installation, avoiding order-dependent donor contamination. It
still reflects assumed source compositions and mapping loss. Without donors,
two filed lists get 50/50 regardless of their component count or evidence.
Splitting one ticket into two can change the allocation.

The zero boundary is especially sharp: in a constructed same-region case,
regional donors assign an unmapped municipality entirely to A. Giving its own
historical votes a $10^{-9}$ correspondence to B suppresses fallback and assigns
**100% to B** instead, preserving the same valid total. The generic policy note
does not quantify this leverage; the proxy warning identifies region/equal use
but supplies no calibrated uncertainty for it. Partial mapping losses have no
corresponding lost-mass summary in the runtime audit.

### 3. National polls can misallocate geographically varying tickets

`latestLegacyPoints` divides a group's mapped destination factors by their sum.
A 20% measured joint group with assumed 60/40 composition, whose second component
has no future destination, becomes a **20%** point for the surviving ticket,
rather than 12% plus 8% explicitly unresolved support. This is an assumption,
not evidence that the surviving component inherited the whole group.

When one component files as North and South on disjoint ballots, a national 20%
point becomes **10% nationally per label**, independent of geographic coverage.
On an 80/20 geography with uniform component support, the natural induced label
totals would be 16%/4%. Legacy's 10%/10% targets imply local **12.5%/50%** instead
of 20%/20%; on smaller territories the arbitrary target can exceed capacity.
The selected model uses a common component field with physical labels, although
its inferred territorial profile remains a modelling assumption.

Chamber-specific projection and duplicate-component rejection are already in
place. They do not correct within-chamber territorial duplication. Latest-wave
selection, historical residual allocation and mixed-wave normalization can further
rescale the resulting points. A national point is not a measured territorial point.

### 4. Municipal filing unions overstate physical capacity

Constructed split municipality: 100 electors, 80 valid votes; A is filed only in
a 20-elector college, B everywhere. Legacy reconciliation accepts A=50% of valid
votes because A is available somewhere in the municipality. The required **40 A
votes exceed 20 physical electors**, and college reconstruction throws. The
selected transportation projector rejects that physical target immediately.
The same union can duplicate a historical component across disjoint tickets
within a split municipality, distorting relative shares even though final legacy
valid mass is restored.

Individual capacity checks also cannot prove joint feasibility: two lists each
requiring 15% of all electors cannot share an area containing only 20% of all
electors. Selected projection already reports a joint-capacity certificate for
this case. Legacy can report generic nonconvergence, or defer failure until
college allocation. Ordinary-poll scope additionally selects whole municipalities
touching ordinary colleges, rather than their actual ordinary physical fractions.

### 5. Greedy reconstruction can reject a feasible allocation

This is an implementation defect, separate from deciding transfer/turnout policy.
In two 50-elector colleges, A is filed in both and B only in the first. Requested
counts A=50, B=50 are feasible. Current reconstruction first spreads A 25/25,
leaving room for only 25 B votes, then throws. Reversing the list iteration order
allocates B=50 in the first and A=50 in the second and succeeds. Thus list order
can change execution success for exactly the same requested vote totals.
A transportation allocator is needed; sorting restricted lists first would not
solve arbitrary overlapping filing sets. This shared municipal reconstruction
also affects the original generator path; preserve historical archives while
designing an explicitly versioned fix.

### 6. New, ceased and unsupported identities require separate interpretation

A legacy list with even one positive historical correspondence is not “new” for
the per-chamber exception, including a joint ticket containing a new component.
Zero source-composition weight produces no positive correspondence although
structural membership remains. Classification can therefore change at zero or
after ticket regrouping. The approved per-chamber classification itself remains
unchanged; the limitation is whole-list classification versus component support.

Without mapped support or a reference, a new list stays at zero when other mapped
support exists. An explicit point reference can seed it; poll references with
empty donor IDs use a labelled essentially flat municipal seed. If the entire
municipality lacks mapped support, the equal-list fallback can instead give that
same unsupported list a positive share. Neither outcome is an observed local
profile. Selected new-component fallbacks are explicit and reference-backed,
but their territorial means/uncertainty are likewise assumptions, not identified
by a missing historical observation. Legacy proxy weights for opaque TAA groups
remain legacy-only assumptions and cannot validate the selected component split.

## Measured exposure in the installed default

Both chambers use 7,904 municipalities and 46,120,143 conditional electors from
the 2022 paired baseline. No municipality needs the missing-history regional
replacement. These figures precede polls and manual references:

| Quantity | Camera | Senate |
| --- | ---: | ---: |
| Historical valid votes | 28,150,536 | 28,122,616 |
| Raw valid mass without mapped/filed destination | 1,359,094 (4.828%) | 1,238,443 (4.404%) |
| Municipalities with incomplete raw retention | 7,896 | 7,849 |
| Equal-list fallback municipalities | 74 | 74 |
| Non-valid fraction under v5 legacy | 38.9626% | 39.0231% |
| Non-valid fraction using original projection policy on these inputs | 41.1734% | 41.7084% |
| Physical split municipalities / differing filing sets within them | 1 / 0 | 0 / 0 |

Raw missing mass differs from the old-versus-v5 total change because the original
policy renormalizes partial source rows. Default raw retained mass never exceeds
historical valid votes; the duplicated-municipal-support case above is a stress
case, not a measured current-default occurrence. The current default also does
not exercise heterogeneous filing within a split municipality.

National expected baseline changes against the original rule include FdI
**+360,887 Camera / +323,070 Senate**, PD **+267,173 / +223,472**, and AVS
**−304,011 / +45,853** votes. Changes are not uniformly positive: preserving
original source fractions before conditional redistribution changes relative
weights compared with renormalizing every surviving source row. The 74 fallback
municipalities give each of the four filed lists about **13,437 Camera / 13,590
Senate** votes nationally, including Futuro Nazionale before its reference inputs.

Integer baseline college differences include FdI **+19,249** in
`camera:P:050101`, AVS **−16,729** in `camera:P:030101`, and Freiheitlichen
**+26,522** in `senato:U:040006`. The complete national list totals and eight largest
absolute college differences per chamber are in the snapshot. These are policy
sensitivity results; they do not estimate seat changes or predictive accuracy.
Poll reconciliation can constrain national margins while retaining different
local profiles, so these baseline changes must not be presented as final forecast
changes. No empirical coverage or local calibration claim follows from this audit.

## Proposed corrections, before implementation

1. **Make turnout and transfer semantics explicit.** Decide whether omission
   means political redistribution, retained non-valid odds, or an authored
   alternative. Separate missing mapping from an intentionally ceased identity.
   Keep historical valid preservation available, but expose retained mass,
   amplification, fallback counts and unsupported components. Correct the
   documentation's equivalence claim now; any numerical change needs approval.
2. **Move legacy filing-sensitive work onto physical cells.** Apportion the
   conditional electorate first; compute correspondences, denominators and
   reference scopes on each ballot. Project national poll support as component
   support or use an explicit territorial allocation assumption before deriving
   ticket totals. Never split national component support equally just because
   it has multiple territorial labels. Retain the legacy stochastic law if
   that remains the intended alternative.
3. **Use joint capacity and a transportation allocator.** Validate overlapping
   reference targets before drawing and allocate integer counts with complete
   feasible-flow support. Distinguish genuinely infeasible margins from the
   current greedy failure. Specify treatment of stochastic draws crossing filing
   capacity; do not silently clip, retry or transfer them.
4. **Require reviewable fallback assumptions.** Keep regional/equal fallback
   explicit, summarize affected valid mass, and decide whether high-amplification
   or unsupported cases need an authored profile/reference. New-component
   support within a joint list and uncertainty around proxies need a separate
   design; no automatic epsilon or invented measurement should supply it.

Implement approved corrections under new policy/provenance identifiers with
characterization tests retaining the old behavior, checkpoint binding and
archive replay checks. Then repeat affected default/boundary tests and model
campaigns. Audit completion does not authorize those numerical changes, FV-09
cleanup, or public deployment.
