# Post-FV-08 correspondence-policy audit

Completed 1 November 2026 against the current v5 implementation. **The audit is
complete; the policy is not validated as generally equivalent between models.**
The correction list below records agreed decisions and remaining proposals.
Implementation is deferred until all correspondence-policy decisions are settled,
then proceeds in one coordinated pass. The future web app includes only the
latest generator algorithms, with no older-archive compatibility requirement.
Earlier generator code may be archived outside the browser bundle for terminal
comparison tests. Current behavior below is audit evidence, not a requirement
to retain old algorithms in the web app.

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

1. **Use one offered-choice-set policy — agreed 1 November 2026.** Treat a
   component known to be absent from the current ballot or poll response options
   identically whether it has ceased to exist or still exists but is not offered.
   Its supporters must choose another offered alternative or abstain/not respond;
   we have no identified substitution pattern for them. Do not automatically
   preserve historical valid-vote totals.

   Use proportional conditioning as the central assumption: for the offered set
   $O$, set $p'_j=p_j/\sum_{k\in O}p_k$ for $j\in O$, and zero for unavailable
   options. The electoral choice set includes non-participation/non-valid support.
   This preserves relative probabilities among the remaining alternatives,
   reallocating removed support to both political choices and non-valid support.
   For example, $(0.40,0.27,0.33)$ becomes $(0.40/0.73,0,0.33/0.73)$ when the
   first political option disappears. These proportions are a modelling
   assumption, not observed transfers; the uncertainty treatment remains to be
   settled and must not imply that substitution is identified.

   For polls, apply conditioning to the represented response set and retain the
   published denominator. Voting-intention percentages conditional on an expressed
   intention do not identify nonresponse/undecided probabilities. Do not infer
   those probabilities from political percentages or substitute electoral
   abstention for poll nonresponse.

   Known absence from an offered set is distinct from an unresolved identity or
   correspondence in the data: unresolved data must not silently become evidence
   of absence. **The scenario must specify an explicit reference for every
   component without historical records or genuinely unidentifiable from the
   available evidence under the applicable model**, including such a component
   inside a joint list. Correction 4 specifies how legacy compositions and
   selected-model identification determine the available information. A
   missing required reference makes the scenario invalid; automatic zero, equal
   allocation or an implicit donor profile cannot replace it. Political
   references must target components in both models, as specified in
   correction 2; uncertainty/profile details remain for the coordinated design.
   Expose retained mass, amplification, fallback counts and unsupported components
   so the adopted assumptions remain reviewable.
2. **Generate legacy votes by component, then aggregate to filed lists — agreed
   1 November 2026.** Replace future-list-based legacy preparation and generation
   with the following component-based pipeline; this is a revised legacy-derived
   generator, not a claim of numerically identical old forecasts:

   - Compile historical component votes for each selected election and
     municipality using the mandatory legacy compositions. Preserve original
     grouped counts, offered-set masks and observation/decomposition provenance.
   - Apply correction 4's component-level local/regional/national support rules,
     required references and correction 5's observed-zero treatment. Calculate
     national probabilities and geographic effects per component.
   - Keep polls attached to measured components/groups, with their original
     denominators, dates, completeness and provenance. Reconcile national support
     using the territorial component profiles; do not divide a component's poll
     share by the number of future territorial ticket labels. A poll describing
     one offered set does not impose the same totals after conditioning onto a
     different future offered set.
   - Apply national, regional and municipal movement shocks per stable component
     identity. **Different components' movement innovations stay independent.**
     Retain the existing shock-scale settings initially; do not estimate new
     component scales or copy the selected model's fitting algorithm as part of
     this decision. A component's national/regional shocks remain shared across
     the corresponding geography, and retain the existing within-component
     Camera/Senate dependence settings.
   - Reconstruct physical-cell electorates before integer vote generation.
     Municipalities with homogeneous filing may use one equivalent cell; split
     municipalities require their actual ballot-specific cells. After shocks,
     remove components not offered in each cell and proportionally condition
     the complete remaining vector, including non-valid support, per correction 1.
   - Generate a joint electorate-conserving allocation to available components
     and non-valid support within each cell, then sum component counts into the
     list carrying them there. Never generate unavailable component counts and
     try to relocate them afterwards. Derived territorial list totals are
     outputs, not separate invented national poll targets.

   Independence concerns movement innovations before normalization. Probability
   normalization, shared observations and references, and count conservation
   naturally induce dependence in resulting component/list shares. Summing
   independent component movements also changes joint-list uncertainty relative
   to the old single-list shock; this needs validation, not an assertion of
   unchanged calibration.

   **Shared poll measurement uncertainty — adopted treatment.** Draw one
   measurement-error realization for each original reported poll group per
   simulation, using the existing measurement-error magnitude/floor at that
   group's level. Obtain its perturbed share under the question's valid
   denominator/completeness rules, then split it using the fixed declared legacy
   composition. For group share $G$ and weights $w_k$, component contributions
   are $w_kG$: they inherit the same group error, not independently sampled
   component errors. Fixed compositions introduce no extra composition-error
   draws. Preserve dependence required by a complete question's total; partial
   named observations must not be silently normalized into a complete survey.
   Reuse the same observation realization across all cells and ticket labels,
   and across chambers when the same observation is reused. Independent
   component movement shocks remain separate from this shared measurement error.
   The selected model retains grouped observation evidence/uncertainty rather
   than receiving the assumed legacy split as independent measurements.

   **Scenario political references must be component-level in both models.**
   Permit references to historical/fitted as well as new components, retaining
   the applicable scope, chamber and uncertainty semantics. Mandatory-reference
   cases and geographic coverage are specified in correction 4; this does not
   require a manual reference for every already identified component. Do not
   implement a list-level reference control or implicit list-to-component
   translation now: whether to offer one and how to translate it remain for later
   discussion. Non-valid references retain their separate meaning. National
   component/group calibration uses territorial profiles and the observation's
   denominator; poll calibration alone supplies no turnout/nonresponse estimate.
3. **Validate physical reference capacity and generate counts directly in cells —
   agreed 1 November 2026.** Correction 2 resolves the causes of findings 4 and 5:
   use each physical cell's electorate and offered components, never the union
   of filings across a municipality. Condition probabilities on that cell's
   choice set before generating electorate-conserving component counts; aggregate
   those counts to filed lists and colleges by summation.

   Remove municipal-to-college greedy vote relocation from the active path.
   No replacement transportation allocator for relocating municipal vote totals
   is required: counts already belong to their destination cells, so list order
   cannot consume another list's capacity during reconstruction. Component-based
   generation at municipality level followed by relocation would not resolve
   these findings and is not the agreed design.

   Retain individual **and joint** physical filing-capacity validation for imposed
   reference constraints, using their applicable geographic scopes and
   denominators. A flow-based feasibility check may serve this purpose; it is
   distinct from relocating generated votes. Direct cell generation does not make
   every requested national/territorial component reference feasible. Report
   incompatible constraints explicitly rather than silently clipping, retrying
   draws or transferring votes. Keep structural filing zeros and cell electorate
   conservation in every draw; do not impose reference margins as exact realized
   integer totals unless separately specified by their semantics.
4. **Choose historical support by observation and identification — agreed
   1 November 2026.** Apply the following rules
   component by component, before conditioning on the current offered set:

   | Historical information in the municipality | Agreed treatment |
   | --- | --- |
   | Identifiable positive support | Use that local evidence even when tiny; do not replace it merely because its magnitude is low |
   | Identifiable zero support | Use the observed zero as evidence, distinct from missing information; apply the legacy half-vote logit treatment in correction 5 without triggering a missing-history fallback |
   | Component never offered locally | Estimate support from usable regional evidence, or national evidence if regional evidence is unavailable |
   | Component offered inside a joint list | Legacy uses mandatory scenario-defined component proportions; selected uses identifiable group evidence and explicit references where the overall evidence cannot identify individual components |

   Regional/national availability means usable evidence, not positive support:
   an identifiable regional zero must not trigger a search for positive national
   support. Use historical offered-set denominators; municipalities where a
   component was not offered are not observations of zero support. Local support
   for one component must not suppress geographic inference for another. This
   replaces the municipality-wide zero-versus-positive mapped-mass switch and
   settles the discontinuous fallback policy. The equal-list emergency allocation
   in finding 2 must be removed: establish support per component, then aggregate
   it to lists. Merely splitting or joining list labels must not redistribute
   another component's support by changing the number of lists. This settles
   that fallback's list-packaging issue; geographic projection onto different
   territorial tickets is addressed by the agreed redesign in correction 2.

   **Legacy joint-list compositions are mandatory.** Explicit proportions provide
   an assumed decomposition of observed historical joint-list votes, including
   explicit zero proportions where intended. Missing compositions must not
   silently become equal weights. The decomposed component counts are usable
   legacy inputs, not separately observed votes or statistically identified
   splits. Historical counts and those proportions remain fixed preparation
   inputs without their own uncertainty distributions; future vote shocks and
   poll measurement uncertainty are separate. Existing legacy generation bypasses
   the logarithm for exact zero support and preserves it unless reference
   intervention changes it.

   **Reference coverage is mandatory.** A genuinely new component needs a national
   reference share, or territorial reference shares collectively covering every
   area where it is filed in each applicable renewed chamber. The same requirement
   applies to components genuinely unidentifiable from the selected model's
   overall historical/poll evidence. Appearing jointly in one municipality does
   not alone make a component unidentifiable if other evidence identifies it.
   Legacy's mandatory historical compositions supply the assumed internal split,
   but do not replace the current reference required for a new component.
   References target components in both models per correction 2, including
   components inside future joint tickets; a reference somewhere outside part of
   the component's filed domain is insufficient.
   Missing required references invalidate the scenario; implicit zero, equal
   allocation or donor assumptions cannot replace them.

   **Selected-model adjustments for the coordinated pass:** preserve original
   local offered/identification information through observable-group coarsening;
   retain regional inference and national-field fallback for missing local
   evidence; and support reference-backed unidentified component splits while
   preserving the observed joint-group totals. Merely adding a reference does
   not currently resolve the source compiler's unidentified fitted basis. Extend
   reference validation to check national or complete territorial/chamber
   coverage. Legacy also needs component-level geographic inference and reference
   application inside joint tickets; its current reference adapter supports
   component references directly only for singleton future lists.

   Selected zero-count smoothing, estimation uncertainty and temporal evolution
   remain compatible with using observed zero evidence: the decision does not
   make its future probability permanently zero or require exact reproduction of
   historical local shares. Exact territorial estimators, reference-backed split
   representation and uncertainty rules remain coordinated design work. Label
   geographic/profile assumptions and summarize affected support; no invented
   measurement should supply missing information. Other policy decisions remain
   unsettled.
5. **Regularize legacy observed-zero support with a half vote — agreed
   1 November 2026; implementation deferred.** After decomposing historical lists
   into components, use a half-vote probability when preparing the local
   historical logit for a component with identifiable zero historical support.
   This replaces the earlier future-list-level wording: regularize each
   component before future list aggregation, not the aggregated ticket.
   For historical electorate $N>0$:

   $$
   \widetilde p_{\mathrm{local}}=\frac{0.5}{N}.
   $$

   In the original national-plus-local-delta formulation, this corresponds to
   $\delta_{\mathrm{local}}=\operatorname{logit}(\widetilde p_{\mathrm{local}})
   -\operatorname{logit}(p_{\mathrm{national}})$, where the national baseline is
   supported and its logit is defined. It supplies a finite negative local effect
   that can evolve under national changes and territorial shocks. The current
   reform legacy path works with prepared municipal probabilities rather than
   the same explicitly stored R delta: install the equivalent regularized
   historical support **before poll/reference reconciliation**, not as a blanket
   replacement of zero in the final generation branch. Nationally unsupported
   components still require the references specified above.

   Apply these boundaries in the coordinated implementation:

   - Regularize identifiable historical zeros only. Never-offered local support
     still follows regional/national inference; missing history is not zero.
   - Preserve raw historical counts as zero. The half vote is a logit-calculation
     assumption, not an added historical observation; retain the corresponding
     historical electorate denominator through geographic/electorate conversion.
   - Keep positive counts unchanged, including positive fractional counts below
     0.5 produced by declared compositions. This is a zero-only rule, not an
     additive half vote for every count or a minimum half-vote floor for all
     positive counts. At the fractional-count boundary, zero uses the half-vote
     calculation while a tiny positive count uses its own value.
   - Apply only to alternatives filed in the current physical choice set.
     Unfiled choices remain impossible. Preserve explicit zero reference
     interventions and zeros required by filing/capacity constraints; historical
     regularization must not revive those choices later in generation.
   - Conserve the final electorate in integer allocation. A positive probability
     can still round to zero votes in a particular draw; the aim is to remove
     permanent historical-zero exclusion, not guarantee a positive integer count.

   The original R preparation (`R/calcolo_parametri_input.R`) and its TypeScript
   parity path (`web/src/lib/politics/parameter-preparation.ts`) already use a
   half-vote floor for municipal logits. That is precedent, not the exact agreed
   rule: those paths also floor positive fractions below half a vote, whereas
   this decision leaves them unchanged. The selected model retains its existing
   zero-count smoothing; this update concerns reform legacy only. Bind the new
   treatment to new policy/provenance identifiers; older generators are only
   retained outside the browser bundle for terminal comparison if useful.

6. **Keep the web app on latest generator algorithms only — agreed
   1 November 2026.** Do not plan backward-compatible execution, import/replay
   adapters or old generator routing for earlier archives in the web app. Keep
   accurate current-run provenance and current-format exports. Earlier production
   algorithm code may stay archived for terminal comparison tests, with no
   transitive imports or generated inclusion in the browser/worker bundle.
   This supersedes the earlier requirement to preserve v1–v3 web routing and
   browser replay compatibility; historical evidence and comparison fixtures
   are not authorization to bundle the old implementations.

Record all remaining policy decisions before implementing any correction. Then
implement the complete agreed set in one coordinated pass under new
policy/provenance identifiers with terminal comparison/characterization tests,
current checkpoint binding and latest-format export/import checks. Verify that
archived generator implementations are absent from browser/worker dependencies.
Then repeat affected default/boundary tests and model campaigns. Agreement on an
individual policy does not authorize early code,
default or data changes. FV-09 cleanup and public deployment remain separate.
