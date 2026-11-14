<script lang="ts">
  import type { PoliticsScenario, Subject, Placement } from '../scenario/schema';
  import { ROSATELLUM } from '../scenario/schema';
  import { locationKey } from '../scenario/validation';
  import { removePlacement, upsertPlacement } from '../scenario/edits';
  import { sameSubject, effectivePlacements } from '../scenario/effective';
  import { filingNodes, type FilingNode, type EditorAssets } from './assets';
  import { candidateReason, type ApplyEdit } from './model';
  import CandidateForm from './CandidateForm.svelte';
  let { scenario, owner, node, assets, edit }: { scenario: PoliticsScenario; owner: Subject; node: FilingNode; assets: EditorAssets; edit: ApplyEdit } = $props();
  let creating = $state(false), editing = $state<string | null>(null), search = $state('');
  let law = $derived(scenario.selection.scrutiny.id);
  let role = $derived(node.role!);
  let pins = $derived(scenario.lawSettings[law].placements.filter(p => sameSubject(p.owner, owner) && p.role === role && locationKey(p.location) === locationKey(node.location)));
  let active = $derived(new Set(effectivePlacements(scenario, law, assets.context.geographyRelations).map(p => p.id)));
  let limit = $state(12);
  function slot(position: number): Placement {
    const old = pins.find(p => p.position === position);
    const common = { id: old?.id ?? `pin-${owner.kind}-${owner.id}-${locationKey(node.location)}-${role}-${position}`, candidateId: '',
      location: node.location, owner, position, affiliationListId: owner.kind === 'list' ? owner.id : null };
    return law === ROSATELLUM ? { ...common, role: role as 'uninominal' | 'plurinominal', minority: false } : { ...common, role: role as 'ordinary' | 'bonus' | 'special' };
  }
  function choose(position: number, id: string) {
    const pin = slot(position);
    const ordinary = role === 'bonus' ? scenario.lawSettings[law].placements.find(p => p.candidateId === id && p.role === 'ordinary' && p.owner.kind === 'list' &&
      (owner.kind === 'list' ? p.owner.id === owner.id : scenario.subjects.lists.some(l => l.id === p.owner.id && l.coalitionId === owner.id)) &&
      assets.context.geographyRelations.some(r => locationKey(r.parent) === locationKey(node.location) && locationKey(r.child) === locationKey(p.location))) : null;
    const affiliationListId = ordinary?.owner.id ?? pin.affiliationListId;
    edit(s => id ? upsertPlacement(s, law, { ...pin, candidateId: id, affiliationListId }, assets.context) : removePlacement(s, law, pin.id));
  }
  function affiliation(candidateId: string) {
    const p = scenario.lawSettings[law].placements.find(p => p.candidateId === candidateId && p.owner.kind === 'list');
    return p ? scenario.subjects.lists.find(l => l.id === p.owner.id)?.name : null;
  }
  function bonusFor(pin: Placement): Placement | null {
    if (pin.role !== 'ordinary') return null;
    const parent = assets.context.geographyRelations.find(r => locationKey(r.child) === locationKey(node.location))?.parent;
    const bonusNode = assets.filings[law].flatMap(filingNodes).find(n => parent && locationKey(n.location) === locationKey(parent) && n.role === 'bonus');
    if (!bonusNode) return null;
    const list = scenario.subjects.lists.find(l => l.id === owner.id)!;
    const bonusOwner: Subject = list.coalitionId ? { kind: 'coalition', id: list.coalitionId } : owner;
    const existing = scenario.lawSettings[law].placements.find(p => p.role === 'bonus' && p.candidateId === pin.candidateId && sameSubject(p.owner, bonusOwner) && locationKey(p.location) === locationKey(bonusNode.location));
    if (existing) return existing;
    const positions = scenario.lawSettings[law].placements.filter(p => p.role === 'bonus' && sameSubject(p.owner, bonusOwner) && locationKey(p.location) === locationKey(bonusNode.location)).map(p => p.position);
    const position = Array.from({ length: bonusNode.slots }, (_, i) => i + 1).find(p => !positions.includes(p));
    if (!position) return null;
    return { id: `bonus-${pin.id}`, candidateId: pin.candidateId, location: bonusNode.location, owner: bonusOwner, role: 'bonus', position, affiliationListId: owner.id };
  }
</script>
<p>Posti non fissati: generazione automatica. Rimuovere una scelta libera solo quel posto; gli altri non vengono spostati.</p>
<label>Cerca candidato<input type="search" bind:value={search} /></label>
{#each Array.from({ length: Math.min(node.slots, limit) }, (_, i) => i + 1) as position (position)}
  {@const pin = pins.find(p => p.position === position)}
  <div class="candidate-slot">
    <label>Candidato {position}
      <select value={pin?.candidateId ?? ''} onchange={e => choose(position, e.currentTarget.value)}>
        <option value="">Genera automaticamente</option>
        {#each scenario.candidates.filter(c => c.id === pin?.candidateId || c.name.toLocaleLowerCase('it').includes(search.toLocaleLowerCase('it'))) as candidate (candidate.id)}
          {@const reason = candidateReason(scenario, candidate, slot(position), assets.context)}
          <option value={candidate.id} disabled={!!reason}>{candidate.name}{affiliation(candidate.id) ? ` · ${affiliation(candidate.id)}` : ''}{reason ? ` — ${reason}` : ''}</option>
        {/each}
      </select>
    </label>
    {#if pin}<span class="badge">{active.has(pin.id) ? 'Fissato' : 'Fissato, inattivo'}</span><button onclick={() => editing = pin.candidateId}>Modifica candidato</button>{/if}
    {#if pin && 'minority' in pin}<label><input type="checkbox" checked={pin.minority} onchange={e => edit(s => upsertPlacement(s, law, { ...pin, minority: e.currentTarget.checked }, assets.context))} />Candidatura di minoranza</label>{/if}
    {#if pin && bonusFor(pin)}
      {@const bonus = bonusFor(pin)!}
      {@const selected = scenario.lawSettings[law].placements.some(p => p.id === bonus.id)}
      {@const reason = candidateReason(scenario, scenario.candidates.find(c => c.id === pin.candidateId)!, bonus, assets.context)}
      <label><input type="checkbox" checked={selected} disabled={!selected && !!reason} onchange={e => edit(s => e.currentTarget.checked ? upsertPlacement(s, law, bonus, assets.context) : removePlacement(s, law, bonus.id))} />Nel listino della circoscrizione{reason && !selected ? ` · ${reason}` : ''}</label>
    {/if}
  </div>
{/each}
{#if node.slots > limit}<button onclick={() => limit += 12}>Altri posti</button>{/if}
<button onclick={() => creating = true}>Crea candidato</button>
{#if creating}<CandidateForm {scenario} {edit} done={() => creating = false} />{/if}
{#if editing}{#key editing}<CandidateForm {scenario} candidate={scenario.candidates.find(c => c.id === editing)} {edit} done={() => editing = null} />{/key}{/if}
