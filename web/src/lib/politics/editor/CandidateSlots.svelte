<script lang="ts">
  import { tick } from 'svelte';
  import Badge from '$lib/ui/Badge.svelte';
  import Dialog from '$lib/ui/Dialog.svelte';
  import SearchCombobox from '$lib/ui/SearchCombobox.svelte';
  import type { PoliticsScenario, Subject, Placement } from '../scenario/schema';
  import { ROSATELLUM } from '../scenario/schema';
  import { locationKey } from '../scenario/validation';
  import { removePlacement, upsertPlacement } from '../scenario/edits';
  import { sameSubject, effectivePlacements } from '../scenario/effective';
  import { filingNodes, type FilingNode, type EditorAssets } from './assets';
  import { candidateReason, type ApplyEdit } from './model';
  import CandidateForm from './CandidateForm.svelte';
  let { scenario, owner, node, assets, edit }: { scenario: PoliticsScenario; owner: Subject; node: FilingNode; assets: EditorAssets; edit: ApplyEdit } = $props();
  let createDialog = $state<HTMLDialogElement>();
  let creation = $state.raw<{ position: number; input: HTMLInputElement } | null>(null);
  let editing = $state<string | null>(null);
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
  function assign(s: PoliticsScenario, pin: Placement, id: string) {
    if (!id) return removePlacement(s, law, pin.id);
    const ordinary = role === 'bonus' ? s.lawSettings[law].placements.find(p => p.candidateId === id && p.role === 'ordinary' && p.owner.kind === 'list' &&
      (owner.kind === 'list' ? p.owner.id === owner.id : s.subjects.lists.some(l => l.id === p.owner.id && l.coalitionId === owner.id)) &&
      assets.context.geographyRelations.some(r => locationKey(r.parent) === locationKey(node.location) && locationKey(r.child) === locationKey(p.location))) : null;
    const affiliationListId = ordinary?.owner.id ?? pin.affiliationListId;
    return upsertPlacement(s, law, { ...pin, candidateId: id, affiliationListId }, assets.context);
  }
  function placeCreated(s: PoliticsScenario, position: number, id: string) {
    const pin = slot(position);
    const candidate = s.candidates.find(c => c.id === id)!;
    const reason = candidateReason(s, candidate, pin, assets.context);
    if (reason) throw new Error(reason);
    return assign(s, pin, id);
  }
  async function startCreation(position: number, input: HTMLInputElement) {
    creation = { position, input };
    await tick();
    createDialog?.showModal();
  }
  function endCreation() {
    const input = creation?.input;
    creation = null;
    input?.focus();
  }
  function affiliation(candidateId: string) {
    const p = scenario.lawSettings[law].placements.find(p => p.candidateId === candidateId && p.owner.kind === 'list');
    return p ? scenario.subjects.lists.find(l => l.id === p.owner.id)?.name : null;
  }
  function candidateOptions(position: number) {
    const pin = slot(position);
    return [
      { value: '', label: 'Genera automaticamente' },
      ...scenario.candidates.map(candidate => ({
        value: candidate.id,
        label: candidate.name,
        detail: affiliation(candidate.id) ?? undefined,
        disabledReason: candidateReason(scenario, candidate, pin, assets.context),
      })),
    ];
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
{#each Array.from({ length: Math.min(node.slots, limit) }, (_, i) => i + 1) as position (position)}
  {@const pin = pins.find(p => p.position === position)}
  <div>
    <SearchCombobox label={`Candidato ${position}`} value={pin?.candidateId ?? ''} options={candidateOptions(position)}
      onValueChange={id => edit(s => assign(s, slot(position), id))}
      action={role === 'bonus' ? undefined : { label: 'Crea candidato', run: input => void startCreation(position, input) }} />
    {#if pin}<Badge>{active.has(pin.id) ? 'Fissato' : 'Fissato, inattivo'}</Badge><button onclick={() => editing = pin.candidateId}>Modifica candidato</button>{/if}
    {#if pin && 'minority' in pin}<label data-field><input type="checkbox" checked={pin.minority} onchange={e => edit(s => upsertPlacement(s, law, { ...pin, minority: e.currentTarget.checked }, assets.context))} />Candidatura di minoranza</label>{/if}
    {#if pin && bonusFor(pin)}
      {@const bonus = bonusFor(pin)!}
      {@const selected = scenario.lawSettings[law].placements.some(p => p.id === bonus.id)}
      {@const reason = candidateReason(scenario, scenario.candidates.find(c => c.id === pin.candidateId)!, bonus, assets.context)}
      <label data-field><input type="checkbox" checked={selected} disabled={!selected && !!reason} onchange={e => edit(s => e.currentTarget.checked ? upsertPlacement(s, law, bonus, assets.context) : removePlacement(s, law, bonus.id))} />Nel listino della circoscrizione{reason && !selected ? ` · ${reason}` : ''}</label>
    {/if}
  </div>
{/each}
{#if node.slots > limit}<button onclick={() => limit += 12}>Altri posti</button>{/if}
{#if creation && role !== 'bonus'}
  {@const position = creation.position}
  <Dialog bind:element={createDialog} title="Crea candidato" onclose={endCreation} --dialog-width="36rem">
    <CandidateForm {scenario} {edit} closeLabel="Annulla" done={() => createDialog?.close()}
      afterCreate={(s, id) => placeCreated(s, position, id)} />
  </Dialog>
{/if}
{#if editing}{#key editing}<CandidateForm {scenario} candidate={scenario.candidates.find(c => c.id === editing)} {edit} done={() => editing = null} />{/key}{/if}
