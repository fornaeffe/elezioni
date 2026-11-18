<script lang="ts">
  import { chambers, type PoliticsScenario, type Subject, type ExplicitBonusLink } from '../scenario/schema';
  import { resolveBonusLinks, eligibleBonusEndpoint, sameSubject } from '../scenario/effective';
  import { removeBonusLink, replaceBonusLink } from '../scenario/edits';
  import { subjectName, type ApplyEdit } from './model';
  let { scenario, subject, edit }: { scenario: PoliticsScenario; subject: Subject; edit: ApplyEdit } = $props();
  let pending = $state.raw<ExplicitBonusLink | null>(null);
  let subjects = $derived([...scenario.subjects.coalitions.map(c => ({ kind: 'coalition' as const, id: c.id })), ...scenario.subjects.lists.map(l => ({ kind: 'list' as const, id: l.id }))]);
  let links = $derived(resolveBonusLinks(scenario).filter(p => sameSubject(p.camera, subject) || sameSubject(p.senato, subject)));
  let replacements = $derived(pending ? scenario.lawSettings['politiche-reform-2026-v1'].bonusLinks.filter(p => sameSubject(p.camera, pending!.camera) || sameSubject(p.senato, pending!.senato)) : []);
  const reason = (value: string) => value.replace('camera:', 'Camera: ').replace('senato:', 'Senato: ').replace('automatic-counterpart', 'collegamento automatico per presenza in entrambe le camere').replace('endpoint-chamber-or-affiliation', 'camera dichiarata o affiliazione non eleggibile');
</script>
<p>I collegamenti conservati si riattivano automaticamente quando entrambi gli estremi tornano eleggibili. Il rinnovo di una sola camera conserva le dichiarazioni.</p>
{#each links as link (`${link.status}:${link.camera.kind}:${link.camera.id}:${link.senato.kind}:${link.senato.id}`)}
  <p>{subjectName(scenario, link.camera)} ↔ {subjectName(scenario, link.senato)} ·
    {link.status === 'automatic' ? 'Automatico' : link.status === 'explicit-active' ? 'Esplicito attivo' : 'Esplicito conservato, inattivo'}
    {#if link.reasons.length}— {link.reasons.map(reason).join('; ')}{/if}
  </p>
  {#if link.status !== 'automatic'}<button onclick={() => edit(s => removeBonusLink(s, link))}>Rimuovi collegamento conservato</button>{/if}
{/each}
{#each chambers as chamber (chamber)}
  {#if eligibleBonusEndpoint(scenario, subject, chamber)}
    {@const opposite = chamber === 'camera' ? 'senato' : 'camera'}
    <label>Controparte {opposite === 'camera' ? 'Camera' : 'Senato'}
      <select value="" onchange={e => { const counterpart = subjects.find(x => `${x.kind}:${x.id}` === e.currentTarget.value); pending = counterpart ? (chamber === 'camera' ? { camera: subject, senato: counterpart } : { camera: counterpart, senato: subject }) : null; }}>
        <option value="">Scegli una controparte</option>
        {#each subjects.filter(x => eligibleBonusEndpoint(scenario, x, opposite)) as item (`${item.kind}:${item.id}`)}
          <option value={`${item.kind}:${item.id}`}>{subjectName(scenario, item)} ({item.kind === 'list' ? 'lista autonoma' : 'coalizione'})</option>
        {/each}
      </select>
    </label>
  {/if}
{/each}
{#if pending}
  <p>Nuovo accordo: {subjectName(scenario, pending.camera)} ↔ {subjectName(scenario, pending.senato)}.</p>
  {#if replacements.length}<p role="status">Saranno sostituiti: {replacements.map(p => `${subjectName(scenario, p.camera)} ↔ ${subjectName(scenario, p.senato)}`).join('; ')}.</p>{/if}
  <button onclick={() => { const pair = pending!; edit(s => replaceBonusLink(s, pair).scenario); pending = null; }}>Conferma collegamento</button>
  <button onclick={() => pending = null}>Annulla collegamento</button>
{/if}
