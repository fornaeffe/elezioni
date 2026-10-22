<script lang="ts">
  import type { ReformGenerationScenario } from './generation-types';
  import { coalitionChambers, reformSubjects, subjectKey, subjectLabel } from './scenario-editor';
  import { upgradeReformDraft } from './presentation-metadata';
  let { scenario, disabled, onaction }: { scenario: ReformGenerationScenario; disabled: boolean; onaction: (action: () => ReformGenerationScenario) => void } = $props();
  let camera = $state('');
  let senato = $state('');
  function add() { onaction(() => {
    const a = reformSubjects(scenario, 'camera').find(s => subjectKey(s) === camera), b = reformSubjects(scenario, 'senato').find(s => subjectKey(s) === senato);
    if (!a || !b) throw new Error('Seleziona entrambi i soggetti del collegamento.');
    const n = upgradeReformDraft(scenario);
    if (n.bonusLinks.some(l => subjectKey(l.camera) === camera || subjectKey(l.senato) === senato)) throw new Error('Un soggetto ha già un collegamento: rimuovilo prima di sostituirlo.');
    n.bonusLinks = [...n.bonusLinks, { camera: a, senato: b, cameraDeclares: true, senatoDeclares: true }]; return n;
  }); }
</script>
<section aria-label="Collegamenti premio">
  <h3>Collegamenti per il premio congiunto</h3>
  <p>Le coalizioni dichiarate presenti in entrambi i rami sono collegate automaticamente quando hanno liste in entrambi. Gli altri collegamenti sono dichiarazioni reciproche esplicite.</p>
  {#if scenario.renewal !== 'joint'}<p>Il premio congiunto non si applica al rinnovo di un solo ramo. I collegamenti salvati restano disponibili per il successivo rinnovo congiunto.</p>{/if}
  {#each scenario.bonusLinks as link, index (subjectKey(link.camera))}<div class="editor-row">
    <span>{subjectLabel(scenario, link.camera, 'camera')} ↔ {subjectLabel(scenario, link.senato, 'senato')}</span>
    {#if link.camera.kind === 'coalition' && link.senato.kind === 'coalition' && link.camera.id === link.senato.id && coalitionChambers(scenario, link.camera.id).length === 2}<span>Automatico dalla presenza</span>{:else}
    <button type="button" {disabled} onclick={() => onaction(() => { const n = upgradeReformDraft(scenario); n.bonusLinks = n.bonusLinks.filter((_, i) => i !== index); return n; })}>Rimuovi collegamento {index + 1}</button>{/if}
  </div>{/each}
  <div class="editor-row">
    <label>Soggetto Camera<select bind:value={camera} {disabled}><option value="">Seleziona</option>{#each reformSubjects(scenario, 'camera') as subject (subjectKey(subject))}<option value={subjectKey(subject)}>{subjectLabel(scenario, subject, 'camera')}</option>{/each}</select></label>
    <label>Soggetto Senato<select bind:value={senato} {disabled}><option value="">Seleziona</option>{#each reformSubjects(scenario, 'senato') as subject (subjectKey(subject))}<option value={subjectKey(subject)}>{subjectLabel(scenario, subject, 'senato')}</option>{/each}</select></label>
    <button type="button" disabled={disabled || !camera || !senato} onclick={add}>Dichiara collegamento reciproco</button>
  </div>
</section>
