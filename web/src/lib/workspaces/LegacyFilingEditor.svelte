<script lang="ts">
  import type { Scenario } from '../core/types';
  import { generatedPoliticsCandidateSlots } from '../scenario/politics-candidate-slots.generated';
  let { scenario = $bindable(), disabled = false }: { scenario: Scenario; disabled?: boolean } = $props();
  const chambers = ['camera', 'senato'] as const;
  const choices = Object.fromEntries(chambers.map(ch => [ch, [...new Map(generatedPoliticsCandidateSlots.uninominal
    .filter(r => r.ramo === ch).map(r => [r.circCode, { code: r.circCode, name: r.circLabel }])).values()]])) as
      Record<'camera' | 'senato', { code: string; name: string }[]>;
  function update(id: string, ch: 'camera' | 'senato', codes: string[]) {
    scenario.lists = scenario.lists.map(l => l.id === id ? { ...l,
      filing: { camera: l.filing?.camera ?? choices.camera.map(c => c.code), senato: l.filing?.senato ?? choices.senato.map(c => c.code), [ch]: codes } } : l);
  }
</script>
<details>
  <summary>Presentazione territoriale e minoranze</summary>
  <p>Seleziona le circoscrizioni in cui ciascuna lista si presenta. Le dichiarazioni di minoranza sono ipotesi dello scenario.</p>
  {#each scenario.lists.filter(l => l.kind !== 'other') as list (list.id)}
    <details><summary>{list.name}</summary>
      <div class="filing-grid">
        {#each chambers as ch (ch)}
          <label>{ch === 'camera' ? 'Camera' : 'Senato'} — {list.name}
            <select multiple size="6" {disabled}
              bind:value={() => list.filing?.[ch] ?? choices[ch].map(c => c.code), codes => update(list.id, ch, codes)}>
              {#each choices[ch] as option (option.code)}<option value={option.code}>{option.name}</option>{/each}
            </select>
          </label>
        {/each}
        <label>Regione della minoranza — {list.name}
          <select value={list.minorityRegion ?? ''} {disabled}
            onchange={e => { const value = e.currentTarget.value; scenario.lists = scenario.lists.map(l => l.id === list.id ? { ...l, minorityRegion: value || undefined } : l); }}>
            <option value="">Nessuna dichiarazione</option>
            {#each choices.senato as option (option.code)}<option value={option.code.padStart(2, '0')}>{option.name}</option>{/each}
          </select>
        </label>
      </div>
    </details>
  {/each}
  {#if scenario.defaultSource.notes?.length}<details><summary>Fonti e ipotesi del predefinito</summary>
    <ul>{#each scenario.defaultSource.notes as note, i (i)}<li>{note}</li>{/each}</ul>
  </details>{/if}
</details>
<style>
  details { margin: .75rem 0; }
  summary { cursor: pointer; font-weight: 650; }
  .filing-grid { display: flex; flex-wrap: wrap; gap: 1rem; margin: .75rem 0; }
  label { display: grid; gap: .4rem; }
  select { font: inherit; padding: .4rem; max-width: 24rem; }
</style>
