<script lang="ts">
  import type { ReleasedScenario } from './release';
  import { generationModels } from './correspondences';
  import { editFast } from './editor-model';
  import { pollChoiceId } from './correspondences';
  import SourceCompositionEditor from './SourceCompositionEditor.svelte';
  let {
    scenario,
    onchange,
    onerror,
  }: {
    scenario: ReleasedScenario;
    onchange: (s: ReleasedScenario) => void;
    onerror: (s: string) => void;
  } = $props();
  function patch(f: (s: ReleasedScenario) => void) {
    try {
      onchange(editFast(scenario, f));
    } catch (e) {
      onerror(String(e));
    }
  }
</script>

{#if scenario.generation}
  <label
    >Modello di generazione<select
      aria-label="Modello di generazione"
      value={scenario.generation.model}
      onchange={(e) => {
        const value = e.currentTarget.value as
          'selected' | 'legacy-poll-informed-v1';
        patch((n) => {
          n.generation!.model = value;
        });
      }}
    >
      {#each generationModels as model (model.id)}<option value={model.id}
          >{model.label}</option
        >{/each}
    </select></label
  >
  {#if scenario.generation.model === 'legacy-poll-informed-v1'}
    <p>
      Ultima coppia Camera/Senato disponibile con elettorato 18+. Aggiornamento
      dall’ultima pubblicazione, con le eccezioni dichiarate del confronto.
      Utilizza i sondaggi Camera; gli altri contesti restano del modello selezionato.
      Deviazioni standard dei riferimenti, moltiplicatori di incertezza e stima
      dei profili sono controlli del solo modello selezionato.
    </p>
  {/if}
  <details>
    <summary>Osservazioni parziali dichiarate nel confronto legacy</summary>
    <p>
      Questi punti nominativi integrano i sondaggi completi selezionati. Non
      sono osservazioni complete utilizzabili per la stima del nuovo modello.
      «Altri» non è una misura di un nuovo partito.
    </p>
    {#each scenario.generation.supplementalPolls as p, pi (p.id)}<fieldset>
        <legend>{p.id}</legend>
        <label
          ><input
            type="checkbox"
            checked={p.enabled}
            onchange={(e) => {
              const v = e.currentTarget.checked;
              patch((n) => {
                n.generation!.supplementalPolls[pi].enabled = v;
              });
            }}
          />Usa nel modello precedente</label
        >
        <label
          >Fine rilevazione legacy<input
            type="date"
            value={p.end}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.generation!.supplementalPolls[pi].end = v;
                n.generation!.supplementalPolls[pi].date = v;
              });
            }}
          /></label
        >
        <label
          >Pubblicazione legacy<input
            type="date"
            value={p.available}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.generation!.supplementalPolls[pi].available = v;
              });
            }}
          /></label
        >
        {#each p.groups as group, gi (gi)}<label
            >{group.label} %<input
              type="number"
              min="0"
              max="100"
              step="any"
              value={group.share * 100}
              onchange={(e) => {
                const v = e.currentTarget.valueAsNumber / 100;
                patch((n) => {
                  n.generation!.supplementalPolls[pi].groups[gi].share = v;
                });
              }}
            /></label
          >
          <SourceCompositionEditor
            {scenario}
            sourceId={pollChoiceId(p.id, group.label)}
            members={group.members}
            {onchange}
            {onerror}
          />
        {/each}
        <p>{p.source} · campione effettivo {p.sample}</p>
      </fieldset>{/each}
  </details>
{/if}
