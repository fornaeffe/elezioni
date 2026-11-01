<script lang="ts">
  import type { ReleasedScenario } from './release';
  import { generationModels, modelRegistry } from './model-registry';
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
          'M02' | 'M01';
        patch((n) => {
          n.generation!.model = value;
          n.generation!.modelVersion = modelRegistry[value].version;
        });
      }}
    >
      {#each generationModels as model (model.id)}<option value={model.id}
          >{model.label}</option
        >{/each}
    </select></label
  >
  {#if scenario.generation.model === 'M01'}
    <p>
      M01 usa l’ultima coppia Camera/Senato disponibile con elettorato 18+ e i
      sondaggi Camera. I riferimenti restano fissi alla data di riferimento;
      seguono shock nazionali, regionali e locali per componente.
      Gli altri contesti, le deviazioni standard dei riferimenti, i moltiplicatori
      di incertezza e la stima dei profili sono controlli di M02.
    </p>
  {/if}
  <details>
    <summary>Osservazioni parziali dichiarate nel M01</summary>
    <p>
      Questi punti nominativi integrano i sondaggi completi selezionati. Non
      sono osservazioni complete utilizzabili per la stima del M02.
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
          />Usa nel M01</label
        >
        <label
          >Fine rilevazione M01<input
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
