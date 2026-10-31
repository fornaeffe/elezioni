<script lang="ts">
  import type { ReleasedScenario } from './release';
  import { editFast } from './editor-model';
  import { equalComposition, normalizedComposition } from './correspondences';
  let {
    scenario,
    sourceId,
    members,
    onchange,
    onerror,
  }: {
    scenario: ReleasedScenario;
    sourceId: string;
    members: string[];
    onchange: (s: ReleasedScenario) => void;
    onerror: (message: string) => void;
  } = $props();
  const row = $derived(scenario.generation?.compositions[sourceId]);
  const ids = $derived(row?.proxy ? Object.keys(row.fractions) : members);
  const normalized = $derived.by(() => {
    try { return normalizedComposition(ids,row); } catch { return []; }
  });
  function apply(id?: string, value?: number) {
    try {
      onchange(
        editFast(scenario, (n) => {
          if (!n.generation)
            throw new Error('La composizione richiede uno scenario unificato.');
          const next = id
            ? {
                ...(row ?? equalComposition(ids)),
                fractions: {
                  ...(row ?? equalComposition(ids)).fractions,
                  [id]: value ?? 0,
                },
                provenance:
                  'Composizione modificata dall’utente; ipotesi, non conteggi osservati.',
              }
            : {
                ...equalComposition(ids),
                ...(row?.proxy ? { proxy: true } : {}),
              };
          normalizedComposition(ids, next);
          n.generation.compositions[sourceId] = next;
        }),
      );
    } catch (e) {
      onerror(String(e));
    }
  }
</script>

{#if scenario.generation && ids.length}
  <details>
    <summary
      >Composizione assunta per il modello precedente{row?.proxy
        ? ' · proxy esplicito'
        : ''}</summary
    >
    <p>
      La selezione dei componenti definisce l’appartenenza. Questi pesi relativi
      valgono solo per il modello precedente; il modello selezionato conserva
      osservazioni e raggruppamenti indivisibili.
    </p>
    {#each ids as id (id)}<label
        >{id} — peso relativo<input
          type="number"
          min="0"
          step="any"
          value={row?.fractions[id] ?? 1}
          onchange={(e) => apply(id, e.currentTarget.valueAsNumber)}
        /></label
      ><span>Quota normalizzata: {((normalized.find(r=>r.id===id)?.fraction??0)*100).toLocaleString('it-IT')}%</span>{/each}
    <button type="button" onclick={() => apply()}
      >Distribuisci in parti uguali</button
    >
    <p>
      {row?.provenance ??
        'Pesi uguali fra le identità riconosciute; ipotesi esplicita del modello precedente.'}
    </p>
  </details>
{/if}
