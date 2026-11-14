<script lang="ts">
  import { onMount } from 'svelte';
  import CommonPoliticsEditor from '$lib/politics/editor/CommonPoliticsEditor.svelte';
  import { createDefaultCommonPoliticsScenario } from '$lib/politics/scenario/defaults';
  import { readPoliticsDraft, writePoliticsDraft } from '$lib/politics/files/draft';
  import type { PoliticsScenario } from '$lib/politics/scenario/schema';
  let scenario = $state.raw(createDefaultCommonPoliticsScenario()), error = $state(''), ready = $state(false);
  function change(next: PoliticsScenario) {
    // Storage failure retains the currently reviewable in-memory edit and is never reported as saved.
    scenario = next;
    try { writePoliticsDraft(localStorage, next); error = ''; } catch (e) { error = `Bozza non salvata: ${String(e)}`; }
  }
  onMount(() => { try { scenario = readPoliticsDraft(localStorage) ?? scenario; } catch (e) { error = `Bozza salvata non caricata: ${String(e)}`; } ready = true; });
</script>
<svelte:head><title>Editor comune · Elezioni politiche</title></svelte:head>
{#if error}<p role="alert">{error}</p>{/if}
{#if ready}<CommonPoliticsEditor {scenario} onchange={change} />{/if}
