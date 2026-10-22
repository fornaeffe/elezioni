<script lang="ts">
  let { label, options, value, onchange, disabled = false }: {
    label: string; options: { id: string; label: string }[]; value: string;
    onchange: (value: string) => void; disabled?: boolean;
  } = $props();
  let search = $state('');
  const matches = $derived(options.filter(o => `${o.label} ${o.id}`.toLocaleLowerCase('it-IT').includes(search.toLocaleLowerCase('it-IT'))));
  const visible = $derived(matches.slice(0, 60));
</script>
<label>Cerca {label}<input type="search" bind:value={search} {disabled} /></label>
<label>{label}<select {value} onchange={(e) => onchange(e.currentTarget.value)} {disabled}>
  <option value="">Seleziona</option>
  {#if value && !visible.some(o => o.id === value)}<option {value}>{options.find(o => o.id === value)?.label ?? value}</option>{/if}
  {#each visible as option (option.id)}<option value={option.id}>{option.label}</option>{/each}
</select></label>
<small>{matches.length} risultati; fino a 60 visibili. Affina la ricerca per gli altri.</small>
