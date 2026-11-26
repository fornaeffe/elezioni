<script lang="ts">
  import type { Snippet } from 'svelte';
  let { title, count, children }: {
    title: string;
    count?: { value: number; label: string };
    children: Snippet;
  } = $props();
  let open = $state(false);
</script>
<details ontoggle={event => open = event.currentTarget.open}>
  <summary>
    {title}{#if count && count.value > 0}
      <span class="summary-count">{` · ${count.value} ${count.label}`}</span>
    {/if}
  </summary>
  {#if open}{@render children()}{/if}
</details>

<style>
  .summary-count {
    color: #4d6272;
    font-size: 0.875em;
    font-weight: 400;
  }
</style>
