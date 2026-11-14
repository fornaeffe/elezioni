<script lang="ts">
  import type { Snippet } from 'svelte';
  let { title, children }: { title: string; children: Snippet } = $props();
  let open = $state(false);
</script>
<details ontoggle={event => open = event.currentTarget.open}>
  <summary>{title}</summary>
  {#if open}{@render children()}{/if}
</details>
