<script lang="ts">
  import type { Snippet } from 'svelte';
  import { provideStatus } from '$lib/app-status.svelte';
  import AppStatus from '$lib/AppStatus.svelte';
  const status = provideStatus();
  let { children }: { children: Snippet } = $props();
</script>
<AppStatus {status} />
{@render children()}
<style>
  :global(body) { margin: 0; min-height: 100vh; background: #f4f7f8; color: #15232d; font-family: system-ui, sans-serif; }
  :global(*), :global(*::before), :global(*::after) { box-sizing: border-box; }
</style>
