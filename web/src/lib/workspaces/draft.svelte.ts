import { onMount } from 'svelte';

export function confirmWorkspaceReset(): boolean {
  return window.confirm('Ripristinare lo scenario predefinito? Le modifiche allo scenario corrente saranno perse. I risultati già calcolati saranno conservati.');
}

/** Draft keys belong to the adapter. A reset never mutates an executed scenario. */
export function persistWorkspaceDraft<T>(options: {
  key: string; read: () => T; restore: (draft: T) => void;
  parse: (text: string) => T; serialize: (draft: T) => string; error: (error: Error) => void;
}) {
  let ready = $state(false);
  onMount(() => {
    try { const saved = localStorage.getItem(options.key); if (saved) options.restore(options.parse(saved)); }
    catch (e) { options.error(e instanceof Error ? e : new Error(String(e))); }
    finally { ready = true; }
  });
  $effect(() => {
    if (!ready) return;
    const text = options.serialize(options.read());
    try { localStorage.setItem(options.key, text); }
    catch (e) { options.error(e instanceof Error ? e : new Error(String(e))); }
  });
}
