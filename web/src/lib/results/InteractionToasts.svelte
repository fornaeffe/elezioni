<script lang="ts">
  let { messages, ondismiss }: { messages: { id: string; text: string }[]; ondismiss: (id: string) => void } = $props();
</script>
<aside class="toasts" aria-label="Messaggi di interazione">
  {#each messages as message (message.id)}
    <div class="toast" role="alert" data-testid={message.id === 'validation' ? 'reform-scenario-validation' : undefined}><p>{message.text}</p><button type="button" aria-label="Chiudi messaggio" onclick={() => ondismiss(message.id)}>×</button></div>
  {/each}
</aside>
<style>
  .toasts { position: fixed; top: 1rem; right: 1rem; z-index: 60; display: grid; gap: .5rem; width: min(28rem, calc(100vw - 2rem)); max-height: calc(100dvh - 7rem); overflow: auto; font: .85rem system-ui, sans-serif; }
  .toast { display: flex; align-items: start; gap: .5rem; padding: .8rem; border: 1px solid #c4822e; border-radius: .5rem; color: #7c2020; background: #fff7ec; box-shadow: 0 3px 16px #18202626; }
  p { margin: 0; flex: 1; overflow-wrap: anywhere; white-space: pre-line; }
  button { cursor: pointer; background: transparent; border: 0; color: inherit; font: 1.25rem system-ui; }
</style>
