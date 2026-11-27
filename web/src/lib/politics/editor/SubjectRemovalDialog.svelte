<script lang="ts">
  import { Trash2 } from '@lucide/svelte';
  import { tick } from 'svelte';
  import { OTHER_LIST_ID } from '$lib/core/other-list';
  import Dialog from '$lib/ui/Dialog.svelte';
  import type { PoliticsScenario, Subject } from '../scenario/schema';

  let { scenario, onconfirm }: {
    scenario: PoliticsScenario;
    onconfirm: (subject: Subject, reviewed: PoliticsScenario, onError: (message: string) => void) => boolean;
  } = $props();

  let dialog = $state<HTMLDialogElement>();
  let pending = $state.raw<{
    subject: Subject;
    name: string;
    reviewed: PoliticsScenario;
    trigger: HTMLButtonElement;
    nextFocus: () => HTMLElement | undefined;
  } | null>(null);
  let error = $state('');

  export async function open(subject: Subject, trigger: HTMLButtonElement, nextFocus: () => HTMLElement | undefined) {
    if (pending) return;
    const rows = subject.kind === 'coalition' ? scenario.subjects.coalitions : scenario.subjects.lists;
    const row = rows.find(row => row.id === subject.id);
    if (!row) return;
    pending = { subject, name: row.name, reviewed: scenario, trigger, nextFocus };
    await tick();
    dialog?.showModal();
  }

  function confirmRemoval() {
    if (!pending) return;
    if (onconfirm(pending.subject, pending.reviewed, message => error = message)) dialog?.close('confirmed');
  }

  function finishRemoval(event: Event & { currentTarget: HTMLDialogElement }) {
    const request = pending;
    pending = null;
    error = '';
    if (!request) return;
    const destination = event.currentTarget.returnValue === 'confirmed' || !request.trigger.isConnected
      ? request.nextFocus() : request.trigger;
    destination?.focus();
  }
</script>

{#if pending}
  {@const { subject, name, reviewed } = pending}
  {@const coalition = subject.kind === 'coalition'}
  {@const other = subject.kind === 'list' && subject.id === OTHER_LIST_ID}
  {@const title = other ? `Disattivare «${name}»?` : `Eliminare ${coalition ? 'la coalizione' : 'la lista'} «${name}»?`}
  <Dialog bind:element={dialog} {title} onclose={finishRemoval}>
    {#snippet description()}
      {#if coalition}
        {@const memberCount = reviewed.subjects.lists.filter(l => l.coalitionId === subject.id).length}
        {#if memberCount > 0}
          <p>{memberCount === 1 ? 'La lista della coalizione diventerà autonoma.' : `Le ${memberCount} liste della coalizione diventeranno autonome.`}</p>
        {/if}
        <p>Eventuali assegnazioni dei candidati e collegamenti per il premio della coalizione saranno rimossi.</p>
      {:else}
        {@const fallbackId = reviewed.memberships.find(m => m.listId === subject.id)?.newComponentId}
        {@const fallbackReferenceCount = reviewed.references.filter(r => r.subject.kind === 'component' && r.subject.id === fallbackId).length}
        <p>Verranno rimossi i depositi e i riferimenti {other ? 'della categoria' : 'della lista, oltre alle assegnazioni dei suoi candidati'}.</p>
        {#if fallbackId}
          <p>Verrà rimosso anche il componente proprio{fallbackReferenceCount === 1 ? ' e il suo riferimento' : fallbackReferenceCount > 1 ? ` e i suoi ${fallbackReferenceCount} riferimenti` : ''}.</p>
        {/if}
        {#if other}<p>La riattivazione non ripristina i riferimenti precedenti.</p>{/if}
      {/if}
    {/snippet}
    {#if error}<p class="error" role="alert">{error}</p>{/if}
    {#snippet actions()}
      <button type="button" onclick={() => dialog?.close()}>Annulla</button>
      <button type="button" data-variant="danger" onclick={confirmRemoval}>
        <Trash2 size={16} aria-hidden="true" />
        {other ? 'Conferma disattivazione' : coalition ? 'Elimina coalizione' : 'Elimina lista'}
      </button>
    {/snippet}
  </Dialog>
{/if}

<style>
  .error { margin: 0; color: var(--color-error-text); }
</style>
