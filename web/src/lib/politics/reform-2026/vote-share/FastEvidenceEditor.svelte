<script lang="ts">
  import type { ReleasedScenario } from './release';
  import type { CompactSource, SourcePoll } from './source-types';
  import {
    editFast,
    pollAvailability,
    saveFastPoll,
    sourceChoicesFor,
  } from './editor-model';
  import { validateEditedPoll } from './source-compiler';
  let {
    scenario,
    source,
    loading,
    onload,
    onchange,
    onerror,
  }: {
    scenario: ReleasedScenario;
    source: CompactSource | null;
    loading: boolean;
    onload: () => void;
    onchange: (s: ReleasedScenario) => void;
    onerror: (s: string) => void;
  } = $props();
  let opened = $state(false),
    search = $state(''),
    pollPage = $state(0),
    eventId = $state(''),
    choicePage = $state(0);
  let choiceId = $state(''),
    members = $state(''),
    provenance = $state('');
  let pollId = $state(''),
    surveyId = $state(''),
    citation = $state(''),
    context = $state<'camera' | 'european'>('camera');
  let start = $state(''),
    end = $state(''),
    observationDate = $state(''),
    published = $state(''),
    sample = $state<number | undefined>(1000),
    questionSample = $state<number | undefined>(),
    decided = $state<number | undefined>(100),
    design = $state<number | undefined>(1),
    errorGroup = $state('');
  let answers = $state<
    {
      label: string;
      members: string[];
      percent: number | undefined;
      other: boolean;
    }[]
  >([]);
  const rawIds = $derived(
    [
      ...new Set([
        ...scenario.polls.added.flatMap((p) =>
          p.normalized.flatMap((a) => a.members),
        ),
        ...(source?.payload.choices.flatMap((c) => c.members) ?? []),
        ...(source?.payload.polls.flatMap((p) =>
          p.normalized.flatMap((a) => a.members),
        ) ?? []),
      ]),
    ].sort(),
  );
  const polls = $derived(
    [
      ...new Map(
        [...(source?.payload.polls ?? []), ...scenario.polls.added].map((p) => [
          p.id,
          p,
        ]),
      ).values(),
    ].filter(
      (p) =>
        !search ||
        p.id.includes(search) ||
        p.citation.toLowerCase().includes(search.toLowerCase()),
    ),
  );
  const choices = $derived(
    source ? sourceChoicesFor(source, eventId, search) : [],
  );
  const pollPages = $derived(Math.max(1, Math.ceil(polls.length / 15))),
    choicePages = $derived(Math.max(1, Math.ceil(choices.length / 15)));
  function action(f: () => ReleasedScenario) {
    try {
      onchange(f());
    } catch (e) {
      onerror(String(e));
    }
  }
  function togglePoll(p: SourcePoll, enabled: boolean) {
    action(() =>
      editFast(scenario, (n) => {
        n.polls.selected = n.polls.selected.filter((id) => id !== p.id);
        if (enabled) {
          n.polls.selected.push(p.id);
          delete n.polls.excluded[p.id];
        } else n.polls.excluded[p.id] = 'Deselezionato dall’utente';
      }),
    );
  }
  function editPoll(p: SourcePoll) {
    if (p.context === 'senato') {
      onerror(
        'I sondaggi Senato non sono supportati e non vengono convertiti in osservazioni Camera.',
      );
      return;
    }
    pollId = p.id;
    surveyId = p.surveyId;
    citation = p.citation;
    context = p.context;
    start = p.fieldworkStart;
    end = p.fieldworkEnd;
    observationDate = p.date;
    published = p.availableDate;
    sample = p.sampleSize;
    questionSample = p.questionSampleSize ?? undefined;
    decided = p.decidedFraction * 100;
    design = p.designEffect;
    errorGroup = p.errorGroup;
    answers = p.normalized.map((a) => ({
      label: a.label,
      members: [...a.members],
      percent: a.share * 100,
      other: a.other,
    }));
  }
  function savePoll() {
    action(() => {
      const id = pollId.trim() || 'manuale:' + crypto.randomUUID(),
        n = sample ?? 0,
        f = (decided ?? 0) / 100,
        d = design ?? 0;
      const p: SourcePoll = {
        id,
        surveyId: surveyId.trim() || id,
        context,
        date: observationDate || end,
        fieldworkStart: start,
        fieldworkEnd: end,
        availableDate: published,
        citation,
        sampleSize: n,
        questionSampleSize: questionSample ?? null,
        decidedFraction: f,
        effectiveSample: (questionSample ?? n * f) / d,
        designEffect: d,
        effectiveSamplePolicy: 'fast-share-effective-sample-v1',
        errorGroup: errorGroup.trim() || id,
        roundingWidth: 0.001,
        original: answers.map((a) => ({
          label: a.label,
          percentage: a.percent ?? 0,
          kind: a.other ? 'residual' : 'list',
        })),
        normalized: answers.map((a) => ({
          label: a.label,
          members: [...a.members],
          share: (a.percent ?? 0) / 100,
          other: a.other,
        })),
        otherDefinition: 'offered-complement',
        status: 'supported',
        reasons: [],
        method:
          'Inserimento manuale con campione effettivo, quote sui decisi e insiemi offerti espliciti.',
      };
      validateEditedPoll(p);
      const named = p.normalized
        .filter((a) => !a.other)
        .flatMap((a) => a.members);
      if (
        new Set(named).size !== named.length ||
        p.normalized.filter((a) => a.other).length > 1
      )
        throw new Error(
          'Le risposte devono essere disgiunte; è ammessa una sola risposta Altri.',
        );
      if (pollAvailability(p, scenario.referenceDate))
        throw new Error('Sondaggio successivo alla data di riferimento.');
      pollId = id;
      return saveFastPoll(scenario, p);
    });
  }
  function selectChoice(id: string) {
    choiceId = id;
    const c = source?.payload.choices.find((c) => c.id === id);
    members = (
      scenario.sourceMemberships[id]?.members ??
      c?.members ??
      []
    ).join(', ');
    provenance = scenario.sourceMemberships[id]?.provenance ?? '';
  }
  function saveMembership() {
    action(() => {
      const c = source?.payload.choices.find((c) => c.id === choiceId),
        m = members
          .split(',')
          .map((x) => x.trim())
          .filter(Boolean);
      if (
        !c ||
        !provenance.trim() ||
        !m.length ||
        new Set(m).size !== m.length ||
        m.some((x) => !rawIds.includes(x))
      )
        throw new Error(
          'Indica componenti originali noti, senza duplicati, e la motivazione.',
        );
      if (c.opaque && m.length !== 1)
        throw new Error(
          'Un conteggio opaco non può essere suddiviso in componenti.',
        );
      return editFast(scenario, (n) => {
        n.sourceMemberships[c.id] = { members: m, provenance };
      });
    });
  }
</script>

<details
  open={opened}
  ontoggle={(e) => {
    opened = e.currentTarget.open;
    if (opened && !source && !loading) onload();
  }}
>
  <summary>Dati avanzati: sondaggi, corrispondenze e data</summary>
  <p>
    Queste modifiche rendono la stima non aggiornata. Premi «Ricalcola
    parametri» per applicarle. I conteggi originali sono scaricati solo quando
    servono al ricalcolo.
  </p>
  <label
    >Data di riferimento<input
      type="date"
      value={scenario.referenceDate}
      onchange={(e) =>
        onchange(
          editFast(scenario, (n) => {
            n.referenceDate = e.currentTarget.value;
          }),
        )}
    /></label
  >
  <label
    >Seme della stima<input
      value={scenario.fitSeed}
      onchange={(e) =>
        onchange(
          editFast(scenario, (n) => {
            n.fitSeed = e.currentTarget.value;
          }),
        )}
    /></label
  >
  {#if loading}<p role="status">
      Caricamento catalogo osservazioni…
    </p>{:else if !source}<button onclick={onload}
      >Carica / riprova catalogo osservazioni</button
    >{:else}
    <p>
      Le osservazioni senza data di pubblicazione nota si considerano
      disponibili entro la fine del giorno successivo all’ultimo giorno di voto.
      I conteggi incompleti non sono utilizzati.
    </p>
    <details>
      <summary>Elezioni storiche e adattamento Camera/Senato</summary>
      {#each source.payload.events.filter((e) => e.role === 'reference' && e.minimumAge === 18 && e.context !== 'senato') as event (event.id)}
        <label
          ><input
            type="checkbox"
            checked={scenario.historySelection.electionIds.includes(event.id)}
            disabled={event.availableDate > scenario.referenceDate}
            onchange={(e) =>
              onchange(
                editFast(scenario, (n) => {
                  n.historySelection.electionIds =
                    n.historySelection.electionIds.filter(
                      (id) => id !== event.id,
                    );
                  if (e.currentTarget.checked)
                    n.historySelection.electionIds.push(event.id);
                }),
              )}
          />{event.id} · disponibile {event.availableDate}</label
        >
      {/each}
      {#each ['pairedCamera', 'pairedSenato'] as key (key)}
        <label
          >{key === 'pairedCamera'
            ? 'Base abbinata Camera'
            : 'Base abbinata Senato'}<select
            value={scenario.historySelection[
              key as 'pairedCamera' | 'pairedSenato'
            ]}
            onchange={(e) =>
              onchange(
                editFast(scenario, (n) => {
                  n.historySelection[key as 'pairedCamera' | 'pairedSenato'] =
                    e.currentTarget.value;
                }),
              )}
          >
            {#each source.payload.events.filter((e) => e.context === (key === 'pairedCamera' ? 'camera' : 'senato') && e.minimumAge === 18) as event (event.id)}<option
                value={event.id}>{event.id} · {event.availableDate}</option
              >{/each}
          </select></label
        >
      {/each}
    </details>
    <label
      >Cerca osservazioni<input
        bind:value={search}
        oninput={() => {
          pollPage = 0;
          choicePage = 0;
        }}
      /></label
    >
    <details>
      <summary>Sondaggi ({scenario.polls.selected.length} selezionati)</summary>
      {#each polls.slice(Math.min(pollPage, pollPages - 1) * 15, (Math.min(pollPage, pollPages - 1) + 1) * 15) as p (p.id)}
        <div>
          <label
            ><input
              type="checkbox"
              checked={scenario.polls.selected.includes(p.id)}
              disabled={!!pollAvailability(p, scenario.referenceDate)}
              onchange={(e) => togglePoll(p, e.currentTarget.checked)}
            />{p.id} · {p.citation}</label
          >
          {#if pollAvailability(p, scenario.referenceDate)}<small
              >{pollAvailability(p, scenario.referenceDate)}</small
            >{/if}<button onclick={() => editPoll(p)}>Modifica copia</button>
          {#if scenario.polls.added.some((a) => a.id === p.id)}<button
              onclick={() =>
                action(() =>
                  editFast(scenario, (n) => {
                    n.polls.added = n.polls.added.filter((a) => a.id !== p.id);
                    n.polls.selected = n.polls.selected.filter(
                      (id) => id !== p.id,
                    );
                  }),
                )}>Elimina modifica</button
            >{/if}
        </div>
      {/each}
      <button disabled={pollPage === 0} onclick={() => pollPage--}
        >Sondaggi precedenti</button
      ><span>{Math.min(pollPage + 1, pollPages)}/{pollPages}</span><button
        disabled={pollPage >= pollPages - 1}
        onclick={() => pollPage++}>Sondaggi successivi</button
      >
    </details>
    <details>
      <summary>Aggiungi / modifica sondaggio</summary>
      <p>
        Quote positive sui decisi, somma 100%. Ogni risposta indica un insieme
        di componenti originali: nessuna ripartizione inventata dei totali di
        coalizione. «Altri» indica il complemento delle liste offerte. Campione
        effettivo = campione × frazione di decisi / effetto del disegno.
      </p>
      <div class="controls">
        <label>ID (vuoto: nuovo)<input bind:value={pollId} /></label><label
          >ID indagine<input bind:value={surveyId} /></label
        ><label>Fonte / citazione<input bind:value={citation} /></label>
        <label
          >Contesto<select bind:value={context}
            ><option value="camera">Camera</option><option value="european"
              >Europee</option
            ></select
          ></label
        >
        <label>Inizio rilevazione<input type="date" bind:value={start} /></label
        ><label>Fine rilevazione<input type="date" bind:value={end} /></label
        ><label
          >Data osservazione (vuoto: fine rilevazione)<input
            type="date"
            bind:value={observationDate}
          /></label
        ><label>Pubblicazione<input type="date" bind:value={published} /></label
        >
        <label
          >Campione<input
            type="number"
            min="1"
            step="1"
            bind:value={sample}
          /></label
        ><label
          >Decisi %<input
            type="number"
            min="0"
            max="100"
            step="any"
            bind:value={decided}
          /></label
        ><label
          >Effetto del disegno<input
            type="number"
            min="1"
            step="any"
            bind:value={design}
          /></label
        ><label
          >Gruppo di errore (indagine comune)<input
            bind:value={errorGroup}
          /></label
        >
      </div>
      <label
        >Sottocampione della domanda (facoltativo)<input
          type="number"
          min="1"
          max={sample}
          step="1"
          bind:value={questionSample}
        /></label
      >
      {#each answers as a, i (i)}<fieldset>
          <legend>Risposta {i + 1}</legend><label
            >Etichetta<input bind:value={a.label} /></label
          ><label
            >Quota %<input
              type="number"
              min="0"
              max="100"
              step="any"
              bind:value={a.percent}
            /></label
          ><label
            ><input type="checkbox" bind:checked={a.other} />Altri: complemento
            offerto</label
          ><label
            >Componenti originali<select
              multiple
              bind:value={a.members}
              disabled={a.other}
              >{#each rawIds as id (id)}<option value={id}>{id}</option
                >{/each}</select
            ></label
          ><button onclick={() => (answers = answers.filter((_, j) => i !== j))}
            >Rimuovi risposta</button
          >
        </fieldset>{/each}
      <button
        onclick={() =>
          (answers = [
            ...answers,
            { label: '', members: [], percent: undefined, other: false },
          ])}>Aggiungi risposta</button
      ><button onclick={savePoll}>Salva sondaggio</button><button
        onclick={() => {
          pollId = '';
          surveyId = '';
          citation = '';
          start = '';
          end = '';
          published = '';
          observationDate = '';
          sample = 1000;
          questionSample = undefined;
          decided = 100;
          design = 1;
          errorGroup = '';
          answers = [];
        }}>Nuovo sondaggio</button
      >
    </details>
    <details>
      <summary>Corrispondenze storiche indivisibili</summary>
      <label
        >Elezione<select bind:value={eventId} onchange={() => (choicePage = 0)}
          ><option value="">Scegli elezione</option
          >{#each source.payload.events as e (e.id)}<option value={e.id}
              >{e.id}</option
            >{/each}</select
        ></label
      >
      <ul>
        {#each choices.slice(Math.min(choicePage, choicePages - 1) * 15, (Math.min(choicePage, choicePages - 1) + 1) * 15) as c (c.id)}<li
          >
            <button onclick={() => selectChoice(c.id)}>{c.label}</button> · {(
              scenario.sourceMemberships[c.id]?.members ?? c.members
            ).join(' + ')}{c.opaque ? ' · conteggio opaco' : ''}
          </li>{/each}
      </ul>
      <button disabled={choicePage === 0} onclick={() => choicePage--}
        >Corrispondenze precedenti</button
      ><span>{Math.min(choicePage + 1, choicePages)}/{choicePages}</span><button
        disabled={choicePage >= choicePages - 1}
        onclick={() => choicePage++}>Corrispondenze successive</button
      >
      {#if choiceId}<p>{choiceId}</p>
        <label
          >Componenti originali, separati da virgola<input
            bind:value={members}
          /></label
        ><label>Motivazione / fonte<input bind:value={provenance} /></label
        ><button onclick={saveMembership}>Salva corrispondenza</button><button
          onclick={() =>
            action(() =>
              editFast(scenario, (n) => {
                delete n.sourceMemberships[choiceId];
              }),
            )}>Ripristina corrispondenza originale</button
        >{/if}
      <p>
        Il raggruppamento dei componenti originali nella base stimata è
        conservato in «componentMap» nel JSON. Modificare la base richiede una
        stima identificabile; gli errori di rango non vengono corretti
        introducendo conteggi artificiali.
      </p>
    </details>
  {/if}
</details>
