<script lang="ts">
  import '../scenario-editor.css';
  import type { ReleasedScenario } from './release';
  import type { CompactSource } from './source-types';
  import type { ReformFitClient } from './fit-client';
  import type {
    PreviewSelection,
    ReformEditorPreview,
  } from '../editor-preview';
  import type { Chamber } from '../input';
  import { OTHER_LIST_ID } from '../../../core/other-list';
  import {
    editFast,
    addFastList,
    addReferencedList,
    moveFastComponent,
    removeFastList,
    setFastPresence,
    setFastOther,
  } from './editor-model';
  import { parseFastScenario } from './scenario';
  import { reformSubjects, subjectLabel } from '../scenario-editor';
  import ReformCandidatesEditor from '../ReformCandidatesEditor.svelte';
  import FastReferencesEditor from './FastReferencesEditor.svelte';
  import FastEvidenceEditor from './FastEvidenceEditor.svelte';
  let {
    scenario,
    editorData,
    source,
    sourceLoading,
    onloadsource,
    onchange,
    onerror,
    onpreview,
    preview,
    previewSelection,
    canPreview,
    onpending,
  }: {
    scenario: ReleasedScenario;
    editorData: ReturnType<ReformFitClient['editorData']> | null;
    source: CompactSource | null;
    sourceLoading: boolean;
    onloadsource: () => void;
    onchange: (s: ReleasedScenario) => void;
    onerror: (s: string) => void;
    onpreview: (s: PreviewSelection) => void;
    preview: Pick<ReformEditorPreview, 'placements' | 'bonusSelection'> | null;
    previewSelection: PreviewSelection | undefined;
    canPreview: boolean;
    onpending: (p: boolean) => void;
  } = $props();
  let chamber = $state<Chamber>('camera'),
    name = $state(''),
    selectedComponents = $state<string[]>([]),
    newShare = $state<number | undefined>(1),
    referenceOnly = $state(false);
  let presenceCamera = $state(true),
    presenceSenato = $state(true),
    otherComponents = $state<string[]>([]),
    otherEnabled = $derived(
      [...scenario.lists.camera, ...scenario.lists.senato].some(
        (l) => l.id === OTHER_LIST_ID,
      ),
    );
  let listId = $state(''),
    collegeSearch = $state(''),
    collegePage = $state(0),
    json = $state(''),
    jsonPending = $state(false);
  let cameraLink = $state(''),
    senatoLink = $state(''),
    cameraDeclares = $state(true),
    senatoDeclares = $state(true);
  const colleges = $derived(
    (editorData?.catalog.colleges ?? []).filter(
      (c) =>
        c.chamber === chamber &&
        (!collegeSearch ||
          c.name.toLowerCase().includes(collegeSearch.toLowerCase())),
    ),
  );
  const pages = $derived(Math.max(1, Math.ceil(colleges.length / 20)));
  const componentOptions = $derived(
    scenario.components.map((c) => ({
      id: c.id,
      label:
        scenario.presentation.components.find((p) => p.id === c.id)?.name ??
        c.id,
      kind: c.kind,
    })),
  );
  function action(f: () => ReleasedScenario) {
    try {
      onchange(f());
    } catch (e) {
      onerror(e instanceof Error ? e.message : String(e));
    }
  }
  function patch(f: (n: ReleasedScenario) => void) {
    action(() => editFast(scenario, f));
  }
  function add() {
    action(() => {
      const sides: Chamber[] = [
        ...(presenceCamera ? ['camera' as const] : []),
        ...(presenceSenato ? ['senato' as const] : []),
      ];
      return referenceOnly
        ? addReferencedList(scenario, name, newShare ?? 0, sides)
        : addFastList(scenario, name, selectedComponents, sides);
    });
  }
  function fileFiltered(enabled: boolean) {
    patch((n) => {
      for (const c of colleges) {
        let row = n.coverage.find(
          (r) => r.chamber === chamber && r.collegeId === c.id,
        );
        if (!row) {
          row = { chamber, collegeId: c.id, listIds: [] };
          n.coverage.push(row);
        }
        row.listIds = row.listIds.filter((id) => id !== listId);
        if (enabled) row.listIds.push(listId);
      }
    });
  }
  function remove(ch: Chamber, id: string) {
    if (
      confirm(
        'Eliminare la lista da questo ramo, inclusi depositi, candidature e riferimenti collegati? I componenti storici sono conservati.',
      )
    )
      action(() => removeFastList(scenario, ch, id));
  }
  function changeJson(value: string) {
    json = value;
    jsonPending = true;
    onpending(true);
  }
  function applyJson() {
    action(() => {
      const next = parseFastScenario(json);
      jsonPending = false;
      onpending(false);
      return next;
    });
  }
  function link() {
    patch((n) => {
      const a = reformSubjects(n, 'camera').find((s) => s.id === cameraLink),
        b = reformSubjects(n, 'senato').find((s) => s.id === senatoLink);
      if (!a || !b) throw new Error('Scegli entrambi i soggetti.');
      n.bonusLinks = [
        ...n.bonusLinks.filter(
          (l) => l.camera.id !== a.id || l.senato.id !== b.id,
        ),
        { camera: a, senato: b, cameraDeclares, senatoDeclares },
      ];
    });
  }
</script>

<div class="scenario-editor" data-testid="fast-scenario-editor">
  <div class="controls">
    <label
      >Nome scenario<input
        value={scenario.presentation.name ?? ''}
        onchange={(e) => {
          const v = e.currentTarget.value;
          patch((n) => {
            n.presentation.name = v;
          });
        }}
      /></label
    >
    <label
      >Data elettorale<input
        type="date"
        value={scenario.electionDate}
        onchange={(e) => {
          const v = e.currentTarget.value;
          patch((n) => {
            n.electionDate = v;
          });
        }}
      /></label
    >
    <label
      >Seme delle simulazioni<input
        value={scenario.seed}
        onchange={(e) => {
          const v = e.currentTarget.value;
          patch((n) => {
            n.seed = v;
          });
        }}
      /></label
    >
    <label
      >Rinnovo<select
        value={scenario.renewal}
        onchange={(e) => {
          const v = e.currentTarget.value as ReleasedScenario['renewal'];
          patch((n) => {
            n.renewal = v;
          });
        }}
        ><option value="joint">Entrambi i rami</option><option value="camera"
          >Camera</option
        ><option value="senato">Senato</option></select
      ></label
    >
    <label
      >Ramo da modificare<select
        bind:value={chamber}
        onchange={() => (collegePage = 0)}
        ><option value="camera">Camera</option><option value="senato"
          >Senato</option
        ></select
      ></label
    >
  </div>
  <details open>
    <summary>Liste e componenti politici</summary>
    <p>
      Un componente stimato conserva le informazioni storiche; uno da
      riferimento usa le ipotesi di fallback dichiarate. Riunire componenti
      nella stessa lista non richiede una nuova stima. La stessa componente può
      comparire in liste diverse solo in collegi senza sovrapposizione.
    </p>
    {#each scenario.lists[chamber] as l (l.id)}
      <details>
        <summary>{l.name} · {l.componentIds.length} componenti</summary>
        <label
          >Nome lista<input
            value={l.name}
            disabled={l.kind === 'other'}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.lists[chamber] = n.lists[chamber].map((x) =>
                  x.id === l.id ? { ...x, name: v } : x,
                );
              });
            }}
          /></label
        >
        <label
          >Colore<input
            type="color"
            value={scenario.presentation.lists.find(
              (p) => p.chamber === chamber && p.id === l.id,
            )?.color ?? '#777777'}
            disabled={l.kind === 'other'}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.presentation.lists.find(
                  (p) => p.chamber === chamber && p.id === l.id,
                )!.color = v;
              });
            }}
          /></label
        >
        <label
          >Moltiplicatore incertezza nazionale<input
            type="number"
            min="0"
            step="any"
            value={l.nationalUncertaintyMultiplier ?? 1}
            onchange={(e) => {
              const v = e.currentTarget.valueAsNumber;
              patch((n) => {
                n.lists[chamber] = n.lists[chamber].map((x) =>
                  x.id === l.id
                    ? { ...x, nationalUncertaintyMultiplier: v }
                    : x,
                );
              });
            }}
          /></label
        >
        <label
          >Moltiplicatore incertezza locale<input
            type="number"
            min="0"
            step="any"
            value={l.localUncertaintyMultiplier ?? 1}
            onchange={(e) => {
              const v = e.currentTarget.valueAsNumber;
              patch((n) => {
                n.lists[chamber] = n.lists[chamber].map((x) =>
                  x.id === l.id ? { ...x, localUncertaintyMultiplier: v } : x,
                );
              });
            }}
          /></label
        >
        {#if l.kind !== 'other'}<label
            >Coalizione<select
              value={l.coalitionId ?? ''}
              onchange={(e) => {
                const v = e.currentTarget.value;
                patch((n) => {
                  n.lists[chamber] = n.lists[chamber].map((x) =>
                    x.id === l.id ? { ...x, coalitionId: v || null } : x,
                  );
                  n.bonusLinks = [];
                });
              }}
              ><option value="">Lista singola</option
              >{#each scenario.presentation.coalitions ?? [] as c (c.id)}<option
                  value={c.id}>{c.name}</option
                >{/each}</select
            ></label
          >{/if}
        <fieldset>
          <legend
            >Composizione: sposta da eventuali altre liste di questo ramo</legend
          >
          {#each componentOptions as c (c.id)}<label
              ><input
                type="checkbox"
                checked={l.componentIds.includes(c.id)}
                onchange={(e) =>
                  action(() =>
                    moveFastComponent(
                      scenario,
                      chamber,
                      l.id,
                      c.id,
                      e.currentTarget.checked,
                      true,
                    ),
                  )}
              />{c.label} · {c.kind === 'fitted'
                ? 'stimato'
                : 'da riferimento'}</label
            >{/each}
        </fieldset>
        {#if l.kind !== 'other'}<button
            onclick={() =>
              patch((n) => {
                const id = 'coalizione:' + crypto.randomUUID();
                n.presentation.coalitions ??= [];
                n.presentation.coalitions.push({
                  id,
                  name: 'Nuova coalizione',
                  color: '#555555',
                });
                n.lists[chamber] = n.lists[chamber].map((x) =>
                  x.id === l.id ? { ...x, coalitionId: id } : x,
                );
                n.bonusLinks = [];
              })}>Nuova coalizione</button
          >{/if}
        <button
          onclick={() =>
            action(() =>
              setFastPresence(
                scenario,
                l.id,
                chamber === 'camera' ? 'senato' : 'camera',
                true,
              ),
            )}>Aggiungi anche all’altro ramo</button
        >
        <button onclick={() => remove(chamber, l.id)}
          >Elimina da questo ramo</button
        >
      </details>
    {/each}
    <fieldset>
      <legend>Nuova lista / lista congiunta</legend>
      <label>Nome nuova lista<input bind:value={name} /></label><label
        ><input type="checkbox" bind:checked={presenceCamera} />Camera</label
      ><label
        ><input type="checkbox" bind:checked={presenceSenato} />Senato</label
      >
      <label
        ><input type="checkbox" bind:checked={referenceOnly} />Nuovo componente
        senza dati storici</label
      >
      {#if referenceOnly}<label
          >Quota iniziale %<input
            type="number"
            min="0"
            max="100"
            step="any"
            bind:value={newShare}
          /></label
        >
        <p>
          Fallback esplicito: profilo geografico logaritmico 0; deviazioni
          standard profilo 0,45, innovazione locale 0,25, nazionale 0,35. Puoi
          modificarli nel JSON.
        </p>
      {:else}<label
          >Componenti della lista<select
            multiple
            bind:value={selectedComponents}
            >{#each componentOptions as c (c.id)}<option value={c.id}
                >{c.label}</option
              >{/each}</select
          ></label
        >{/if}
      <button onclick={add}>Aggiungi lista</button>
      <p>
        Una nuova lista non è depositata automaticamente. Completa i depositi;
        se riusi componenti, rimuovi le sovrapposizioni.
      </p>
    </fieldset>
    <fieldset>
      <legend>Altre liste</legend>
      <p>
        Residuo esplicito senza candidati né seggi, con identità e colore fissi.
        Scegli la composizione prima di attivarlo.
      </p>
      <label
        >Componenti del residuo<select multiple bind:value={otherComponents}
          >{#each componentOptions as c (c.id)}<option value={c.id}
              >{c.label}</option
            >{/each}</select
        ></label
      >
      <button
        onclick={() =>
          action(() => setFastOther(scenario, !otherEnabled, otherComponents))}
        >{otherEnabled ? 'Disattiva Altre liste' : 'Attiva Altre liste'}</button
      >
    </fieldset>
    {#each scenario.presentation.coalitions ?? [] as c (c.id)}<fieldset>
        <legend>Coalizione</legend><label
          >Nome<input
            value={c.name}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.presentation.coalitions!.find((x) => x.id === c.id)!.name = v;
              });
            }}
          /></label
        ><label
          >Colore<input
            type="color"
            value={c.color}
            onchange={(e) => {
              const v = e.currentTarget.value;
              patch((n) => {
                n.presentation.coalitions!.find((x) => x.id === c.id)!.color =
                  v;
              });
            }}
          /></label
        >
      </fieldset>{/each}
  </details>
  <FastReferencesEditor {scenario} {onchange} {onerror} />
  <details>
    <summary>Depositi nei collegi</summary>
    {#if !editorData}<p>Disponibili dopo il caricamento del modello.</p>{:else}
      <label
        >Lista da depositare<select bind:value={listId}
          ><option value="">Scegli lista</option
          >{#each scenario.lists[chamber] as l (l.id)}<option value={l.id}
              >{l.name}</option
            >{/each}</select
        ></label
      >
      <label
        >Cerca collegio<input
          bind:value={collegeSearch}
          oninput={() => (collegePage = 0)}
        /></label
      >
      <div class="controls">
        <button disabled={!listId} onclick={() => fileFiltered(true)}
          >Deposita nei collegi filtrati</button
        ><button disabled={!listId} onclick={() => fileFiltered(false)}
          >Ritira dai collegi filtrati</button
        >
      </div>
      {#each colleges.slice(Math.min(collegePage, pages - 1) * 20, (Math.min(collegePage, pages - 1) + 1) * 20) as c (c.id)}<label
          ><input
            type="checkbox"
            disabled={!listId}
            checked={scenario.coverage.some(
              (row) =>
                row.chamber === chamber &&
                row.collegeId === c.id &&
                row.listIds.includes(listId),
            )}
            onchange={(e) => {
              const checked = e.currentTarget.checked;
              patch((n) => {
                let row = n.coverage.find(
                  (row) => row.chamber === chamber && row.collegeId === c.id,
                );
                if (!row) {
                  row = { chamber, collegeId: c.id, listIds: [] };
                  n.coverage.push(row);
                }
                row.listIds = row.listIds.filter((id) => id !== listId);
                if (checked) row.listIds.push(listId);
              });
            }}
          />{c.name}</label
        >{/each}
      <button disabled={collegePage === 0} onclick={() => collegePage--}
        >Collegi precedenti</button
      ><span>{Math.min(collegePage + 1, pages)}/{pages}</span><button
        disabled={collegePage >= pages - 1}
        onclick={() => collegePage++}>Collegi successivi</button
      >
    {/if}
  </details>
  <details>
    <summary>Collegamenti per il premio Camera/Senato</summary>
    <p>
      Una modifica alle coalizioni azzera i collegamenti: confermali nuovamente.
    </p>
    <label
      >Soggetto Camera<select bind:value={cameraLink}
        ><option value="">Scegli soggetto</option
        >{#each reformSubjects(scenario, 'camera') as s (s.id)}<option
            value={s.id}>{subjectLabel(scenario, s, 'camera')}</option
          >{/each}</select
      ></label
    >
    <label
      >Soggetto Senato<select bind:value={senatoLink}
        ><option value="">Scegli soggetto</option
        >{#each reformSubjects(scenario, 'senato') as s (s.id)}<option
            value={s.id}>{subjectLabel(scenario, s, 'senato')}</option
          >{/each}</select
      ></label
    >
    <label
      ><input type="checkbox" bind:checked={cameraDeclares} />Dichiarazione
      Camera</label
    ><label
      ><input type="checkbox" bind:checked={senatoDeclares} />Dichiarazione
      Senato</label
    ><button onclick={link}>Salva collegamento</button>
    {#each scenario.bonusLinks as link, i (i)}<p>
        {subjectLabel(scenario, link.camera, 'camera')} ↔ {subjectLabel(
          scenario,
          link.senato,
          'senato',
        )} · {link.cameraDeclares ? 'sì' : 'no'}/{link.senatoDeclares
          ? 'sì'
          : 'no'}
        <button
          onclick={() =>
            patch((n) => {
              n.bonusLinks = n.bonusLinks.filter((_, j) => i !== j);
            })}>Elimina collegamento</button
        >
      </p>{/each}
  </details>
  {#if editorData}<details>
      <summary>Candidati e preferenze</summary>
      <p>
        La verifica mostra il campione di simulazione 1; non costituisce una
        prova per tutte le simulazioni. Preferenze e voti forniti sono
        modificabili nel JSON.
      </p>
      <label
        >Profilo preferenze<select
          value={scenario.preferenceProfileId}
          onchange={(e) => {
            const v = e.currentTarget.value;
            patch((n) => {
              n.preferenceProfileId = v;
            });
          }}
          >{#each editorData.profiles.preferences as p (p.id)}<option
              value={p.id}>{p.id}</option
            >{/each}</select
        ></label
      >
      <label
        >Probabilità di riutilizzo dei candidati<input
          type="number"
          min="0"
          max="1"
          step="any"
          value={scenario.candidateGeneration.reuseProbability}
          onchange={(e) => {
            const v = e.currentTarget.valueAsNumber;
            patch((n) => {
              n.candidateGeneration.reuseProbability = v;
            });
          }}
        /></label
      >
      <label
        >Massimo candidature ordinarie<input
          type="number"
          min="1"
          step="1"
          value={scenario.candidateGeneration.maximumOrdinaryCandidacies}
          onchange={(e) => {
            const v = e.currentTarget.valueAsNumber;
            patch((n) => {
              n.candidateGeneration.maximumOrdinaryCandidacies = v;
            });
          }}
        /></label
      >
      <ReformCandidatesEditor
        {scenario}
        catalog={editorData.catalog}
        disabled={!canPreview}
        onaction={action}
        {onpreview}
        {preview}
        {previewSelection}
      />
    </details>{/if}
  <FastEvidenceEditor
    {scenario}
    {source}
    loading={sourceLoading}
    onload={onloadsource}
    {onchange}
    {onerror}
  />
  <details>
    <summary>JSON completo: ipotesi, raggruppamenti e input forniti</summary>
    <p>
      Include componentMap, ipotesi dei nuovi componenti, candidature,
      preferenze e voti forniti. I campi sconosciuti sono rifiutati. Applica o
      scarta la bozza prima di avviare un calcolo.
    </p>
    <button
      onclick={() => {
        json = JSON.stringify($state.snapshot(scenario), null, 2);
        jsonPending = false;
        onpending(false);
      }}>Carica JSON corrente / scarta bozza</button
    >
    <label
      >Scenario JSON<textarea
        rows="18"
        value={json}
        oninput={(e) => changeJson(e.currentTarget.value)}></textarea></label
    ><button disabled={!jsonPending} onclick={applyJson}>Applica JSON</button>
  </details>
</div>
