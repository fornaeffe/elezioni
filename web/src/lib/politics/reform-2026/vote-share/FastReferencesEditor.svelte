<script lang="ts">
  import type { ReleasedScenario } from './release';
  import type { ReferenceScope, Chamber } from '../input';
  import { setFastReference } from './editor-model';
  let {
    scenario,
    onchange,
    onerror,
  }: {
    scenario: ReleasedScenario;
    onchange: (s: ReleasedScenario) => void;
    onerror: (s: string) => void;
  } = $props();
  let subject = $state(''),
    channel = $state<'common' | Chamber>('common');
  let scopeKind = $state<ReferenceScope['kind']>('national'),
    code = $state('');
  let percent = $state<number | undefined>(),
    sd = $state<number | undefined>(0);
  const subjects = $derived([
    ...new Map(
      [...scenario.lists.camera, ...scenario.lists.senato].map((l) => [
        'list:' + l.id,
        { value: 'list:' + l.id, label: l.name },
      ]),
    ).values(),
    ...scenario.components.map((c) => ({
      value: 'component:' + c.id,
      label:
        'Componente: ' +
        (scenario.presentation.components.find((p) => p.id === c.id)?.name ??
          c.id),
    })),
  ]);
  function save() {
    try {
      if (scopeKind !== 'national' && !code.trim())
        throw new Error('Indica il codice geografico del riferimento.');
      const [kind, ...id] = subject.split(':');
      onchange(
        setFastReference(scenario, {
          scope:
            scopeKind === 'national'
              ? { kind: 'national' }
              : { kind: scopeKind, code: code.trim() },
          channel,
          subject: subject
            ? { kind: kind as 'list' | 'component', id: id.join(':') }
            : null,
          percent,
          sd: sd ?? 0,
        }),
      );
    } catch (e) {
      onerror(String(e));
    }
  }
  function edit(
    r:
      | ReleasedScenario['references'][number]
      | ReleasedScenario['nonValidReferences'][number],
  ) {
    subject = 'subject' in r ? r.subject.kind + ':' + r.subject.id : '';
    channel = r.channel;
    scopeKind = r.scope.kind;
    code = 'code' in r.scope ? r.scope.code : '';
    percent = r.mean * 100;
    sd = r.sdPercentagePoints;
  }
  function label(r: ReleasedScenario['nonValidReferences'][number]) {
    const subject = (r as ReleasedScenario['references'][number]).subject;
    return subject
      ? (subjects.find((s) => s.value === subject.kind + ':' + subject.id)
          ?.label ?? subject.id)
      : 'Non valido / astensione';
  }
</script>

<details>
  <summary>Quote di riferimento e incertezza</summary>
  <p>
    Le quote delle liste riguardano i soli voti validi e mantengono fissa la
    partecipazione. Il non valido comprende astensioni e schede non valide ed è
    un input separato. Campo vuoto: eredita il modello; zero: vincolo esplicito.
  </p>
  <div class="controls">
    <label
      >Soggetto<select bind:value={subject}
        ><option value="">Non valido / astensione</option
        >{#each subjects as s (s.value)}<option value={s.value}
            >{s.label}</option
          >{/each}</select
      ></label
    >
    <label
      >Ramo<select bind:value={channel}
        ><option value="common">Comune</option><option value="camera"
          >Camera</option
        ><option value="senato">Senato</option></select
      ></label
    >
    <label
      >Ambito<select bind:value={scopeKind}
        ><option value="national">Nazionale</option><option value="region"
          >Regione</option
        ><option value="province">Provincia</option><option value="municipality"
          >Comune</option
        ></select
      ></label
    >
    {#if scopeKind !== 'national'}<label
        >Codice geografico<input bind:value={code} /></label
      >{/if}
    <label
      >Quota %<input
        aria-label="Quota di riferimento %"
        type="number"
        min="0"
        max="100"
        step="any"
        bind:value={percent}
      /></label
    >
    <label
      >Deviazione standard (punti %)<input
        type="number"
        min="0"
        step="any"
        bind:value={sd}
      /></label
    >
    <button onclick={save}>Applica riferimento</button>
  </div>
  <ul>
    {#each [...scenario.references, ...scenario.nonValidReferences] as r (r.id)}
      <li>
        <button onclick={() => edit(r)}>Modifica</button>
        {label(r)} · {r.channel} · {r.scope.kind}{'code' in r.scope
          ? ' ' + r.scope.code
          : ''}: {(r.mean * 100).toLocaleString('it-IT')}% ± {r.sdPercentagePoints}
        punti
        <button
          aria-label={'Elimina riferimento ' + r.id}
          onclick={() => {
            const n = structuredClone($state.snapshot(scenario));
            n.references = n.references.filter((x) => x.id !== r.id);
            n.nonValidReferences = n.nonValidReferences.filter(
              (x) => x.id !== r.id,
            );
            onchange(n);
          }}>Elimina</button
        >
      </li>
    {/each}
  </ul>
</details>
