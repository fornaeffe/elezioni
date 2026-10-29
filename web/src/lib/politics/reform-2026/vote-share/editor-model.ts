/** Scenario-only edits. No fitting, source mutation, random draws or legal scrutiny. */
import { cloneReformValue, subjectColor } from '../presentation-metadata';
import {
  OTHER_LIST_ID,
  OTHER_LIST_NAME,
  OTHER_LIST_COLOR,
} from '../../../core/other-list';
import { canonicalJson } from './source-codec';
import type { ReleasedScenario } from './release';
import type { Chamber, ReferenceScope } from '../input';
import type { SourcePoll, CompactSource } from './source-types';
export const copyFastDraft = (s: ReleasedScenario) => cloneReformValue(s);
export function labelFastDraft(s: ReleasedScenario): ReleasedScenario {
  const n = copyFastDraft(s),
    old = n.presentation;
  n.presentation = {
    ...old,
    name: old.name ?? 'Scenario riforma 2026',
    lists: (['camera', 'senato'] as const).flatMap((ch) =>
      n.lists[ch].map((l) => ({
        chamber: ch,
        id: l.id,
        name: l.name,
        color:
          l.kind === 'other'
            ? OTHER_LIST_COLOR
            : (old.lists.find((p) => p.id === l.id && p.chamber === ch)
                ?.color ?? subjectColor(l.id)),
      })),
    ),
    coalitions: [
      ...new Set(
        [
          ...(old.coalitions ?? []).map((c) => c.id),
          ...n.lists.camera,
          ...n.lists.senato,
        ].flatMap((l) =>
          typeof l === 'string' ? [l] : l.coalitionId ? [l.coalitionId] : [],
        ),
      ),
    ].map(
      (id) =>
        old.coalitions?.find((c) => c.id === id) ?? {
          id,
          name: [
            ...new Set(
              [...n.lists.camera, ...n.lists.senato]
                .filter((l) => l.coalitionId === id)
                .map((l) => l.name),
            ),
          ].join(' + '),
          color: subjectColor(id),
        },
    ),
  };
  return n;
}
export function editFast(
  s: ReleasedScenario,
  edit: (n: ReleasedScenario) => void,
) {
  const n = copyFastDraft(s);
  edit(n);
  return labelFastDraft(n);
}
export function futureEvidenceSignature(s: ReleasedScenario) {
  return canonicalJson({
    referenceDate: s.referenceDate,
    fitSeed: s.fitSeed,
    components: s.components
      .filter((c) => c.kind === 'fitted')
      .map((c) => c.id)
      .sort(),
    history: s.historySelection,
    sourceMemberships: s.sourceMemberships,
    componentMap: s.componentMap,
    polls: s.polls,
  });
}
const nextId = (prefix: string, ids: string[]) => {
  let i = 1;
  while (ids.includes(prefix + i)) i++;
  return prefix + i;
};
export function addFastList(
  s: ReleasedScenario,
  name: string,
  components: string[],
  chambers: Chamber[],
) {
  if (!name.trim() || !components.length || !chambers.length)
    throw new Error('Indica nome, componenti e almeno un ramo.');
  const id = nextId(
    'lista-',
    [...s.lists.camera, ...s.lists.senato].map((l) => l.id),
  );
  return editFast(s, (n) => {
    for (const ch of chambers)
      n.lists[ch].push({
        id,
        name: name.trim(),
        coalitionId: null,
        protectedMinority: null,
        componentIds: [...new Set(components)],
      });
  });
}
export function addReferencedList(
  s: ReleasedScenario,
  name: string,
  percent: number,
  chambers: Chamber[],
) {
  if (!Number.isFinite(percent) || percent <= 0 || percent >= 100)
    throw new Error(
      'Il nuovo supporto richiede una quota positiva inferiore al 100%.',
    );
  const id = nextId(
    'nuova-',
    s.components.map((c) => c.id),
  );
  let n = editFast(s, (n) => {
    n.components.push({
      id,
      kind: 'new',
      provenance:
        'Componente definita dall’utente, senza conteggi storici inventati.',
      fallback: {
        geographicLogProfile: 0,
        profileSd: 0.45,
        localInnovationSd: 0.25,
        nationalInnovationSd: 0.35,
      },
    });
    n.presentation.components.push({ id, name, support: 'reference' });
    for (const ch of chambers)
      n.references.push({
        id: 'riferimento:' + id + ':' + ch,
        channel: ch,
        scope: { kind: 'national' },
        subject: { kind: 'component', id },
        mean: percent / 100,
        sdPercentagePoints: 0,
      });
  });
  return addFastList(n, name, [id], chambers);
}
export function removeFastList(s: ReleasedScenario, ch: Chamber, id: string) {
  if (s.suppliedVotes.some((v) => v.chamber === ch))
    throw new Error(
      'Aggiorna prima i voti forniti per questo ramo nel JSON: non saranno eliminati automaticamente.',
    );
  return editFast(s, (n) => {
    n.lists[ch] = n.lists[ch].filter((l) => l.id !== id);
    n.coverage = n.coverage.map((c) =>
      c.chamber === ch
        ? { ...c, listIds: c.listIds.filter((x) => x !== id) }
        : c,
    );
    n.candidateTemplates = n.candidateTemplates.filter(
      (t) =>
        t.chamber !== ch || (t.ownerId !== id && t.affiliationListId !== id),
    );
    n.suppliedPreferences = n.suppliedPreferences.filter(
      (p) => p.chamber !== ch || p.listId !== id,
    );
    n.references = n.references.filter(
      (r) =>
        !(
          r.subject.kind === 'list' &&
          r.subject.id === id &&
          (r.channel === ch ||
            !n.lists[ch === 'camera' ? 'senato' : 'camera'].some(
              (l) => l.id === id,
            ))
        ),
    );
    const used = new Set(
        [...n.lists.camera, ...n.lists.senato].flatMap((l) => l.componentIds),
      ),
      removed = new Set(
        n.components
          .filter((c) => c.kind === 'new' && !used.has(c.id))
          .map((c) => c.id),
      );
    n.components = n.components.filter((c) => !removed.has(c.id));
    n.presentation.components = n.presentation.components.filter(
      (c) => !removed.has(c.id),
    );
    n.references = n.references.filter(
      (r) => r.subject.kind !== 'component' || !removed.has(r.subject.id),
    );
    for (const r of n.references)
      if (
        r.channel === 'common' &&
        r.subject.kind === 'list' &&
        r.subject.id === id
      )
        r.channel = ch === 'camera' ? 'senato' : 'camera';
    const subjects = (side: Chamber) =>
      new Set(
        n.lists[side]
          .filter((l) => l.kind !== 'other')
          .map((l) => l.coalitionId ?? l.id),
      );
    n.bonusLinks = n.bonusLinks.filter(
      (b) =>
        subjects('camera').has(b.camera.id) &&
        subjects('senato').has(b.senato.id),
    );
  });
}
export function moveFastComponent(
  s: ReleasedScenario,
  ch: Chamber,
  listId: string,
  component: string,
  enabled: boolean,
  move: boolean,
) {
  return editFast(s, (n) => {
    n.lists[ch] = n.lists[ch].map((l) =>
      l.id === listId
        ? {
            ...l,
            componentIds: enabled
              ? [...new Set([...l.componentIds, component])]
              : l.componentIds.filter((c) => c !== component),
          }
        : enabled && move
          ? {
              ...l,
              componentIds: l.componentIds.filter((c) => c !== component),
            }
          : l,
    );
  });
}
export function setFastPresence(
  s: ReleasedScenario,
  id: string,
  ch: Chamber,
  present: boolean,
) {
  if (!present) return removeFastList(s, ch, id);
  return editFast(s, (n) => {
    if (!n.lists[ch].some((l) => l.id === id)) {
      const source = n.lists[ch === 'camera' ? 'senato' : 'camera'].find(
        (l) => l.id === id,
      );
      if (!source) throw new Error('Lista assente.');
      n.lists[ch].push(cloneReformValue(source));
    }
  });
}
export function setFastOther(
  s: ReleasedScenario,
  enabled: boolean,
  components: string[],
) {
  if (!enabled) {
    let n = s;
    for (const ch of ['camera', 'senato'] as const)
      if (n.lists[ch].some((l) => l.id === OTHER_LIST_ID))
        n = removeFastList(n, ch, OTHER_LIST_ID);
    return n;
  }
  if (!components.length)
    throw new Error('Scegli esplicitamente i componenti di Altre liste.');
  return editFast(s, (n) => {
    for (const ch of ['camera', 'senato'] as const)
      if (!n.lists[ch].some((l) => l.id === OTHER_LIST_ID)) {
        n.lists[ch].push({
          kind: 'other',
          id: OTHER_LIST_ID,
          name: OTHER_LIST_NAME,
          coalitionId: null,
          protectedMinority: null,
          componentIds: [...components],
        });
        n.coverage = n.coverage.map((c) =>
          c.chamber === ch
            ? { ...c, listIds: [...new Set([...c.listIds, OTHER_LIST_ID])] }
            : c,
        );
      }
  });
}
export function setFastReference(
  s: ReleasedScenario,
  input: {
    scope: ReferenceScope;
    channel: 'common' | Chamber;
    subject: { kind: 'list' | 'component'; id: string } | null;
    percent: number | undefined;
    sd: number;
  },
) {
  if (
    (input.percent !== undefined &&
      (!Number.isFinite(input.percent) ||
        input.percent < 0 ||
        input.percent > 100)) ||
    !Number.isFinite(input.sd) ||
    input.sd < 0
  )
    throw new Error('Quota o incertezza non valida.');
  return editFast(s, (n) => {
    const key = canonicalJson([input.scope, input.channel, input.subject]),
      id = 'riferimento:' + key;
    if (input.subject) {
      n.references = n.references.filter(
        (r) => canonicalJson([r.scope, r.channel, r.subject]) !== key,
      );
      if (input.percent !== undefined)
        n.references.push({
          id,
          scope: input.scope,
          channel: input.channel,
          subject: input.subject,
          mean: input.percent / 100,
          sdPercentagePoints: input.sd,
        });
    } else {
      n.nonValidReferences = n.nonValidReferences.filter(
        (r) => canonicalJson([r.scope, r.channel, null]) !== key,
      );
      if (input.percent !== undefined)
        n.nonValidReferences.push({
          id,
          scope: input.scope,
          channel: input.channel,
          mean: input.percent / 100,
          sdPercentagePoints: input.sd,
        });
    }
  });
}
export function pollAvailability(p: SourcePoll, referenceDate: string) {
  return p.status !== 'supported'
    ? p.reasons.join('; ') || 'Trattamento non supportato'
    : [p.date, p.fieldworkEnd, p.availableDate].some(
          (date) => date > referenceDate,
        )
      ? 'Non disponibile alla data di riferimento'
      : null;
}
export function saveFastPoll(s: ReleasedScenario, poll: SourcePoll) {
  return editFast(s, (n) => {
    n.polls.added = [
      ...n.polls.added.filter((p) => p.id !== poll.id),
      cloneReformValue(poll),
    ];
    n.polls.selected = [...new Set([...n.polls.selected, poll.id])];
  });
}
export function sourceChoicesFor(
  source: CompactSource,
  eventId: string,
  search: string,
) {
  return source.payload.choices.filter(
    (c) =>
      c.eventId === eventId &&
      (!search ||
        c.label.toLocaleLowerCase().includes(search.toLocaleLowerCase())),
  );
}
