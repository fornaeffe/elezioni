import future from '../../../data/fast-share-future-scenario.json' with { type: 'json' };
import review from '../../../data/legacy-comparison-future-polls.json' with { type: 'json' };
import comparison from '../../../data/legacy-comparison-policy.json' with { type: 'json' };
import { loadSourceMemberships, sourceMembership } from '../data-preparation/fast-share-memberships';
import type { Scenario } from '../../src/lib/core/types';
import type { PoliticsStaticSnapshot } from '../../src/lib/politics/types';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';

/** The authored choice set is shared; each model keeps its own compilation law. */
export async function compilePoliticsDefault(root: string, snapshot: PoliticsStaticSnapshot, baseline: Scenario, released: ReleasedScenario): Promise<Scenario> {
  const { policy } = await loadSourceMemberships(root);
  const aliases = new Map(future.aliases.map(a => [a.from, a.to]));
  const canonical = (id: string) => aliases.get(id) ?? id;
  const historicalByChoice = new Map<string, NonNullable<PoliticsStaticSnapshot['data']['comuni_liste_elezioni']>[number]>();
  for (const row of snapshot.data.comuni_liste_elezioni ?? []) {
    const key = `${row.DATA}:${row.ELEZIONE}:${row.LISTA}`, previous = historicalByChoice.get(key);
    // Membership support belongs to the whole source, not the final municipality
    // encountered in the snapshot (which can legitimately report zero votes).
    if (!previous || previous.VOTI === 0 && row.VOTI > 0) historicalByChoice.set(key, row);
  }
  const historical = [...historicalByChoice.values()];
  const observed = new Set(historical.filter(r => r.VOTI > 0 && r.LISTA !== 'astensione').flatMap(r =>
    sourceMembership(policy, r.ELEZIONE.replace('camera ', 'camera-').replace('senato ', 'senato-').replace('europee ', 'european-'), r.LISTA).members.map(canonical)));
  // Stable reform Camera numbering -> old-law constituency codes, matched by
  // names rather than assuming the distinct numeric namespaces coincide.
  const names = ['Piemonte 1', 'Piemonte 2', 'Lombardia 1', 'Lombardia 2', 'Lombardia 3', 'Lombardia 4', 'Veneto 1', 'Veneto 2',
    'Friuli-Venezia Giulia', 'Liguria', 'Emilia-Romagna', 'Toscana', 'Umbria', 'Marche', 'Lazio 1', 'Lazio 2', 'Abruzzo', 'Molise',
    'Campania 1', 'Campania 2', 'Puglia', 'Basilicata', 'Calabria', 'Sicilia 1', 'Sicilia 2', 'Sardegna'];
  const byName = new Map(snapshot.data.camera.uni.map(r => [r.CIRC_DEN, String(r.CIRC_COD)]));
  const ordinary = names.map(name => {
    const code = byName.get(name); if (!code) throw new Error(`Missing default constituency ${name}`); return code;
  });
  const restricted = future.restrictedCameraTerritories.map(id => ordinary[Number(id) - 1]);
  const lists = future.lists.map((l, i) => {
    const camera = l.filing === 'all' ? [...ordinary, '401', '201'] : l.filing === 'ordinary-taa' ? [...ordinary, '401'] :
      l.filing === 'ordinary' ? [...ordinary] : l.filing === 'restricted' ? [...restricted] : l.filing === 'vda' ? ['201'] : ['401'];
    const senato = [...new Set(camera.map(c => String(Math.floor(Number(c) / 100))))];
    const memberSet = new Set(l.members.map(canonical));
    // Reviewed, named generic polls, not the superseded provisional row.
    // Mixed-wave/group approximations remain explicit scenario assumptions.
    let reference: number | undefined;
    for (const p of [...review.polls].sort((a, b) => a.date.localeCompare(b.date))) {
      let sum = 0, matched = false;
      p.groups.forEach((g, index) => { if (g.every(id => memberSet.has(canonical(id)))) { sum += p.shares[index]; matched = true; } });
      if (matched) reference = sum;
    }
    const original = baseline.lists.find(b => b.id === l.id || b.name === l.name);
    return { id: l.id, name: l.name,
      coalition: 'coalition' in l ? future.coalitions.find(c => c.id === l.coalition)!.name : null,
      color: original?.color ?? ['#3267b1', '#5d8ed8', '#2f8a68', '#9b6b37', '#7559a0'][i % 5],
      startingShare: reference === undefined ? 0 : reference * 100,
      shareOverride: reference !== undefined && ![...memberSet].some(id => observed.has(id)),
      filing: { camera, senato }, ...('minorityRegion' in l ? { minorityRegion: l.minorityRegion } : {}) };
  });
  const coalitions = future.coalitions.map((c, i) => ({ id: c.id, name: c.name, color: i ? '#c04444' : '#3267b1' }));
  const correspondences = historical.flatMap((row, index) => {
    if (row.LISTA === 'astensione') return [];
    const event = row.ELEZIONE.replace('camera ', 'camera-').replace('senato ', 'senato-').replace('europee ', 'european-');
    const membership = sourceMembership(policy, event, row.LISTA);
    const explicit = (comparison.jointWeights as Record<string, Record<string, number>>)[`${event}:${row.LISTA}`];
    const weights = new Map<string, number>();
    for (const member of membership.members) {
      const target = future.lists.find(l => l.members.map(canonical).includes(canonical(member)))?.name ?? 'astensione';
      weights.set(target, (weights.get(target) ?? 0) + (explicit ? explicit[member] ?? 0 : 1 / membership.members.length));
    }
    const total = [...weights.values()].reduce((a, b) => a + b, 0);
    if (!total) throw new Error(`Empty historical composition ${event}/${row.LISTA}`);
    return [...weights].filter(([, w]) => w > 0).map(([futureList, weight], j) => ({
      id: `shared-default-${index}-${j}`, futureList, pastElection: row.ELEZIONE,
      pastDate: new Intl.DateTimeFormat('en-CA', { timeZone: 'Europe/Rome' }).format(new Date(row.DATA)),
      pastList: row.LISTA, factor: weight / total, source: 'bundled' as const,
    }));
  });
  const localShareOverrides: Scenario['localShareOverrides'] = [];
  for (const ref of released.references.filter(r => r.channel === 'common' || r.channel === 'camera')) {
    const target = ref.subject.kind === 'list' ? lists.find(l => l.id === ref.subject.id) :
      lists.find(l => future.lists.find(f => f.id === l.id)!.members.includes(ref.subject.id));
    if (!target) throw new Error(`Unresolved authored reference ${ref.id}`);
    if (ref.scope.kind === 'national') {
      // Existing named poll points retain precedence over weak unidentified priors.
      if (!target.shareOverride && target.startingShare === 0) { target.startingShare = ref.mean * 100; target.shareOverride = true; }
    } else localShareOverrides.push({ id: `shared-${ref.id}`, scope: ref.scope.kind,
      locationCode: ref.scope.code, list: target.name, startingShare: ref.mean * 100 });
  }
  return { ...baseline, id: `politics-${future.id}`, name: 'Politiche — scenario nazionale', electionDate: released.electionDate, lists, coalitions,
    listCorrespondences: correspondences, candidateTemplates: [], localShareOverrides,
    defaultSource: { ...baseline.defaultSource, notes: [...future.identityNotes,
      'Previous-law adapter: standalone lists have singleton electoral tickets; these are not political alliances.',
      'Senate filing covers whole corresponding regions, as in the shared authored definition.',
      'Historical joint counts use reviewed comparison weights, otherwise equal member weights. These are explicit legacy assumptions, not observed component counts.',
      'Named share references come from legacy-comparison-future-polls.json; unmapped historical votes retain the legacy abstention semantics.',
      'Historical lists retain projected means by default. Named poll references initialize editable shares; only lists without compatible historical membership activate those national overrides automatically.',
      `Special references retain the accepted reform default ${released.data.sourceBundleId}. The old shared field uses Camera/common means in both chambers; reference SDs retain the legacy mean-mode uncertainty law.`,
      'Territorial filing conditions the shared latent votes separately for each chamber while preserving each unit non-valid share.',
      'Special-only lists without identifiable compatible history retain a labelled proxy reference; no absent history is treated as an observed zero.',
    ] } };
}
