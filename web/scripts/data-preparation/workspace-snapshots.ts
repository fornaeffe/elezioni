import { readFile, mkdir, writeFile, rename } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import type { Scenario } from '../../src/lib/core/types';
import type { PoliticsStaticSnapshot } from '../../src/lib/politics/types';
import type { RegionalErStaticSnapshot } from '../../src/lib/regional-er/types';
import { buildPoliticsParametersFromHistoricalVotes } from '../../src/lib/politics/parameter-preparation';
import { readDefinitiveGeography } from './definitive-geography';
import { readManifest } from './normalize';
import { prepareWorkspaceHistory, electionTime } from './workspace-history';
import { prepareWorkspaceGeography } from './workspace-geography';
import { sha256, writeVersion } from './sources';

interface AuthoredWorkspace {
  id: string; name: string; electionDate: string; startingElection: string | null; defaultBirthDate: string;
  lists: { name: string; coalition: string | null; color: string }[];
  coalitions: { name: string; color: string }[];
  correspondences: { election: string; pastList: string; futureList: string; factor: number }[];
  candidateGeneration: { uninominalToPlurinominalShare: number; plurinominalCandidacyCountShares: number[] };
}
export function authoredScenario(input: AuthoredWorkspace, kind: 'politics' | 'regional-er'): Scenario {
  if (!input.lists.length || new Set(input.lists.map(l => l.name)).size !== input.lists.length ||
      input.correspondences.some(r => !Number.isFinite(r.factor) || r.factor < 0 || !['astensione', ...input.lists.map(l => l.name)].includes(r.futureList))) throw new Error(`Invalid authored ${kind} choices`);
  return { id: input.id, name: input.name, electionDate: input.electionDate,
    defaultSource: { kind: 'bundled', electionKind: kind === 'politics' ? 'politiche' : 'regionali-er', territory: kind === 'politics' ? 'Italia' : 'Emilia-Romagna', dataVersion: 'workspace-preparation-v1' },
    globalShareMode: 'mean', abstentionShare: 0, abstentionOverride: false,
    lists: input.lists.map((l, i) => ({ ...l, id: `list-${i + 1}`, startingShare: 0, shareOverride: false })),
    coalitions: input.coalitions.map((c, i) => ({ ...c, id: `coalition-${i + 1}` })),
    listCorrespondences: input.correspondences.map((r, i) => ({ id: `source-${i}`, pastElection: r.election, pastDate: electionTime(r.election).slice(0, 10), pastList: r.pastList, futureList: r.futureList, factor: r.factor, source: 'bundled' })),
    localShareOverrides: [], candidateTemplates: [], candidateGeneration: { uninominalToPlurinominalShare: input.candidateGeneration.uninominalToPlurinominalShare,
      // The source stores the existing R fractions; the existing default compiler converts them.
      plurinominalCandidacyCountShares: [1, 0, 0, 0, 0] } };
}
const metadata = (m: Awaited<ReturnType<typeof readDefinitiveGeography>>['municipalities'][number]) => ({
  CODICE_COMUNE: m.code, COMUNE: m.name, CODICE_PROVINCIA: m.provinceId, PROVINCIA: m.province, CODICE_REGIONE: m.regionId, REGIONE: m.region,
});
export async function prepareWorkspaceSnapshots(root: string, sources: string) {
  const configBytes = await readFile(resolve(root, 'data/workspace-defaults.json'), 'utf8');
  const config = JSON.parse(configBytes) as { schemaVersion: number; workspaces: Record<'politics' | 'regional-er', AuthoredWorkspace> };
  if (config.schemaVersion !== 1) throw new Error('Unsupported authored defaults');
  const manifest = await readManifest(root), geo = await readDefinitiveGeography(manifest, sources);
  const history = await prepareWorkspaceHistory(root, sources, geo);
  const electorate = (election: string) => {
    const result = new Map<string, number>();
    for (const r of history.rows.filter(r => r.ELEZIONE === election)) result.set(String(r.CODICE_COMUNE), (result.get(String(r.CODICE_COMUNE)) ?? 0) + r.VOTI);
    return result;
  };
  const geography = await prepareWorkspaceGeography(geo, manifest, sources, electorate('europee 2024'));
  const provenance = { method: 'node-workspace-preparation-v1', geographyVersion: geography.version, authoredDefaultsSha256: sha256(JSON.stringify(config)), sourceHashes: [...geography.audit.sourceHashes, ...history.sourceHashes],
    notes: [...geography.audit.notes, 'Risultati di lista diretti; il residuo sugli elettori conserva la convenzione astensione del modello precedente. Le ricostruzioni per fusioni/scorpori sono stime censuarie esplicite.',
      'Le anomalie dei denominatori nelle fonti storiche restano esplicite nell’audit: i residui negativi sono conservati, senza inventare elettori o cancellare voti.'] };
  const buildParameters = (kind: 'politics' | 'regional-er') => {
    const authored = config.workspaces[kind], scenario = authoredScenario(authored, kind);
    const codes = new Set(geo.municipalities.filter(m => kind === 'politics' || m.regionId === '08').map(m => m.code));
    const votes = history.rows.filter(r => codes.has(String(r.CODICE_COMUNE)) && (kind !== 'politics' || !r.ELEZIONE.startsWith('regionali')));
    const prepared = buildPoliticsParametersFromHistoricalVotes(votes, scenario, { percentualiPartenza: authored.startingElection });
    return { authored, scenario, votes, prepared,
      correspondences: scenario.listCorrespondences.map(r => ({ DATA: electionTime(r.pastElection), ELEZIONE: r.pastElection, LISTA_ORIGINALE: r.pastList, LISTA: r.futureList, FATTORE: r.factor })),
      coalitions: authored.coalitions.map(c => ({ COALIZIONE: c.name, COLORE: c.color })) };
  };
  const p = buildParameters('politics');
  const candidates = (chamber: 'camera' | 'senato') => ({
    candidati_uni: p.authored.coalitions.flatMap(c => geography[chamber].uni.map(u => ({ COALIZIONE: c.name, UNI_COD: u.UNI_COD, LISTA_MINORANZA: null, CANDIDATO_ID: null, DATA_NASCITA: null }))),
    candidati_pluri: p.authored.lists.flatMap(l => geography[chamber].pluri.flatMap(u => u.PLURI_COD === null ? [] : Array.from({ length: u.MAX_CANDIDATI ?? 0 }, (_, i) => ({ CIRC_COD: u.CIRC_COD, LISTA: l.name, PLURI_COD: u.PLURI_COD!, NUMERO_CANDIDATO: i + 1, MINORANZA: false, CANDIDATO_ID: null, DATA_NASCITA: null })))),
  });
  const politics: PoliticsStaticSnapshot = {
    metadata: { schema_version: 4, source: 'Pinned Node/TypeScript workspace preparation', purpose: 'Production previous-law data; definitive geography without reform overlays.', preparation: provenance },
    data: { base_dati: geography.base_dati, municipalities: geo.municipalities.map(metadata), comuni_liste_elezioni: p.votes, camera: geography.camera, senato: geography.senato },
    default_scenario: { id: p.authored.id, name: p.authored.name, data_elezione: `${p.authored.electionDate}T00:00:00.000Z`, frazione_uni_in_pluri: p.authored.candidateGeneration.uninominalToPlurinominalShare,
      frazioni_pluricandidature: p.authored.candidateGeneration.plurinominalCandidacyCountShares, default_data_nascita: `${p.authored.defaultBirthDate}T00:00:00.000Z`,
      liste: p.prepared.liste, comuni_liste: p.prepared.comuni_liste, liste_elezioni: p.prepared.liste_elezioni, coalizioni: p.coalitions, corrispondenza_liste: p.correspondences,
      camera: candidates('camera'), senato: candidates('senato') },
  };
  const er = buildParameters('regional-er'), erElectors = electorate('regionali 2024');
  const regionalMunicipalities = geo.municipalities.filter(m => m.regionId === '08'), byCode = new Map(regionalMunicipalities.map(m => [m.code, m]));
  const regional: RegionalErStaticSnapshot = {
    metadata: { schema_version: 1, source: 'Pinned Node/TypeScript workspace preparation', purpose: 'Production Emilia-Romagna data on the shared census reference.', region: 'Emilia-Romagna',
      preparation: { ...provenance, notes: ['Montecopiolo e Sassofeltrio non hanno osservazioni regionali 2020 perché allora fuori regione: dati assenti, non zeri. Le osservazioni successive sono disponibili nel riferimento comunale del censimento 2021.'] } },
    data: { municipalities: regionalMunicipalities.map(m => ({ ...metadata(m), ELETTORI: erElectors.get(m.code)! })), comuni_liste_elezioni: er.votes,
      pop_legale: regionalMunicipalities.map(m => ({ ...metadata(m), POPOLAZIONE: m.population })) },
    default_scenario: { id: er.authored.id, name: er.authored.name, data_elezione: `${er.authored.electionDate}T00:00:00.000Z`,
      liste: er.prepared.liste.map(l => ({ ...l, COLORE: er.authored.lists.find(a => a.name === l.LISTA)?.color ?? null })),
      comuni_liste: er.prepared.comuni_liste.map(r => ({ ...r, ...metadata(byCode.get(String(r.CODICE_COMUNE))!), ELETTORI: erElectors.get(String(r.CODICE_COMUNE))! })),
      liste_elezioni: er.prepared.liste_elezioni, coalizioni: er.coalitions, corrispondenza_liste: er.correspondences },
  };
  return { politics, regional, audit: { provenance, geography: geography.audit, history: history.audits } };
}

export async function replaceGenerated(path: string, bytes: Uint8Array | string) {
  await mkdir(dirname(path), { recursive: true });
  const temporary = `${path}.${process.pid}.tmp`;
  await writeFile(temporary, bytes); await rename(temporary, path);
}
export async function writeWorkspaceSnapshots(root: string, sources: string, output: string, publish: boolean, check = false) {
  const prepared = await prepareWorkspaceSnapshots(root, sources), entries = [];
  for (const [name, snapshot] of [['politics-static', prepared.politics], ['regional-er-static', prepared.regional]] as const) {
    const bytes = `${JSON.stringify(snapshot)}\n`, digest = sha256(bytes), version = `${name}-node-v1-${digest.slice(0, 12)}`;
    if (check) {
      const installed = await readFile(resolve(root, `web/static/data/v1/${name}.json`));
      if (sha256(installed) !== digest) throw new Error(`Stale workspace data ${name}; run npm run data:prepare`);
    } else {
      await writeVersion(resolve(output, `${version}.json`), snapshot);
      if (publish) await replaceGenerated(resolve(root, `web/static/data/v1/${name}.json`), bytes);
    }
    entries.push({ name, version, bytes: Buffer.byteLength(bytes), sha256: digest });
  }
  if (!check) await writeVersion(resolve(output, `workspace-audit-${sha256(JSON.stringify(prepared.audit)).slice(0, 12)}.json`), prepared.audit);
  return entries;
}
