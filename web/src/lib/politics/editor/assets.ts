import { pollChoiceId } from '../reform-2026/vote-share/correspondences';
import { base } from '$app/paths';
import type { PoliticsScenario, Asset, Location, LawId, ReferenceScope } from '../scenario/schema';
import { laws, REFORM } from '../scenario/schema';
import { verifyAssetBytes } from '../scenario/dependencies';
import { locationKey, type DomainContext } from '../scenario/validation';
import type { ExecutionCatalogue, ExecutionElectorate } from '../execution/assets';
import type { RosatellumSeats } from '../execution/rosatellum';
import type { SeatTableBundle } from '../reform-2026/endowments';
import type { CompactSource, SourceGeography } from '../reform-2026/vote-share/source-types';
import type { CandidateContext } from './model';

export interface ReferenceNode { scope: ReferenceScope; name: string; children: ReferenceNode[] }
export interface FilingNode { location: Location; name: string; children: FilingNode[]; slots: number; role: 'ordinary' | 'special' | 'plurinominal' | 'uninominal' | 'bonus' | null }
export interface EditorAssets {
  context: CandidateContext;
  components: { id: string; name: string }[];
  sources: { sourceId: string; questionId: string; choiceId: string; name: string; members: readonly string[]; other: boolean }[];
  references: ReferenceNode[];
  filings: Record<LawId, FilingNode[]>;
  electorate: ExecutionElectorate;
}
/** Fetch exact pins, including inactive law geographies needed to create complete new subjects. No default substitution. */
export async function loadEditorAssets(s: PoliticsScenario, signal: AbortSignal): Promise<EditorAssets> {
  const values = new Map<string, unknown>();
  const pins = [s.data.catalogue, s.data.referenceGeography, s.data.historical, s.data.electorate,
    ...laws.flatMap(law => [s.data.lawAssets[law].geography, s.data.lawAssets[law].seats]),
    ...s.data.lawAssets[s.selection.scrutiny.id].models[s.selection.generatorsByLaw[s.selection.scrutiny.id].id]];
  await Promise.all(pins.map(async pin => {
    if (pin.schemaVersion !== 1) throw new Error(`Versione dati non supportata: ${pin.id} · schema ${pin.schemaVersion}`);
    const response = await fetch(`${base}/${pin.uri.replace(/^\//, '')}`, { signal });
    if (!response.ok) throw new Error(`Dati esatti non disponibili: ${pin.id} (${response.status})`);
    const bytes = new Uint8Array(await response.arrayBuffer());
    await verifyAssetBytes(pin, bytes);
    values.set(pin.id, JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes)));
  }));
  const get = <T>(pin: Asset) => values.get(pin.id) as T;
  const catalogue = get<ExecutionCatalogue>(s.data.catalogue);
  const locationKinds: Record<string, string> = {};
  const context: CandidateContext = { ...catalogue, referenceGeography: get(s.data.referenceGeography), availableAssets: pins, locationKinds };
  const historical = get<CompactSource>(s.data.historical), reform = get<SeatTableBundle>(s.data.lawAssets[REFORM].seats);
  const regions = reform.geography.regions;
  const geography = get<SourceGeography>(s.data.lawAssets[REFORM].geography);
  const references: ReferenceNode[] = regions.map(r => ({ scope: { kind: 'region', namespace: s.data.referenceGeography.version, code: r.id }, name: r.name, children: [] }));
  for (const target of geography.targets) {
    const region = references.find(r => r.scope.kind !== 'national' && r.scope.code === target.region);
    if (!region) continue;
    let province = region.children.find(p => p.scope.kind !== 'national' && p.scope.code === target.province);
    if (!province) { province = { scope: { kind: 'province', namespace: s.data.referenceGeography.version, code: target.province }, name: `Provincia ${target.province}`, children: [] }; region.children.push(province); }
    province.children.push({ scope: { kind: 'municipality', namespace: s.data.referenceGeography.version, code: target.code }, name: target.name, children: [] });
  }
  const filings = Object.fromEntries(laws.map(law => {
    const seats = get<RosatellumSeats | SeatTableBundle>(s.data.lawAssets[law].seats);
    const nodes = new Map<string, FilingNode>();
    for (const location of context.locations.filter(l => l.law === law)) {
      let name = location.code, slots = 0, role: FilingNode['role'] = null;
      if ('geography' in seats) {
        const t = seats.geography.territories.find(t => t.id === location.code && t.chamber === location.chamber);
        const c = seats.geography.colleges.find(c => c.id === location.code && c.chamber === location.chamber);
        if (c) locationKinds[locationKey(location)] = c.kind;
        name = t?.name ?? c?.name ?? name;
        const grid = seats.chambers.find(c => c.chamber === location.chamber);
        if (t?.kind === 'ordinary') { slots = grid?.territories.find(x => x.id === t.id)?.bonusSeats ?? 0; role = 'bonus'; }
        // C18-bis(3)/S9(4): ordinary and TAA proportional slates have seven candidates.
        if (c) { slots = c.kind === 'special-uninominal' ? 1 : 7; role = c.kind === 'special-uninominal' ? 'special' : 'ordinary'; }
      } else {
        const grid = seats.chambers.find(c => c.chamber === location.chamber);
        const uni = grid?.uninominal.find(c => String(c.UNI_COD) === location.code);
        const pluri = grid?.plurinominal.find(c => String(c.PLURI_COD) === location.code);
        if (location.level === 'uninominal') { name = `Collegio uninominale ${String(uni?.UNI_DEN ?? location.code)}`; slots = 1; role = 'uninominal'; }
        else if (location.level === 'plurinominal') { name = `Collegio plurinominale ${String(pluri?.PLURI_DEN ?? location.code)}`; slots = pluri?.MAX_CANDIDATI ?? 4; role = 'plurinominal'; }
        else { const row = grid?.uninominal.find(c => String(c.CIRC_COD) === location.code); name = `Circoscrizione ${String(row?.CIRC_DEN ?? location.code)}`; }
      }
      nodes.set(locationKey(location), { location, name, children: [], slots, role });
    }
    const children = new Set<string>();
    for (const relation of context.geographyRelations.filter(r => r.parent.law === law)) {
      const parent = nodes.get(locationKey(relation.parent)), child = nodes.get(locationKey(relation.child));
      if (parent && child) { parent.children.push(child); children.add(locationKey(child.location)); }
    }
    return [law, [...nodes].filter(([key]) => !children.has(key)).map(([, n]) => n)];
  })) as Record<LawId, FilingNode[]>;
  const sources = context.sources.flatMap(source => source.choiceIds.map(choiceId => {
    const choice = historical.payload.choices.find(c => c.id === choiceId);
    const poll = historical.payload.polls.find(p => p.id === source.id);
    const option = poll?.normalized.find(o => choiceId === pollChoiceId(poll!.id, o.label));
    return { sourceId: source.id, questionId: source.questionId, choiceId,
      name: choice?.label ?? option?.label ?? choiceId, members: choice?.members ?? option?.members ?? [], other: option?.other ?? false };
  }));
  return { context, components: context.componentIds.map(id => ({ id, name: s.subjects.components.find(c => c.id === id)?.name ??
      historical.payload.choices.find(c => c.members.length === 1 && c.members[0] === id)?.label ?? id })),
    sources, references, filings, electorate: get(s.data.electorate) };
}
export const referenceScopes = (node: ReferenceNode): ReferenceScope[] => [node.scope, ...node.children.flatMap(referenceScopes)];
export const filingNodes = (node: FilingNode): FilingNode[] => [node, ...node.children.flatMap(filingNodes)];
