import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { sha256 } from './sources';
import type { GenerationHistory } from './generation-history';
import { componentResolution } from './party-drift-identifiability';

export interface MembershipRule { label?: string; event?: string; id?: string; labels?: string[]; members?: string[]; sources: string[]; note?: string; }
export interface MembershipPolicy { schemaVersion: 1; version: string; status: string; policy: string; aliases: MembershipRule[]; joints: MembershipRule[]; eventOverrides: MembershipRule[]; reviewWarnings: string[]; }
export async function loadMembershipPolicy(root: string) {
  const path = 'data/sources/party-drift/memberships-policy.json', sourcesPath = 'data/sources/party-drift/membership-sources.json';
  const [bytes, sourceBytes] = await Promise.all([readFile(resolve(root, path)), readFile(resolve(root, sourcesPath))]);
  const policy = JSON.parse(bytes.toString('utf8')) as MembershipPolicy;
  const sources = JSON.parse(sourceBytes.toString('utf8')) as { sources: { id: string; url: string; supports: string; type: string }[] };
  if (policy.schemaVersion !== 1) throw new Error('Unknown membership policy');
  for (const rule of [...policy.aliases, ...policy.joints, ...policy.eventOverrides]) for (const id of rule.sources)
    if (!sources.sources.some(s => s.id === id)) throw new Error('Unpinned membership citation: ' + id);
  return { policy, sources, versions: [path + ':' + sha256(bytes), sourcesPath + ':' + sha256(sourceBytes)] };
}
export function membershipFor(policy: MembershipPolicy, event: string, label: string) {
  const override = policy.eventOverrides.filter(r => r.event === event && r.label === label);
  if (override.length > 1) throw new Error('Duplicate membership override');
  const joint = policy.joints.filter(r => r.label === label);
  if (joint.length > 1) throw new Error('Duplicate joint-list rule');
  const alias = policy.aliases.filter(r => r.labels?.includes(label));
  if (alias.length > 1) throw new Error('Overlapping membership aliases: ' + label);
  const rule = override[0] ?? joint[0] ?? alias[0];
  const members = rule?.members ?? (rule?.id ? [rule.id] : ['atomic-' + sha256(label.trim().normalize('NFKC')).slice(0, 10)]);
  if (!members.length || new Set(members).size !== members.length) throw new Error('Empty/overlapping list membership');
  return { members, sources: rule?.sources ?? ['official-label'], note: rule?.note ?? (rule ? null : 'Atomic source-list identity; no unsupported organizational decomposition'),
    method: override.length ? 'event-override' : joint.length ? 'joint' : alias.length ? 'alias' : 'atomic' };
}
export function buildMembershipTable(history: GenerationHistory, policy: MembershipPolicy) {
  const rows = history.events.flatMap(e => [...new Set(e.municipalities.flatMap(c => c.lists.map(l => l.name)))].sort().map(label => ({
    event: e.id, date: e.date, kind: e.kind, sourceId: e.sourceId, sourceSha256: e.sourceSha256, label,
    ...membershipFor(policy, e.id, label), usedInCommonDomain: e.municipalities.some(c => c.regionId !== '02' && c.matchable && c.electors > 0 && c.validVotes <= c.electors && c.lists.some(l => l.name === label)) } )));
  const ids = [...new Set(rows.filter(r => r.usedInCommonDomain).flatMap(r => r.members))].sort();
  const components = ids.map(id => ({ id, supportStart: rows.filter(r => r.usedInCommonDomain && r.members.includes(id)).map(r => r.date).sort()[0],
    supportConvention: 'first-supported-observation', sources: [...new Set(rows.filter(r => r.members.includes(id)).flatMap(r => r.sources))] }));
  let cells = 0, validVotes = 0, electors = 0;
  for (const e of history.events) for (const c of e.municipalities.filter(c => c.regionId !== '02' && c.matchable && c.electors > 0 && c.validVotes <= c.electors)) {
    const memberships = c.lists.flatMap(l => membershipFor(policy, e.id, l.name).members);
    if (memberships.length !== new Set(memberships).size) throw new Error('Component competes in two local lists: ' + e.id + '/' + c.id);
    if (c.lists.reduce((s, l) => s + l.votes, 0) !== c.validVotes) throw new Error('Original list-count conservation failure');
    cells++; validVotes += c.validVotes; electors += c.electors;
  }
  const resolution = componentResolution(ids, rows.filter(r => r.usedInCommonDomain).map(r => ({ id: r.event + ':' + r.label, groups: [r.members] })));
  const eventAudits = history.events.map(e => {
    const observed = rows.filter(r => r.usedInCommonDomain && r.date <= e.date), active = [...new Set(observed.flatMap(r => r.members))].sort();
    return { event: e.id, date: e.date, ...componentResolution(active, observed.map(r => ({ id: r.event + ':' + r.label, groups: [r.members] }))) };
  });
  return { schemaVersion: 1, policyVersion: policy.version, policyStatus: policy.status, rows, components, resolution, eventAudits,
    conservation: { municipalElectionObservations: cells, originalValidVotes: validVotes, electors, originalCountsDecomposed: false }, reviewWarnings: policy.reviewWarnings };
}
