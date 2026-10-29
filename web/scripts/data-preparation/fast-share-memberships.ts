/** Promoted source correspondence lookup. Original grouped counts are never split. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { sha256 } from './sources';
export interface MembershipRule { label?: string; event?: string; id?: string; labels?: string[]; members?: string[]; sources: string[] }
export interface MembershipPolicy { schemaVersion: 1; version: string; status: string; aliases: MembershipRule[]; joints: MembershipRule[]; eventOverrides: MembershipRule[]; reviewWarnings: string[] }
export async function loadSourceMemberships(root: string) {
  const paths = ['memberships-policy.json', 'membership-sources.json'];
  const bytes = await Promise.all(paths.map(p => readFile(resolve(root, 'data/sources/fast-share', p))));
  const policy = JSON.parse(bytes[0].toString()) as MembershipPolicy;
  const citations = JSON.parse(bytes[1].toString()) as { sources: { id: string; url: string }[] };
  if (policy.schemaVersion !== 1) throw new Error('Unsupported source membership policy');
  for (const r of [...policy.aliases, ...policy.joints, ...policy.eventOverrides])
    if (!r.sources.length || r.sources.some(id => !citations.sources.some(s => s.id === id))) throw new Error('Unknown membership citation');
  return { policy, citations, versions: paths.map((p, i) => 'fast-share/' + p + ':' + sha256(bytes[i])) };
}
export function sourceMembership(policy: MembershipPolicy, event: string, label: string, special = false) {
  if (special) return { members: ['special-' + sha256(label.normalize('NFKC')).slice(0, 16)], method: 'opaque-special-ballot', citations: ['official-label'], opaque: true };
  const override = policy.eventOverrides.filter(r => r.event === event && r.label === label);
  const joint = policy.joints.filter(r => r.label === label), alias = policy.aliases.filter(r => r.labels?.includes(label));
  if ([override, joint, alias].some(rs => rs.length > 1)) throw new Error('Ambiguous source membership: ' + label);
  const r = override[0] ?? joint[0] ?? alias[0];
  const members = r?.members ?? [r?.id ?? 'atomic-' + sha256(label.trim().normalize('NFKC')).slice(0, 10)];
  if (!members.length || new Set(members).size !== members.length) throw new Error('Invalid source correspondence');
  return { members: [...members].sort(), method: override.length ? 'event-override' : joint.length ? 'joint' : alias.length ? 'alias' : 'atomic',
    citations: r?.sources ?? ['official-label'], opaque: !r };
}
