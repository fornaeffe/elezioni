import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { prepareShareHistory } from './fast-share-history';
import { prepareSharePolls } from './fast-share-polls';
import { encodeSource, compressAsset, canonicalJson, decodeSourceRows } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { OriginalSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';

export async function prepareFastShareSource(root: string, sourceDirectory: string) {
  const history = await prepareShareHistory(root, sourceDirectory), polls = await prepareSharePolls(root);
  const policy = JSON.parse(await readFile(resolve(root, 'data/fast-share-source-manifest.json'), 'utf8'));
  const colleges = new Map(history.catalog.colleges.map(c => [c.id, c]));
  const original: OriginalSource = {
    sourceVersions: [...history.sourceVersions, ...polls.sourceVersions],
    methodVersions: ['fast-share-history-v1', 'reviewed-source-correspondences-v1', 'retrospective-thirty-day-lag-v1', 'fast-share-effective-sample-v1'], assumptions: policy.assumptions,
    correspondencePolicy: { id: history.membership.policy.version, status: history.membership.policy.status, warnings: history.membership.policy.reviewWarnings },
    citations: history.membership.citations.sources.map(s => ({ id: s.id, url: s.url })),
    units: history.units, choices: history.choices, events: history.events, polls: polls.polls,
    geography: { version: history.catalog.version, censusDate: history.catalog.censusDate,
      targets: history.catalog.municipalities.map(m => ({ code: m.code, name: m.name, region: m.regionId, province: m.provinceId, population: m.population })),
      colleges: history.catalog.colleges.filter(c => c.kind !== 'taa-proportional').map(c => ({ id: c.id, chamber: c.chamber, kind: c.kind, territoryId: c.territoryId })),
      cells: history.catalog.memberships.filter(m => colleges.get(m.collegeId)!.kind !== 'taa-proportional').map(m => ({ chamber: m.chamber, collegeId: m.collegeId, unitId: m.unitId, municipality: m.municipalityCode, population: m.population })) }
  };
  const encoded = await encodeSource(original);
  // Verify every encoded event against exact input counts before writing files.
  for (const event of encoded.envelope.payload.events) {
    const rows = await decodeSourceRows(encoded.envelope, event.id, encoded.assets.get(event.chunk.path)!);
    const expected = original.events.find(e => e.id === event.id)!.rows;
    if (canonicalJson(rows) !== canonicalJson(expected)) throw new Error('Original count round trip differs: ' + event.id);
  }
  const meta = await compressAsset('source', new TextEncoder().encode(canonicalJson(encoded.envelope)));
  encoded.assets.set(meta.descriptor.path, meta.compressed);
  const unitById = new Map(history.units.map(u => [u.id, u]));
  return { ...encoded, descriptor: meta.descriptor, audit: { stage: 'FV-03', sourceId: encoded.envelope.id,
    sourcePolicy: policy.id, sourceEvents: history.audits, geography: { version: original.geography.version, municipalities: original.geography.targets.length, physicalColleges: original.geography.colleges.length, cells: original.geography.cells.length },
    targetMissingHistories: original.events.map(e => { const observed = new Set(e.rows.map(r => unitById.get(r.unit)!.targetCode)); return { event: e.id, missingTargets: original.geography.targets.filter(t => !observed.has(t.code)).map(t => t.code) }; }),
    polls: { total: polls.polls.length, supported: polls.polls.filter(q => q.status === 'supported').length, exclusions: polls.polls.filter(q => q.status === 'excluded').map(q => ({ id: q.id, reasons: q.reasons })) },
    membershipWarnings: history.membership.policy.reviewWarnings,
    sizes: { compressedBytes: [...encoded.assets.values()].reduce((n, b) => n + b.length, 0), decodedBytes: meta.descriptor.decodedBytes + encoded.envelope.payload.events.reduce((n, e) => n + e.chunk.decodedBytes, 0) },
    roundTripOriginalCounts: true, productionDefaultSelected: false, senateAdapterValidated: false } };
}
