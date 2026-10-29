import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { SourcePoll } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import { prepareSelectedPolls } from './polls';
import { sha256 } from './sources';
import { loadSourceMemberships, sourceMembership } from './fast-share-memberships';

export const SHARE_POLL_POLICY = { version: 'fast-share-effective-sample-v1', designEffect: 2, maximumEffectiveSample: 600, unknownDecidedFraction: 0.5 } as const;
export function reviewedEarlyView(original: { id: string; shares: number[] }, refined?: { id: string; shares: number[] }) {
  if (!refined) return { groups: [['pd'], ['m5s'], ['lega']], shares: original.shares.slice(0, 3), other: original.shares[3] };
  if (refined.id !== original.id || refined.shares.length !== 6 || refined.shares.some(x => !Number.isFinite(x) || x < 0) ||
    Math.abs(refined.shares.reduce((s, x) => s + x, 0) - 1) > 1e-8 || original.shares.slice(0, 3).some((x, i) => Math.abs(x - refined.shares[i]) > 1e-8) ||
    Math.abs(original.shares[3] - refined.shares.slice(3).reduce((s, x) => s + x, 0)) > 1e-8) throw new Error('Inconsistent reviewed early shares: ' + original.id);
  return { groups: [['pd'], ['m5s'], ['lega'], ['fdi'], ['fi-pdl']], shares: refined.shares.slice(0, 5), other: refined.shares[5] };
}
function entityMembers(id: string, date: string): string[] | undefined {
  // These named contemporary answers have no asserted historical lineage or
  // geographic donor. Retain source atoms; FV-04 must supply a new-party policy.
  if (['futuro-nazionale', 'progetto-civico', 'piu-uno', 'libdem', 'ora'].includes(id)) return ['poll-only-' + id];
  const groups: Record<string, string[]> = {
    pd: date >= '2019-01-01' ? ['pd', 'art1'] : ['pd'], fdi: ['fdi'], fi: ['fi-pdl'], pdl: ['fi-pdl'], lega: ['lega'], m5s: ['m5s'], sel: ['sel-si'], udc: ['udc'], fare: ['fare'],
    'scelta-civica': ['scelta-civica'], 'la-destra': ['la-destra'], 'piu-europa': date >= '2017-01-01' && date < '2019-01-01' ? ['piu-europa', 'centro-democratico'] : ['piu-europa'],
    'azione-italia-viva': ['azione', 'italia-viva'], azione: ['azione'], 'italia-viva': ['italia-viva'], 'noi-moderati': ['noi-residual', 'udc', 'scelta-civica'],
    'noi-italia': ['noi-residual', 'udc', 'scelta-civica'], avs: date >= '2022-01-01' && date < '2023-01-01' ? ['verdi', 'sel-si', 'possible'] : ['verdi', 'sel-si'],
    leu: ['sel-si', 'possible', 'art1'], insieme: ['psi-civici', 'verdi'], pap: ['prc', 'pci-pdci', 'pap-residual'], 'rivoluzione-civile': ['prc', 'pci-pdci', 'verdi', 'idv'],
    'impegno-civico': ['impegno-residual', 'centro-democratico'], 'unione-popolare': ['prc', 'pap-residual'], svp: ['svp-patt'], italexit: ['italexit'], psi: ['psi-civici']
  };
  return groups[id];
}
export async function prepareSharePolls(root: string) {
  const membership = await loadSourceMemberships(root), versions: string[] = [], polls: SourcePoll[] = [];
  const json = async (path: string) => { const bytes = await readFile(resolve(root, path)); versions.push(path + ':' + sha256(bytes)); return JSON.parse(bytes.toString()); };
  const refinements = await json('data/sources/fast-share/poll-refinements.json') as { questions: { id: string; sourceFile: string; sourceSha256: string; answers: { label: string; percentage: number; members?: string[]; sourceLabel?: string; other?: boolean }[] }[] };
  for (const q of refinements.questions) if (Math.abs(q.answers.reduce((n, a) => n + a.percentage, 0) - 100) > 1e-8) throw new Error('Refinement does not conserve shares');
  // Reviewed JSON is authoritative; optional documents never become required inputs.
  for (const directory of ['data/sources/polls/history', 'data/sources/polls']) {
    const archive = await prepareSelectedPolls(root, resolve(root, directory));
    versions.push(...archive.bundle.sourceVersions.map(v => directory + '/' + v));
    for (const q of archive.bundle.questions) {
      const refined = refinements.questions.find(r => r.id === q.id), reasons: string[] = [];
      if (q.ballot.kind === 'conditional') reasons.push('Conditional ballot requires explicit baseline treatment');
      if (q.normalization.method === 'missing-support-residual' && !refined) reasons.push('Material missing political support; no invented residual accepted');
      const normalized: SourcePoll['normalized'] = refined ? refined.answers.map(a => ({ label: a.label, share: a.percentage / 100, other: !!a.other,
        members: a.members ?? (a.sourceLabel ? sourceMembership(membership.policy, 'european-2024', a.sourceLabel).members : []) })) : q.support.map(a => ({ label: a.label, share: a.share, other: !a.entityIds.length,
        members: [...new Set(a.entityIds.flatMap(id => entityMembers(id, q.source.fieldworkEnd) ?? (id === 'fli' ? sourceMembership(membership.policy, 'camera-2013', "FUTURO E LIBERTA'").members : id === 'civica-popolare' ? sourceMembership(membership.policy, 'camera-2018', 'CIVICA POPOLARE LORENZIN').members : id === 'sud-chiama-nord' ? sourceMembership(membership.policy, 'camera-2022', 'SUD CHIAMA NORD').members : [])))] }));
      if (normalized.some(a => !a.other && !a.members.length)) reasons.push('Unmapped political answer');
      if (normalized.some(a => a.share === 0)) reasons.push('Printed-zero answer needs a censored observation model');
      const nonresponse = q.answers.find(a => a.kind === 'nonresponse')?.percentage, decided = nonresponse === undefined ? SHARE_POLL_POLICY.unknownDecidedFraction : 1 - nonresponse / 100;
      const context = /senato/i.test(q.text + ' ' + q.source.title) ? 'senato' : /european|europe[ae]|europee/i.test(q.text + ' ' + q.source.title) ? 'european' : 'camera';
      polls.push({ id: q.id, surveyId: q.independenceGroup, context,
        date: new Date((Date.parse(q.source.fieldworkStart) + Date.parse(q.source.fieldworkEnd)) / 2).toISOString().slice(0, 10), fieldworkStart: q.source.fieldworkStart, fieldworkEnd: q.source.fieldworkEnd, availableDate: q.source.publicationDate,
        citation: q.source.url, sampleSize: q.source.sampleSize, questionSampleSize: q.questionSampleSize, decidedFraction: decided,
        effectiveSample: Math.min(SHARE_POLL_POLICY.maximumEffectiveSample, (q.questionSampleSize ?? q.source.sampleSize * decided) / SHARE_POLL_POLICY.designEffect),
        designEffect: SHARE_POLL_POLICY.designEffect, effectiveSamplePolicy: SHARE_POLL_POLICY.version, errorGroup: q.source.fieldworkEnd.slice(0, 4) + '-H' + (Number(q.source.fieldworkEnd.slice(5, 7)) <= 6 ? 1 : 2) + '-' + context,
        roundingWidth: 0.001, original: q.answers.map(a => ({ label: a.label, percentage: a.percentage, kind: a.kind })), normalized, otherDefinition: 'offered-complement',
        status: reasons.length ? 'excluded' : 'supported', reasons, method: refined ? 'reviewed-full-question-replaces-coarse-view' : 'reviewed-reported-support' });
    }
  }
  const early = await json('data/sources/polls/history/generation-horizon-observations.json');
  const six = await json('data/sources/polls/history/generation-six-category-observations.json');
  if (six.panel.join(',') !== 'pd,m5s,lega,fdi,fi-pdl,other') throw new Error('Unknown early poll panel');
  for (const pin of six.inputPins) {
    if (!['archive.json','manifest.json','elections-manifest.json','generation-horizon-observations.json','generation-horizon-manifest.json'].includes(pin.path) ||
      sha256(await readFile(resolve(root, 'data/sources/polls/history', pin.path))) !== pin.sha256) throw new Error('Reviewed early poll input pin changed');
  }
  for (const p of early.observations) {
    const refined = six.earlyObservations.find((r: { id: string }) => r.id === p.id);
    if (refined && ['publicationDate', 'fieldworkStart', 'fieldworkEnd'].some(k => refined[k] !== p[k])) throw new Error('Early view dates differ');
    const view = reviewedEarlyView(p, refined), normalized: SourcePoll['normalized'] = view.groups.map((g, i) => ({ label: g.join('+'), members: g, share: view.shares[i], other: false }));
    normalized.push({ label: 'Other', members: [], share: view.other, other: true });
    polls.push({ id: p.id, surveyId: p.sourceId, context: 'camera', date: new Date((Date.parse(p.fieldworkStart) + Date.parse(p.fieldworkEnd)) / 2).toISOString().slice(0, 10),
      fieldworkStart: p.fieldworkStart, fieldworkEnd: p.fieldworkEnd, availableDate: p.publicationDate, citation: p.tableUrl ?? p.sourceUrl,
      sampleSize: p.sampleSize, questionSampleSize: p.questionSampleSize, decidedFraction: 0.5, designEffect: 2, effectiveSamplePolicy: SHARE_POLL_POLICY.version,
      effectiveSample: Math.min(600, (p.questionSampleSize ?? p.sampleSize * 0.5) / 2), errorGroup: p.fieldworkEnd.slice(0, 4) + '-H' + (Number(p.fieldworkEnd.slice(5, 7)) <= 6 ? 1 : 2) + '-camera', roundingWidth: 0.001,
      original: p.shares.map((x: number, i: number) => ({ label: early.panel[i], percentage: x * 100, kind: i === 3 ? 'residual' : 'list' })), normalized,
      otherDefinition: 'offered-complement', status: normalized.some(a => a.share <= 0) ? 'excluded' : 'supported', reasons: normalized.some(a => a.share <= 0) ? ['Printed-zero answer'] : [], method: refined ? 'reviewed-six-category-FDI-FI-repair' : 'reviewed-coarse-question' });
  }
  if (new Set(polls.map(p => p.id)).size !== polls.length) throw new Error('Duplicate bundled question ID');
  return { polls: polls.sort((a, b) => a.id.localeCompare(b.id)), sourceVersions: [...versions, ...membership.versions].sort() };
}
