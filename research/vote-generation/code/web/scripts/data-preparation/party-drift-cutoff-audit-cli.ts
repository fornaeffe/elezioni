/** Necessary admissibility audit before expensive full-list chronological fits.
 * Unknown parties receive NO held-out share, pseudo-count or silent zero. */
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { loadGenerationHistory } from './generation-history';
import { prepareSelectedPolls } from './polls';
import { buildMembershipTable, loadMembershipPolicy, membershipFor } from './party-drift-memberships';
import { componentResolution } from './party-drift-identifiability';
import { sha256 } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url)), history = await loadGenerationHistory(root), inputs = await loadMembershipPolicy(root), table = buildMembershipTable(history, inputs.policy);
const refinementPath = 'data/sources/party-drift/poll-refinements.json', bytes = await readFile(resolve(root, refinementPath));
const refinements = JSON.parse(bytes.toString('utf8')) as { questions: { id: string; sourceFile: string; sourceSha256: string; answers: { label: string; percentage: number; members?: string[]; sourceLabel?: string; other?: boolean }[] }[] };
// Missing local PDFs do not invalidate reviewed JSON; present bytes must match.
for (const q of refinements.questions) {
  let pdf: Buffer | undefined;
  try { pdf = await readFile(resolve(root, q.sourceFile)); } catch (error) { if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error; }
  if (pdf && sha256(pdf) !== q.sourceSha256) throw new Error('Refined poll PDF differs: ' + q.id);
  if (Math.abs(q.answers.reduce((s, a) => s + a.percentage, 0) - 100) > 1e-8) throw new Error('Refined source percentages do not conserve');
}
const polls = [], sources = [...history.sourceVersions, ...inputs.versions, refinementPath + ':' + sha256(bytes)];
const entity = (id: string, date: string): string[] | undefined => {
  const literal: Record<string, string[]> = { pd: date >= '2019-01-01' ? ['pd', 'art1'] : ['pd'], fdi: ['fdi'], fi: ['fi-pdl'], pdl: ['fi-pdl'], lega: ['lega'], m5s: ['m5s'],
    sel: ['sel-si'], udc: ['udc'], fare: ['fare'], 'scelta-civica': ['scelta-civica'], 'la-destra': ['la-destra'], 'piu-europa': date >= '2017-01-01' && date < '2019-01-01' ? ['piu-europa', 'centro-democratico'] : ['piu-europa'],
    'azione-italia-viva': ['azione', 'italia-viva'], azione: ['azione'], 'italia-viva': ['italia-viva'], 'noi-moderati': ['noi-residual', 'udc', 'scelta-civica'], 'noi-italia': ['noi-residual', 'udc', 'scelta-civica'],
    avs: date >= '2022-01-01' && date < '2023-01-01' ? ['verdi', 'sel-si', 'possible'] : ['verdi', 'sel-si'],
    leu: ['sel-si', 'possible', 'art1'], insieme: ['psi-civici', 'verdi'], pap: ['prc', 'pci-pdci', 'pap-residual'],
    'rivoluzione-civile': ['prc', 'pci-pdci', 'verdi', 'idv'], 'impegno-civico': ['impegno-residual', 'centro-democratico'], 'unione-popolare': ['prc', 'pap-residual'], svp: ['svp-patt'], italexit: ['italexit'],
    fli: membershipFor(inputs.policy, 'camera-2013', "FUTURO E LIBERTA'").members,
    'civica-popolare': membershipFor(inputs.policy, 'camera-2018', 'CIVICA POPOLARE LORENZIN').members };
  return literal[id];
};
for (const path of ['data/sources/polls/history', 'data/sources/polls']) {
  const archive = await prepareSelectedPolls(root, resolve(root, path)); sources.push(...archive.bundle.sourceVersions);
  for (const q of archive.bundle.questions) {
    if (q.ballot.kind === 'conditional') continue;
    const refined = refinements.questions.find(r => r.id === q.id), groups: string[][] = []; let other = false, unresolved = false;
    if (refined) {
      for (const a of refined.answers) {
        if (a.other) { other = true; continue; }
        const members = a.members ?? (a.sourceLabel ? membershipFor(inputs.policy, 'european-2024', a.sourceLabel).members : undefined);
        if (members) groups.push(members); else unresolved = true;
      }
    } else for (const a of q.support) {
      if (!a.entityIds.length) { other = true; continue; }
      const members = a.entityIds.flatMap(id => entity(id, q.source.fieldworkEnd) ?? []);
      if (a.entityIds.some(id => !entity(id, q.source.fieldworkEnd))) unresolved = true;
      else groups.push([...new Set(members)]);
    }
    // Known original answer groups are retained collectively; no inferred
    // subdivision of a coarse complementary answer creates information.
    polls.push({ id: q.id, date: q.source.fieldworkEnd, available: q.source.publicationDate, groups, other, unresolved, refined: !!refined });
  }
}
const earlyPath = 'data/sources/polls/history/generation-horizon-observations.json', earlyBytes = await readFile(resolve(root, earlyPath));
sources.push(earlyPath + ':' + sha256(earlyBytes));
for (const q of JSON.parse(earlyBytes.toString('utf8')).observations) polls.push({ id: q.id, date: q.fieldworkEnd, available: q.publicationDate, groups: [['pd'], ['m5s'], ['lega']], other: true, unresolved: false, refined: false });
const rows = [];
for (const target of history.events.filter(e => e.date >= '2013-01-01')) for (const horizon of [730, 365, 90, 15]) {
  const targetDate = target.id === 'european-2024' ? '2024-06-08' : target.date;
  const cutoff = new Date(Date.parse(targetDate) - horizon * 86400000).toISOString().slice(0, 10);
  const observed = table.rows.filter(r => r.usedInCommonDomain && r.date < cutoff), future = table.rows.filter(r => r.usedInCommonDomain && r.event === target.id);
  const currentPolls = polls.filter(q => q.date <= cutoff && q.available <= cutoff && !q.unresolved);
  const allIds = [...new Set([...observed.flatMap(r => r.members), ...future.flatMap(r => r.members), ...currentPolls.flatMap(q => q.groups.flat())])].sort();
  const information = [...observed.map(r => ({ id: r.event + ':' + r.label, groups: [r.members] })), ...currentPolls.map(q => {
    const named = q.groups.flat(), complement = allIds.filter(id => !named.includes(id));
    // Deliberately OPTIMISTIC: even grant all unknown future components a place
    // in Other. Failure still cannot be cured by merely adding those components
    // to the complement. This is not a fitted questionnaire-availability policy.
    return { id: q.id, groups: q.other && complement.length ? [...q.groups, complement] : q.groups };
  })];
  const audit = componentResolution(allIds, information), futureIds = new Set(future.flatMap(r => r.members));
  const futureDirections = audit.nullDirections.filter(d => d.some(x => futureIds.has(x.id)));
  const distinguishable = new Set([...observed.flatMap(r => r.members), ...currentPolls.flatMap(q => q.groups.flat())]);
  const unseen = [...futureIds].filter(id => !distinguishable.has(id));
  // Components may be made atomic only when every relevant membership INCLUDING
  // the future ballot agrees. Do not collapse parties that must compete later.
  const signatures = new Map<string, string[]>();
  for (const id of allIds) {
    const signature = information.flatMap(q => q.groups).map(g => Number(g.includes(id))).join('') + '/' + future.map(r => Number(r.members.includes(id))).join('');
    const ids = signatures.get(signature) ?? []; ids.push(id); signatures.set(signature, ids);
  }
  const collapsible = [...signatures.values()].filter(g => g.length > 1);
  const irreducibleFutureDirections = futureDirections.filter(d => !collapsible.some(group => d.every(x => group.includes(x.id))));
  rows.push({ target: target.id, horizon, cutoff, rank: audit.rank, dimensions: audit.dimensions, unseenFutureComponents: unseen,
    unresolvedFutureDirections: futureDirections, irreducibleFutureDirections, collapsibleGroups: collapsible,
    // Being unnamed is not itself disqualifying: one unknown contribution can
    // be constrained through an informative complement and independent inputs.
    passedNecessaryResolution: irreducibleFutureDirections.length === 0,
    availablePolls: currentPolls.map(q => q.id), refinedQuestions: currentPolls.filter(q => q.refined).map(q => q.id),
    note: 'Optimistic necessary screen only; no Stan fit, birth initialization, availability validation or posterior accuracy acceptance is implied.' });
}
const version = 'cutoff-resolution-' + sha256(JSON.stringify({ sources, policy: inputs.policy, rows })).slice(0, 12), dir = resolve(root, 'data/derived/party-drift', version);
await mkdir(dir, { recursive: true });
await writeFile(resolve(dir, 'audit.json'), JSON.stringify({ version, sources, rows, productionChanged: false, fullDataFitStarted: false }, null, 2) + '\n');
console.log(JSON.stringify({ version, path: resolve(dir, 'audit.json'), cases: rows.length, blocked: rows.filter(r => !r.passedNecessaryResolution).length,
  casesSummary: rows.map(r => ({ target: r.target, horizon: r.horizon, unseen: r.unseenFutureComponents.length, irreducible: r.irreducibleFutureDirections.length, passedNecessaryResolution: r.passedNecessaryResolution })) }));
