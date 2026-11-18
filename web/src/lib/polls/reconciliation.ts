import type { NormalizedPollQuestion, PollEntityCatalog, PollReconciliation, PollReconciliationContext } from './types';

export const pollNameKey = (name: string): string => name.normalize('NFKD').replace(/[\u0300-\u036f]/g, '')
  .toUpperCase().replace(/[^A-Z0-9+]/g, '');
const sum = (values: Iterable<number>) => [...values].reduce((a, b) => a + b, 0);

/** Deterministic mass-conserving preparation preview. Missing political evidence
 * uses labelled estimates; malformed inputs still fail. No scenario is mutated. */
export function reconcilePoll(
  question: NormalizedPollQuestion,
  catalog: PollEntityCatalog,
  context: PollReconciliationContext
): PollReconciliation {
  const { lists, historicalLists, correspondences } = context;
  if (!lists.length || new Set(lists.map(l => l.id)).size !== lists.length || lists.some(l => !l.id || !l.name ||
    ['ASTENSIONE', 'ABSTENTION'].includes(pollNameKey(l.name)) ||
    l.baselineShare != null && (!Number.isFinite(l.baselineShare) || l.baselineShare < 0 || l.baselineShare > 1))) {
    throw new Error('Poll reconciliation requires unique eligible lists and finite baseline shares');
  }
  const entities = new Map(catalog.entities.map(e => [e.id, e]));
  const aliases = new Map(catalog.entities.flatMap(e => [e.name, ...e.aliases].map(a => [pollNameKey(a), e] as const)));
  const entityIds = (name: string) => {
    const e = aliases.get(pollNameKey(name));
    return e ? e.components.length ? e.components : [e.id] : [];
  };
  const targets = new Set(lists.map(l => l.id));
  const pastIds = new Set(historicalLists.map(l => l.id));
  for (const c of correspondences) if (!pastIds.has(c.pastListId) || !targets.has(c.futureListId) || !Number.isFinite(c.factor) || c.factor < 0) {
    throw new Error('Invalid historical poll correspondence');
  }
  const mappings = context.mappings ?? [];
  for (const m of mappings) if (!entities.has(m.entityId) || !targets.has(m.listId) || !Number.isFinite(m.factor) || m.factor < 0) {
    throw new Error('Invalid explicit poll mapping');
  }
  for (const group of [correspondences.map(c => [c.pastListId, c.factor] as const), mappings.map(c => [c.entityId, c.factor] as const)]) {
    for (const id of new Set(group.map(r => r[0]))) if (!sum(group.filter(r => r[0] === id).map(r => r[1]))) throw new Error('Poll mapping has zero total factor');
  }
  if (!question.support.length || question.support.some(r => !Number.isFinite(r.share) || r.share < 0 || r.share > 1) ||
    Math.abs(sum(question.support.map(r => r.share)) - 1) > 1e-9) throw new Error('Poll support must sum to one');
  const result: PollReconciliation = { questionId: question.id, policyVersion: 'poll-reconciliation-v1',
    shares: lists.map(l => ({ listId: l.id, share: 0 })), assignments: [], estimates: [], territorialPriors: [] };
  const weights = (rows: { listId: string; weight: number }[]) => {
    const grouped = new Map<string, number>();
    for (const r of rows) grouped.set(r.listId, (grouped.get(r.listId) ?? 0) + r.weight);
    const total = sum(grouped.values());
    return total ? [...grouped].filter(([, n]) => n > 0).map(([listId, n]) => ({ listId, fraction: n / total })) : [];
  };
  const resolve = (id: string): { method: 'explicit' | 'alias' | 'historical-correspondence'; rows: { listId: string; fraction: number }[] } | null => {
    const explicit = mappings.filter(m => m.entityId === id);
    if (explicit.length) return { method: 'explicit', rows: weights(explicit.map(m => ({ listId: m.listId, weight: m.factor }))) };
    const direct = lists.filter(l => entityIds(l.name).includes(id));
    if (direct.length) {
      const baseline = weights(direct.map(l => ({ listId: l.id, weight: l.baselineShare ?? 0 })));
      return { method: 'alias', rows: baseline.length ? baseline : weights(direct.map(l => ({ listId: l.id, weight: 1 }))) };
    }
    const ids = new Set(historicalLists.filter(l => entityIds(l.name).includes(id)).map(l => l.id));
    const links = correspondences.filter(c => ids.has(c.pastListId));
    // Historical factors are relative within each past list, just as in the
    // existing parameter preparer. Their arbitrary scale cannot change support.
    return links.length ? { method: 'historical-correspondence', rows: weights(links.map(c => ({ listId: c.futureListId,
      weight: c.factor / sum(links.filter(p => p.pastListId === c.pastListId).map(p => p.factor)) }))) } : null;
  };
  // Resolve observed parties before residuals, so "Altri" preferentially fills
  // unobserved eligible lists. Its mass never becomes turnout/abstention.
  const residuals: typeof question.support = [];
  for (const row of question.support) {
    if (!row.entityIds.length) { residuals.push(row); continue; }
    const resolved = row.entityIds.map(id => {
      const direct = resolve(id);
      if (direct) return direct;
      const proxy = entities.get(id)?.proxyEntityIds.map(p => resolve(p)).find(p => p !== null);
      return proxy ? { ...proxy, method: 'proxy' as const } : null;
    });
    // An unresolved member retains its fraction as residual; known members are
    // not allowed to absorb all its support silently.
    const available = resolved.filter(r => r !== null);
    const groupSplit = row.entityIds.length > 1;
    for (let i = 0; i < resolved.length; i++) {
      const match = resolved[i], share = row.share / row.entityIds.length;
      if (!match) { residuals.push({ ...row, label: `${row.label} [${row.entityIds[i]}]`, share, entityIds: [] }); continue; }
      const estimated = groupSplit || match.method === 'proxy' || match.rows.length > 1;
      result.assignments.push({ label: groupSplit ? `${row.label} [${row.entityIds[i]}]` : row.label, share,
        method: match.method, estimated, targets: match.rows });
      for (const t of match.rows) result.shares.find(r => r.listId === t.listId)!.share += share * t.fraction;
      if (estimated) result.estimates.push({ code: match.method === 'proxy' ? 'POLL_PARTY_PROXY' : 'POLL_SPLIT_ESTIMATE', share,
        listIds: match.rows.map(r => r.listId), message: match.method === 'proxy'
          ? `${row.entityIds[i]}: ${entities.get(row.entityIds[i])?.proxyNote ?? 'Reviewed political proxy used for an absent scenario option.'}`
          : `${row.label}: group/split allocation uses equal entity weights, reviewed correspondence factors or target baseline weights; it is not an observed voter flow.` });
    }
    if (available.length < resolved.length) result.estimates.push({ code: 'POLL_UNMAPPED_SUPPORT', message: `${row.label}: unresolved support retained for estimated allocation.`,
      share: row.share * (resolved.length - available.length) / resolved.length, listIds: [] });
  }
  const observed = new Set(result.assignments.flatMap(a => a.targets.map(t => t.listId)));
  const unobserved = lists.filter(l => !observed.has(l.id));
  const remainderTargets = unobserved.length ? unobserved : lists;
  const baseline = weights(remainderTargets.map(l => ({ listId: l.id, weight: l.baselineShare ?? 0 })));
  const fallback = baseline.length ? baseline : weights(remainderTargets.map(l => ({ listId: l.id, weight: 1 })));
  for (const row of residuals) {
    result.assignments.push({ label: row.label, share: row.share, method: baseline.length ? 'residual-baseline' : 'residual-equal', estimated: true, targets: fallback });
    for (const t of fallback) result.shares.find(r => r.listId === t.listId)!.share += row.share * t.fraction;
    result.estimates.push({ code: 'POLL_RESIDUAL_ESTIMATE', message: `${row.label}: allocated to ${unobserved.length ? 'unobserved' : 'all'} eligible lists using ${baseline.length ? 'historical baseline' : 'equal'} weights.`,
      share: row.share, listIds: fallback.map(t => t.listId) });
  }
  for (const list of lists) {
    const links = correspondences.filter(c => c.futureListId === list.id && c.factor > 0).map(c => c.pastListId);
    const same = historicalLists.filter(p => entityIds(list.name).some(id => entityIds(p.name).includes(id))).map(p => p.id);
    const historical = [...new Set(links.length ? links : same)];
    const proxyIds = entityIds(list.name).flatMap(id => entities.get(id)?.proxyEntityIds ?? []);
    const proxy = historicalLists.filter(p => entityIds(p.name).some(id => proxyIds.includes(id))).map(p => p.id);
    const method = historical.length ? 'historical-correspondence' : proxy.length ? 'entity-proxy' : 'uniform-filed-support';
    result.territorialPriors.push({ listId: list.id, method, historicalListIds: [...new Set(historical.length ? historical : proxy)] });
    if (!historical.length) result.estimates.push({ code: 'POLL_TERRITORIAL_PRIOR_ESTIMATE', share: null, listIds: [list.id],
      message: `${list.name}: substep 2 must use ${method === 'entity-proxy' ? 'the declared historical donor geography' : 'uniform support over filed eligible units'} where territorial evidence is missing.` });
  }
  const total = sum(result.shares.map(r => r.share));
  if (!Number.isFinite(total) || Math.abs(total - 1) > 1e-9) throw new Error('Poll reconciliation lost support');
  return result;
}
