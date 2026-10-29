import { requireWorkspace, sum } from './types';
export interface ProjectionRow {
  weight: number;
  scores: ArrayLike<number>;
}
type Edge = { to: number; reverse: number; capacity: number; initial: number };
/** Transportation feasibility plus the exact support of its boundary face.
 * Groups with identical offered sets share a capacity node. Positive-flow edges
 * and residual cycles characterize possible positive mass; no support epsilon. */
function feasibleSupport(
  target: Float64Array,
  rows: number,
  source: (i: number) => ProjectionRow,
  memory?: number
) {
  const j = target.length,
    groups = new Map<string, { ids: number[]; mass: number; node: number }>(),
    rowKeys: string[] = [];
  let total = 0;
  for (let i = 0; i < rows; i++) {
    const r = source(i);
    if (!(r.weight >= 0) || !Number.isFinite(r.weight) || r.scores.length !== j)
      throw new Error('PROJECTION_INVALID_ROW');
    const ids = Array.from({ length: j }, (_, k) => k).filter(
      (k) => target[k] > 0 && Number.isFinite(r.scores[k])
    );
    if (Array.from(r.scores).some((x) => Number.isNaN(x) || x === Infinity))
      throw new Error('PROJECTION_INVALID_SCORE');
    if (r.weight > 0 && !ids.length) throw new Error('PROJECTION_EMPTY_CHOICE_SET');
    const key = ids.join(',');
    rowKeys.push(key);
    total += r.weight;
    const g = groups.get(key) ?? { ids, mass: 0, node: j + groups.size };
    g.mass += r.weight;
    groups.set(key, g);
  }
  if (!(total > 0)) throw new Error('PROJECTION_EMPTY_ELECTORATE');
  const edgeCount = [...groups.values()].reduce((n, g) => n + g.ids.length + 1, j);
  requireWorkspace(edgeCount * 192 + rows * 48 + (j + groups.size) * 128, memory);
  const start = j + groups.size,
    end = start + 1,
    graph: Edge[][] = Array.from({ length: end + 1 }, () => []);
  const add = (a: number, b: number, capacity: number) => {
    const f = { to: b, reverse: graph[b].length, capacity, initial: capacity },
      r = { to: a, reverse: graph[a].length, capacity: 0, initial: 0 };
    graph[a].push(f);
    graph[b].push(r);
    return f;
  };
  for (let k = 0; k < j; k++) add(start, k, target[k]);
  const links: { k: number; group: typeof groups extends Map<string, infer G> ? G : never; edge: Edge }[] =
    [];
  for (const g of groups.values()) {
    for (const k of g.ids) links.push({ k, group: g, edge: add(k, g.node, 2) });
    add(g.node, end, g.mass / total);
  }
  let flow = 0;
  for (;;) {
    const level = new Int32Array(graph.length).fill(-1),
      queue = [start];
    level[start] = 0;
    for (let q = 0; q < queue.length; q++)
      for (const e of graph[queue[q]])
        if (e.capacity > 1e-14 && level[e.to] < 0) {
          level[e.to] = level[queue[q]] + 1;
          queue.push(e.to);
        }
    if (level[end] < 0) break;
    const cursor = new Int32Array(graph.length);
    const push = (node: number, mass: number): number => {
      if (node === end) return mass;
      for (; cursor[node] < graph[node].length; cursor[node]++) {
        const e = graph[node][cursor[node]];
        if (e.capacity <= 1e-14 || level[e.to] !== level[node] + 1) continue;
        const sent = push(e.to, Math.min(mass, e.capacity));
        if (sent > 1e-14) {
          e.capacity -= sent;
          graph[e.to][e.reverse].capacity += sent;
          return sent;
        }
      }
      return 0;
    };
    let sent;
    while ((sent = push(start, 1)) > 1e-14) flow += sent;
  }
  if (flow < 1 - 1e-11)
    throw new Error('REFERENCE_CAPACITY: requested composition exceeds joint physical filing capacity');
  const count = start,
    adjacency: number[][] = Array.from({ length: count }, () => []),
    reverse: number[][] = Array.from({ length: count }, () => []);
  for (let a = 0; a < count; a++)
    for (const e of graph[a])
      if (e.to < count && e.capacity > 1e-14) {
        adjacency[a].push(e.to);
        reverse[e.to].push(a);
      }
  const seen = new Uint8Array(count),
    order: number[] = [];
  for (let i = 0; i < count; i++)
    if (!seen[i]) {
      seen[i] = 1;
      const stack: [number, number][] = [[i, 0]];
      while (stack.length) {
        const top = stack.at(-1)!;
        if (top[1] < adjacency[top[0]].length) {
          const n = adjacency[top[0]][top[1]++];
          if (!seen[n]) {
            seen[n] = 1;
            stack.push([n, 0]);
          }
        } else {
          order.push(top[0]);
          stack.pop();
        }
      }
    }
  const component = new Int32Array(count).fill(-1);
  let label = 0;
  for (const root of order.reverse())
    if (component[root] < 0) {
      const stack = [root];
      component[root] = label;
      while (stack.length)
        for (const next of reverse[stack.pop()!])
          if (component[next] < 0) {
            component[next] = label;
            stack.push(next);
          }
      label++;
    }
  const allowed = new Map<string, Set<number>>([...groups.keys()].map((key) => [key, new Set()]));
  for (const { k, group: g, edge } of links)
    if (edge.initial - edge.capacity > 1e-14 || component[k] === component[g.node])
      allowed.get(g.ids.join(','))!.add(k);
  return { total, allowed, rowKeys };
}
export function projectMargins(
  targetValues: ArrayLike<number>,
  rows: number,
  source: (i: number) => ProjectionRow,
  options: { chunkSize?: number; maximumWorkspaceBytes?: number } = {}
) {
  // The callback is a deterministic score field. Retain one row only, avoiding
  // duplicate RNG/score reconstruction for its weight and probabilities.
  const uncached = source;
  let cachedIndex = -1,
    cachedRow: ProjectionRow;
  source = (i: number) => {
    if (i !== cachedIndex) {
      cachedRow = uncached(i);
      cachedIndex = i;
    }
    return cachedRow;
  };
  const target = Float64Array.from(targetValues),
    j = target.length,
    chunkSize = options.chunkSize ?? 128;
  if (
    !j ||
    !Number.isSafeInteger(rows) ||
    rows < 1 ||
    target.some((x) => !Number.isFinite(x) || x < 0) ||
    Math.abs(sum(target) - 1) > 1e-12 ||
    !Number.isSafeInteger(chunkSize) ||
    chunkSize < 1
  )
    throw new Error('PROJECTION_INVALID_TARGET');
  const support = feasibleSupport(target, rows, source, options.maximumWorkspaceBytes),
    offsets = new Float64Array(j);
  const probabilities = (i: number) => {
    const r = source(i),
      mask = support.allowed.get(support.rowKeys[i])!,
      p = new Float64Array(j);
    let maximum = -Infinity;
    for (let k = 0; k < j; k++) if (mask.has(k)) maximum = Math.max(maximum, r.scores[k] + offsets[k]);
    if (!Number.isFinite(maximum) && r.weight > 0) throw new Error('PROJECTION_EMPTY_CHOICE_SET');
    let mass = 0;
    for (let k = 0; k < j; k++)
      if (mask.has(k)) {
        p[k] = Math.exp(r.scores[k] + offsets[k] - maximum);
        mass += p[k];
      }
    if (mass) for (let k = 0; k < j; k++) p[k] /= mass;
    return p;
  };
  for (let iteration = 0; iteration <= 120; iteration++) {
    const margins = new Float64Array(j);
    for (let start = 0; start < rows; start += chunkSize)
      for (let i = start; i < Math.min(rows, start + chunkSize); i++) {
        const weight = source(i).weight / support.total,
          p = probabilities(i);
        for (let k = 0; k < j; k++) margins[k] += weight * p[k];
      }
    const residual = Math.max(...target.map((x, k) => Math.abs(x - margins[k])));
    if (residual <= 1e-9)
      return { probabilities, offsets, margins, residual, iterations: iteration, totalWeight: support.total };
    if (iteration === 120) break;
    for (let k = 0; k < j; k++)
      if (target[k] > 0) {
        if (!(margins[k] > 0)) throw new Error('PROJECTION_UNAVAILABLE_TARGET');
        offsets[k] += Math.log(target[k] / margins[k]);
      }
  }
  throw new Error('PROJECTION_NOT_CONVERGED: global margins after 120 updates');
}
