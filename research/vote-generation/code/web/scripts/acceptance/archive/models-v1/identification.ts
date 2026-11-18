/** Observationally identical columns define exact identifiable group totals. */
export function identificationGroups(
  components: string[],
  observations: readonly { groups: string[][] }[],
) {
  const signatures = new Map<string, string[]>();
  for (const id of [...components].sort()) {
    const signature = JSON.stringify(
      observations.map((r) => r.groups.findIndex((g) => g.includes(id))),
    );
    const g = signatures.get(signature) ?? [];
    g.push(id);
    signatures.set(signature, g);
  }
  return [...signatures.values()];
}

export const originalPartitions = (source: {
  elections: { groups: string[][]; strata?: { groups: string[][] }[] }[];
  polls: { groups: string[][] }[];
}) => [...source.elections.flatMap((e) => e.strata ?? [e]), ...source.polls];

/** Null directions of all original conditional group contrasts plus simplex.
 * Keeps overlapping evidence: absence of identical columns does not prove rank. */
export function unidentifiedDirections(
  components: string[],
  observations: readonly { groups: string[][] }[],
) {
  const k = components.length,
    index = new Map(components.map((id, i) => [id, i])),
    rows = [Array(k).fill(1) as number[]];
  for (const r of observations) {
    const offered = r.groups.flat();
    for (const group of r.groups) {
      const row = Array(k).fill(0) as number[];
      offered.forEach(
        (id) => (row[index.get(id)!] = -group.length / offered.length),
      );
      group.forEach((id) => (row[index.get(id)!] += 1));
      rows.push(row);
    }
  }
  const pivots: number[] = [];
  let rank = 0;
  for (let col = 0; col < k; col++) {
    const pivot = rows.findIndex(
      (r, i) => i >= rank && Math.abs(r[col]) > 1e-10,
    );
    if (pivot < 0) continue;
    [rows[rank], rows[pivot]] = [rows[pivot], rows[rank]];
    const value = rows[rank][col];
    rows[rank] = rows[rank].map((v) => v / value);
    rows.forEach((r, i) => {
      if (i !== rank) {
        const scale = r[col];
        for (let j = 0; j < k; j++) r[j] -= scale * rows[rank][j];
      }
    });
    pivots.push(col);
    rank++;
  }
  const basis: number[][] = [];
  for (let col = 0; col < k; col++)
    if (!pivots.includes(col)) {
      const v = Array(k).fill(0) as number[];
      v[col] = 1;
      pivots.forEach((p, i) => (v[p] = -rows[i][col]));
      for (const u of basis) {
        const dot = u.reduce((a, x, j) => a + x * v[j], 0);
        u.forEach((x, j) => (v[j] -= dot * x));
      }
      const norm = Math.hypot(...v);
      basis.push(v.map((x) => x / norm));
    }
  return basis;
}
