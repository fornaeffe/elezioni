/** Structural membership-resolution screen. This does not certify a Gaussian
 * posterior, identify municipal trajectories, or replace information diagnostics. */
export function componentResolution(components: string[], information: { id: string; groups: string[][] }[]) {
  if (!components.length || new Set(components).size !== components.length) throw new Error('Invalid component resolution basis');
  const index = new Map(components.map((id, i) => [id, i]));
  const rows = information.flatMap(o => o.groups.map(group => {
    const row = Array(components.length).fill(0);
    if (!group.length || new Set(group).size !== group.length) throw new Error('Invalid distinguishing group');
    for (const id of group) { const k = index.get(id); if (k === undefined) throw new Error('Unknown distinguishing identity'); row[k] = 1; }
    return row;
  }));
  const matrix = rows.map(row => [...row]), pivotColumns: number[] = []; let rank = 0;
  for (let c = 0; c < components.length && rank < matrix.length; c++) {
    let p = rank; for (let r = rank + 1; r < matrix.length; r++) if (Math.abs(matrix[r][c]) > Math.abs(matrix[p][c])) p = r;
    if (Math.abs(matrix[p][c]) < 1e-10) continue;
    [matrix[p], matrix[rank]] = [matrix[rank], matrix[p]];
    const v = matrix[rank][c]; for (let j = 0; j < components.length; j++) matrix[rank][j] /= v;
    for (let r = 0; r < matrix.length; r++) if (r !== rank) {
      const factor = matrix[r][c]; for (let j = 0; j < components.length; j++) matrix[r][j] -= factor * matrix[rank][j];
    }
    pivotColumns.push(c); rank++;
  }
  const free = components.map((_, i) => i).filter(i => !pivotColumns.includes(i));
  const nullDirections = free.map(c => {
    const row = Array(components.length).fill(0); row[c] = 1;
    pivotColumns.forEach((p, r) => row[p] = -matrix[r][c]);
    return components.flatMap((id, i) => Math.abs(row[i]) > 1e-9 ? [{ id, coefficient: row[i] }] : []);
  });
  return { resolved: rank === components.length, rank, dimensions: components.length, nullDirections,
    informationIds: information.map(o => o.id), interpretation: 'Structural component resolution only; dates, strength and latent pooling still require statistical validation' };
}
