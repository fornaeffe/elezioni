/** Simulation error conditional on this fixed election, fit and evaluated units.
 * Aggregate whole joint draws before estimating error; towns are not replicates. */
export class JointTailScores {
  private previous?: string;
  private groups = 0;
  private units = 0;
  private pending: Float64Array[];
  private totals: Float64Array[];
  constructor(readonly draws: number) {
    this.pending = Array.from({ length: 3 }, () => new Float64Array(draws));
    this.totals = this.pending.map((a) => a.slice());
  }
  private flush() {
    if (!this.groups) return;
    for (let k = 0; k < 3; k++)
      for (let d = 0; d < this.draws; d++) {
        this.totals[k][d] += this.pending[k][d] / this.groups;
        this.pending[k][d] = 0;
      }
    this.groups = 0;
    this.units++;
  }
  add(unit: string, values: number[], truth: number) {
    if (values.length !== this.draws)
      throw new Error("Inconsistent predictive draw count");
    if (unit !== this.previous) {
      this.flush();
      this.previous = unit;
    }
    this.groups++;
    for (let d = 0; d < this.draws; d++) {
      const error = Math.abs(values[d] - truth);
      this.pending[0][d] += Number(error > 0.3);
      this.pending[1][d] += Number(error > 0.15);
      this.pending[2][d] += Number(values[d] > 0.8);
    }
  }
  finish() {
    this.flush();
    const entries = ["error30", "error15", "extreme80"].map((key, k) => {
      const values = this.totals[k].map((x) => x / this.units);
      return [key, { ...mcSummary(values), draws: [...values] }] as const;
    });
    return Object.fromEntries(entries);
  }
}
export function mcSummary(values: ArrayLike<number>) {
  const n = values.length,
    mean = Array.from(values).reduce((a, b) => a + b, 0) / n,
    variance =
      Array.from(values).reduce((s, x) => s + (x - mean) ** 2, 0) / (n - 1),
    mcse = Math.sqrt(variance / n);
  return { mean, mcse, normal95: [mean - 1.96 * mcse, mean + 1.96 * mcse] };
}
