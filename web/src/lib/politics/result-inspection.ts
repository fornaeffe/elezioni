/** A diagnostic navigation request, independent of either result view's controls. */
export interface ResultInspection {
  simulation: number;
  chamber?: string;
  revision: number;
}
