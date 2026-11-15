/** Exact implementation pins for the retained numerical transport boundary. */
declare const __FAST_REFORM_IMPLEMENTATIONS__: import('./fit-types').FitImplementations;
export const fastReformImplementations =
  typeof __FAST_REFORM_IMPLEMENTATIONS__ !== 'undefined'
    ? __FAST_REFORM_IMPLEMENTATIONS__
    : undefined;
