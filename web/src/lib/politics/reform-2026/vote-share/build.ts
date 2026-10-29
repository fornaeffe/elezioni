/** FV-07 preview gate. FV-08 owns the switch of installed defaults. */
declare const __FAST_REFORM_CANDIDATE__: boolean;
declare const __FAST_REFORM_IMPLEMENTATIONS__: import('./fit-types').FitImplementations;
export const fastReformCandidate =
  typeof __FAST_REFORM_CANDIDATE__ !== 'undefined' && __FAST_REFORM_CANDIDATE__;
export const fastReformImplementations =
  typeof __FAST_REFORM_IMPLEMENTATIONS__ !== 'undefined'
    ? __FAST_REFORM_IMPLEMENTATIONS__
    : undefined;
