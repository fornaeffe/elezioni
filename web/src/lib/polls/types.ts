/** Preparation contracts. Polls are observations, never historical election rows
 * or instructions to change a saved scenario. Shares are valid-vote fractions. */
export interface PollEntity {
  id: string;
  name: string;
  aliases: string[];
  /** Joint electoral lists, e.g. Azione / Italia Viva. Empty for a single entity. */
  components: string[];
  /** Reviewed modelling proxies, not measured voter transfers. */
  proxyEntityIds: string[];
  proxyNote: string | null;
}
export interface PollEntityCatalog {
  schemaVersion: 1;
  version: string;
  entities: PollEntity[];
}
export interface PollSource {
  id: string;
  /** Government survey reference for a manual audit, not a local file path. */
  reference: string;
  url: string;
  retrieval: string;
  capturedOn: string;
  title: string;
  pollster: string;
  commissioner: string;
  purchaser: string;
  fieldworkStart: string;
  fieldworkEnd: string;
  publicationDate: string;
  population: string;
  territory: string;
  sampleSize: number;
  sampleDescription: string;
  samplingDescription: string;
  collectionModes: string[];
  marginOfError: number | null;
  confidenceLevel: number | null;
}
export interface PollAnswer {
  label: string;
  percentage: number;
  kind: 'list' | 'residual' | 'coalition-total' | 'nonresponse';
  entityIds: string[];
}
export interface ReviewedPollQuestion {
  id: string;
  sourceId: string;
  questionNumber: number;
  pages: number[];
  /** All alternatives from the same survey share this ID and its sample. */
  independenceGroup: string;
  text: string;
  ballot: { kind: 'generic' | 'conditional'; label: string | null; assumptions: string[] };
  denominator: { kind: 'declared-vote-intentions'; basis: 'reported' | 'estimated'; note: string };
  questionSampleSize: number | null;
  answers: PollAnswer[];
  notes: string[];
}
export interface ReviewedPollArchive {
  schemaVersion: 1;
  version: string;
  reviewedOn: string;
  questions: ReviewedPollQuestion[];
}
export interface NormalizedPollQuestion extends ReviewedPollQuestion {
  source: PollSource;
  reportedTotal: number;
  support: { label: string; entityIds: string[]; share: number; kind: 'list' | 'residual' }[];
  normalization: { method: 'reported-total' | 'missing-support-residual'; factor: number };
}
export interface PollReconciliationContext {
  /** Only lists eligible in the intended scope. Omit abstention. */
  lists: { id: string; name: string; baselineShare?: number | null }[];
  historicalLists: { id: string; name: string }[];
  correspondences: { pastListId: string; futureListId: string; factor: number }[];
  /** Explicit poll mappings take precedence over aliases and historical links. */
  mappings?: { entityId: string; listId: string; factor: number }[];
}
export interface PollReconciliation {
  questionId: string;
  policyVersion: 'poll-reconciliation-v1';
  shares: { listId: string; share: number }[];
  assignments: {
    label: string;
    share: number;
    method: 'explicit' | 'alias' | 'historical-correspondence' | 'proxy' | 'residual-baseline' | 'residual-equal';
    estimated: boolean;
    targets: { listId: string; fraction: number }[];
  }[];
  estimates: { code: string; message: string; share: number | null; listIds: string[] }[];
  /** A reproducible prior recipe for substep 2, not invented municipal data. */
  territorialPriors: {
    listId: string;
    method: 'historical-correspondence' | 'entity-proxy' | 'uniform-filed-support';
    historicalListIds: string[];
  }[];
}
