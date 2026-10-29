/** FV-03 portable original observations. This schema does not install a scenario. */
export type SourceContext = 'camera' | 'european' | 'senato';
export interface SourceChoice {
  id: string; eventId: string; label: string; members: string[];
  method: string; citations: string[]; opaque: boolean;
}
export interface SourceUnit {
  id: string; name: string; region: string; province: string | null;
  targetCode: string | null; match: 'same-place' | 'unmatched' | 'unlocated';
}
export interface OriginalRow {
  unit: string; electors: number; voters: number | null;
  choices: { id: string; count: number; direct: number }[];
}
export interface SourceEvent {
  id: string; date: string; availableDate: string; context: SourceContext;
  minimumAge: 18 | 25; role: 'reference' | 'paired-adaptation' | 'age-audit';
  sourceId: string; sourceUrl: string; sourceSha256: string;
  availabilityBasis: string; rows: OriginalRow[];
}
export interface SourcePoll {
  id: string; surveyId: string; context: SourceContext;
  date: string; fieldworkStart: string; fieldworkEnd: string; availableDate: string;
  citation: string; sampleSize: number; questionSampleSize: number | null;
  decidedFraction: number; effectiveSample: number; designEffect: number;
  effectiveSamplePolicy: string; errorGroup: string; roundingWidth: number;
  original: { label: string; percentage: number; kind: string }[];
  normalized: { label: string; members: string[]; share: number; other: boolean }[];
  otherDefinition: 'offered-complement';
  status: 'supported' | 'excluded'; reasons: string[]; method: string;
}
export interface SourceGeography {
  version: string; censusDate: string;
  targets: { code: string; name: string; region: string; province: string; population: number }[];
  /** Physical ballots only. Camera TAA proportional results are derived from these. */
  cells: { chamber: 'camera' | 'senato'; collegeId: string; unitId: string; municipality: string; population: number }[];
  colleges: { id: string; chamber: 'camera' | 'senato'; kind: string; territoryId: string }[];
}
export interface OriginalSource {
  sourceVersions: string[]; methodVersions: string[]; assumptions: string[];
  correspondencePolicy: { id: string; status: string; warnings: string[] };
  citations: { id: string; url: string }[];
  units: SourceUnit[]; choices: SourceChoice[]; events: SourceEvent[];
  polls: SourcePoll[]; geography: SourceGeography;
}
export type ColumnName = 'unit' | 'electors' | 'voters' | 'flags' | 'offsets' | 'choice' | 'count' | 'direct';
export interface NumericColumn { dtype: 'uint32'; offset: number; length: number }
export interface SourceChunk {
  path: string; compressedBytes: number; decodedBytes: number;
  sha256: string; decodedSha256: string; byteOrder: 'little-endian';
  rows: number; values: number; columns: Record<ColumnName, NumericColumn>;
}
export interface CompactSource {
  schemaVersion: 1; kind: 'fast-share-source'; id: string; modelId: string;
  sourceVersions: string[]; methodVersions: string[]; assumptions: string[];
  payload: {
    range: { first: string; latestObservation: string; latestPollPublication: string };
    missing: string; flags: Record<string, number>;
    correspondencePolicy: OriginalSource['correspondencePolicy']; citations: OriginalSource['citations'];
    units: SourceUnit[]; choices: SourceChoice[]; polls: SourcePoll[]; geography: SourceGeography;
    events: (Omit<SourceEvent, 'rows'> & { national: { electors: number; voters: number | null; valid: number; direct: number }; chunk: SourceChunk })[];
  };
}
