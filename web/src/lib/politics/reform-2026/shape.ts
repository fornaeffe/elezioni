import type { ReformScrutinyInput } from './input';
import { REFORM_ALGORITHM_ID, REFORM_INPUT_VERSION } from './input';

import { scalar, text, boolean, count, positive, share, date, literal, nullable, optional, array, object, union, type Schema } from '../../core/json-shape';
export { isCalendarDate, type Schema, type ValidationIssue } from '../../core/json-shape';

const chamber = literal('camera', 'senato');
const subject = object({ kind: literal('list', 'coalition'), id: text });
const provenance = union(
  object({ kind: literal('supplied'), reference: text }),
  object({ kind: literal('generated'), modelId: text, parameterVersion: text, profileId: text,
    sourceVersions: array(text), seed: text, assumptions: array(text) })
);
const listVote = object({ listId: text, votes: count });
const candidateVote = object({ candidateId: text, votes: count });
const electorate = object({ minimumAge: literal(18), electors: nullable(count) });
const preferences = union(
  object({ kind: literal('missing'), reason: text }),
  object({ kind: literal('counts'), counts: array(object({ candidateId: text, count })), provenance,
    validity: union(
      object({ kind: literal('adjudicated'), reference: text }),
      object({ kind: literal('combinations'), ballots: array(object({ candidateIds: array(text), count })) })
    ) })
);
const ordinaryVotes = union(
  object({ kind: literal('components'), directListVotes: array(listVote),
    bonusOnlyVotes: array(object({ subject, votes: count })), provenance }),
  object({ kind: literal('attributed'), listVotes: array(listVote), provenance })
);
const specialVotes = union(
  object({ kind: literal('components'), directListVotes: array(listVote),
    candidateOnlyVotes: array(candidateVote), candidateTotals: array(candidateVote), provenance }),
  object({ kind: literal('attributed'), listVotes: array(listVote), candidateTotals: array(candidateVote), provenance })
);
const chamberInput = object({
  chamber,
  regions: array(object({ id: text, autonomous: boolean })),
  territories: array(object({ id: text, regionId: text, kind: literal('ordinary', 'vda', 'taa'), bonusSeats: count })),
  colleges: array(object({ id: text, territoryId: text, kind: literal('ordinary', 'taa-proportional', 'special-uninominal'),
    seatsWithoutBonus: count, seatsWithBonus: count })),
  seatTable: union(
    object({ kind: literal('official-decree', 'constructed'), reference: text, version: text, populationNotes: optional(array(text)) }),
    object({ kind: literal('provisional-census'), reference: text, version: text, censusDate: date,
      geographyVersion: text, method: literal('I08-v1'), populationNotes: optional(array(text)) })
  ),
  lists: array(object({ kind: optional(literal('list', 'other')), id: text, name: text, coalitionId: nullable(text),
    protectedMinority: nullable(object({ regionId: text, legalReference: text })) })),
  coalitions: array(object({ id: text, listIds: array(text) })),
  otherListCoverage: optional(array(object({ collegeId: text, listId: text }))),
  ordinarySlates: array(object({ collegeId: text, listId: text, candidateIds: array(text), preferences })),
  bonusSlates: array(object({ territoryId: text, subject,
    candidates: array(object({ candidateId: text, affiliationListId: text })) })),
  specialCandidatures: array(object({ collegeId: text, candidateId: text, linkedListIds: array(text),
    affiliationListId: nullable(text) })),
  ordinaryVotes: array(object({ collegeId: text, electorate, votes: ordinaryVotes })),
  specialVotes: array(object({ collegeId: text, electorate, votes: specialVotes }))
});
const reference = { share, referenceDate: date };
const resolved = object({ ...reference, origin: literal('supplied', 'model-derived') });

/** Type assignment checks that the runtime schema covers the public input shape. */
export const inputShape: Schema<ReformScrutinyInput> = object({
  schemaVersion: literal(REFORM_INPUT_VERSION), algorithmId: literal(REFORM_ALGORITHM_ID),
  simulationId: positive, electionDate: date, legalVersion: text, geographyVersion: text,
  randomness: object({ seed: text, streamVersion: text }),
  candidates: array(object({ id: text, name: text, birthDate: date, gender: literal('female', 'male'),
    availability: union(object({ kind: literal('available') }), object({ kind: literal('unavailable'), reason: text })),
    externalCandidacies: array(object({ chamber, kind: literal('domestic', 'foreign') })) })),
  renewal: union(object({ kind: literal('joint'), camera: chamberInput, senato: chamberInput }),
    object({ kind: literal('single'), chamber: chamberInput })),
  bonusLinks: array(object({ camera: subject, senato: subject, cameraDeclares: boolean, senatoDeclares: boolean })),
  chamberModel: union(object({ kind: literal('supplied'), reference: text }),
    object({ kind: literal('generated'), modelId: literal('paired-chamber-votes-v1', 'fast-share-paired-v1'), parameterVersion: text,
      covarianceProfileVersion: text, sourceVersions: array(text), seed: text, assumptions: array(text), mode: literal('mean'),
      references: array(object({ scope: union(object({ kind: literal('national') }),
        object({ kind: literal('region', 'province', 'municipality'), code: text })),
        listIds: object({ camera: nullable(text), senato: nullable(text) }), common: nullable(object(reference)),
        camera: nullable(resolved), senato: nullable(resolved) })) })
  )
});

// Reuse the same strict JSON grammar for versioned generation envelopes.
export { scalar, text, boolean, count, positive, share, date, literal, nullable,
  optional, array, object, union, chamber, subject, provenance, preferences,
  ordinaryVotes, specialVotes };
