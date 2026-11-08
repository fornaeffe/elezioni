import { REFORM, type PoliticsScenario } from '../scenario/schema';
import { effectivePlacements, materializeBonusLinks, renewed } from '../scenario/effective';
import type { DomainContext } from '../scenario/validation';
import type { ReformAssemblyContext, ReformLegalSettings } from '../reform-2026/generation-types';
import type { M01SourceSettings } from '../generation/m01-compiler';

export function reformLegalSettings(s: PoliticsScenario, domain: DomainContext,
  inputs: M01SourceSettings): ReformLegalSettings {
  const block = s.lawSettings[REFORM];
  return { kind: 'politics-reform-scenario', algorithmId: REFORM, seed: s.execution.simulationSeed,
    electionDate: s.electionDate, renewal: s.renewal, coverage: inputs.coverage,
    lists: { camera: inputs.lists.camera.map(({ componentIds, nationalUncertaintyMultiplier, localUncertaintyMultiplier, ...l }) => l),
      senato: inputs.lists.senato.map(({ componentIds, nationalUncertaintyMultiplier, localUncertaintyMultiplier, ...l }) => l) },
    candidateGeneration: { ...block.candidateGeneration },
    candidateTemplates: effectivePlacements(s, REFORM, domain.geographyRelations).map(p => {
      if (p.role !== 'ordinary' && p.role !== 'bonus' && p.role !== 'special') throw new Error('REFORM_PIN_ROLE');
      const c = s.candidates.find(c => c.id === p.candidateId)!;
      return { chamber: p.location.chamber, role: p.role, locationId: p.location.code, ownerId: p.owner.id,
        position: p.position, affiliationListId: p.affiliationListId,
        candidate: { id: c.id, name: c.name, gender: c.gender, birthDate: c.birthDate, availability: c.availability,
          externalCandidacies: block.externalCandidacies.filter(e => e.candidateId === c.id).map(({ chamber, kind }) => ({ chamber, kind })) } };
    }),
    suppliedVotes: block.suppliedVotes.filter(r => renewed(s, r.location.chamber)).map(r => ({ chamber: r.location.chamber, collegeId: r.location.code, votes: r.votes })),
    suppliedPreferences: block.suppliedPreferences.filter(r => renewed(s, r.location.chamber)).map(r => ({ chamber: r.location.chamber, collegeId: r.location.code, listId: r.listId, preferences: r.preferences })),
    bonusLinks: materializeBonusLinks(s).map(l => ({ ...l, cameraDeclares: true, senatoDeclares: true })),
    preferenceProfileId: block.preferenceProfileId, chamberProfileId: block.chamberProfileId };
}
export function reformCoverage(s: PoliticsScenario, context: ReformAssemblyContext) {
  // Include the derived TAA proportional view only here; source geography remains physical.
  return context.seats.geography.colleges.filter(c => renewed(s, c.chamber)).map(c => ({ chamber: c.chamber,
    collegeId: c.id, listIds: s.subjects.lists.filter(l => s.lawSettings[REFORM].filings.find(f => f.listId === l.id)!.locations.some(f =>
      f.chamber === c.chamber && (f.code === c.id || c.kind === 'taa-proportional' && context.seats.geography.colleges.some(p =>
        p.id === f.code && p.chamber === c.chamber && p.territoryId === c.territoryId && p.kind === 'special-uninominal'))) &&
      (l.coalitionId === null || s.subjects.coalitions.find(co => co.id === l.coalitionId)!.chambers.includes(c.chamber))).map(l => l.id) }));
}
