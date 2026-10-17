import type { Chamber, Subject } from './input';
import { ChamberContext, key } from './validation-context';

export interface CandidateUse {
  readonly chamber: Chamber;
  readonly role: 'ordinary' | 'taa-proportional' | 'bonus' | 'special-uninominal';
  readonly territoryId: string;
  readonly collegeId: string | null;
  readonly listId: string | null;
  readonly linkedListIds: readonly string[];
}

export function validateCandidatures(c: ChamberContext): Map<string, CandidateUse[]> {
  const { data: d, validation: v, path: p } = c;
  const uses = new Map<string, CandidateUse[]>();
  function use(id: string, value: CandidateUse, path: string) {
    const person = v.reference(v.people, id, path);
    if (!person) return;
    const election = v.input.electionDate;
    const age = Number(election.slice(0, 4)) - Number(person.birthDate.slice(0, 4)) -
      Number(election.slice(5) < person.birthDate.slice(5));
    // K56/K58: «venticinque anni» / «quarantesimo anno» alla data delle elezioni.
    if (age < (d.chamber === 'camera' ? 25 : 40)) v.add('INVALID_CANDIDATURE', path, 'Candidate is below the constitutional age at the election date.', 'K56/K58');
    uses.set(id, [...(uses.get(id) ?? []), value]);
  }
  v.index(d.ordinarySlates, s => key(s.collegeId, s.listId), `${p}.ordinarySlates`);
  for (const slate of d.ordinarySlates) {
    const path = `${p}.ordinarySlates.${slate.collegeId}.${slate.listId}`;
    const college = v.reference(c.colleges, slate.collegeId, path);
    v.reference(c.lists, slate.listId, path);
    if (college?.kind === 'special-uninominal') v.add('INVALID_CANDIDATURE', path, 'Ordinary slates cannot be filed in uninominal colleges.');
    v.index(slate.candidateIds, x => x, `${path}.candidateIds`);
    // C18-bis(3), S9(4): «pari a sette, compreso il capolista»;
    // «collocati secondo un ordine alternato di genere».
    if (slate.candidateIds.length !== 7) v.add('INVALID_CANDIDATURE', path, 'A filed ordinary slate must have seven ordered candidates.', 'C18-bis(3)/S9(4)');
    slate.candidateIds.forEach((id, index) => {
      const person = v.people.get(id);
      const previous = v.people.get(slate.candidateIds[index - 1]);
      if (person && previous?.gender === person.gender) v.add('INVALID_CANDIDATURE', path, 'Filed ordinary genders must alternate.', 'C18-bis(3)/S9(4)');
      use(id, { chamber: d.chamber, role: college?.kind === 'taa-proportional' ? 'taa-proportional' : 'ordinary',
        territoryId: college?.territoryId ?? '', collegeId: slate.collegeId, listId: slate.listId, linkedListIds: [slate.listId] }, path);
    });
  }

  v.index(d.bonusSlates, s => key(s.territoryId, s.subject.kind, s.subject.id), `${p}.bonusSlates`);
  const bonusPeople = new Set(d.bonusSlates.flatMap(s => s.candidates.map(x => x.candidateId)));
  const bonusGenders = new Map<string, string[]>();
  for (const slate of d.bonusSlates) {
    const path = `${p}.bonusSlates.${slate.territoryId}.${slate.subject.id}`;
    const territory = v.reference(c.territories, slate.territoryId, path);
    const members = c.subject(slate.subject, path);
    v.index(slate.candidates, x => x.candidateId, `${path}.candidates`);
    // C18-bis(2-bis), imported by S9: «un numero di candidati circoscrizionali
    // pari al numero dei seggi da attribuire nella circoscrizione, quale premio».
    if (!territory || territory.kind !== 'ordinary' || territory.bonusSeats === 0 ||
        slate.candidates.length !== territory.bonusSeats) v.add('INVALID_CANDIDATURE', path, 'Filed bonus slate size must equal an eligible territorial bonus endowment.', 'C18-bis(2-bis)/S9');
    if (!d.ordinarySlates.some(s => members.includes(s.listId) && c.colleges.get(s.collegeId)?.territoryId === slate.territoryId)) v.add('INVALID_COVERAGE', path, 'A bonus slate requires linked ordinary coverage in its territory.', 'C18-bis(2-bis)/S11(3-bis)');
    for (const slot of slate.candidates) {
      v.reference(c.lists, slot.affiliationListId, path);
      if (!members.includes(slot.affiliationListId)) v.add('INVALID_LINK', path, 'Bonus affiliation must belong to the bonus-winning subject.');
      // C19(4): «almeno un collegio ... della circoscrizione»; preceding candidates
      // «siano candidati nelle liste circoscrizionali ... del premio».
      const requiredPlacement = d.ordinarySlates.some(s => s.listId === slot.affiliationListId &&
        c.colleges.get(s.collegeId)?.territoryId === slate.territoryId &&
        s.candidateIds.includes(slot.candidateId) &&
        s.candidateIds.slice(0, s.candidateIds.indexOf(slot.candidateId)).every(id => bonusPeople.has(id)));
      if (!requiredPlacement) v.add('INVALID_CANDIDATURE', path, 'Bonus candidate lacks the required local ordinary placement/prefix.', 'C19(4)/S9(5)');
      use(slot.candidateId, { chamber: d.chamber, role: 'bonus', territoryId: slate.territoryId,
        collegeId: null, listId: slot.affiliationListId, linkedListIds: members }, path);
      const group = key(slate.subject.kind, slate.subject.id);
      const gender = v.people.get(slot.candidateId)?.gender;
      if (gender) bonusGenders.set(group, [...(bonusGenders.get(group) ?? []), gender]);
    }
  }

  v.index(d.specialCandidatures, x => key(x.collegeId, x.candidateId), `${p}.specialCandidatures`);
  const specialLinks = new Set<string>();
  const specialGenders = new Map<string, string[]>();
  for (const slot of d.specialCandidatures) {
    const path = `${p}.specialCandidatures.${slot.collegeId}.${slot.candidateId}`;
    const college = v.reference(c.colleges, slot.collegeId, path);
    if (college && college.kind !== 'special-uninominal') v.add('INVALID_CANDIDATURE', path, 'Special candidature requires a uninominal college.');
    v.index(slot.linkedListIds, x => x, `${path}.linkedListIds`);
    if (!slot.linkedListIds.length) v.add('INVALID_LINK', path, 'Special candidates require declared list links.', 'C92/C93-quinquies; S20');
    const subjects: Subject[] = [];
    for (const listId of slot.linkedListIds) {
      if (v.reference(c.lists, listId, path)) subjects.push(c.subjectForList(listId));
      const k = key(slot.collegeId, listId);
      if (specialLinks.has(k)) v.add('INVALID_LINK', path, 'A list cannot support multiple candidates in one special contest.');
      specialLinks.add(k);
    }
    if (new Set(subjects.map(s => key(s.kind, s.id))).size > 1) v.add('INVALID_LINK', path, 'Special links conflict with coalition declarations.', 'C93-quinquies/S20');
    if (slot.affiliationListId !== null && !slot.linkedListIds.includes(slot.affiliationListId)) v.add('INVALID_LINK', path, 'Candidate affiliation is outside the declared linked lists.');
    use(slot.candidateId, { chamber: d.chamber, role: 'special-uninominal', territoryId: college?.territoryId ?? '',
      collegeId: slot.collegeId, listId: slot.affiliationListId, linkedListIds: slot.linkedListIds }, path);
    // C93-quinquies(4) concerns Camera TAA; S20(a-ter) covers Senate special contests.
    if (d.chamber === 'senato' || c.territoryForCollege(slot.collegeId)?.kind === 'taa') {
      const gender = v.people.get(slot.candidateId)?.gender;
      const subject = subjects[0];
      if (gender && subject) {
        const group = key(subject.kind, subject.id);
        specialGenders.set(group, [...(specialGenders.get(group) ?? []), gender]);
      }
    }
  }
  // «nessuno dei due generi ... superiore al 60 per cento ... all'unità più prossima».
  for (const groups of [bonusGenders, specialGenders]) for (const genders of groups.values()) {
    const maximum = Math.floor((genders.length * 6 + 5) / 10);
    if (['female', 'male'].some(g => genders.filter(x => x === g).length > maximum)) v.add('INVALID_CANDIDATURE', p, 'National subject candidatures exceed the rounded 60% gender limit.', 'C18-bis(3.1)/C93-quinquies(4); S9(4-bis)/S20(a-ter)');
  }

  for (const [id, positions] of uses) {
    const ordinary = positions.filter(x => x.role === 'ordinary' || x.role === 'taa-proportional');
    const badges = new Set(positions.map(x => x.listId).filter(x => x !== null));
    // C19(1)-(3): «Nessun candidato può presentarsi con diversi contrassegni»;
    // «in più di cinque collegi plurinominali»; «in più di una lista circoscrizionale».
    if (badges.size > 1 || ordinary.length > 5 || positions.filter(x => x.role === 'bonus').length > 1) v.add('INVALID_CANDIDATURE', `${p}.candidates.${id}`, 'Incompatible badges or excessive multiple candidacies.', 'C19(1)-(4)/S9(5)');
    // C93-octies/S20-bis: «in più di un collegio uninominale»; C93-octies(2):
    // «non può essere candidato ... ai fini dell'attribuzione del premio».
    if (positions.filter(x => x.role === 'special-uninominal').length > 1 ||
        (positions.some(x => x.role === 'special-uninominal' || x.role === 'taa-proportional') && positions.some(x => x.role === 'bonus'))) v.add('INVALID_CANDIDATURE', `${p}.candidates.${id}`, 'Incompatible special/bonus candidatures.', 'C93-octies/S20-bis');
    for (const special of positions.filter(x => x.role === 'special-uninominal')) {
      if (ordinary.some(x => x.listId !== null && !special.linkedListIds.includes(x.listId))) v.add('INVALID_CANDIDATURE', `${p}.candidates.${id}`, 'Ordinary and special candidatures have incompatible list links.', 'C93-octies(3)');
    }
  }

  validateCoverage(c);
  return uses;
}

function validateCoverage(c: ChamberContext): void {
  const { validation: v, data: d, path: p } = c;
  for (const list of d.lists) {
    const slates = d.ordinarySlates.filter(s => s.listId === list.id);
    const territories = new Set(slates.map(s => c.colleges.get(s.collegeId)?.territoryId).filter((x): x is string => !!x));
    // C18-bis says «presentare candidature»: retain declared special-contest
    // coverage too, without requiring ordinary slates in those uni-only colleges.
    for (const special of d.specialCandidatures.filter(x => x.linkedListIds.includes(list.id))) {
      const territory = c.territoryForCollege(special.collegeId);
      if (territory) territories.add(territory.id);
    }
    const regions = new Set([...territories].map(id => c.territories.get(id)?.regionId));
    if (list.protectedMinority && [...regions].some(r => r !== list.protectedMinority?.regionId)) v.add('INVALID_COVERAGE', p, 'Minority exception requires exclusive presentation in its declared autonomous region.', 'C18-bis(1)');
    const hasOrdinary = slates.some(s => c.colleges.get(s.collegeId)?.kind === 'ordinary');
    // C18-bis(1): «almeno la metà delle circoscrizioni»; S9(2): «anche in una sola regione».
    // Do not extend nationwide filing coverage to special-only candidatures (I09).
    if (d.chamber === 'camera' && hasOrdinary && !list.protectedMinority && territories.size < Math.ceil(d.territories.length / 2)) v.add('INVALID_COVERAGE', p, 'Camera list lacks half-constituency coverage.', 'C18-bis(1)');
    for (const territoryId of territories) {
      if (c.territories.get(territoryId)?.kind !== 'ordinary') continue;
      const total = d.colleges.filter(x => x.territoryId === territoryId && x.kind === 'ordinary').length;
      const filed = slates.filter(s => c.colleges.get(s.collegeId)?.territoryId === territoryId).length;
      // C18-bis(1), imported by S9: «almeno due terzi dei collegi ... della circoscrizione».
      if (filed * 3 < total * 2) v.add('INVALID_COVERAGE', p, 'List lacks two-thirds college coverage in a filed ordinary territory.', 'C18-bis(1)/S9(2)');
    }
  }
  const joint = v.input.renewal.kind === 'joint';
  // C18-bis(2-ter)/S9(2-bis): «non presentano liste di candidati circoscrizionali»
  // in the renewal of only one chamber.
  if (!joint && d.bonusSlates.length) v.add('INCOMPATIBLE_RENEWAL', `${p}.bonusSlates`, 'Single-chamber renewals have no bonus slates.', 'C18-bis(2-ter)/S9(2-bis)');
  if (joint) {
    const required = new Set<string>();
    for (const slate of d.ordinarySlates) {
      const t = c.territoryForCollege(slate.collegeId);
      if (!t || t.bonusSeats === 0) continue;
      const subject = c.subjectForList(slate.listId);
      const k = key(t.id, subject.kind, subject.id);
      if (required.has(k)) continue;
      required.add(k);
      if (!d.bonusSlates.some(b => key(b.territoryId, b.subject.kind, b.subject.id) === k)) v.add('INVALID_CANDIDATURE', `${p}.bonusSlates`, 'Missing required territorial bonus slate.', 'C18-bis(2-bis)/S9');
    }
  }
}
