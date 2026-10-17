import type { Chamber, ReformScrutinyInput } from './input';
import { inputShape } from './shape';
import type { ValidationIssue } from './shape';
import { ChamberContext, key, validateEndowments, validatePoliticalLinks, ValidationContext } from './validation-context';
import { validateCandidatures, type CandidateUse } from './candidature-validation';
import { validateVotes } from './vote-validation';

declare const validated: unique symbol;
export type ValidatedReformScrutinyInput = ReformScrutinyInput & { readonly [validated]: true };
export type ReformInputValidation =
  | { readonly valid: true; readonly input: ValidatedReformScrutinyInput; readonly issues: readonly [] }
  | { readonly valid: false; readonly issues: readonly ValidationIssue[] };

/** Strict, non-coercing boundary. Never migrates legacy snapshots or attributes votes. */
export function validateReformScrutinyInput(value: unknown): ReformInputValidation {
  const issues: ValidationIssue[] = [];
  if (!inputShape.check(value, '$', issues)) return { valid: false, issues };
  const input = value as ReformScrutinyInput;
  const v = new ValidationContext(input);
  const renewal = input.renewal;
  if (renewal.kind === 'joint' && (renewal.camera.chamber !== 'camera' || renewal.senato.chamber !== 'senato')) {
    v.add('INCOMPATIBLE_RENEWAL', '$.renewal', 'Joint renewal requires the correctly identified Camera and Senato inputs.');
  }
  const chambers = (renewal.kind === 'joint' ? [renewal.camera, renewal.senato] : [renewal.chamber]).map(d => new ChamberContext(v, d));
  const uses = new Map<string, CandidateUse[]>();
  for (const chamber of chambers) {
    validatePoliticalLinks(chamber);
    validateEndowments(chamber);
    for (const [id, placements] of validateCandidatures(chamber)) uses.set(id, [...(uses.get(id) ?? []), ...placements]);
    validateVotes(chamber);
  }
  for (const person of input.candidates) {
    const placements = uses.get(person.id) ?? [];
    const path = `$.candidates.${person.id}`;
    const activeChambers = new Set(placements.map(x => x.chamber));
    // C19(5)-(6): «nella circoscrizione Estero» / «contemporaneamente».
    if (activeChambers.size > 1) v.add('INVALID_CANDIDATURE', path, 'The same person cannot contest both chambers.', 'C19(6)');
    v.index(person.externalCandidacies, x => key(x.chamber, x.kind), `${path}.externalCandidacies`);
    for (const declaration of person.externalCandidacies) {
      if (placements.some(x => x.chamber !== declaration.chamber)) v.add('INVALID_CANDIDATURE', path, 'External declaration conflicts with a candidature in the other chamber.', 'C19(6)');
      if (declaration.kind === 'domestic' && activeChambers.has(declaration.chamber)) v.add('INVALID_CANDIDATURE', path, 'All domestic candidatures in a renewed chamber must be included in its slates.');
      if (declaration.kind === 'foreign' && placements.some(x => x.chamber === declaration.chamber && x.role !== 'special-uninominal')) v.add('INVALID_CANDIDATURE', path, 'Foreign and ordinary/bonus candidatures are incompatible.', 'C19(5); S9');
    }
  }
  validateBonusLinks(v, chambers);
  validateModelMetadata(v, chambers);
  if (v.issues.length) return { valid: false, issues: v.issues };
  // Ownership of a validated payload is separate from the caller's mutable data.
  return { valid: true, input: structuredClone(input) as ValidatedReformScrutinyInput, issues: [] };
}

function validateBonusLinks(v: ValidationContext, chambers: readonly ChamberContext[]): void {
  const links = v.input.bonusLinks;
  if (v.input.renewal.kind === 'single' && links.length) v.add('INCOMPATIBLE_RENEWAL', '$.bonusLinks', 'Single-chamber renewal cannot declare joint bonus links.', 'C18-bis(2-ter)/S9(2-bis)');
  for (const side of ['camera', 'senato'] as const) {
    v.index(links, x => key(x[side].kind, x[side].id), `$.bonusLinks.${side}`);
    for (const link of links) {
      chambers.find(x => x.data.chamber === side)?.subject(link[side], `$.bonusLinks.${side}`);
      if (!link.cameraDeclares || !link.senatoDeclares) v.add('INVALID_LINK', '$.bonusLinks', 'Cross-chamber bonus links must be explicitly reciprocal.', 'C14-bis(3-bis)/S8(1-bis)');
    }
  }
}

function validateModelMetadata(v: ValidationContext, chambers: readonly ChamberContext[]): void {
  const model = v.input.chamberModel;
  if (model.kind === 'supplied') return;
  const p = '$.chamberModel.references';
  const seen = new Set<string>();
  model.references.forEach((row, index) => {
    const path = `${p}[${index}]`;
    if (row.listIds.camera === null && row.listIds.senato === null) v.add('INVALID_METADATA', path, 'A resolved reference must identify at least one chamber list.');
    for (const side of ['camera', 'senato'] as const satisfies readonly Chamber[]) {
      const id = row.listIds[side];
      const resolved = row[side];
      if ((id === null) !== (resolved === null)) v.add('INVALID_METADATA', path, 'List identity and resolved chamber reference must be present together.');
      if (id !== null) {
        const context = chambers.find(c => c.data.chamber === side);
        if (!context) v.add('INVALID_METADATA', path, 'References must concern a renewed chamber.');
        else v.reference(context.lists, id, path);
        const identity = key(side, id, row.scope.kind, row.scope.kind === 'national' ? '' : row.scope.code);
        if (seen.has(identity)) v.add('INVALID_METADATA', path, 'Duplicate resolved reference for a chamber list and scope.');
        seen.add(identity);
      }
    }
  });
}
