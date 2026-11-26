import type { GeneratorId } from '../scenario/schema';

export function nationalReferenceHelp(model: GeneratorId, target: 'list' | 'component' | 'non-valid'): string {
  const subject = target === 'list' ? 'della lista' : 'del componente';
  const denominator = target === 'non-valid'
    ? 'Quota nazionale degli elettori che si astengono o non esprimono un voto valido.'
    : `Quota nazionale ${subject} sul totale dei voti validi.`;
  const interpretation = model === 'M01'
    ? 'M01 usa il valore come punto di partenza alla data di riferimento e simula le variazioni fino all’elezione.'
    : 'M02 usa il valore come media alla data di riferimento e tiene conto della sua incertezza prima di proiettare il voto.';
  const estimate = target === 'non-valid'
    ? 'sulla base della partecipazione storica'
    : 'dai dati storici e dai sondaggi disponibili';

  const newComponent = model === 'M02' && target !== 'non-valid'
    ? ' Un componente senza storico richiede comunque un riferimento esplicito.'
    : '';

  return `${denominator} ${interpretation} Se lasci il campo vuoto, valgono gli altri riferimenti applicabili. In loro assenza, il modello stima la quota ${estimate}.${newComponent}`;
}
