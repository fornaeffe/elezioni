import type { GeneratorId, ReferenceScope } from '../scenario/schema';

export function referenceQuotaHelp(
  model: GeneratorId, target: 'list' | 'component' | 'non-valid', scope: ReferenceScope['kind'] = 'national'
): string {
  const geography = scope === 'national' ? 'a livello nazionale' : 'nel territorio';
  const subject = target === 'list' ? 'della lista' : 'del componente';
  const denominator = target === 'non-valid'
    ? `Quota degli elettori che si astengono o non esprimono un voto valido ${geography}.`
    : `Quota ${subject} sul totale dei voti validi ${geography}.`;
  const interpretation = model === 'M01'
    ? 'M01 usa il valore come punto di partenza alla data di riferimento e simula le variazioni fino all’elezione.'
    : 'M02 usa il valore come media alla data di riferimento e tiene conto della sua incertezza prima di proiettare il voto.';
  const estimate = target === 'non-valid'
    ? 'sulla base della partecipazione storica'
    : 'dai dati storici e dai sondaggi disponibili';

  const newComponent = model === 'M02' && target !== 'non-valid'
    ? ' Un componente senza storico richiede comunque un riferimento esplicito.'
    : '';

  return `${denominator} ${interpretation} Se lasci il campo vuoto (% auto), valgono gli altri riferimenti applicabili. In loro assenza, il modello stima la quota ${estimate}. Lo zero è un valore esplicito.${newComponent}`;
}

export function commonReferenceHelp(national: boolean): string {
  const quotaLocation = national ? ' La quota comune si modifica nel campo nazionale di base.' : '';
  return `Il riferimento comune vale per Camera e Senato. Usa il campo Camera o Senato per dichiarare un riferimento specifico per quel ramo.${quotaLocation}`;
}

export const referenceSdHelp = 'Deviazione standard della quota in punti percentuali, usata da M02. Puoi usare il valore predefinito dello scenario o indicarne uno. Con una quota di 0% o 100%, la SD deve essere zero.';
