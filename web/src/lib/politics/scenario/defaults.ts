/** The authored input is shared by Node preparation, browser startup and reset. */
import authored from '../../../../../data/politics-default-scenario.json' with { type: 'json' };
import { parsePoliticsScenario } from './validation';

export const authoredPoliticsDefault = parsePoliticsScenario(authored);
export const createDefaultCommonPoliticsScenario = () => structuredClone(authoredPoliticsDefault);
