/** Build identity is separate from probability-law, schema and data versions. */
declare const __COMMON_POLITICS_IMPLEMENTATION__: string;
export const commonPoliticsImplementation = typeof __COMMON_POLITICS_IMPLEMENTATION__ === 'undefined'
  ? undefined : __COMMON_POLITICS_IMPLEMENTATION__;
/** Node implementation keys are namespaced; file envelopes store the digest. */
export function implementationSha256(id: string): string {
  const match = /^(?:implementation-v1-)?([a-f0-9]{64})$/.exec(id);
  if (!match) throw new Error('Invalid common politics implementation identity');
  return match[1];
}
