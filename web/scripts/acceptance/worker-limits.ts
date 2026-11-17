/** Browser contexts contain their own application workers. Size them separately
 * from Node unit-test workers, especially on the hosted four-CPU runner. */
export function acceptanceWorkers(env: NodeJS.ProcessEnv) {
  const read = (name: string, fallback: string) => {
    const value = env[name] ?? env.ACCEPTANCE_TEST_WORKERS ?? fallback;
    if (!/^\d+$/.test(value) || Number(value) < 1 || Number(value) > 64)
      throw new Error(`${name} (or legacy ACCEPTANCE_TEST_WORKERS) must be an integer from 1 through 64`);
    return Number(value);
  };
  return { units: read('ACCEPTANCE_UNIT_WORKERS', '2'), browsers: read('ACCEPTANCE_BROWSER_WORKERS', env.CI ? '1' : '2') };
}
