export function requireUnitAcceptance(report: { success: boolean; numTotalTests: number; numPassedTests: number; numPendingTests: number; numTodoTests: number }) {
  if (!report.success || !report.numTotalTests || report.numPassedTests !== report.numTotalTests || report.numPendingTests || report.numTodoTests) throw new Error('Unit acceptance requires every test to pass, without skips or TODOs');
  return { passed: report.numPassedTests, skipped: 0 };
}

interface Suite { suites?: Suite[]; specs?: { tests: { expectedStatus: string; results: { status: string }[] }[] }[] }
export function requireBrowserAcceptance(report: { suites: Suite[]; errors?: unknown[] }) {
  let passed = 0;
  function visit(suite: Suite) {
    for (const spec of suite.specs ?? []) for (const test of spec.tests) {
      if (test.expectedStatus !== 'passed' || test.results.length !== 1 || test.results[0].status !== 'passed') throw new Error('Browser acceptance requires every test to pass on its first attempt, without skips');
      passed++;
    }
    suite.suites?.forEach(visit);
  }
  report.suites.forEach(visit);
  if (!passed || report.errors?.length) throw new Error('Browser acceptance has no passing tests or contains errors');
  return { passed, skipped: 0 };
}
