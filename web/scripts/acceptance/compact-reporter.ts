import type { FullConfig, FullResult, Reporter, Suite, TestCase, TestError, TestResult } from '@playwright/test/reporter';
import { closeSync, mkdirSync, openSync, writeSync } from 'node:fs';
import { dirname } from 'node:path';

/** Full text stays on disk; the JSON co-reporter owns structured test evidence. */
export default class CompactReporter implements Reporter {
  private file: number;
  private failed = 0;
  private passed = 0;
  private discovered = 0;
  constructor(private options: { log: string; report: string }) {
    mkdirSync(dirname(options.log), { recursive: true });
    this.file = openSync(options.log, 'w');
  }
  printsToStdio() { return true; }
  onBegin(_config?: FullConfig, suite?: Suite) {
    this.discovered = suite?.allTests().length ?? 0;
    console.log(`Resource campaign runner started; log=${this.options.log}; tests=${this.options.report}`);
  }
  onStdOut(chunk: string | Buffer) {
    writeSync(this.file, typeof chunk === 'string' ? Buffer.from(chunk) : chunk);
    // Only the harness's deliberately compact messages are forwarded.
    for (const line of chunk.toString().split(/\r?\n/)) if (line.startsWith('[campaign] ')) console.log(line);
  }
  onStdErr(chunk: string | Buffer) { writeSync(this.file, typeof chunk === 'string' ? Buffer.from(chunk) : chunk); }
  private errors(errors: TestError[]) {
    const full = errors.map(e => e.stack ?? e.message ?? String(e.value)).join('\n');
    writeSync(this.file, full + '\n');
    console.error(`Resource failure (last 3500 characters; full details: ${this.options.report}):\n${full.slice(-3500)}`);
  }
  onError(error: TestError) { this.errors([error]); }
  onTestEnd(test: TestCase, result: TestResult) {
    if (result.status === 'passed' && test.expectedStatus === 'passed') this.passed++;
    else { this.failed++; this.errors(result.errors); }
  }
  onEnd(result: FullResult) {
    closeSync(this.file);
    if (this.passed + this.failed === 0) {
      console.log(`Resource campaign not executed; discovered=${this.discovered}; runnerStatus=${result.status}; tests=${this.options.report}`);
      return;
    }
    console.log(`Resource campaign ${result.status}; passed=${this.passed}; other=${this.failed}; elapsed=${(result.duration / 1000).toFixed(1)}s; log=${this.options.log}; tests=${this.options.report}`);
  }
}
