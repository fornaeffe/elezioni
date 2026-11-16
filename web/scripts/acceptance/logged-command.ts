import { spawn } from 'node:child_process';
import { mkdir, open } from 'node:fs/promises';
import { dirname } from 'node:path';

export interface CommandResult {
  name: string;
  status: 'passed' | 'failed';
  log: string;
  elapsedMs: number;
  exitCode: number | null;
  signal: NodeJS.Signals | null;
  error?: string;
}

/** Read only the end of a log, even when the child produced gigabytes. */
export async function logTail(path: string, bytes = 3500) {
  const file = await open(path, 'r');
  try {
    const size = (await file.stat()).size;
    const buffer = Buffer.alloc(Math.min(bytes, size));
    await file.read(buffer, 0, buffer.length, size - buffer.length);
    return buffer.toString('utf8');
  } finally { await file.close(); }
}

/** Child output goes directly to disk, without buffering or flooding the agent. */
export async function runLoggedCommand(options: {
  name: string; command: string; args: string[]; cwd: string; log: string;
  env?: NodeJS.ProcessEnv;
}): Promise<CommandResult> {
  const { name, log } = options;
  await mkdir(dirname(log), { recursive: true });
  const file = await open(log, 'w');
  const started = performance.now();
  console.log(`ACCEPTANCE start: ${name}; log=${log}`);
  const timer = setInterval(() => {
    console.log(`ACCEPTANCE running: ${name}; elapsed=${Math.round((performance.now() - started) / 1000)}s; log=${log}`);
  }, 60_000);
  let result: CommandResult;
  try {
    const exit = await new Promise<Pick<CommandResult, 'exitCode' | 'signal' | 'error'>>(done => {
      const child = spawn(options.command, options.args, {
        cwd: options.cwd, env: options.env, windowsHide: true,
        stdio: ['ignore', file.fd, file.fd],
      });
      let error: string | undefined;
      child.on('error', e => { error = e.message; });
      // close waits for the child streams to finish, including spawn failures.
      child.on('close', (exitCode, signal) => done({ exitCode, signal, ...(error ? { error } : {}) }));
    });
    result = { name, status: exit.exitCode === 0 && !exit.error ? 'passed' : 'failed',
      log, elapsedMs: performance.now() - started, ...exit };
    if (exit.error) await file.write(`\nSpawn error: ${exit.error}\n`);
  } finally { clearInterval(timer); await file.close(); }
  console.log(`ACCEPTANCE ${result.status}: ${name}; elapsed=${(result.elapsedMs / 1000).toFixed(1)}s; exit=${result.exitCode ?? result.signal ?? 'unavailable'}; log=${log}`);
  if (result.status === 'failed') console.error(`Last log bytes (full output: ${log}):\n${await logTail(log)}`);
  return result;
}
