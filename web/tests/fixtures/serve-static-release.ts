/** Test server for adapter-static's final build, including transparent HTTP gzip. */
import { createServer } from 'node:http';
import { readFile, stat } from 'node:fs/promises';
import { resolve, relative, isAbsolute, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
const root = fileURLToPath(new URL('../../build/', import.meta.url)),
  base = process.env.BASE_PATH ?? '';
const types: Record<string, string> = {
  '.html': 'text/html',
  '.js': 'text/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.svg': 'image/svg+xml',
  '.gz': 'application/octet-stream',
};
const server = createServer(async (request, response) => {
  try {
    const url = decodeURIComponent(
      new URL(request.url!, 'http://localhost').pathname,
    );
    if (!url.startsWith(base + '/')) throw new Error('Outside base');
    let path = resolve(root, '.' + url.slice(base.length));
    const rel = relative(root, path);
    if (rel.startsWith('..') || isAbsolute(rel))
      throw new Error('Outside build');
    if ((await stat(path)).isDirectory()) path = resolve(path, 'index.html');
    const body = await readFile(path);
    response.writeHead(200, {
      'Content-Type': types[extname(path)] ?? 'application/octet-stream',
      'Content-Length': body.length,
      ...(path.endsWith('.gz') ? { 'Content-Encoding': 'gzip' } : {}),
    });
    response.end(body);
  } catch {
    response.writeHead(404);
    response.end('Missing static release file');
  }
});
server.listen(4174, '127.0.0.1');
