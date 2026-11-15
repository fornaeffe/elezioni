/** Coalesce small row strings before the encoding/compression stream boundary.
 * Memory is bounded by the target plus one already-bounded source record.
 * A single TextEncoderStream preserves split UTF-16 surrogate pairs. */
export function encodedExportStream(values: AsyncGenerator<string>, onChunk?: () => void): ReadableStream<Uint8Array<ArrayBuffer>> {
  let done = false;
  return new ReadableStream<string>({
    async pull(controller) {
      try {
        const parts: string[] = []; let length = 0;
        while (!done && length < 65536) {
          const next = await values.next(); done = !!next.done;
          if (!next.done) { parts.push(next.value); length += next.value.length; }
        }
        if (parts.length) { onChunk?.(); controller.enqueue(parts.join('')); }
        if (done) controller.close();
      } catch (error) { await values.return(undefined).catch(() => undefined); controller.error(error); }
    },
    async cancel() { await values.return(undefined); }
  }).pipeThrough(new TextEncoderStream());
}
