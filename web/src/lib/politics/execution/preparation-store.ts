import type { PoliticsScenario } from '../scenario/schema';
import type { PoliticsCheckpoint } from '../scenario/envelopes';

export interface StoredPreparation {
  scenario: PoliticsScenario;
  checkpoint: PoliticsCheckpoint;
  fitKey: string;
  profileKey: string;
}
/** A single replaceable entry. This cache never supplies a draft or a result;
 * loss/quota/corruption falls back to ordinary visible preparation. */
async function database() {
  return new Promise<IDBDatabase>((resolve, reject) => {
    const r = indexedDB.open('elezioni-politics-preparation-v1', 1);
    r.onupgradeneeded = () => r.result.createObjectStore('preparation');
    r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error);
  });
}
export async function readPreparation(): Promise<StoredPreparation | undefined> {
  const db = await database();
  try { return await new Promise((resolve, reject) => {
    const r = db.transaction('preparation').objectStore('preparation').get('latest');
    r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error);
  }); } finally { db.close(); }
}
export async function writePreparation(value: StoredPreparation): Promise<void> {
  const db = await database();
  try { await new Promise<void>((resolve, reject) => {
    const tx = db.transaction('preparation', 'readwrite'); tx.objectStore('preparation').put(value, 'latest');
    tx.oncomplete = () => resolve(); tx.onabort = () => reject(tx.error); tx.onerror = () => {};
  }); } finally { db.close(); }
}
