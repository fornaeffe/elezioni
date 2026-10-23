import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { loadPollCalibrationCases, weightedPollPoint, evaluateCalibration } from './poll-calibration';
import { evaluateEndpointUncertainty } from './poll-uncertainty';
import { auditGeographicNationalDrift } from './poll-geography-audit';
import { sha256, writeVersion } from './sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const command = process.argv[2];
if (!['check', 'generate'].includes(command) || process.argv.length !== 3) throw new Error('Usage: poll-uncertainty-cli check|generate');
const { archive, cases, sourceVersions } = await loadPollCalibrationCases(root);
const endpoints = cases.map(c => ({ id: c.id, kind: c.kind, date: c.date, cutoff: c.cutoff, point: weightedPollPoint(c), truth: c.truth }));
const previous = evaluateCalibration(cases);
const body = { schemaVersion: 1, kind: 'poll-uncertainty-study', archiveVersion: archive.bundle.version, sourceVersions,
  endpoints, ...evaluateEndpointUncertainty(endpoints), baselineMetrics: previous.metrics.filter(m => ['poll-only-original', 'poll-only-floor'].includes(m.mode)),
  baselineCameraMetrics: previous.byKind.camera.filter(m => ['poll-only-original', 'poll-only-floor'].includes(m.mode)),
  geographyAudit: auditGeographicNationalDrift(),
  assumptions: ['Research only: active forecast/runtime policies are unchanged.',
    'Every fit uses only earlier elections. Three ILR coordinates are modeled jointly; validation units remain whole elections.',
    'Zero-centered errors retain the weighted-poll point, not a fitted partisan/house correction. Predictive share means/medians can differ after inverse ILR.',
    'Endpoint errors include sampling, common polling error, late movement and scope mismatch; do not add the old national movement or poll measurement variance again.',
    'Inverse-Wishart covariance prior: dimension 3, df 5, scale 0.2^2 I; prior 0.1/0.3 and same-kind fits are sensitivity analyses, not tuned alternatives.',
    'Symmetrized bootstrap assumes symmetric endpoint errors and has only 2n error vectors; missing tail events cannot be invented.',
    'Conformal max-share-error boxes use exact finite-sample rank, not an interpolated percentile; exchangeability across elections/kinds is unverified.',
    'Only the current positive four-category panel and 15-16 day endpoint horizon are tested. No proof of coverage for future elections, other lists, horizons or seats.'] };
const report = { ...body, version: `poll-uncertainty-v1-${sha256(JSON.stringify(body)).slice(0, 12)}` };
if (command === 'generate') await writeVersion(resolve(root, `data/derived/polls/${report.version}.json`), report);
console.log(JSON.stringify({ version: report.version, metrics: report.metrics, cameraMetrics: report.byKind.camera, geographyAudit: report.geographyAudit }, null, 2));
