# Structured operation errors

The political coordinator, compute worker, and result service now send a
[structured diagnostic](../../../web/src/lib/core/operation-error.ts) with a
code, plain message, and available exception metadata. The workspaces give
failures operation-specific labels. The shared status box announces the label
and any corrective guidance, and expansion reveals the diagnostic code and
message. The preparation failure keeps its diagnostic if the user attempts to
run before the model is ready.

The worker boundary translates the existing `REFERENCE_SUBJECT` diagnostic so
the status box can guide users to check scenario references. The computation
fingerprint inputs were checked against `HEAD` and matched for politics,
generation, fitting, and profiles. This preserves checkpoint compatibility for
matching computation inputs. The reporting conventions are in
[AGENTS.md](../../../AGENTS.md).

`npm run check` reported zero errors and warnings. All 22 focused unit tests
and 10 Chromium tests passed. The unit tests cover the real reference exception
and the diagnostic round trip. Browser checks cover a simulated reference
reply, a real result-service failure for a missing local session, and import
errors in both workspaces. The existing reference diagnostic lacks a field
identifier, so its corrective guidance appears in the status box.
