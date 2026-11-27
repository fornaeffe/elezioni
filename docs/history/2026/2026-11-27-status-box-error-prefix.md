# Duplicate error prefixes in the status box

The politics worker sends some failures as strings beginning with `Error:`.
The client wraps the string in an `Error`, which can add a second prefix when
the workspace converts it back to text. The [shared status box](../../../web/src/lib/AppStatus.svelte)
collapses repeated `Error:` prefixes in displayed details and retains the
diagnostic code and message.

The [browser regression](../../../web/tests/e2e/app-status.spec.ts) injects the
reported `REFERENCE_SUBJECT` worker reply and exercises the client and status
box. It reproduced the duplicate before the fix. All four status-box Chromium
tests passed afterward. `npm run check` reported zero errors and warnings.
