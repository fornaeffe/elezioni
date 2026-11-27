# Politics model progress and editor responsiveness

Two 30-second layout checks timed out during the 82-test Chromium run at 14
workers. The traces showed the shared landmarks resolving. In a single-page
profile, preparation progress repeatedly invalidated the editor, where
validation scanned the pinned location catalogue for each filing. The resulting
main-thread tasks lasted about 140 ms each while fitting ran in the worker.

The [workspace controller](../../../web/src/lib/politics/workspace-controller.ts)
now publishes progress separately from scenario and session changes. The
[workspace](../../../web/src/lib/politics/CommonPoliticsWorkspace.svelte) keeps
the editor's scenario reference stable across status and session updates.
[Validation](../../../web/src/lib/politics/scenario/validation.ts) indexes the
same location keys once per pass and retains the existing diagnostics. The
[page-frame checks](../../../web/tests/e2e/workflow-selector.spec.ts) use the
existing editor preparation fixture and keep their original assertions and
timeout. The controller and refit tests cover delivery to the shared status
panel and isolation from scenario updates.

`npm run check` and `npm run build` passed. All 54 focused controller and
scenario tests passed. Chromium passed 30 workflow-selector and status checks
at 14 workers over three repeats, plus the dedicated refit progress and
cancellation check and two constructed execution lifecycle checks. In the first
5.5 seconds of the updated production build,
a single Chromium page received 36 real preparation messages and recorded one
70 ms main-thread task. The same probe before the fix recorded repeated tasks
of about 140 ms with 36 messages. Full-suite verification remains pending.
