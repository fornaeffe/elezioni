# Vote-share workspace integration

M01/M02 now run through the common politics workspace. The separate FV-07
workspace/editor, its draft key, explicit default-model button and old file
imports are retired.

| Task | Current guide |
| --- | --- |
| Edit, select a model and run | [Politics user workflow](politics-reform-2026-editor.md) |
| M01/M02 statistical semantics and numerical entry points | [Unified generation](vote-generation-unified-workflow.md) |
| Exact authored defaults and release preparation | [Default packaging](common-politics-scenario/defaults.md) |
| Automatic fitting, durable sessions, worker/resource bounds | [Lifecycle](common-politics-scenario/lifecycle.md) |
| Scenario, checkpoint and archive formats | [Strict common files](common-politics-scenario/files.md) |
| Functional and national performance checks | [CP-08 acceptance](common-politics-scenario/acceptance.md) |

The retained numerical release schema and portable numeric states are internal
model inputs, not compatible user scenario/result files. Old file uploads are
rejected and old draft keys remain untouched. Deferred D14 editors are listed
in `TODO.md`; retained settings still validate and survive roundtrips.
