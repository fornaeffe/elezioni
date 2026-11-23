## SvelteKit path resolution (2026-11-15)

Replaced every deprecated `$app/paths` `base` import in `web/src` with
`resolve()` for route navigation and `asset()` for pinned static assets. Worker
requests still receive the base prefix, derived from `resolve('/')` with its
trailing slash removed.

The first editor browser run caught that `asset(pin.uri)` treats a relative URI
as relative to the current route. Passing an absolute static path to `asset()`
fixed the resulting `/politics/data/...` requests.

Verification: the final `npm run check` passed with zero errors and warnings.
The common politics editor suite passed 10/10 at `/`; its representative file
reload test passed at `/elezioni`. Workflow selector and regional worker tests
passed at both prefixes. Vite emitted its existing large-chunk warning during
the browser builds.
