# Politics editor toggletips

The [shared toggletip](../../../web/src/lib/ui/Toggletip.svelte) opens on demand,
announces the help through a live region, and uses native popover dismissal.
The list grid puts national-reference help in the shared heading on desktop and
next to each field label on narrow screens.

The Italian help explains the M01 and M02 interpretations, the vote and elector
denominators, and what leaving a reference blank does. It also covers the
reference date, M02 multipliers and fallback parameters, candidate availability
and birth date, and the suggested progression from 10 to 1,000 simulations.
Candidate creation and the help share the same assumed birth date calculation.

Verification passed with zero Svelte errors or warnings, 40 scenario unit
tests, and all 11 common politics editor browser tests in Chromium. The focused
toggletip test also passed in Firefox. It covers keyboard and outside dismissal,
all requested help topics, model switching, and placement at 320 pixels.
The editor tests use controlled preparation. They exercise the UI without
running the vote generation models. The existing advanced editor test now
locates a historical membership control by the component’s displayed name.
