# Use a common politics scenario

Merge the politics workspaces in a single workspace: switching generator
and/or scrutiny algorithm should be as simple as choosing an option in a select.
The scenario editor should simply hide sections not relevant to the current
selected generator and scrutiny algorithm.

Simplify and rationalize the web app structure as much as possible, without changing vote 
generation models and scrutiny algorithms and without limiting the user's 
possibilities to control the scenario.

While the current update is scoped on politics only, choose solutions that would
allow reuse in regional workspace also. Ultimately, differences in contracts, 
logic and UI should only be the unavoidable differences due to different laws,
models and geographical scope, both between regional and politics and among politics
laws and vote generation models.

Substitute the default politics 
scenario building pipeline with a single
default scenario json file with all necessary information for previous law,
reform M01 and reform M2 workflows. Make all three workflow use the same
shared scenario contract. Depending on the selected workflow, only the 
relevant part of the scenario will be used.

Look at the 
[politics page draft](docs\common-politics-scenario\politics-page-draft.svelte)
for a general
idea about what the politics page should look like (but do not take that draft
as mandatory).

## What information should a scenario contain

A not exausthive list (add information if needed)

- Scenario schema version
- Historical data version
- Name
- Data
- Electoral law (scrutiny algorithm) and its version
- Vote generation model and its version
- Chambers to be renewed
- Coalitions and lists, with filings, links, components memberships, reference shares and uncertainties, candidates. Try to use the same data objects for all laws if the different laws do not explicitly require different data shapes. When data shape differs among different laws, store that data separately for each law.
- "Osservazioni parziali dichiarate nel M01"
- Candidate reuse probability and maximum ordinary filings for candidate
- Number of simulations
- Seeds for fitting and simulation
- Number of compute workers

I think that scenario should _not_ contain:

- Historical data and historical components
