# TODO prompt list

This is a list of topics that I will later discuss with the agent. You can
read it to know which topics will be discussed later, but do not take this list
as accepted task list that you must implement.

- **D01 Generator identity/version**, **D02 Combination scope and selection transitions**, **D08 M00 correspondence controls:** and **D17 result sessions and resources**:
  - The architecture should be open to new vote generation models for each
    electoral law.
  - Assess which is better:
    - allow reuse of a same vote generation model with different laws
    - each law has its own vote generation models, with models of the same family
      possibly sharing a common core. E.g. rosatellum (previous law) + M01-rosatellum,
      rosatellum + M02-rosatellum, reform + M01, reform + M02.
  - Asses wich is better:
    - adapt the previous law generator M00 to M01/M02 durable storage/multiworker
      execution and result sessions selection and version policy, 
      removing the 1000 run limit, and using for M00 the same membership 
      with fractions semantics of M01.
    - substitute the previous law generator with M01 (possibly adapted if the 
      previous assessment outcome is law-specific generators).
    Note that I have no reason for retaining 
    M00 other that ease of implementation, if any.
  - Workflows required:
    - Previous law (Rosatellum) + M00 (or M01 or M01-rosatellum according to 
      previous assessments' outcomes)
    - Reform + M01
    - Reform + M02
  - Switching to Rosatellum: select its only generation model
  - Switching to reform: select M02

- Discuss per-chamber future membership editor

- docs\common-politics-scenario\guidelines.md:178-180 discuss if “not yet 
  configured for this law” is a possible situation, given that default lists
  should have default filings for each law, and new lists should be filed 
  everywhere by default.

- discuss filings selectable tree view anatomy and behavior

- clarify docs\common-politics-scenario\guidelines.md:229:230 
  "Continue generic export through
  `web/src/lib/core/result-export.ts`" and csv exports.

- **D04 Authored default and data pins**: 
  - Approved:
    - `data/politics-default-scenario.json` filename
    - source/seat/geography manifest and all default values, including dates,
      references, memberships, per-law filings, candidate settings and run 
      count/seeds already in default scenario where shared.
  - Where scenario defaults differ:
    - use reform M02 defaults where applicable
    - list all remaining decisions about different or missing defaults.
  - Include in this task the party-color task (MIGRATION_PLAN.md:161-162)
