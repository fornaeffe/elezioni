<!-- Approved page draft; see guidelines.md for the common contract and decisions.
  Decompose this page in multiple components as appropriate. -->
<!-- Prefer Svelte's conditional and/or lazy mounting for non visible components, instead
  of DOM-mounted but visually hidden components, if this may improve
  browser performances, given the expected large number of possible components in the 
  page. -->
<svelte:head><title>Elezioni politiche</title></svelte:head>

<!-- Insert home button or breadcrumbs here -->

<div id="scenario-editor">
  <div> <!-- This should be a row -->
    <label>
      Carica scenario
      <input
        type="file"
        accept=".json,.gz"
      />
    </label>

    <button>
      Scarica scenario
    </button>

    <button>
      Ripristina scenario predefinito
    </button>
  </div>

  <!-- Scenario name -->

  <!-- Election date -->

  <!-- Put here the electoral law selector. Default to reform-2026 -->

  <!-- Which chambers are to be renewed selector. Default both. -->

  <div id="subjects-editor">
    <div id="coalitions-editor">
      <!-- {#each scenario.coalitions as coalition} -->
        <div> 
          <div> <!-- Coalition basic controls, in a row -->
            <!-- Color -->
            <!-- Name -->
          </div>
          <div class="advanced-coalition-editor"> <!-- collapsed by default -->
            <!-- In which chambers they have been filed. 
             Default both, for new coalitions.
            -->
            <!-- If not filed in both chambers, optional linking with another
             coalition in another chamber. If filed in both chambers, the two
             per-chamber coalitions should automatically be linked. Only visible
             under 2026 reform -->
            <div class="coalition-candidates-editor"> <!-- Collapsed by default -->
              <!-- Reform 2026: Collapsible tree view of chamber - constituency -->
                <!-- {#each tree view constituency }-->
                  <div class="bonus-slate">
                    <!-- {#each slate slot}-->
                      <div class="bonus-slate-slot">
                        <!-- Select or combobox to select an available candidate
                          for this coalition, showing also his/her list. 
                          Candidates not available (not filed in an eligible position,
                          or already in a different bonus slate)
                          should be shown disabled. Offer also a "new" option:
                          this should open a dialog to enter a new candidate data, including list. -->
                        <!-- If a candidate is selected, offer a "edit candidate" button -->
                      </div>
                    <!-- {/each}-->
                  </div>
                <!-- {/each }-->
              <!-- Rosatellum (previous law): Collapsible tree view of chamber 
               - constituency - plurinominal college - uninominal college -->
                <!-- {#each tree view uninominal college }-->
                  <div class="uninominal-slot">
                    <!-- Select or combobox to select an available candidate
                      for this coalition, showing also his/her list if any. 
                      Candidates not available (e.g. already filed in another
                      uninominal college)
                      should be shown disabled. Offer also a "new" option:
                      this should open a dialog to enter a new candidate data, including optional list. -->
                    <!-- If a candidate is selected, offer a "edit candidate" button -->
                  </div>
                <!-- {/each }-->
            </div>
          </div>
        </div>
      <!-- {/each} -->
    </div>
    <div id="lists-editor">
      <!-- {#each scenario.lists as list} -->
        <div> 
          <div> <!-- List basic controls, in a row -->
            <!-- Color -->
            <!-- Name -->
            <!-- Coalition -->
            <!-- Optional national reference share -->
          </div>
          <div class="advanced-list-editor"> <!-- collapsed by default -->
            <!-- Optional national reference share uncertainty -->            
            <!-- National and local uncertainty multiplicators, visible when applicable-->
            <!-- Optional per-chamber national reference share and uncertainty inputs -->
            <div class="list-filings-editor"> <!-- Collapsed by default,
              with a candidate badge if any candidate is set. Do not add a partial
              coverage badge where a tri-state checkbox already conveys it. -->
              <!-- Selectable and collapsible tree view of chambers - constituencies - colleges -->
                <!-- {#each tree view element }-->
                  <!-- Checkbox to file or unfile list in this college or 
                   all children colleges. If partially filed, the checkbox should
                   render this condition. Unfiling when some children candidate filings
                   are present makes that candidate filings inactive (but not erased from data) -->
                  <!-- {#if for this element a candidate slate can be filed} -->
                    <div class="candidate-slate"> <!-- Collapsed by default, badge if any candidate is filed -->
                      <!-- {#each slate slot}-->
                        <div class="candidate-slot">
                          <!-- Select or combobox to select an available candidate
                           for this list. Candidates already at their maximum filings
                           should be shown disabled. Offer also a "new" option:
                           this should open a dialog to enter a new candidate data. -->
                          <!-- If a candidate is selected here, and reform 2026 is 
                           selected, and this candidate can be added to the parent 
                           constituency's bonus
                           slate, show a checkbox to add/remove this candidate 
                           to the parent constituency's bonus slate -->
                          
                          <!-- If a candidate is selected, offer a "edit candidate" button -->
                        </div>
                      <!-- {/each}-->
                    </div>
                  <!-- {/if} -->
                <!-- {/each }-->
              <!-- End of selectable and collapsible tree view-->
              <!-- Underlying data should be stored per-law, and only the data 
               for the selected law should be shown and edited. -->
              <!-- Initialize every law block for every new subject, including
               inactive laws. New lists are filed in all eligible colleges of both
               chambers under every law. Default lists have their authored default
               filings for every law, including intentional restrictions. -->
              <!-- If the list is member of a coalition filed in only one chamber,
               filings in the other chamber are disabled and inactive (but not erased). -->
              <!-- Cross-law filing conversion helper is deferred. Do not offer
               its controls in this implementation. Switching law selects its
               existing filings without remapping them. -->
            </div>
            <!-- If not filed in both chambers and not member of a coalition, 
             optional linking with another
             list in another chamber. If filed in both chambers and not member
             of a coalition, the two
             per-chamber lists should automatically be linked. Only visible
             under 2026 reform -->
            <div class="list-local-references-editor"> <!-- Collapsed by default,
              with a badge if any local reference share or uncertainty is defined -->
              <!-- Collapsible tree view of region - province - municipality -->
              <!-- {#each tree view element} -->
                <div> <!-- Row -->
                  <!-- Optional reference share -->
                  <!-- Optional reference share uncertainty -->
                  <!-- Button to delete the reference share for this tree element -->
                  <!-- Button to delete the reference share for this tree element and
                  all its children at any level below -->
                  <!-- If its children are collapsed and any of them, at any level,
                  has declared reference shares, show a badge or some other indicator -->
                </div>
                <div class="per-chamber-local-references"> <!-- Collapsed by default,
                  with a badge if any per-chamber reference share or uncertainty is defined here -->
                  <!-- Optional per-chamber reference share and uncertainty inputs -->
                  <!-- Button to delete this per-chamber data -->
                </div>
              <!-- {/each } -->
            </div>
          </div>
        </div>
        <!-- Same list controls (where applicable) for "other" list, if enabled -->
      <!-- {/each} -->
      <!-- Add list button: add a new list with default values. Default to
       new component without historical data. -->
      <!-- "Add other lists" button: enable "other" lsit with default values. 
       Default to
       new component without historical data. -->
    </div>
  </div> 

  <div id="advanced-scenario-editor"><!-- Collapsed by default -->

    <label>
      Modello di generazione voti:
      <!-- Choices from the supported-pair registry: M00 (previous-law generator),
       M01 and M02. Default selection is reform + M02 + joint renewal. -->
      <select>
      </select>
    </label>
    <!-- We should not have a "load default model" button: 
    - on a default scenario, automatically load default vote generation model;
    - on a loaded scenario, automatically load the vote generation model defined
      in the scenario
    - on a model change, automatically load the newly selected model
   -->
  
    <!-- Put here "Osservazioni parziali dichiarate nel M01", visible only if 
    M01 is selected. -->

    <div id="membership-editor"> <!-- Collapsed by default -->      
      <!-- Historical source / future lists selector. Default to future lists -->
      <!-- {#each selectedSource.options as option} -->
        <div>
          <!-- if selectedSource is a poll and option is "other", do not show
           member components: "other" option contains all components not member
           of any other option of the selected source. -->
          <!-- New components for new lists without historical components should
           not be listed here: a new list without historical components here 
           means that a new component for that list is required. If the user adds
           a historical component here for a new list, the new component is 
           no more required. -->
          <!-- {#each option.memberComponents as memberComponent}-->
            <!-- memberComponent.name, readonly text -->
            <!-- memberComponent.fraction, visible only under M01 and historical source -->
            <!-- remove memberComponent button -->
          <!-- {/each}-->
          <div> <!-- Row -->
            <!-- 
              Add memberComponent select.
              Components already selected for this option should not be shown.
              Components already selected for other options of this source should
                be shown as disabled.
            -->
            <!-- Add memberComponent confirm button -->
          </div>
        </div>
      <!-- {/each} -->
    </div>

    <!-- Component reference shares editor: the same as list reference shares
     editor, but per component. New components for new lists without historical
     data are visible and active only while those list have no historical component
     memberships. -->

    <!-- Candidate reuse probability and maximum ordinary filings for candidate.
     Visible if applicable under current law/model. -->

    <!-- Do not offer json editing. This can be already done downloading the scenario,
     editing it in an editor and uploading it. -->

  </div>
  
  <div id="simulation-controls">
    <!-- Number of simulations -->
    <!-- Run button -->
    <div id="advanced-simulation-controls"> <!-- Collapsed by default -->
      <!-- Fitting seed input, with a "Genera seed casuale" button beside it.
       Clicking fills only this input with a random seed visible to the user. -->
      <!-- Simulation seed input, with its own "Genera seed casuale" button.
       Clicking fills only this input with a random seed visible to the user. -->
      <!-- Persist each concrete seed as an ordinary scenario edit. No checkbox,
       launch-time randomization mode, or automatic replacement on load/run. -->
      <!-- Requested number of compute workers / explicit auto policy, saved with
       the scenario. Record actual workers separately in the executed run. -->
    </div>
  </div>

</div>

<div id="results">

</div>

<div id="state-monitor">
  <!-- This should be a small fixed div near top right corner of the window.
   It should have a colored icon representing the state, and a progress
   indication when applicable. -->
  <!-- Here should be displayed app state, with cancel button when a cancelable
   task is running. -->
</div>
