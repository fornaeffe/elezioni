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

  <!-- Put here the electoral law selector. Default to reform-2026.
   Keep a selected generator/version per law in the scenario, initially M01 for
   Rosatellum and M02 for reform. Switching law atomically activates its saved
   choice; it does not reset either choice. Preserve shared inputs, inactive
   settings and valid loaded choices. Missing/unsupported choices are errors.
   Lock law/model switching during execution; offer explicit cancellation. -->

  <!-- Which chambers are to be renewed selector. Default both. -->

  <div id="subjects-editor">
    <div id="coalitions-editor">
      <!-- {#each scenario.coalitions as coalition} -->
        <div> 
          <div> <!-- Coalition basic controls, in a row -->
            <!-- Color -->
            <!-- Name, shared across chambers; no chamber-specific name override. -->
          </div>
          <div class="advanced-coalition-editor"> <!-- collapsed by default -->
            <!-- In which chambers they have been filed. 
             Default both, for new coalitions.
            -->
            <!-- Reform only: if effectively filed in one chamber only, offer an
             explicit reciprocal bonus-link declaration to an eligible coalition
             OR autonomous list filed in the opposite chamber. Do not match by
             name or require the same ID/kind. Derive effective coalition presence
             from member-list filings within its chamber restrictions. A populated
             coalition present in both chambers has the automatic link; D11 still
             settles its serialization/precedence. Retain temporarily inactive links. -->
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
            <!-- Name, shared across chambers. -->
            <!-- Coalition, shared across chambers; null means autonomous.
             Use separate list identities for differing names, affiliations or
             minority declarations; equal names never merge identities. -->
            <!-- Optional national reference share -->
          </div>
          <div class="advanced-list-editor"> <!-- collapsed by default -->
            <!-- Shared minority declaration, interpreted under the selected law.
             No per-chamber name, affiliation or minority overrides. -->
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
              <!-- List participation is determined by these filings, with no
               separate list-presence toggle. File only in Camera or only in
               Senato to declare chamber-only participation. -->
              <!-- If the list is member of a coalition filed in only one chamber,
               filings in the other chamber are disabled and inactive (but not erased). -->
              <!-- Cross-law filing conversion helper is deferred. Do not offer
               its controls in this implementation. Switching law selects its
               existing filings without remapping them. -->
            </div>
            <!-- Reform only: for an autonomous list effectively filed in one
             chamber only, offer an explicit reciprocal bonus-link declaration to
             an eligible coalition OR autonomous list in the opposite chamber.
             A list in a coalition uses that coalition's link. A populated autonomous
             list filed in both chambers has the automatic link; D11 still settles
             serialization/precedence. Retain temporarily inapplicable declarations. -->
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
      <!-- Choices from the supported-pair registry: M01 only for Rosatellum;
       M01 or M02 for reform. Default is reform + M02 + joint renewal. Reuse the
       M01 identity through law-specific adapters; M00 is retired. Use exact
       current supported versions and reject unsupported pairs without fallback.
       Changing this select updates only the active law's saved generator/version.
       Persist both laws' choices through reload and scenario import/export. -->
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
      <!-- {#if selectedSource is future lists} -->
        <!-- {#each scenario.lists as list} -->
          <div class="future-list-membership-editor">
            <div> <!-- Row: future list name and its own lock/chain toggle. -->
              <!-- list.name, readonly; IDs distinguish equal names. -->
              <button type="button" aria-label="Collega le modifiche alle appartenenze di Camera e Senato" aria-pressed="true">
                🔗
              </button>
              <!-- Default locked for new entries; restore saved per-list state.
               Locked: only Camera editable, Senato read-only and inherits every
               Camera add/remove atomically. Unlocked: edit each set independently.
               Re-locking REPLACES Senato with Camera, including removals/empty sets;
               never merge or copy Senato into Camera. Validate both chambers first;
               reject conflicts without partial changes and keep the pair unlocked.
               The button's accessible pressed state/label must reflect its state. -->
            </div>
            <div class="future-membership-chambers">
              <!-- Responsive layout: Camera and Senato side by side when space
               permits, stacked on narrow screens. Both remain visible when locked. -->
              <div class="camera-membership-list">
                <h4>Camera</h4>
                <!-- {#each list.memberships.camera as memberComponent} -->
                  <!-- memberComponent.name, readonly text -->
                  <!-- Remove memberComponent button -->
                <!-- {/each} -->
                <div> <!-- Row -->
                  <!-- Add memberComponent select: omit current members; disable
                   conflicting choices in Camera, and also Senato while locked,
                   with an explanation. Membership remains binary, without fractions. -->
                  <!-- Add memberComponent confirm button -->
                </div>
              </div>
              <div class="senato-membership-list">
                <h4>Senato</h4>
                <!-- {#each list.memberships.senato as memberComponent} -->
                  <!-- memberComponent.name, readonly text -->
                  <!-- Remove memberComponent button, disabled while locked -->
                <!-- {/each} -->
                <div> <!-- Row -->
                  <!-- Add memberComponent select, disabled while locked. When
                   unlocked: omit current members and disable conflicting choices
                   in Senato only. Membership remains binary, without fractions. -->
                  <!-- Add memberComponent confirm button, disabled while locked -->
                </div>
                <!-- While locked, show that Senato inherits Camera memberships. -->
              </div>
            </div>
            <!-- Persist both sets and this list's lock state through loading,
             law/model/renewal switching and draft/scenario roundtrips. Loading must
             not overwrite differing saved sets with the default locked behavior;
             a saved locked pair must have equal sets. Filings/renewal do not erase
             either set or change the edit direction. Component reuse across
             different chamber-only lists is not a global membership conflict. -->
            <!-- Hide artificial new components here. Derive fallback need per
             chamber where historical members are absent; preserve stable component
             identity and saved fallback assumptions when membership changes. -->
          </div>
        <!-- {/each} -->
      <!-- {:else} Historical source editing: retain one member list per option. -->
      <!-- {#each selectedSource.options as option} -->
        <div>
          <!-- if selectedSource is a poll and option is "other", do not show
           member components: "other" option contains all components not member
           of any other option of the selected source. -->
          <!-- {#each option.memberComponents as memberComponent}-->
            <!-- memberComponent.name, readonly text -->
            <!-- Historical composition fraction, visible only under M01 and
             historical source, for either law. Preserve assumption provenance;
             equal fractions are assumptions, not observed component counts.
             Future memberships remain binary under both models. -->
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
      <!-- {/if} -->
    </div>

    <!-- Component reference shares editor: the same as list reference shares
     editor, but per component. New components for new lists without historical
     data are visible and active only in chambers where those lists have no
     historical component memberships. Preserve inactive fallback declarations. -->

    <!-- Candidate reuse probability and maximum ordinary filings for candidate.
     Visible if applicable under current law/model. -->

    <!-- Do not offer json editing. This can be already done downloading the scenario,
     editing it in an editor and uploading it. -->

  </div>
  
  <div id="simulation-controls">
    <!-- Number of simulations: shared resource validation and bounded durable
     processing for all supported pairs, without a Rosatellum-specific 1000 cap.
     Acceptance campaigns do not include runs above 1000. -->
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
  <!-- One session list across Rosatellum + M01, reform + M01 and reform + M02,
   with optional filters. Show law, generator/version, creation time, renewal,
   and completed/requested runs. Keep viewed session independent of the draft.
   Edits or law/model switching retain the viewed session and its executed labels;
   show whether it differs from the draft. Starting a run selects its new session.
   Browsing/importing a supported new-format result selects it without replacing
   the draft; offer an explicit action to load its scenario through validation.
   Reload restores the selected session if available, otherwise reports its absence.
   Use durable chunked results, lazy queries and streaming exports for all pairs;
   preserve committed results after cancellation, failure or storage exhaustion. -->
</div>

<div id="state-monitor">
  <!-- This should be a small fixed div near top right corner of the window.
   It should have a colored icon representing the state, and a progress
   indication when applicable. -->
  <!-- Here should be displayed app state, with cancel button when a cancelable
   task is running. -->
</div>
