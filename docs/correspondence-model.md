Let $P$ be the set of all individual political parties that were present at least once in the considered timespan in an election or poll as a single list.

For $j\in \{0\}\cup P$, define latent log score

$$
s_{j,c,k,t}=\eta_{j,n,t}+\delta_{j,c,t}+\gamma_{j,k}
$$

Let $P_{e}$ be the subset of $P$ containing the parties present, alone or joined in a multi-party list, at election (or poll) $e$.

For $j\in \{0\}\cup P_{e}$, define the latent probability

$$
\pi_{j,c,k,e}
=\frac{\exp(s_{j,c,k,t_e})}
{\sum_{h\in\{0\}\cup P_{e}}\exp(s_{h,c,k,t_e})}
$$

Let $L_{e}$ be the set of the lists present at election (or poll) $e$.

For $l\in L_{e}$ let $P_{e,l}$ be the parties joined under list $l$ (one-element set if the party is present with its own list), then

$$
\pi_{l,c,k,e}
=\sum_{h\in P_{e,l}}\pi_{h,c,k,e}
$$

($\pi_{0,c,k,e}$ keeps its value).

Then we use **lists**' probabilities (not parties probabilities) to model observed votes in relations

$$
\boldsymbol y_{c,e}\mid E_{c,e},\boldsymbol\pi_{c,k(e),e}
\sim \operatorname{Multinomial}(E_{c,e},\boldsymbol\pi_{c,k(e),e})
$$
and 
$$
r^{\mathrm{agg}}_{l,q}
=
\frac{\sum_{c\in S_q}E_{c,e}\pi_{l,c,k,e}}
{\sum_{c\in S_q}E_{c,e}(1-\pi_{0,c,k,e})}.
$$
The rest of the model can stay the same, I suppose.

The correspondence matrix thus need, for each election, to link each election's list to one or more parties. Each party, in each election, can be linked to 0 or 1 lists.

Party links for historical elections and polls pertains to the data preparation. Party links for the modelled (next) election pertains to the scenario. Scenario's brand new parties should be given an explicit $\pi$.