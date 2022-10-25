# Slavenregisters


## matching.R
Contains the matching programs to match certificates between and within series. We matched names of enslaved, mothers, and owners with a maximum Levenshtein distance based on the length of the name. Data is matched in five steps as shown in the table underneath. 

| Step | Between matching | Within matching |
| ---- | ---------------- | --------------- |
| *0. Data selection* | **Filter** </br> - Out event entry 1 is end series *or* event year is last year of series </br> - In event entry 2 is begin series *or* event year is first year of series </br> | **Filter** </br> - Out event entry 1 is a transfer </br> - In event entry 2 is a transfer |
| *1. Retrieval* | **Match entries by** </br> - Name enslaved </br> - Name owner | **Match entries by** </br> - Name enslaved |
| *2. Rule-based filter* | **Select matches if:** </br> - Sex is identical *or* unknown </br> - Name mother matches *or* is unknown </br> - Year of birth is identical *or* unknown </br> *or* names enslaved in preceding and proceeding entries match </br> *or* name enslaved in preceding entry matches and no proceeding event entry 1 | **Select matches if:** </br> - Prevent that entries match themselves </br> - Sex is identical *or* unknown </br> - Name mother matches *or* is unknown </br> - Year of birth is identical *or* unknown *or* month and day of birth are identical </br> - Year of transfer is identical *or* unknown *or* month and day of transfer are identical |
| *3. Probabilistic matching* | **Score matching indicators and select highest scoring match per entry:** </br> - Identical name match (0.5 pts) *only between 1-2* </br> - Name mother (2.5 pts) </br> - Addendum name mother (1 pt) </br> - Addendum name enslaved (1 pt) </br> - Year of birth (2 pts) </br> - Name enslaved in preceding entry (1 pt) </br> - Name enslaved in proceeding entry (1 pt) | **Score matching indicators and select highest scoring match per entry:** </br> - Name mother (2.5 pts) </br> - Addendum name mother (1 pt) </br> - Addendum name enslaved (1 pt) </br> - Year of birth (2 pt) </br> - Year of transfer (2 pts) </br> - Month of transfer (1 pt) </br> - Day of transfer (1 pt) </br> - Month *and* day of transfer (1 pt) </br> - Name enslaved in preceding entry (1 pt) </br> - Name enslaved in proceeding entry (1 pt) |
| *4. Add unmatched cases* | **Append** </br> - Matches records from step 3 </br> - Unmatched records from step 0 </br>| **Append** </br> - Matched records from step 3 </br> - Unmatched records from step 0 |
| *5. Add metadata* | **Show Levenshtein distance:** </br> - Name enslaved </br> - Name owner </br> - Name mother </br> - Name preceding entry </br> - Name proceeding entry | **Show Levenshtein distance:** </br> - Name enslaved </br> - Name mother </br> - Name preceding entry </br> - Name proceeding entry  |
| | | **Add variables** </br> - Remarks entry </br> - Remarks exit |
| | **Flag matched information:** </br> - Entry </br> - Addendum name enslaved </br> - Name mother </br> - Addendum name mother </br> - Birth year </br> - Name preceding entry </br> - Name proceeding entry </br> - Out event | **Flag matched information:** </br> - Entry </br> - Addendum name enslaved </br> - Name mother </br> - Addendum name mother </br> - Birth year </br> - Name preceding entry </br> - Name proceeding entry </br> - Year of transfer </br> - Month of transfer </br> - Day of transfer |

### Levenshtein distance

Names of enslaved and mothers are matched with a maximum Levenshtein distance based on the length of the name.

| Characters | Max Lev dist between series | Max Lev dist within series |
| ---------- | ---------------------------- | -------------------------- |
| 3 | 1 | 1 |
| 4-8 | 2 | 2 |
| 9+ | 3 | 2 |

Names of plantations are matched exactly, private owners with a maximum Levenshtein distance based on the length of the name.

| Characters | Max Lev dist between series | 
| ---------- | ---------------------------- | 
| 3-5 | 1 | 
| 6-8 | 2 |
| 9+ | 3 | 

### Reconstitution




