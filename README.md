# Slavenregisters

## clean Preparing_Record_Linkage_dta.R
Cleans the names in the slave registers.


### 1. Control entries
- Split/combine rows to ensure that each row contains 1 enslaved person
- Set starting date to 1830 for series 1


### 2. Standardise sex
- Flagged all 571 names where occured as both male and female names
- Manually controlled these entries
- Recoded all 440 names where <25% had the alternative sex, except for: Cato, Jannie, Jantje, Minosabi(e), and Pietje.
 

### 3. Separate names and adjectives enslaved persons & mothers
- Split names and person descriptions
- Standardise person descriptions 
 
| Type | Variation | Standardisation	|
| - | - | - |
| *Numerical* | 1, 1e, I, Ie | I |
| *Numerical* | 2, 2e, II, IIe | II |
| *Numerical* | 3, 3e, III, IIIe | III |
| *Numerical* | 4, 4e, IV, IVe | IV | 
| *Numerical* | 5, 5e, V, Ve | V |
| *Numerical* | no, nr, n | |
| *Stature* | kl, klein, kleine, kleintje, klijn, klijne, klijntje | klein |
| *Stature* | korte | korte |
| *Stature* | littel, little |  little |
| *Stature* | big | big |
| *Stature* | gr, groot, groote, gt, qt | groote |
| *Stature* | lang, lange | lange |
| *Stature* | long | long |
| *Age* | jr, junior |  junior |
| *Age* | sr, senior |  senior |
| *Age* | jong, jonge | jonge |
| *Age* | nieuw, nw | nieuw |
| *Age* | oud, oude, o | oude |
| *Colour* | carb, carboeg, karboeg | carboeger |
| *Colour* | creo, creole, creool | creool |
| *Colour* | mulat, mulatt | mulat |
| *Colour* | neger | neger |
| *Colour* | rood, roode | roode |
| *Colour* | sw, swart, swarte, zw | zwart |
| *Occupation* | delver | delver |
| *Occupation* | kuiper | kuiper |
| *Occupation* | offikier | officier |
| *Mother* | over, overl, overled, overleden, overlijden, verl | overleden |
| *Plantation* | Brouwerslust | Brouwerslust |
| *Plantation* | LR, L R, L en R, L R, LsR | Land en Rust |
| *Plantation* | Res, Resol, Resolutie, Revolutie | Resolutie |
| *Plantation* | la Simp, la Simplicite, la Simplikite | la Simplicite |
| *Plantation* | Standv, Standi, Standvastigheid | Standvastigheid |
| *Plantation* | Stolk, Stokw, Stokwijk, Stolwijk, Stolkwijk, Stw | Stolkwijk |
| *Plantation* | Zeew, Zeewijk, Zeewjk | Zeewijk |
| *Other* | AkB | AkB |
| *Other* | AcB, A C B, A C-B | AcB |
| *Other* | BW | BW |
| *Other* | DB | DB |
| *Other* | DG, D G | DG |
| *Other* | DP, D P | DP |
| *Other* | E D | ED |
| *Other* | F A | FA |
| *Other* | FB | FB |
| *Other* | FD | FD |
| *Other* | IB | IB |
| *Other* | jd | jd |
| *Other* | Ns, NS | NS |
| *Other* | p w, P W | SP |
| *Other* | SP | SP |
| *Other* | vz, VZ | VZ |
| *Other* | NS | NS |
| *Other* | VCIP, V C I P | VCIP |
| *Other* | ZB, z b, Z B | ZB |


### 4. Standardise names enslaved persons & mothers
- Write out abbreviated names
- Separate 

| Type | Variation | Standardisation	|
| ---- | --------- | --------------- |
| *Name* | Alex: | Alexander |
| *Name* | Carol: | Carolina |
| *Name* | Charl: | Charlotte |
| *Name* | Chris: | Christina |
| *Name* | Elis:, Els: | Elisabeth |
| *Name* | Hendr: | Hendrika |
| *Name moeder* | Henr: | Henrietta |
| *Name moeder* | H Wilh: | Henrietta Wilhelmina |
| *Name* | John / Robn | Johan of Robijn |
| *Name moeder* | Th:, Theod: | Theodora |
| *Name moeder* | Wilh: | Theodora |
| *Name* | Wilhelmina Barth: | Wilhelmina Bartholomina |
| *General* | bij den doop, bij den doop genaamd, bij den doop genmd | gedoopt |
| *General* | , /, alias, bijgenaamd, bijgent, genaamd, ook, ook bekend als, ook genaamd, ook genmd | of |
| *General* | . - | |


### 5. manually restructure private owner names
- Data was manually cleaned to ensure that:

| Action | Example |
| ------ | ------- |
| *1. Mark straatvoogd* | Moron J, als straatvoogd |
| *2. Mark representatitives* | Berner J P boedel, door S F Flu qq |
| *3. Mark husband nom ux as representative* | Betting C D nom ux, door C D Betting qq |
| *4. Start with last name owner* | Berner J P |
| *5. Prefixes are moved towards the end of the entry* | Parra Josua de la |
| *6. Mark last names with multiple words* | Behm_Wendholdt J B L ten |
| *7. Mark last names of formerly enslaved partly as prefixes* | Beeldsnijder Charles van Anna van Betje van |
| *8. Remove redundant "de" and "van" from entries* | Thol geboren Hendriks *de* weduwe *van* M. C. C. van | 
| *9. Mark "van" in maiden names with underscore* | Krebs geboren van_Nerevalk C E |
| *10. Split references to new owner with a dash* | Mesquita Jh. Ab. Bo. de - thans Weduwe Jh. Ab. Bo. de Mesquita |


### 6. standardise names private owners
- Make Mac and Mc part of first names
- Standardise variations of geboren and weduwe

| Action | Variation | Standardisation	|
| ------ | --------- | --------------- |
| *1. Remove whitespace between Mac and last name* | Mac Neal | MacNeal |
| *2. Remove whitespace between Mac and last name* | Mc Neal | McNeal |
| *3. Standardise variations geboren* | geb, gebr, gebs, gebn | geboren |
| *4. Standardise variations weduwe* | wd | weduwe |


### 7. standardise names private owners
- Separate between firms and persons
- Identify representatives and straatvoogden
- Extract prefix
- Extract last name of first owner
- Flag widows

| Retrieve | String search | 
| -------- | ------------- |
| *Firms* | set personal name | en co, en zn, en zo, comp, bank, firma, fonds, gemeente, lands grond, maatschappij, plantage, respect |
| *Representatives* | door ... qq |
| *Straatvoogd* | als straatvoogd |
| *Prefixes* | last word(s): d', da, de, del, des, de la, d' la, du, l', la, le, ter, van, van van, van, van de, van du, van den, van der, van het, van 't, van la, von |
| *last name* | first word |
| *Flag widows* | weduwe |


### 8. summarise in and out events
| Standardisation	| In event | Out event |
| --------------- | -------- | --------- |
| *Beginning* | Start Series |
| *Ended* | End Series, End Series/Freedom |
| *Transferred* | Acquired (executie), Acquired (publieke veiling), Acquired (vendu), Acquired/Inherited For Freedom, Acquired/Transferred, Exchanged, Inherited, Verpand | Exchanged, Given away, Given away by inheritance, Overgeschreven, Sold, Sold (executie), Sold (publieke veiling), Sold/Given for Freedom |
| *Birth* | Birth |  |
| *Death* |  | Death, Drowned, Killed |
| *Unknown* | Unknown | Unknown |
| *Other* | Remove | Afgeschreven, Diseased, Escaped, Freedom, Remove |


### 9. combine birth information in one variable
- Determine year of birth
- Flag origin of birth information

| Priority	| Type of information | Flag |
| -------- | -------- | ---- |
| *1.* | year of birth mentioned | year_birth |  
| *2.* | birth registered | in_event |
| *3.* | age mentioned | year_birth_age_based |
| *4.* | NA | NA |


### 10. re-order dataset
INSERT TABLES


## matching.R
Contains the matching programs to match certificates between and within series. We matched names of enslaved, mothers, and owners with a maximum Levenshtein distance based on the length of the name. Data is matched in five steps as shown in the table underneath. 

| Step | Between matching | Within matching |
| ---- | ---------------- | --------------- |
| *0. Data selection* | **Filter** </br> - Out event entry 1 is end series *or* event year is last year of series </br> - In event entry 2 is begin series *or* event year is first year of series </br> | **Filter** </br> - Out event entry 1 is a transfer </br> - In event entry 2 is a transfer |
| *1. Retrieval* | **Match entries by** </br> - Name enslaved </br> - Name owner | **Match entries by** </br> - Name enslaved |
| *2. Rule-based filter* | **Select matches if:** </br> - Sex is identical *or* unknown </br> - Name mother matches *or* is unknown </br> - Year of birth is identical *or* unknown </br> *or* names enslaved in preceding and proceeding entries match </br> *or* name enslaved in preceding entry matches and no proceeding event entry 1 | **Select matches if:** </br> - Prevent that entries match themselves </br> - Sex is identical *or* unknown </br> - Name mother matches *or* is unknown </br> - Year of birth is identical *or* unknown </br> - Year of transfer is identical *or* unknown |
| *3. Probabilistic matching* | **Score matching indicators and select highest scoring match per entry:** </br> - Name mother (2.5 pts) </br> - Addendum name mother (1 pt) </br> - Addendum name enslaved (1 pt) </br> - Year of birth (2 pts) </br> - Name enslaved in preceding entry (1 pt) </br> - Name enslaved in proceeding entry (1 pt) </br> - Out event is *end of series* and in event is *beginning of series* (1 pt) | **Score matching indicators and select highest scoring match per entry:** </br> - Name mother (2.5 pts) </br> - Addendum name mother (1 pt) </br> - Addendum name enslaved (1 pt) </br> - Year of birth (2 pt) </br> - Year of transfer (2 pts) </br> - Month of transfer (1 pt) </br> - Day of transfer (1 pt) </br> - Month *and* day of transfer (1 pt) </br> - Name enslaved in preceding entry (1 pt) </br> - Name enslaved in proceeding entry (1 pt) |
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
