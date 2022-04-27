# Slavernijregisters

## clean Preparing_Record_Linkage_dta.R
Cleans the names in the slave registers.

### 1. control entries
- split/combine rows to ensure that each row contains 1 enslaved person
- set starting date to 1830 for series 1

### 2. standardise names enslaved persons & mothers
- standardise person descriptions 
- split names and person descriptions
- write out abbreviated names
 
| Type | Variation | Standardisation	|
| ---- | --------- | --------------- |
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
| *Plantation* | L, L en R, L R, LsR | Land en Rust |
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
| *Other* | NS | NS |
| *Other* | SP | SP |
| *Other* | NS | NS |
| *Other* | VCIP, V C I P | VCIP |
| *Other* | ZB, Z B | ZB |



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
| *General* | . - | |
| *General* | bij den doop, bij den doop genaamd, bij den doop genmd | gedoopt |
| *General* | , /, alias, bijgenaamd, bijgent, genaamd, ook, ook bekend als, ook genaamd, ook genmd | of |



### 3. put private owner names in uniform format
Data was manually cleaned to ensure that:
| Action | Format	| Example |
| ------ | ------ | ------- |
| Mark straatvoogd | OWNER, als straatvoogd | Moron J, als straatvoogd |
| Mark representatitives | OWNER, door ... qq | Berner J P boedel, door S F Flu qq |
| Mark husband nom ux as representative | OWNER, door ... qq | Betting C D nom ux, door C D Betting qq |
| Start with last name owner | LAST NAME FIRST NAME PREFIX | Berner J P |
| Prefixes are moved towards the end of the entry | Parra Josua de la |
| Mark last names with multiple words | LAST NAME _ LASTNAME | Behm_Wendholdt J B L ten |
| Mark last names of formerly enslaved partly as prefixes | | Beeldsnijder Charles van Anna van Betje van |
| Remove redundant "de" and "van" from entries | | Thol geb. Hendriks *de* weduwe *van* M. C. C. van | 
| Mark "van" in maiden names with underscore | | Krebs geboren van_Nerevalk C E |
| Split references to new owner situation in title with a dash | | Mesquita Jh. Ab. Bo. de - thans Weduwe Jh. Ab. Bo. de Mesquita |



### 4. summarise in and out events
| Standardisation	| In event | Out event |
| --------------- | -------- | --------- |
| *Beginning* | Start Series |
| *Ended* | End Series, End Series/Freedom |
| *Transferred* | Acquired (executie), Acquired (publieke veiling), Acquired (vendu), Acquired/Inherited For Freedom, Acquired/Transferred, Exchanged, Inherited, Verpand | Exchanged, Given away, Given away by inheritance, Overgeschreven, Sold, Sold (executie), Sold (publieke veiling), Sold/Given for Freedom |
| *Birth* | Birth |  |
| *Death* |  | Death, Drowned, Killed |
| *Unknown* | Unknown | Unknown |
| *Other* | Remove | Afgeschreven, Diseased, Escaped, Freedom, Remove |

### 5. re-order dataset
INSERT TABLES


## Between Series.R
Matches certificates within series 4. Data is matched in five steps:

### 1. retrieval
- match names ego + owner on Levenshtein distance 3

### 2. filter
INSERT TABLES

### 3. probabilistic matching
INSERT TABLES

### 4. present meta-data
- source_order
- year in + year out
- event in + event out
- Levenshtein distance names ego
- Levenshtein distance names mother
- name + number ego
- name + number mother
- year of birth
