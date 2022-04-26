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
| *Numerical* | 1 | I |
| *Numerical* | 2 | II |
| *Numerical* | 3 | III |
| *Numerical* | 4 | IV | 
| *Numerical* | 5 | V |
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
| *Colour* | carboeg, karboeg | carboeger |
| *Colour* | creo, creole, creool | creool |
| *Colour* | mulat, mulatt | mulat |
| *Colour* | neger | neger |
| *Colour* | rood, roode | roode |
| *Colour* | sw, swart, swarte, zw | zwart |
| *Occupation* | delver | delver |
| *Occupation* | kuiper | kuiper |
| *Occupation* | offikier | officier |
| *Plantation* | Brouwerslust | Brouwerslust |
| *Plantation* | L en R | Land en Rust |
| *Plantation* | Res, Resol, Resolutie | Resolutie |
| *Plantation* | Standvastigheid | Standvastigheid |
| *Plantation* | Stolk | Stolkwijk |
| *Plantation* | Zeew | Zeewijk |
| *Name* | Alex: | Alexander |
| *Name* | Carol: | Carolina |
| *Name* | Charl: | Charlotte |
| *Name* | Elis: | Elisabeth |
| *Name* | Els: | Elisabeth |
| *Name* | Hendr: | Hendrika |
| *Name* | John / Robn | Johan of Robijn |
| *Name* | Wilhelmina Barth: | Wilhelmina Bartholomina |
| *Name* | Mk | Mc |
| *General* | . - | |
| *General* | bij den doop, bij den doop genaamd, bij den doop genmd | gedoopt |
| *General* | , /, alias, bijgenaamd, genaamd, ook bekend als, ook genaamd, ook genmd | of |



### 3. put owner names in uniform format


### 4. summarise in and out events
| Standardisation	| In event | Out event |
| --------------- | -------- | --------- |
| *Beginning* | Start Series |
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
