# Slavernijregisters

## clean Preparing_Record_Linkage_dta.R
Cleans the names in the slave registers.

### 1. control entries
- split/combine rows to ensure that each row contains 1 enslaved person
- set starting date to 1830 for series 1

### 2. standardise names enslaved persons
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
| *Numerical* | no <br/> nr, n | |
| *Stature* | gr | groote |
| *Stature* | groot | groote |
| *Stature* | kl | klein |
| *Stature* | kleintje | klein |
| *Stature* | littel |  little |
| *Age* | jr |  junior |
| *Age* | sr |  senior |
| *Age* | senior |  senior |
| *Age* | jong | jonge |
| *Age* | oud | oude |
| *Colour* | mulatt | mulat |
| *Colour* | sw | zwart |
| *Colour* | zw | zwart |
| *Plantation* | Res | Resolutie |
| *Plantation* | Resol | Resolutie |
| *Plantation* | Resolutie | Resolutie |
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
| *General* | . | |
| *General* | bij den doop | gedoopt |
| *General* | bij den doop genaamd | gedoopt |
| *General* | bij den doop genmd | gedoopt |
| *General* | / | of |
| *General* | alias | of |
| *General* | bijgenaamd | of |
| *General* | genaamd | of |
| *General* | ook bekend als | of |
| *General* | ook genaamd | of |
| *General* | ook gend | of |



### 3. standardise names mother enslaved persons
- standardise descriptions, like roman numericals, plantation names, stature, or 
- split names and person descriptions
INSERT TABLES

### 4. put owner names in uniform format


### 5. summarise in and out events
INSERT TABLES


### 6. re-order dataset
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
