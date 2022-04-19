# Slavernijregisters

## clean Preparing_Record_Linkage_dta.R
Cleans the names in the slave registers.

### 1. control entries
- split/combine rows to ensure that each row contains 1 enslaved person
- set starting date to 1830 for series 1

### 2. standardise names enslaved persons
- standardise person descriptions, like numbers into roman numericals and variation on age, colour, or plantation names into one name. 
- split names and person descriptions
 
| Variation | Standardisation	|
| --------- | --------------- |
| 1 | I |
| 2 | II |
| 3 | III |
| 4 | IV | 
| 5 | V |
| no | |
| nr | |
| n | |
| gr | groote |
| groot | groote |
| groote | groote |
| kl | klein |
| kleintje | klein |
| klein |  klein |
| little |  little |
| jr |  junior |
| sr |  senior |
| senior |  senior |
| jong | jonge |
| oud | oude |
| mulat | mulat |
| sw | zwart |
| zw | zwart |



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
