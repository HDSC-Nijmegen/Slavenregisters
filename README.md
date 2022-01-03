# Slavernijregisters

## clean Preparing_Record_Linkage_dta.R
Cleans the names in the slave registers.

### 1. control entries
- remove duplicated entries
- ensure that each row contains 1 enslaved person only
- ensure that each row contains 1 owner only

### 2. standardise names enslaved persons
- standardise descriptions, like roman numericals, plantation names, stature, or 
- split names and person descriptions
INSERT TABLES

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
