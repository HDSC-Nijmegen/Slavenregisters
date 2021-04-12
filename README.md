# Slavernijregisters

## Within_particulieren.R
Matches certificates within series 4. Data is matched in five steps:

### 1. split names and numbers
- ego
- mother

### 2. filter incomplete records
- UNEQUALS "begging" & "ended"

### 3. retrieval:
- match names ego + mother on Levenshtein distance 2

### 4. present meta-data
- source_order
- year in + year out
- event in + event out
- Levenshtein distance names ego
- Levenshtein distance names mother
- name + number ego
- name + number mother
- year of birth

### 5. precision
- none



## Between_particulieren.R
Matches certificates within series 4. Data is matched in three steps:

### 1. split names and numbers
- ego
- mother

### 2. filter incomplete records
- UNEQUALS "begging" & "ended"

### 3. retrieval:
- match names ego + mother + owner on Levenshtein distance 2

### 4. present meta-data
- source_order
- Levenshtein distance names ego
- Levenshtein distance names mother
- Levenshtein distance names owner
- name + number ego
- name + number mother
- name owner
- year of birth
- name previous entry
- name next entry

### 5. precision
- none