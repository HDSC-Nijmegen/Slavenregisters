

######################## From long to wide format ##############################
#load packages
library("data.table")
library("dplyr")
library("stringdist")
library("openxlsx")

#clean environment
rm(list=ls())



#######################################################
#### section 5: make excerpt in wide format series ####
#######################################################
library(tidyr)

# Open data set in long format
#df <- fread("C:/Users/Matth/OneDrive/Desktop/Radboud/Matching HDSC/2022-06-30SR life courses.txt", encoding="UTF-8")
df <- read.delim("//cnas.ru.nl/U709207/Documents/HDS and Curacao/Matching/2022-06-30SR life courses.txt", encoding="UTF-8")

# Only keep variables that we want to include in wide data set and create wide format with pivot_wider
df_wide_sel <- df %>% select(Id_person, Id_source, Name_enslaved, B_year, StartEntryYear, LastEntryYear, Source_series) %>%
  group_by(Id_person) %>% mutate(id = row_number()) %>% pivot_wider(names_from = id, values_from = c(Name_enslaved, Id_source, B_year, StartEntryYear, LastEntryYear, Source_series )) 

# Create separate file to create the flags for the four series
df_wide_flags <- df %>% select(Id_person, Source_series, StartEntryYear) %>% 
  group_by(Id_person, Source_series) %>% 
  mutate(id = row_number()) %>% 
  filter(id == 1) %>% 
  select(-id) %>%
  pivot_wider(names_from = c(Source_series), values_from = StartEntryYear) %>% 
  rename("Serie1" ="1830-1838",
         "Serie2" ="1838-1848",
         "Serie3" ="1848-1851",
         "Serie4" ="1851-1863") %>%
  group_by(Id_person) %>%
  summarise(Serie1 = ifelse(is.na(Serie1), 0, 1),
            Serie2 = ifelse(is.na(Serie2), 0, 1),
            Serie3 = ifelse(is.na(Serie3), 0, 1),
            Serie4 = ifelse(is.na(Serie4), 0, 1))

# Merge both files and re-arrange columns
df_wide <- left_join(df_wide_sel, df_wide_flags) %>%
  relocate(c(Serie1, Serie2, Serie3, Serie4,ends_with("1"), ends_with("2"), ends_with("3"), ends_with("4"),
             ends_with("5"), ends_with("6"), ends_with("7"),ends_with("8")), .after = Id_person)
  









