
##########################################################
# MATCH BETWEEN END OF SERIE 4 AND EMANCIPATION REGISTER #
##########################################################

#load packages
library("data.table")
library("dplyr")
library("stringdist")
library("openxlsx")
library("readxl")
library("tidyr")
library("stringr")
library("data.table")

#clean environment
rm(list=ls())

#set working directory
#Home
#setwd("~/HDS/Emancipatieregisters")
#Work
setwd("//cnas.ru.nl/U709207/Documents/Suriname/Emancipatieregisters")


#open datasets
df_emanc <- read.csv("Emancipatieregister_cleaned.csv") #Cleaned emancipation register
df_sr <- read.csv("Serie4_Exit.csv") #Cleaned Serie4
sex <- read_excel("Sekse naar naam - slaafgemaakten.xlsx") #File with sex according to first name originally derived from slave registers

################################
# 1 Set Levensthein distance   #
################################

#set max lev dist for matching procedure
set_lv_dist <- 2
lev_dist_naam <- 2
lev_dist_eigenaar <- 3

##########################################################
# 2 Rename columns and add sex to emancipation register  #
##########################################################

df1 <- df_emanc %>% rename(Naam = Name) %>% arrange(Naam)
df2 <- df_sr %>% arrange(Naam)  %>%
  rename(Eigenaar_2 = Eigenaar_Last_name) %>%
  select(source_order, Naam, Naam_number, year_birth, plantation_name, Place_name1_match, Eigenaar_2, sex) 


#Add sex based on name to emancipation registers to later exclude matches between males and females
df1 <- left_join(df1, sex, by = "Naam") %>%
  mutate(sex = replace(sex, is.na(sex), "unknown")) %>%
  rename(sex_emanc = sex,
         Eigenaar_1 = Eigenaar_Lastname) %>%
  mutate(Eigenaar_1 = tolower(Eigenaar_1)) %>%
  select(Id_person, Naam, Naam_number, B_year, B_year2, Place_name1, plantation_match, Eigenaar_1, sex_emanc) %>%
  rename(source_order = Id_person)



#standardise names
df1$Naam_original <- df1$Naam
df1$Naam <- tolower(df1$Naam)
df1$Naam <- gsub(" of ", " ", df1$Naam)
df1$Naam <- gsub("ç", "c", df1$Naam)
df1$Naam <- gsub("é", "e", df1$Naam)
df1$Naam <- gsub("kw", "qu", df1$Naam)
df1$Naam <- gsub("ph", "f", df1$Naam)


df2$Naam_original <- df2$Naam
df2$Naam <- tolower(df2$Naam)
df2$Naam <- gsub(" of ", " ", df2$Naam)
df2$Naam <- gsub("ç", "c", df2$Naam)
df2$Naam <- gsub("é", "e", df2$Naam)
df2$Naam <- gsub("kw", "qu", df2$Naam)
df2$Naam <- gsub("ph", "f", df2$Naam)


#split name
source("split_names.R")
df1 <- split_names(df1, "Naam")
df2 <- split_names(df2, "Naam")


#remove white spaces
df1$Naam <- gsub(" ", "", df1$Naam)
df2$Naam <- gsub(" ", "", df2$Naam)

df1 <- df1 %>%
  rename(Id_person = source_order)

########################################################################################################################
# 3 Load the !customized! between_matching function earlier applied to the slave registers (saved in separate script)  #
########################################################################################################################

source("between_matching_emancipation.R")

##############################################################################################
# 4 Apply matching algorithm to matching between slave registers and emancipation registers  #
##############################################################################################

list1 <- match_between_emancipation(df1, df2, lev_dist_naam=set_lv_dist)


########################################################################################################################################
# 5 Run the same algorithm but now replace plantation names for specific slave groups that were apparently rented to other plantations #
########################################################################################################################################

df_not_matched_ER <- list1[[2]] %>%
  mutate(plantation_match = replace(plantation_match, plantation_match ==1, 0)) %>%
  rename(Naam = Naam_1,
         Naam_number = Naam_number_1) %>%
  select(Id_person, Naam, Naam_number, B_year, B_year2, Place_name1, plantation_match, Eigenaar_1, sex_emanc)

df_not_matched_SR <- list1[[3]] %>%
  rename(Naam = Naam_2,
         Naam_number = Naam_number_2) %>%
  select(source_order, Naam, Naam_number, year_birth, plantation_name, Place_name1_match, Eigenaar_2, sex)

list2 <- match_between_emancipation(df_not_matched_ER, df_not_matched_SR, lev_dist_naam=set_lv_dist)



#################################
# 6 Generate final data set     #
#################################

#Append unique matches
unique1 <- list1[[4]] %>%
  select(source_order, Id_person, Naam_lv, Match_score)

unique2 <- list2[[4]]  %>%
  select(source_order, Id_person, Naam_lv, Match_score)

unique_bind <- bind_rows(unique1, unique2)
rm(unique1, unique2)

#Append identical matches
ident1 <- list1[[5]] %>%
  select(source_order, Id_person, Naam_lv, Match_score)

ident2 <- list2[[5]]  %>%
  select(source_order, Id_person, Naam_lv, Match_score)

ident_bind <- bind_rows(ident1, ident2) %>%
  distinct(source_order, Id_person, .keep_all = TRUE)
rm(ident1, ident2)

#Remove duplicates from non-matched entries
df_not_matched_ER <- list2[[2]] %>%
  distinct(source_order, Id_person, Naam_lv, Match_score)

df_not_matched_SR <- list2[[3]] %>%
  distinct(source_order, Id_person, Naam_lv, Match_score)


final_data <- function(df1){

  df_final <- left_join(df1, df_emanc) %>%
    select(Id_person, source_order, Voornamen, Naam_Family, Naam.voor.1863, Naam_number, Extrainformatiebijnaam, Doopnaam, B_day, B_month, B_year, B_year2, Verwantschap.en.Erkenning, occupation, general_remarks,
           Place_name1, Eigenaar, Naam_lv, Match_score) %>%
    arrange(Id_person) %>%
    mutate(Naam_Family = str_to_title(Naam_Family),
           StartEntryYear = 1863,
           StartEntryMonth = 7,
           StartEntryDay = 1) %>%
    rename(source_order_SR = source_order,
           Plantation = Place_name1,
           Owner = Eigenaar)
  
  df_final
}

df_unique_final <- final_data(unique_bind)

df_ident_final <- final_data(ident_bind) %>% 
  mutate (identity_flag =1)
  
df_unmatched_final <- final_data(df_not_matched_ER) %>%
  mutate(unmatched_flag =1)

df_final <- bind_rows(df_unique_final, df_ident_final, df_unmatched_final) %>%
  mutate(identity_flag = replace(identity_flag, is.na(identity_flag), 0),
         unmatched_flag = replace(unmatched_flag, is.na(unmatched_flag), 0)) %>%
  arrange(Id_person) %>%
  select(-StartEntryYear, -StartEntryMonth, -StartEntryDay) %>%
  rename (Slave_name = Naam.voor.1863,
          First_name = Voornamen,
          Family_name = Naam_Family,
          Baptized_name = Doopnaam,
          Family_relations = Verwantschap.en.Erkenning) %>%
  distinct(Id_person, source_order_SR, .keep_all = TRUE) %>%
  group_by(Id_person) %>%
  mutate(volgnr = row_number()) %>%
  mutate(volgnr = replace(volgnr, volgnr ==1, "source_order_SR_1"),
         volgnr = replace(volgnr, volgnr ==2, "source_order_SR_2"),
         volgnr = replace(volgnr, volgnr ==3, "source_order_SR_3")) %>%
  pivot_wider(names_from = volgnr, values_from = source_order_SR) %>%
  relocate(source_order_SR_1, source_order_SR_2, source_order_SR_3, .after = Id_person)

write.csv(df_final, file = "Emancipation_Register_Linked.csv", row.names = FALSE)


df_compare <- df_final %>%
  left_join(df_sr, by = c("source_order_SR_1" = "source_order")) %>%
  select(source_order_SR_1, Id_person, Naam_lv, Match_score, Slave_name, Naam, B_year, B_year2, year_birth, Plantation, plantation_name, Owner, Eigenaar, Naam_number.x, Naam_number.y, Extrainformatiebijnaam.x, Extrainformatiebijnaam.y, B_day, B_month,
         day_birth, month_birth) #%>%
  #filter(Match_score < 3 | Naam_lv > 0)


####################################################
# Add-on 1: Have a closer look at identicals #########
####################################################


emanc_match <- df_emanc %>%
  filter(volgnr ==1)

sr_match <- df_sr %>%
  filter(volgnr ==1)

emanc_double <- emanc_match %>% 
  group_by(Name, B_year, Place_name1) %>%
  filter(n() >1,
        plantation_match ==1,
        Name != "(.)") %>%
  arrange(Name, Place_name1, B_year, B_year2) %>%
  select(Id_person, Name, Place_name1, B_year, B_year2, Verwantschap.en.Erkenning)


sr_double <- sr_match %>% 
  group_by(Naam, year_birth, plantation_name) %>%
  filter(n() >1,
         Place_name1_match ==1,
         Naam != "") %>%
  arrange(Naam, plantation_name, year_birth) %>%
  ungroup() %>%
  select(source_order, Naam_original, plantation_name, year_birth, Moeder, Moeder_number)



#################################################################
# Add-on 2: Have a closer look at unmatched individuals #########
#################################################################

unmatched_SR <- list2[[3]] %>%
  distinct(source_order, .keep_all = T)


plantation_ER <- as.data.frame(table(list2[[2]]$Place_name1)) %>%
  arrange(-Freq) %>%
  filter(Freq >= 10)

plantation_SR <- as.data.frame(table(list2[[3]]$plantation_name)) %>%
  arrange(-Freq) %>%
  filter(Freq >= 10)

write.csv(plantation_ER, file = "Plantation_Unmatched_ER.csv", row.names = FALSE)
write.csv(plantation_SR, file = "Plantation_Unmatched_SR.csv", row.names = FALSE)


owner_ER <- list2[[2]] %>%
  distinct(source_order, Id_person) %>%
  left_join(emanc_match) %>%
  filter(plantation_match == 0,
         !(is.na(Eigenaar_Lastname))) %>%
  group_by(Eigenaar_Lastname) %>%
  summarize(Freq = n()) %>%
  arrange(-Freq) %>%
  filter(Freq >= 3)
  

owner_SR <- list2[[3]] %>%
  distinct(source_order, Id_person) %>%
  left_join(sr_match) %>%
  filter(Place_name1_match == 0,
         !(is.na(Eigenaar_Last_name))) %>%
  group_by(Eigenaar_Last_name) %>%
  summarize(Freq = n()) %>%
  arrange(-Freq) %>%
  filter(Freq >= 3)

write.csv(owner_ER, file = "Particulier_Unmatched_ER.csv", row.names = FALSE)
write.csv(owner_SR, file = "Particulier_Unmatched_SR.csv", row.names = FALSE)

###############################################
# 7 Append to Reconstituted slave registers   #
###############################################

### REWORK THIS SECTION LATER

sl_recon <-  read_excel("//cnas.ru.nl/U709207/Documents/HDS and Curacao/Emancipatieregisters/2022-08-01SR life courses.xlsx") %>%
  rename(Name = Name_enslaved,
         Name_extra = Name_enslaved_extra) %>%
  group_by(Id_person) %>%
  mutate(Id_match = cur_group_id()) %>%
  mutate(Owner = NA,
         Plantation = NA) %>%
  mutate(Owner = replace(Owner, Source_type == "Particulieren", Name_owner),
         Plantation = replace(Plantation, Source_type == "Plantages", Name_owner))


df_bjoern <- bind_rows(sl_recon, df_unique_final) %>%
  group_by(Id_source) %>% 
  mutate(Id_match = min(Id_match, na.rm = TRUE)) %>%
  arrange(Id_match, StartEntryYear, StartEntryMonth, StartEntryDay) %>%
  group_by(Id_match) %>% ##Remove following rows once Rick solved the bug
  mutate(Id_match2 = cur_group_id()) %>%
  ungroup() %>%
  select(-Id_match, -Name_owner, -B_month, -B_day, -B_year_min, -B_year_max) %>%
  rename(Id_match = Id_match2) %>%
  relocate (Id_match, Id_person, Id_source, Id_match, Name, Naam_number, Name_extra, Sex, B_year, B_year2, Plantation, Owner, Name_mother, Name_mother_extra) %>%
  mutate(Id_source = replace(Id_source, StartEntryYear == 1863 , NA)) %>%
  distinct(Id_source, Id_match, .keep_all = TRUE) %>%
  rename(Name_baptized_ER = Doopnaam,
         First_name_ER = Voornamen,
         Family_name_ER = Naam_Family,
         Relation_ER = Verwantschap.en.Erkenning,
         Name_number= Naam_number)
  

write.csv(df_bjoern, file = "File_for_Bjoern.csv", row.names = FALSE)







