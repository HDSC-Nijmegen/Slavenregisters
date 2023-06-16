
#load packages
library("data.table")
library("dplyr")
library("stringdist")
library("openxlsx")
library("tidyr")
library("stringr")

#clean environment
rm(list=ls())

########################MATCHING RECORDS SLAVE REGISTERS 4#######################
####  This script matches records of the enslaved BETWEEN and WITHIN series  ####
####                                                                         ####
####  We use 3 conditions to establish matches BETWEEN series:               ####
####    1. Levenshtein distance name enslaved                                ####
####    2. Levenshtein distance name owner                                   ####
####    3. Levenshtein distance name mother                                  ####
####  Further filtering is done based on the:                                ####
####    1. Sex                                                               ####
####    2. Date of birth OR                                                  ####
####       Name enslaved in the preceding and following registration         ####
####  Matches are scored and selected probabilistically                      ####
####  Ties are deleted                                                       ####
####                                                                         ####
####  We use 2 conditions to establish matches WITHIN series:                ####
####    1. Levenshtein distance name enslaved                                ####
####    2. Levenshtein distance name mother                                  ####
####  Further filtering is done based on the:                                ####
####    1. Sex                                                               ####
####    2. Year of birth                                                     ####
####    3. Year of transfer                                                  ####
####  Matches are scored and selected probabilistically                      ####
####  Ties are deleted                                                       ####
#################################################################################

##################################
#### !!!! SET PARAMETERS !!!! ####
##################################

#WORK
#set working directory for data
wd_ER <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname Emancipatieregister/Emancipatieregisters - Cleaning and Matching"
wd_namenlijst <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten"
wd_SR <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/"
#set working directory for scripts
wd_scripts <- "U:/Surfdrive/GitHub/Slavenregisters/Matching Suriname/"

#HOME
#set working directory for data
#wd_ER <- "C:/Users/Matth/surfdrive3/Emancipatieregisters - Cleaning and Matching"
#wd_namenlijst <- "C:/Users/Matth/surfdrive2/Namenlijsten"
#wd_SR <- "C:/Users/Matth/surfdrive2/Matching/"
#set working directory for scripts
#wd_scripts <- "C:/Users/Matth/surfdrive4/Slavenregisters/Matching Suriname/"





#set max lev dist for matching procedure
set_lv_dist <- 3

#set threshold for filtering during reconstitution
threshold34 <- 1
threshold23 <- 0
threshold12 <- 0
threshold24 <- 0
threshold14 <- 0
threshold13 <- 0
threshold44 <- 7
threshold33 <- 7
threshold22 <- 8
threshold11 <- 8

#load scripts
source(paste(wd_scripts, "within matching.R", sep=""))
source(paste(wd_scripts, "between matching.R", sep=""))
source(paste(wd_scripts, "between matching emancipation.R", sep=""))
source(paste(wd_scripts, "split_names.R", sep=""))


######################
#### open dataset ####
######################

#open Emancipation Register
setwd(wd_ER)
ER <- fread("Emancipatieregister_cleaned.csv") %>% rename(Naam = Name) %>% arrange(Naam)
#add sex
sex <- read.xlsx(paste(wd_namenlijst, "Sekse naar naam - slaafgemaakten.xlsx", sep="/") ) #File with sex according to first name originally derived from slave registers
ER <- left_join(ER, sex, by = "Naam") %>%
  mutate(sex = replace(sex, is.na(sex), "unknown")) %>%
  rename(sex_emanc = sex,
         Eigenaar_1 = Eigenaar_Lastname) %>%
  mutate(Eigenaar_1 = tolower(Eigenaar_1)) %>%
  rename(source_order = Id_person)

#open Slave Register
setwd(wd_SR)
SR <- fread("Cleaned Registry/cleaned slave register 2023-05-22.txt", encoding="UTF-8") 


######################################
#### section 0: standardise names ####
######################################

#select name owner & year of birth SR
SR$Eigenaar_original <- SR$Eigenaar
SR$Eigenaar <- ifelse(SR$Typeregister=="Plantages", SR$plantation_name, SR$Eigenaar_Last_name)
SR$year_birth_original <- SR$year_birth
SR$year_birth <- SR$year_birth2

#standardise names ER
ER$Naam_original <- ER$Naam
ER$Naam <- tolower(ER$Naam)
ER$Naam <- gsub(" of ", " ", ER$Naam)
ER$Naam <- gsub("ç", "c", ER$Naam)
ER$Naam <- gsub("é", "e", ER$Naam)
ER$Naam <- gsub("kw", "qu", ER$Naam)
ER$Naam <- gsub("ph", "f", ER$Naam)

#standardise names SR
SR$Naam_original <- SR$Naam
SR$Naam <- tolower(SR$Naam)
SR$Naam <- gsub(" of ", " ", SR$Naam)
SR$Naam <- gsub("ç", "c", SR$Naam)
SR$Naam <- gsub("é", "e", SR$Naam)
SR$Naam <- gsub("kw", "qu", SR$Naam)
SR$Naam <- gsub("ph", "f", SR$Naam)

#standardise moeder SR
SR$Moeder_original <- SR$Moeder
SR$Moeder <- tolower(SR$Moeder)
SR$Moeder <- gsub(" of ", " ", SR$Moeder)
SR$Moeder <- gsub("ç", "c", SR$Moeder)
SR$Moeder <- gsub("é", "e", SR$Moeder)
SR$Moeder <- gsub("kw", "qu", SR$Moeder)
SR$Moeder <- gsub("ph", "f", SR$Moeder)

#split naam
ER <- split_names(ER, "Naam")
SR <- split_names(SR, "Naam")
SR <- split_names(SR, "Moeder")

#remove white spaces
ER$Naam <- gsub(" ", "", ER$Naam)
SR$Naam <- gsub(" ", "", SR$Naam)
SR$Moeder <- gsub(" ", "", SR$Moeder)


###################################################
#### section 1a: retrieve matches SR-ER series ####
###################################################

#matches serie 4 & ER
#select SR serie 4
Serie4 <- SR %>% filter (Serieregister == "1851-1863" & out_event == "End Series/Freedom")
#edit reconstructed shifts in plantation names/plantation owners
Serie4 <- Serie4 %>% mutate(plantation_name = replace(plantation_name, plantation_name =="De Eendragt", "Eendragt"),
                            plantation_name = replace(plantation_name, plantation_name =="Alkmaar ( voor het 1/2 aandeel aankomende den Boedel A. Ferrier )", "Alkmaar"),
                            plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot", "Concordia"),
                            plantation_name = replace(plantation_name, plantation_name =="Johanna Charlotte (1/2 aandeel mevrouw G. C. Henkel geboren Vogt)", "Rustenburg"),
                            plantation_name = replace(plantation_name, plantation_name =="Waterwijk", "Hazard boven Commewijne"),
                            plantation_name = replace(plantation_name, plantation_name =="Harmonie", "l'Aventure"),
                            plantation_name = replace(plantation_name, plantation_name =="Lotland No 34", "Crappahoek nr. 34"),
                            plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot L. L.", "Concordia"))

#make a dummy for matching plantation names between ER and SR Serie 4  
#make Place_name1_match
emanc <- ER %>% 
  select(Place_name1) %>% 
  distinct() %>%
  filter(Place_name1 != "") %>%
  mutate(Place_name1_match = 1)
#add information to Serie4
Serie4 <- left_join(Serie4, emanc, by = c("plantation_name" = "Place_name1")) %>% 
  mutate(Place_name1_match = ifelse(is.na(Place_name1_match), 0, 1))

#match SR to ER
Serie4 <- Serie4 %>% arrange(Naam)  %>%
  rename(Eigenaar_2 = Eigenaar_Last_name) %>%
  select(source_order, Naam, Naam_number, year_birth, plantation_name, Place_name1_match, Eigenaar_2, sex) 
#rename ER source_order to id_person
ER <- ER %>%
  rename(Id_person = source_order)
#match using the match_between_emancipation function loaded earlier
list1 <- match_between_emancipation(ER, Serie4, lev_dist_naam=set_lv_dist, lev_dist_eigenaar=set_lv_dist)

#run the same function for the so far unmatched records but now also allow for potential matches between
#plantations and private owners

#prepare the so far unmatched ER-records for the re-run and select relevant variables
df_not_matched_ER <- list1[[2]] %>%
  mutate(plantation_match = replace(plantation_match, plantation_match ==1, 0)) %>%
  rename(Naam = Naam_1,
         Naam_number = Naam_number_1) %>%
  select(Id_person, Naam, Naam_number, B_year, B_year2, Place_name1, plantation_match, Eigenaar_1, sex_emanc)
#prepare the so far unmatched SR-Serie4-records for the re-run and select relevant variables
df_not_matched_SR <- list1[[3]] %>%
  rename(Naam = Naam_2,
         Naam_number = Naam_number_2) %>%
  select(source_order, Naam, Naam_number, year_birth, plantation_name, Place_name1_match, Eigenaar_2, sex)
#match the so far unmatched records 
list2 <- match_between_emancipation(df_not_matched_ER, df_not_matched_SR, lev_dist_naam=set_lv_dist, lev_dist_eigenaar=set_lv_dist)


#retrieve reliable matches from list1 and list2
matches <- bind_rows(list1[[4]], list2[[4]]) %>%
  select(source_order, Id_person, Naam_lv, Match_score)

#retrieve identical matches from list1 and list2 (matches that have more than one possible match)
ident_matches <- bind_rows(list1[[5]], list2[[5]]) %>%
  select(source_order, Id_person, Naam_lv, Match_score) %>%
  distinct(source_order, Id_person, .keep_all = TRUE)

#retrieve non-matched entries from list1 and list2, by ER and SR
unmatched_ER <- list2[[2]] %>%
  distinct(source_order, Id_person, Naam_lv, Match_score)
unmatched_SR <- list2[[3]] %>%
  distinct(source_order, Id_person, Naam_lv, Match_score)

ER_basic <- ER %>%
  distinct(Id_person, .keep_all = T)

#run a function that prepares the retrieved datasets for final output and for linkage with the SR
prepare_final_data <- function(df1){
  
  df_final <- left_join(df1, ER_basic) %>%
    select(Id_person, source_order, Voornamen, Naam_Family, "Naam voor 1863", Naam_number, Extrainformatiebijnaam, Doopnaam, B_day, B_month, B_year, B_year2, "Verwantschap en Erkenning", occupation, general_remarks,
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
#reliably matched records
df_matches <- prepare_final_data(matches)
#identical records; add dummy indicating identical match record
df_identical <- prepare_final_data(ident_matches) %>% 
  mutate (identity_flag =1)
#unmatched records (ER); add dummy indicating unmatched record
df_unmatched <- prepare_final_data(unmatched_ER) %>%
  mutate(unmatched_flag =1)

#generate final dataset by binding the three retrieved datasets together, by arranging the data according to identifier
#by preparing the variables and their names, by making a long format for each record in ER, and by creating an variable
#about the matching status ("matching_status_ER")
ER_final <- bind_rows(df_matches, df_identical, df_unmatched) %>%
  mutate(identity_flag = replace(identity_flag, is.na(identity_flag), 0),
         unmatched_flag = replace(unmatched_flag, is.na(unmatched_flag), 0)) %>%
  arrange(Id_person) %>%
  rename (Slave_name = "Naam voor 1863",
          First_name = Voornamen,
          Family_name = Naam_Family,
          Baptized_name = Doopnaam,
          Family_relations = "Verwantschap en Erkenning") %>%
  distinct(Id_person, source_order_SR, .keep_all = TRUE) %>%
  group_by(Id_person) %>%
  mutate(volgnr = row_number()) %>%
  mutate(volgnr = replace(volgnr, volgnr ==1, "source_order_SR_1"),
         volgnr = replace(volgnr, volgnr ==2, "source_order_SR_2"),
         volgnr = replace(volgnr, volgnr ==3, "source_order_SR_3")) %>%
  pivot_wider(names_from = volgnr, values_from = source_order_SR) %>%
  relocate(source_order_SR_1, source_order_SR_2, source_order_SR_3, .after = Id_person) %>%
  mutate(matching_status_ER = "matched") %>%
  mutate(matching_status_ER = replace(matching_status_ER, unmatched_flag==1, "unmatched"),
         matching_status_ER = replace(matching_status_ER, identity_flag==1, "more than one match")) %>%
  select(-identity_flag, -unmatched_flag)

#remove unnecessary data frames
rm(list1, list2, df_identical, df_matches, df_not_matched_ER, df_not_matched_SR, 
   df_unmatched, ident_matches, unmatched_ER, unmatched_SR, matches, emanc, ER_basic)



#####################################################
#### section 1b: retrieve matches BETWEEN series ####
#####################################################

#match serie 3 & 4
#select series
Serie3 <- SR[which(SR$Serieregister_nr==3 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie4 <- SR[which(SR$Serieregister_nr==4 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie34 <- match_between(Serie3, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=3, NUMMER2=4)

#match serie 2 & 3
#select series
Serie2 <- SR[which(SR$Serieregister_nr==2 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie3 <- SR[which(SR$Serieregister_nr==3 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie23 <- match_between(Serie2, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=3)

#match serie 1 & 2
#select series
Serie1 <- SR[which(SR$Serieregister_nr==1 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie2 <- SR[which(SR$Serieregister_nr==2 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie12 <- match_between(Serie1, Serie2, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=2)

#match serie 2 & 4
#select series
Serie2 <- SR[which(SR$Serieregister_nr==2 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie4 <- SR[which(SR$Serieregister_nr==4 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie24 <- match_between(Serie2, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=4)

#match serie 1 & 4
#select series
Serie1 <- SR[which(SR$Serieregister_nr==1 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie4 <- SR[which(SR$Serieregister_nr==4 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie14 <- match_between(Serie1, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=4)

#match serie 1 & 3
#select series
Serie1 <- SR[which(SR$Serieregister_nr==1 & out_event == "End Series"), c("source_order", "Typeregister", "out_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
Serie3 <- SR[which(SR$Serieregister_nr==3 & in_event == "Start Series"), c("source_order", "Typeregister", "in_event", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
#match series
Serie13 <- match_between(Serie1, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=3)


####################################################
#### section 1c: retrieve matches WITHIN series ####
####################################################

Serie4 <- SR[which(SR$Serieregister_nr==4), c("source_order", 
                                              "in_event_general", "out_event_general",
                                              "Naam", "Naam_number", 
                                              "Moeder", "Moeder_number", 
                                              "Eigenaar",
                                              "year_birth", "month_birth", "day_birth",
                                              "year_entry", "month_entry", "day_entry",
                                              "year_exit", "month_exit", "day_exit",
                                              "sex")]
Serie4 <- Serie4[which(Serie4$out_event_general=="Transferred" | Serie4$in_event_general=="Transferred"),]
Serie3 <- SR[which(SR$Serieregister_nr==3 ), c("source_order", 
                                               "in_event_general", "out_event_general",
                                               "Naam", "Naam_number", 
                                               "Moeder", "Moeder_number", 
                                               "Eigenaar",
                                               "year_birth", "month_birth", "day_birth",
                                               "year_entry", "month_entry", "day_entry",
                                               "year_exit", "month_exit", "day_exit",
                                               "sex")]
Serie3 <- Serie3[which(Serie3$out_event_general=="Transferred" | Serie3$in_event_general=="Transferred"),]
Serie2 <- SR[SR$Serieregister_nr==2, c("source_order", 
                                       "in_event_general", "out_event_general",
                                       "Naam", "Naam_number", 
                                       "Moeder", "Moeder_number", 
                                       "Eigenaar",
                                       "year_birth", "month_birth", "day_birth",
                                       "year_entry", "month_entry", "day_entry",
                                       "year_exit", "month_exit", "day_exit",
                                       "sex")]
Serie2 <- Serie2[which(Serie2$out_event_general=="Transferred" | Serie2$in_event_general=="Transferred"),]
Serie1 <- SR[SR$Serieregister_nr==1, c("source_order", 
                                       "in_event_general", "out_event_general",
                                       "Naam", "Naam_number", 
                                       "Moeder", "Moeder_number", 
                                       "Eigenaar",
                                       "year_birth", "month_birth", "day_birth",
                                       "year_entry", "month_entry", "day_entry",
                                       "year_exit", "month_exit", "day_exit",
                                       "sex")]
Serie1 <- Serie1[which(Serie1$out_event_general=="Transferred" | Serie1$in_event_general=="Transferred"),]

Serie44 <- match_within(Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=4)
Serie33 <- match_within(Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=3)
Serie22 <- match_within(Serie2, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2)
Serie11 <- match_within(Serie1, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1)


##############################################################
#### section 2: load program to reconstruct life courses  ####
##############################################################

source(paste(wd_scripts, "filtering + closure.R", sep=""))

test <- reconstitution %>%
  pivot_longer(cols = starts_with("Source"),names_to = "Name", values_to = "Value") %>%
  filter(!(is.na(Value))) %>%
  group_by(Value) %>%
  filter(n() >1) %>%
  arrange(Value)

##############################################################
### section 3: assign grouped certificates same id_person ####
##############################################################

#generate SR id 
reconstitution$Id_person <- paste("SR", sprintf("%06d", 1:length(reconstitution$Source_order_1)), sep="-")
reconstitution <- melt(setDT(reconstitution), id.vars = c("Id_person"), value.name = "source_order")
reconstitution <- reconstitution[!is.na(reconstitution$source_order), c("Id_person", "source_order")]


reconstitution <- reconstitution %>%
  group_by(source_order) %>%
  mutate(n = n()) %>% 
  group_by(Id_person) %>%
  mutate(test = ifelse(n == 2, source_order, NA)) %>%
  mutate(Id2  = first(na.omit(test)),) %>%
  mutate(Id_new = ifelse(!(is.na(Id2)), Id2, Id_person)) %>%
  group_by(Id_new) %>%
  mutate(Id_nearly = cur_group_id()) %>%
  ungroup() %>%
  select(source_order, Id_nearly) %>%
  distinct() 

reconstitution <- reconstitution %>%
  group_by(Id_nearly) %>%
  mutate(id = row_number()) %>%
  pivot_wider(values_from = source_order, names_from = id) 

reconstitution$Id_person <- paste("SR", sprintf("%06d", 1:length(reconstitution$Id_nearly)), sep="-")
reconstitution$Id_nearly <- NULL
reconstitution <- melt(setDT(reconstitution), id.vars = c("Id_person"), value.name = "source_order")
reconstitution <- reconstitution[!is.na(reconstitution$source_order), c("Id_person", "source_order")]





#add unlinkable records
x <- SR[!duplicated(SR$source_order) & !is.na(SR$source_order),]
x <- x[!(x$source_order %in% reconstitution$source_order),]
x$Id_person <- paste("SR", sprintf("%06d", (as.numeric(max(substr(reconstitution$Id_person,4,9)))+1) : (as.numeric(max(substr(reconstitution$Id_person,4,9)))+length(x$source_order))), sep="-")
x <- x[,c("Id_person", "source_order")]
reconstitution <- rbind(reconstitution, x)

#collapse overlapping records
#filter overlapping entries
x <- reconstitution %>% group_by(source_order) %>% filter(n()>1) %>% mutate(nummer=row_number()) %>% ungroup()
#make new SR id
x1 <- x[!duplicated(x$source_order),] %>% arrange(Id_person)
x1$Id_person2 <- paste("SR-c", sprintf("%05d", 1:length(x1$Id_person)), sep="")
x <- merge(x, x1, by="source_order", all=F)
x <- x[!duplicated(x$source_order), c("source_order", "Id_person2")]
#replace old SR id
reconstitution <- merge(reconstitution, x, by="source_order", all=T)
reconstitution$Id_person <- ifelse(is.na(reconstitution$Id_person2), reconstitution$Id_person, reconstitution$Id_person2)
reconstitution$Id_person2 <- NULL
#remove duplicated rows
reconstitution <- reconstitution[!duplicated(reconstitution), ]
#clean environment
rm(x, x1)



#####################################################
#### section 4: store file in long format series ####
#####################################################

#select relevant variables from SR
SR2 <- SR[, c("source_order", "Inventarisnummer", "Folionummer", "Geslacht",
              "sex", "Serieregister", "Typeregister",
              "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
              "year_entry", "month_entry", "day_entry" , "in_event_general", "in_event", "Aanvullendeinformatieinschrijv",
              "year_exit", "month_exit", "day_exit", "out_event_general", "out_event", "Aanvullendeinformatieuitschrij", "age",
              "year_birth", "month_birth", "day_birth", "year_birth_age_based")]
#set -1 to NA
SR2[SR2=="-1"] <- NA
#generate B_year_min and B_year_max
SR2$B_year_min <- SR2$year_birth_age_based-1
SR2$B_year_max <- SR2$year_birth_age_based
#add Id_person
SR2 <- merge(reconstitution, SR2, by="source_order")
SR2 <- SR2[!duplicated(SR2[,c("Id_person", "source_order")])]
#reallign dataset
SR2 <- SR2[, c("Id_person", "source_order", "Inventarisnummer", "Folionummer", "Geslacht",
               "sex", "Serieregister", "Typeregister",
               "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
               "year_entry", "month_entry", "day_entry", "in_event_general", "in_event", "Aanvullendeinformatieinschrijv",
               "year_exit", "month_exit", "day_exit", "out_event_general", "out_event", "Aanvullendeinformatieuitschrij", "age",
               "year_birth", "month_birth", "day_birth", "B_year_min", "B_year_max")]
#rename variables
colnames(SR2) <- c("Id_person", "Id_source", "Inventarisnummer", "Folionummer", "Age_Sex",
                   "Sex", "Source_series", "Source_type",
                   "Name_enslaved", "Name_enslaved_extra", "Name_mother", "Name_mother_extra", "Name_owner",
                   "StartEntryYear", "StartEntryMonth", "StartEntryDay", "StartEntryEvent", "StartEntryEventDetailed", "StartEntryExtraInfo",
                   "LastEntryYear", "LastEntryMonth", "LastEntryDay", "LastEntryEvent", "LastEntryEventDetailed", "LastEntryExtraInfo", "Age",
                   "B_year", "B_month", "B_day", "B_year_min", "B_year_max")
#reorder dataset
SR2 <- SR2 %>% arrange(Id_person, Source_series, StartEntryYear, StartEntryMonth, StartEntryDay)
#update labels for entry and exit events

SR2 <- SR2 %>% mutate(StartEntryEvent = replace(StartEntryEvent, StartEntryEvent == "Beginning", "Start Series"))

SR2 <- SR2 %>% mutate(LastEntryEventDetailed = replace(LastEntryEventDetailed, LastEntryEventDetailed == "Afgeschreven", "Written off"),
                      LastEntryEventDetailed = replace(LastEntryEventDetailed, LastEntryEventDetailed == "Overgeschreven", "Transferred"))

SR2 <- SR2 %>% mutate(LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed == "Written off", "Written off"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Transferred", "Transferred"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Freedom", "Freedom"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Death", "Death"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Killed", "Death"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Drowned", "Death"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "Escaped", "Freedom"),
                      LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed ==  "End Series/Freedom", "Ended"))

#update labels for Age_Sex
SR2 <- SR2 %>% mutate(Age_Sex = replace(Age_Sex, Age_Sex == "Jongens", "boy"),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Leeg", NA),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Mannelijk", NA),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Mannen", "man"),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Meisjes", "girl"),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Onleesbaar", NA),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Vrouwelijk", NA),
                      Age_Sex = replace(Age_Sex, Age_Sex == "Vrouwen", "woman"))

#Make own category for escaped, diseased, and birth
SR2 <- SR2 %>% mutate(LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed == "Escaped", "Escaped"))
SR2 <- SR2 %>% mutate(StartEntryEvent = replace(StartEntryEvent, StartEntryEventDetailed == "Birth", "Birth"))
SR2 <- SR2 %>% mutate(LastEntryEvent = replace(LastEntryEvent, LastEntryEventDetailed == "Diseased", "Diseased"))

#add standardized plantation names
SR_plantation <- SR %>% select(source_order, plantation_name) %>% distinct(source_order, plantation_name)
SR2 <- left_join(SR2, SR_plantation, by = c("Id_source" = "source_order")) %>% rename(Plantation = plantation_name) 

#remove semicolons
SR2 <- data.frame(lapply(SR2, function(x) {gsub(";", ",", x)}))

#write outfiles
#write.table(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.csv"), quote=T, sep =",", row.names=F, fileEncoding="UTF-8", na = "")
#write.xlsx(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.xlsx"), overwrite=T)



###############################################
# 5 Append to Reconstituted slave registers   #
###############################################

cleaned <- fread("Cleaned Registry/cleaned slave register 2023-05-22.txt", encoding="UTF-8")

death <-  cleaned %>%
  select(source_order, year_death, day_death, month_death, year_exit, out_event) %>%
  group_by(source_order) %>%
  mutate(year_death = replace(year_death, is.na(year_death) & (out_event == "Death" | out_event == "Killed" | out_event == "Drowned")  & !(is.na(year_exit)), year_exit)) %>%
  select(source_order, year_death, day_death, month_death) %>%
  rename(Year_death = year_death,
         Month_death = month_death,
         Day_death = day_death) %>%
  ungroup()

#load cleaned registry to add original names to dataset
names_orig <- cleaned %>% 
  select(source_order, Naam_original, Moeder_original)



#######################################################################
### 6 Prepare Slave register for appending with Emancipation register ###
#######################################################################
sr2 <- SR2 %>%
  #add a unique identifier called Id_person
  group_by(Id_person) %>%
  mutate(Id_match = cur_group_id()) %>%
  rename(Owner = Name_owner) %>%
  #rename categories for appending with emancipation register
  mutate(Source_type = replace(Source_type, Source_type == "Particulieren", "Slave register private owner"),
         Source_type = replace(Source_type, Source_type == "Plantages", "Slave register plantation")) %>%
  #remove unnecessary variables
  select(-B_year_min, -B_year_max, -Name_enslaved, -Name_enslaved_extra, -Name_mother, -Name_mother_extra)
#add date of death from cleaned registry
sr2 <- left_join(sr2, death, by = c("Id_source" = "source_order"))
#add original names from cleaned registry and rename variables
sr2 <- left_join(sr2, names_orig, by = c("Id_source" = "source_order")) %>%
  rename(Name_enslaved = Naam_original,
         Name_mother = Moeder_original,
         Inventory_number = Inventarisnummer,
         Folio_number = Folionummer)

sr2$StartEntryYear <- as.numeric(sr2$StartEntryYear)
sr2$StartEntryMonth <- as.numeric(sr2$StartEntryMonth)
sr2$StartEntryDay <- as.numeric(sr2$StartEntryDay)
sr2$B_year <- as.numeric(sr2$B_year)
sr2$B_month <- as.numeric(sr2$B_month)
sr2$B_day <- as.numeric(sr2$B_day)

#######################################################################
### 7 Prepare Emancipation register for appending with Slave register ###
#######################################################################
emanc2 <- ER_final %>%
  #rename variables for appending with emancipation register
  rename(Id_source= source_order_SR_1,
         Name_enslaved = Slave_name,
         Year_birth2_ER = B_year2,
         Occupation = occupation,
         Remarks_ER = general_remarks) %>%
  #generate variables for appending with slave register
  mutate(Source_series = "1863",
         Source_type = "Emancipation Register") %>%
  #remove unmatched cases and those with identical match scores
  filter(matching_status_ER == "matched") %>%
  #remove unnecessary variables
  select(-Naam_lv, - Match_score, -matching_status_ER, -source_order_SR_2, -source_order_SR_3, -Naam_number, -Extrainformatiebijnaam)


#######################################################
### 8 Append Emancipation register and Slave register ###
#######################################################
combined <- bind_rows(sr2, emanc2) %>%
  #add unique identifier to matched records in emancipation register
  group_by(Id_source) %>%
  mutate(Id_match = min(Id_match, na.rm = TRUE)) %>%
  #arrange according to identifier and start date for correct row order
  arrange(Id_match, StartEntryYear, StartEntryMonth, StartEntryDay) %>%
  mutate(Id_source = replace(Id_source, Source_type == "Emancipation Register" , NA)) %>%
  ungroup() %>%
  select(-Id_person) %>%
  #rename variables to match those in Curacao data
  rename(Id_person = Id_match,
         Year_birth = B_year,
         Month_birth = B_month,
         Day_birth = B_day,
         Name_owner = Owner,
         StartEntryInfo = StartEntryExtraInfo,
         EndEntryYear = LastEntryYear,
         EndEntryMonth = LastEntryMonth,
         EndEntryDay = LastEntryDay,
         EndEntryEvent = LastEntryEvent,
         EndEntryEventDetailed = LastEntryEventDetailed,
         EndEntryInfo = LastEntryExtraInfo,
         Serieregister = Source_series,
         Typeregister = Source_type)  %>%
  #relocate
  relocate (Id_person, Id_source, Name_enslaved, Sex, Age, Age_Sex, Day_birth, Month_birth, Year_birth, Year_birth2_ER, Day_death,  Month_death, Year_death, Name_mother, Plantation, Name_owner,
            StartEntryDay, StartEntryMonth, StartEntryYear, StartEntryInfo, StartEntryEventDetailed, StartEntryEvent,
            EndEntryDay, EndEntryMonth, EndEntryYear, EndEntryInfo, EndEntryEvent, EndEntryEventDetailed,
            First_name, Family_name, Baptized_name, Family_relations, Occupation, Remarks_ER,
            Inventory_number, Folio_number, Serieregister, Typeregister) %>%
  arrange(Id_person, StartEntryYear, StartEntryMonth, StartEntryDay) 

#remove semicolons
combined <- data.frame(lapply(combined, function(x) {gsub(";", ",", x)}))

#write outfiles
write.table(combined, paste0("Reconstituted registry/", Sys.Date(), "SR ER life courses.csv"), quote=T, sep =",", row.names=F, fileEncoding="UTF-8", na = "")
write.xlsx(combined, paste0("Reconstituted registry/", Sys.Date(), "SR ER life courses.xlsx"), overwrite=T)



#################################################
## 9 Make wide format for selected variables   ####
#################################################


# Only keep variables that we want to include in wide data set and create wide format with pivot_wider
df_wide_sel <- combined %>% select(Id_person, Year_birth, Year_death, StartEntryYear, EndEntryYear) %>%
  group_by(Id_person) %>% mutate(id = row_number()) %>% pivot_wider(names_from = id, values_from = c(Year_birth, Year_death, StartEntryYear, EndEntryYear)) 

# Create separate file to create the flags for the four series
df_wide_flags <- combined %>% select(Id_person, Serieregister, StartEntryYear) %>% 
  group_by(Id_person, Serieregister) %>% 
  mutate(id = row_number()) %>% 
  filter(id == 1) %>% 
  select(-id) %>%
  pivot_wider(names_from = c(Serieregister), values_from = StartEntryYear) %>% 
  rename("Serie1" ="1830-1838",
         "Serie2" ="1838-1848",
         "Serie3" ="1848-1851",
         "Serie4" ="1851-1863",
         "ER" = "1863") %>%
  group_by(Id_person) %>%
  summarise(Serie1 = ifelse(is.na(Serie1), 0, 1),
            Serie2 = ifelse(is.na(Serie2), 0, 1),
            Serie3 = ifelse(is.na(Serie3), 0, 1),
            Serie4 = ifelse(is.na(Serie4), 0, 1),
            ER = ifelse(is.na(ER), 0, 1)) 

# Merge both files and create relevant variables
df_wide <- left_join(df_wide_sel, df_wide_flags) 

my_min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
my_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

#Birth year
birth <- df_wide %>% select(Id_person, starts_with("Year_birth"))
birth$B_year <- round(apply(birth[, -1], 1, FUN = mean, na.rm = TRUE))
birth <-  birth %>% select(Id_person, B_year)
birth <-  birth %>%
  mutate_all(~ifelse(is.nan(.), NA, .))
#Start year
entry <- df_wide %>% select(Id_person, starts_with("StartEntryYear"))
entry$Start_year <- apply(entry[, -1], 1, my_min)
entry <-  entry %>% select(Id_person, Start_year)
#Last year
exit <- df_wide %>% select(Id_person, starts_with("EndEntryYear"))
exit$Last_year <- apply(exit[, -1], 1, my_max) 
exit <-  exit %>% select(Id_person, Last_year)
#Death year
died <- df_wide %>% select(Id_person, starts_with("Year_death"))
died$D_year <- apply(died[, -1], 1, my_min)
died <-  died %>% select(Id_person, D_year)

df_wide <- left_join(left_join(left_join(left_join(df_wide, birth), entry), exit), died) %>%
  select(Id_person, Serie1, Serie2, Serie3, Serie4, ER, B_year, Start_year, Last_year, D_year)

#write ourfiles
write.table(df_wide, paste0("Reconstituted registry/", Sys.Date(), "SR ER life courses wide.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
write.xlsx(df_wide, paste0("Reconstituted registry/", Sys.Date(), "SR ER life courses wide.xlsx"), overwrite=T)

