  
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
  
  #set working directory for data
  wd_ER <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname Emancipatieregister/Emancipatieregisters - Cleaning and Matching"
  wd_namenlijst <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten"
  wd_SR <- "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/"
  #set working directory for scripts
  wd_scripts <- "U:/Surfdrive/GitHub/Slavenregisters/"
  
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
    ER <- fread("Emancipatieregister_cleaned.csv", encoding="UTF-8") %>% rename(Naam = Name) %>% arrange(Naam)
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
    SR <- fread("Cleaned Registry/cleaned slave register 2022-10-21.txt", encoding="UTF-8")
    
    
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
    
   #run a function that prepares the retrieved datasets for final output and for linkage with the SR
    prepare_final_data <- function(df1){
      df_final <- left_join(df1, ER) %>%
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
       df_unmatched, ident_matches, unmatched_ER, unmatched_SR, matches, emanc)
    
    
      
  #####################################################
  #### section 1b: retrieve matches BETWEEN series ####
  #####################################################
    
  #match serie 3 & 4
   #select series
    Serie3 <- SR[which(SR$Serieregister_nr==3), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- SR[SR$Serieregister_nr==4, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie34 <- match_between(Serie3, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=3, NUMMER2=4)
    
  #match serie 2 & 3
   #select series
    Serie2 <- SR[which(SR$Serieregister_nr==2 & SR$out_event2=="Ended" |
                         SR$Serieregister_nr==2 & SR$year_entry>=1848) , c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- SR[SR$Serieregister_nr==3 & SR$in_event2=="Beginning" |
                   SR$Serieregister_nr==3 & SR$year_entry==1848, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie23 <- match_between(Serie2, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=3)
    
  #match serie 1 & 2
   #select series
    Serie1 <- SR[which(SR$Serieregister_nr==1 & SR$out_event2=="Ended" |
                         SR$Serieregister_nr==1 & SR$year_entry>=1838), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie2 <- SR[SR$Serieregister_nr==2 & SR$in_event2=="Beginning" |
                   SR$Serieregister_nr==2 & SR$year_entry==1838, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie12 <- match_between(Serie1, Serie2, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=2)
    
  #match serie 2 & 4
   #select series
    Serie2 <- SR[SR$Serieregister_nr==2 & SR$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- SR[SR$Serieregister_nr==4 & SR$in_event2=="Beginning" & SR$year_birth<=1848 |
                   SR$Serieregister_nr==4 & SR$in_event2=="Beginning" & SR$year_birth==-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie24 <- match_between(Serie2, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=4)
    
  #match serie 1 & 4
   #select series
    Serie1 <- SR[SR$Serieregister_nr==1 & SR$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- SR[SR$Serieregister_nr==4 & SR$in_event2=="Beginning" & SR$year_birth<=1838 |
                   SR$Serieregister_nr==4 & SR$in_event2=="Beginning" & SR$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie14 <- match_between(Serie1, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=4)
    
  #match serie 1 & 3
   #select series
    Serie1 <- SR[SR$Serieregister_nr==1 & SR$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- SR[SR$Serieregister_nr==3 & SR$in_event2=="Beginning" & SR$year_birth<=1838 |
                   SR$Serieregister_nr==3 & SR$in_event2=="Beginning" & SR$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie13 <- match_between(Serie1, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=3)
    
    
  ####################################################
  #### section 1c: retrieve matches WITHIN series ####
  ####################################################
    
    Serie4 <- SR[which(SR$Serieregister_nr==4), c("source_order", 
                                                  "in_event2", "out_event2",
                                                  "Naam", "Naam_number", 
                                                  "Moeder", "Moeder_number", 
                                                  "Eigenaar",
                                                  "year_birth", "month_birth", "day_birth",
                                                  "year_entry", "month_entry", "day_entry",
                                                  "year_exit", "month_exit", "day_exit",
                                                  "sex")]
    Serie4 <- Serie4[which(Serie4$out_event2=="Transferred" | Serie4$in_event2=="Transferred"),]
    Serie3 <- SR[which(SR$Serieregister_nr==3 ), c("source_order", 
                                                   "in_event2", "out_event2",
                                                   "Naam", "Naam_number", 
                                                   "Moeder", "Moeder_number", 
                                                   "Eigenaar",
                                                   "year_birth", "month_birth", "day_birth",
                                                   "year_entry", "month_entry", "day_entry",
                                                   "year_exit", "month_exit", "day_exit",
                                                   "sex")]
    Serie3 <- Serie3[which(Serie3$out_event2=="Transferred" | Serie3$in_event2=="Transferred"),]
    Serie2 <- SR[SR$Serieregister_nr==2, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth", "month_birth", "day_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie2 <- Serie2[which(Serie2$out_event2=="Transferred" | Serie2$in_event2=="Transferred"),]
    Serie1 <- SR[SR$Serieregister_nr==1, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth", "month_birth", "day_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie1 <- Serie1[which(Serie1$out_event2=="Transferred" | Serie1$in_event2=="Transferred"),]
    
    Serie44 <- match_within(Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=4)
    Serie33 <- match_within(Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=3)
    Serie22 <- match_within(Serie2, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2)
    Serie11 <- match_within(Serie1, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1)
    
    
  ###############################
  #### section 2: store info ####
  ###############################
    
  #make directories
    if (file.exists(paste0("Between/", Sys.Date()))){
      getwd()
    } else {
      dir.create(paste0(getwd(), "/Between/", Sys.Date()))
      dir.create(paste0(getwd(), "/Between/", Sys.Date(), "/Particulieren"))
      dir.create(paste0(getwd(), "/Between/", Sys.Date(), "/Plantages"))
      dir.create(paste0(getwd(), "/Within/", Sys.Date()))
    }
    
  #write outfile ER
    write.table(ER_final, paste0(wd_ER, "/", Sys.Date(), "Emancipation_Register_Linked.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    
  #write outfiles BETWEEN
   #csv
    write.table(Serie34, paste0("Between/", Sys.Date(), "/3-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie23, paste0("Between/", Sys.Date(), "/2-3 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie12, paste0("Between/", Sys.Date(), "/1-2 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie24, paste0("Between/", Sys.Date(), "/2-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie14, paste0("Between/", Sys.Date(), "/1-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie13, paste0("Between/", Sys.Date(), "/1-3 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
   #excel plantages
    Plantages34 <- Serie34[which(Serie34$Typeregister_3=="Plantages" | Serie34$Typeregister_4=="Plantages"),]
    Plantages23 <- Serie23[which(Serie23$Typeregister_2=="Plantages" | Serie23$Typeregister_3=="Plantages"),]
    Plantages12 <- Serie12[which(Serie12$Typeregister_1=="Plantages" | Serie12$Typeregister_2=="Plantages"),]
    Plantages24 <- Serie24[which(Serie24$Typeregister_2=="Plantages" | Serie24$Typeregister_4=="Plantages"),]
    Plantages14 <- Serie14[which(Serie14$Typeregister_1=="Plantages" | Serie14$Typeregister_4=="Plantages"),]
    Plantages13 <- Serie13[which(Serie13$Typeregister_1=="Plantages" | Serie13$Typeregister_3=="Plantages"),]
    write.xlsx(Plantages34, paste0("Between/", Sys.Date(), "/Plantages/3-4 matches plantages.xlsx"), overwrite=T)
    write.xlsx(Plantages23, paste0("Between/", Sys.Date(), "/Plantages/2-3 matches plantages.xlsx"), overwrite=T)
    write.xlsx(Plantages12, paste0("Between/", Sys.Date(), "/Plantages/1-2 matches plantages.xlsx"), overwrite=T)
    write.xlsx(Plantages24, paste0("Between/", Sys.Date(), "/Plantages/2-4 matches plantages.xlsx"), overwrite=T)
    write.xlsx(Plantages14, paste0("Between/", Sys.Date(), "/Plantages/1-4 matches plantages.xlsx"), overwrite=T)
    write.xlsx(Plantages13, paste0("Between/", Sys.Date(), "/Plantages/1-3 matches plantages.xlsx"), overwrite=T)
   #excel particulieren
    Particulieren34 <- Serie34[which(Serie34$Typeregister_3=="Particulieren" | Serie34$Typeregister_4=="Particulieren"),]
    Particulieren23 <- Serie23[which(Serie23$Typeregister_2=="Particulieren" | Serie23$Typeregister_3=="Particulieren"),]
    Particulieren12 <- Serie12[which(Serie12$Typeregister_1=="Particulieren" | Serie12$Typeregister_2=="Particulieren"),]
    Particulieren24 <- Serie24[which(Serie24$Typeregister_2=="Particulieren" | Serie24$Typeregister_4=="Particulieren"),]
    Particulieren14 <- Serie14[which(Serie14$Typeregister_1=="Particulieren" | Serie14$Typeregister_4=="Particulieren"),]
    Particulieren13 <- Serie13[which(Serie13$Typeregister_1=="Particulieren" | Serie13$Typeregister_3=="Particulieren"),]
    write.xlsx(Particulieren34, paste0("Between/", Sys.Date(), "/Particulieren/3-4 matches particulieren.xlsx"), overwrite=T)
    write.xlsx(Particulieren23, paste0("Between/", Sys.Date(), "/Particulieren/2-3 matches particulieren.xlsx"), overwrite=T)
    write.xlsx(Particulieren12, paste0("Between/", Sys.Date(), "/Particulieren/1-2 matches particulieren.xlsx"), overwrite=T)
    write.xlsx(Particulieren24, paste0("Between/", Sys.Date(), "/Particulieren/2-4 matches particulieren.xlsx"), overwrite=T)
    write.xlsx(Particulieren14, paste0("Between/", Sys.Date(), "/Particulieren/1-4 matches particulieren.xlsx"), overwrite=T)
    write.xlsx(Particulieren13, paste0("Between/", Sys.Date(), "/Particulieren/1-3 matches particulieren.xlsx"), overwrite=T)
    
    
  #write outfiles WITHIN
    #csv
    write.table(Serie44, paste0("Within/", Sys.Date(), "/4-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie33, paste0("Within/", Sys.Date(), "/3-3 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie22, paste0("Within/", Sys.Date(), "/2-2 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie11, paste0("Within/", Sys.Date(), "/1-1 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
   #excel
    write.xlsx(Serie44, paste0("Within/", Sys.Date(), "/4-4 matches.xlsx"), overwrite=T)
    write.xlsx(Serie33, paste0("Within/", Sys.Date(), "/3-3 matches.xlsx"), overwrite=T)
    write.xlsx(Serie22, paste0("Within/", Sys.Date(), "/2-2 matches.xlsx"), overwrite=T)
    write.xlsx(Serie11, paste0("Within/", Sys.Date(), "/1-1 matches.xlsx"), overwrite=T)
    
    
    
  ##############################################################
  #### section 3: load program to reconstruct life courses  ####
  ##############################################################
    
    source(paste(wd_scripts, "filtering + closure.R", sep=""))
    
    
    
  ##############################################################
  ### section 4: assign grouped certificates same id_person ####
  ##############################################################
    
   #generate SR id 
    reconstitution$Id_person <- paste("SR", sprintf("%06d", 1:length(reconstitution$Source_order_1)), sep="-")
    reconstitution <- melt(setDT(reconstitution), id.vars = c("Id_person"), value.name = "source_order")
    reconstitution <- reconstitution[!is.na(reconstitution$source_order), c("Id_person", "source_order")]
    
   #remove duplicated rows
    reconstitution <- reconstitution[!duplicated(reconstitution), ]
    
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
  #### section 5: store file in long format series ####
  #####################################################
    
   #select relevant variables from SR
    SR2 <- SR[, c("source_order", "Inventarisnummer", "Folionummer",
                 "sex", "Serieregister", "Typeregister",
                 "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
                 "year_entry", "month_entry", "day_entry", "in_event2", "in_event", "Aanvullendeinformatieinschrijv",
                 "year_exit", "month_exit", "day_exit", "out_event2", "out_event", "Aanvullendeinformatieuitschrij",
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
    SR2 <- SR2[, c("Id_person", "source_order", "Inventarisnummer", "Folionummer",
                 "sex", "Serieregister", "Typeregister",
                 "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
                 "year_entry", "month_entry", "day_entry", "in_event2", "in_event", "Aanvullendeinformatieinschrijv",
                 "year_exit", "month_exit", "day_exit", "out_event2", "out_event", "Aanvullendeinformatieuitschrij",
                 "year_birth", "month_birth", "day_birth", "B_year_min", "B_year_max")]
   #rename variables
    colnames(SR2) <- c("Id_person", "Id_source", "Inventarisnummer", "Folionummer",
                      "Sex", "Source_series", "Source_type",
                      "Name_enslaved", "Name_enslaved_extra", "Name_mother", "Name_mother_extra", "Name_owner",
                      "StartEntryYear", "StartEntryMonth", "StartEntryDay", "StartEntryEvent", "StartEntryEventDetailed", "StartEntryExtraInfo",
                      "LastEntryYear", "LastEntryMonth", "LastEntryDay", "LastEntryEvent", "LastEntryEventDetailed", "LastEntryExtraInfo",
                      "B_year", "B_month", "B_day", "B_year_min", "B_year_max")
   #reorder dataset
    SR2 <- SR2 %>% arrange(Id_person, Source_series, StartEntryYear, StartEntryMonth, StartEntryDay)
   #write outfiles
    write.table(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.xlsx(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.xlsx"), overwrite=T)
    
    
    
    ###############################################
    # 6 Append to Reconstituted slave registers   #
    ###############################################
    
    #######################################################################
    ### Prepare Slave register for appending with Emancipation register ###
    #######################################################################
    sr2 <- sr %>%
      #add a unique identifier called Id_person
      group_by(Id_person) %>%
      mutate(Id_match = cur_group_id()) %>%
      #generate variables Owner and Plantation for appending with emancipation register
      mutate(Owner = replace(Name_owner, Source_type != "Particulieren", NA),
             Plantation = replace(Name_owner, Source_type != "Plantages", NA)) %>%
      #rename categories for appending with emancipation register
      mutate(Source_type = replace(Source_type, Source_type == "Particulieren", "Slave register private owner"),
             Source_type = replace(Source_type, Source_type == "Plantages", "Slave register plantation")) %>%
      #remove unnecessary variables
      select(-StartEntryEventDetailed, -LastEntryEventDetailed, -Name_owner, -B_year_min, -B_year_max)
    
    #######################################################################
    ### Prepare Emancipation register for appending with Slave register ###
    #######################################################################
    emanc2 <- emanc %>%
      #rename variables for appending with emancipation register
      rename(Id_source= source_order_SR_1,
             Name_enslaved = Slave_name,
             Name_enslaved_extra = Extrainformatiebijnaam,
             B_year2_ER = B_year2,
             Occupation = occupation,
             Remarks_ER = general_remarks,
             Name_enslaved_number = Naam_number) %>%
      #generate variables for appending with slave register
      mutate(Source_series = "1863",
             Source_type = "Emancipation Register") %>%
      #remove unmatched cases and those with identical match scores
      filter(matching_status_ER == "matched") %>%
      #remove unnecessary variables
      select(-Naam_lv, - Match_score, -matching_status_ER, -source_order_SR_2, -source_order_SR_3)
    
    
    #######################################################
    ### Append Emancipation register and Slave register ###
    #######################################################
    combined <- bind_rows(sr2, emanc2) %>%
      #add unique identifier to matched records in emancipation register
      group_by(Id_source) %>%
      mutate(Id_match = min(Id_match, na.rm = TRUE)) %>%
      #arrange according to identifier and start date for correct row order
      arrange(Id_match, StartEntryYear, StartEntryMonth, StartEntryDay) %>%
      #relocate
      relocate (Id_match, Id_person, Id_source, Name_enslaved, Name_enslaved_extra, Name_enslaved_number, Sex, B_day, B_month, B_year, B_year2_ER, Name_mother, Name_mother_extra, Plantation, Owner) %>%
      mutate(Id_source = replace(Id_source, StartEntryYear == 1863 , NA)) %>%
      ungroup()
    
    setwd("//cnas.ru.nl/U709207/Documents/Suriname/Matching")
    write.xlsx(combined, file = "SR ER life courses.xlsx", overwrite=T)
    
    
    #################################################
    ## Make wide format for selected variables   ####
    #################################################
    
    # Only keep variables that we want to include in wide data set and create wide format with pivot_wider
    df_wide_sel <- combined %>% select(Id_match, B_year, StartEntryYear, LastEntryYear) %>%
      group_by(Id_match) %>% mutate(id = row_number()) %>% pivot_wider(names_from = id, values_from = c(B_year, StartEntryYear, LastEntryYear)) 
    
    # Create separate file to create the flags for the four series
    df_wide_flags <- combined %>% select(Id_match, Source_series, StartEntryYear) %>% 
      group_by(Id_match, Source_series) %>% 
      mutate(id = row_number()) %>% 
      filter(id == 1) %>% 
      select(-id) %>%
      pivot_wider(names_from = c(Source_series), values_from = StartEntryYear) %>% 
      rename("Serie1" ="1830-1838",
             "Serie2" ="1838-1848",
             "Serie3" ="1848-1851",
             "Serie4" ="1851-1863",
             "ER" = "1863") %>%
      group_by(Id_match) %>%
      summarise(Serie1 = ifelse(is.na(Serie1), 0, 1),
                Serie2 = ifelse(is.na(Serie2), 0, 1),
                Serie3 = ifelse(is.na(Serie3), 0, 1),
                Serie4 = ifelse(is.na(Serie4), 0, 1),
                ER = ifelse(is.na(ER), 0, 1)) %>%
      select(-Source_series)
    
    # Merge both files and re-arrange columns
    df_wide <- left_join(df_wide_sel, df_wide_flags) %>%
      relocate(c(Serie1, Serie2, Serie3, Serie4, ER, ends_with("1"), ends_with("2"), ends_with("3"), ends_with("4"),
                 ends_with("5"), ends_with("6"), ends_with("7"),ends_with("8")), .after = Id_match) %>%
      rowwise() %>%
      mutate(B_year = round(mean(c_across(starts_with("B_year")), na.rm = TRUE)),
             Start_year = min(c_across(starts_with("StartEntryYear")), na.rm = TRUE),
             Last_year = max(c_across(starts_with("LastEntryYear")), na.rm = TRUE))
    
    
    
    
    
    #load data
    #sr <- fread("//cnas.ru.nl/U709207/Documents/Suriname/Matching/2022-10-24SR life courses.txt", encoding="UTF-8")
    #emanc <- fread("//cnas.ru.nl/U709207/Documents/Suriname/Emancipatieregisters/2022-10-24Emancipation_Register_Linked.txt", encoding="UTF-8")
    sr <- fread("~/HDS/Matching/2022-10-24SR life courses.txt", encoding="UTF-8")
    emanc <- fread("~/HDS/Matching/2022-10-24Emancipation_Register_Linked.txt", encoding="UTF-8")
    #load cleaned registry to add year of death to dataset; prepare year of death
    cleaned <- fread("~/HDS/Matching/cleaned slave register 2022-10-21.txt", encoding="UTF-8") %>%
      select(source_order, year_death, year_exit, out_event2) %>%
      group_by(source_order) %>%
      mutate(year_death = replace(year_death, is.na(year_death) & out_event2 == "Death" & !(is.na(year_exit)), year_exit)) %>%
      select(source_order, year_death) %>%
      rename(D_year = year_death) %>%
      ungroup()
    #######################################################################
    ### Prepare Slave register for appending with Emancipation register ###
    #######################################################################
    sr2 <- sr %>%
      #add a unique identifier called Id_person
      group_by(Id_person) %>%
      mutate(Id_match = cur_group_id()) %>%
      #generate variables Owner and Plantation for appending with emancipation register
      mutate(Owner = replace(Name_owner, Source_type != "Particulieren", NA),
             Plantation = replace(Name_owner, Source_type != "Plantages", NA)) %>%
      #rename categories for appending with emancipation register
      mutate(Source_type = replace(Source_type, Source_type == "Particulieren", "Slave register private owner"),
             Source_type = replace(Source_type, Source_type == "Plantages", "Slave register plantation")) %>%
      #remove unnecessary variables
      select(-StartEntryEventDetailed, -LastEntryEventDetailed, -Name_owner, -B_year_min, -B_year_max)
    #add date of death from cleaned registry
    sr2 <- left_join(sr2, cleaned, by = c("Id_source" = "source_order"))
    
    
    #######################################################################
    ### Prepare Emancipation register for appending with Slave register ###
    #######################################################################
    emanc2 <- emanc %>%
      #rename variables for appending with emancipation register
      rename(Id_source= source_order_SR_1,
             Name_enslaved = Slave_name,
             Name_enslaved_extra = Extrainformatiebijnaam,
             B_year2_ER = B_year2,
             Occupation = occupation,
             Remarks_ER = general_remarks,
             Name_enslaved_number = Naam_number) %>%
      #generate variables for appending with slave register
      mutate(Source_series = "1863",
             Source_type = "Emancipation Register") %>%
      #remove unmatched cases and those with identical match scores
      filter(matching_status_ER == "matched") %>%
      #remove unnecessary variables
      select(-Naam_lv, - Match_score, -matching_status_ER, -source_order_SR_2, -source_order_SR_3)
    
    
    #######################################################
    ### Append Emancipation register and Slave register ###
    #######################################################
    combined <- bind_rows(sr2, emanc2) %>%
      #add unique identifier to matched records in emancipation register
      group_by(Id_source) %>%
      mutate(Id_match = min(Id_match, na.rm = TRUE)) %>%
      #arrange according to identifier and start date for correct row order
      arrange(Id_match, StartEntryYear, StartEntryMonth, StartEntryDay) %>%
      #relocate
      relocate (Id_match, Id_person, Id_source, Name_enslaved, Name_enslaved_extra, Name_enslaved_number, Sex, B_day, B_month, B_year, B_year2_ER, D_year, Name_mother, Name_mother_extra, Plantation, Owner) %>%
      mutate(Id_source = replace(Id_source, StartEntryYear == 1863 , NA)) %>%
      ungroup()
    
    #write data
    write_xlsx(combined,"~/HDS/Matching/SR ER life courses.xlsx")
    
    
    #################################################
    ## Make wide format for selected variables   ####
    #################################################
    
    # Only keep variables that we want to include in wide data set and create wide format with pivot_wider
    df_wide_sel <- combined %>% select(Id_match, B_year, D_year, StartEntryYear, LastEntryYear) %>%
      group_by(Id_match) %>% mutate(id = row_number()) %>% pivot_wider(names_from = id, values_from = c(B_year, D_year, StartEntryYear, LastEntryYear)) 
    
    # Create separate file to create the flags for the four series
    df_wide_flags <- combined %>% select(Id_match, Source_series, StartEntryYear) %>% 
      group_by(Id_match, Source_series) %>% 
      mutate(id = row_number()) %>% 
      filter(id == 1) %>% 
      select(-id) %>%
      pivot_wider(names_from = c(Source_series), values_from = StartEntryYear) %>% 
      rename("Serie1" ="1830-1838",
             "Serie2" ="1838-1848",
             "Serie3" ="1848-1851",
             "Serie4" ="1851-1863",
             "ER" = "1863") %>%
      group_by(Id_match) %>%
      summarise(Serie1 = ifelse(is.na(Serie1), 0, 1),
                Serie2 = ifelse(is.na(Serie2), 0, 1),
                Serie3 = ifelse(is.na(Serie3), 0, 1),
                Serie4 = ifelse(is.na(Serie4), 0, 1),
                ER = ifelse(is.na(ER), 0, 1)) 
    
    # Merge both files and create relevant variables
    df_wide <- left_join(df_wide_sel, df_wide_flags) %>%
      relocate(c(Serie1, Serie2, Serie3, Serie4, ER, ends_with("1"), ends_with("2"), ends_with("3"), ends_with("4"),
                 ends_with("5"), ends_with("6"), ends_with("7"),ends_with("8")), .after = Id_match) %>%
      rowwise() %>%
      mutate(B_year = round(mean(c_across(starts_with("B_year")), na.rm = TRUE)),
             Start_year = min(c_across(starts_with("StartEntryYear")), na.rm = TRUE),
             D_year = min(c_across(starts_with("D_year")), na.rm = TRUE),
             Last_year = max(c_across(starts_with("LastEntryYear")), na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate_all(~ifelse(is.nan(.), NA, .)) %>%
      mutate_if(is.numeric, list(~na_if(., Inf))) %>%
      select(Id_match, Serie1, Serie2, Serie3, Serie4, ER, B_year, Start_year, Last_year, D_year)
    
    #write data
    write_xlsx(df_wide, "~/HDS/Matching/SR ER life courses wide.xlsx")
    
    
    
    