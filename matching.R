  
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
  lev_dist_naam_ER <- 2
  lev_dist_eigenaar_ER <- 3
  
  #set threshold for filtering during reconstitution
  threshold34 <- 1
  threshold23 <- 0
  threshold12 <- 0
  threshold24 <- 0
  threshold14 <- 0
  threshold13 <- 0
  threshold44 <- 11.5
  threshold33 <- 11.5
  threshold22 <- 8
  threshold11 <- 8
  
  #load scripts
  source(paste(wd_scripts, "within matching.R", sep=""))
  source(paste(wd_scripts, "between matching.R", sep=""))
  source(paste(wd_scripts, "between_matching_emancipation.R", sep=""))
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
    SR <- fread("Cleaned Registry/cleaned slave register 2022-09-17.txt", encoding="UTF-8")
    
    
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
    #select serie 4, corrigeer voor afwijkende planta
    Serie4 <- SR %>% filter (Serieregister == "1851-1863" & out_event == "End Series/Freedom")
    #edit known shifts in plantation names
    Serie4 <- Serie4 %>% mutate(plantation_name = replace(plantation_name, plantation_name =="De Eendragt", "Eendragt"),
                                plantation_name = replace(plantation_name, plantation_name =="Alkmaar ( voor het 1/2 aandeel aankomende den Boedel A. Ferrier )", "Alkmaar"),
                                plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot", "Concordia"),
                                plantation_name = replace(plantation_name, plantation_name =="Johanna Charlotte (1/2 aandeel mevrouw G. C. Henkel geboren Vogt)", "Rustenburg"),
                                plantation_name = replace(plantation_name, plantation_name =="Waterwijk", "Hazard boven Commewijne"),
                                plantation_name = replace(plantation_name, plantation_name =="Harmonie", "l'Aventure"),
                                plantation_name = replace(plantation_name, plantation_name =="Lotland No 34", "Crappahoek nr. 34"),
                                plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot L. L.", "Concordia"))
    
  #mark whether plantation names from ER to Serie 4 match  
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
    #match
    list1 <- match_between_emancipation(ER, Serie4, lev_dist_naam=lev_dist_naam_ER, lev_dist_eigenaar=lev_dist_eigenaar_ER)
    
  #Run the same algorithm but now replace plantation names for specific slave groups that were apparently rented to other plantations
    #???
    df_not_matched_ER <- list1[[2]] %>%
      mutate(plantation_match = replace(plantation_match, plantation_match ==1, 0)) %>%
      rename(Naam = Naam_1,
             Naam_number = Naam_number_1) %>%
      select(Id_person, Naam, Naam_number, B_year, B_year2, Place_name1, plantation_match, Eigenaar_1, sex_emanc)
    #???
    df_not_matched_SR <- list1[[3]] %>%
      rename(Naam = Naam_2,
             Naam_number = Naam_number_2) %>%
      select(source_order, Naam, Naam_number, year_birth, plantation_name, Place_name1_match, Eigenaar_2, sex)
    #match
    list2 <- match_between_emancipation(df_not_matched_ER, df_not_matched_SR, lev_dist_naam=set_lv_dist, lev_dist_eigenaar=lev_dist_eigenaar_ER)
    
    
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
      relocate(source_order_SR_1, source_order_SR_2, source_order_SR_3, .after = Id_person)
    
      
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
    
    Serie4 <- SR[which(SR$Serieregister_nr==3 | SR$Serieregister_nr==4), c("source_order", 
                                                                           "in_event2", "out_event2",
                                                                           "Naam", "Naam_number", 
                                                                           "Moeder", "Moeder_number", 
                                                                           "Eigenaar",
                                                                           "year_birth", "month_birth", "day_birth",
                                                                           "year_entry", "month_entry", "day_entry",
                                                                           "year_exit", "month_exit", "day_exit",
                                                                           "sex")]
    Serie4 <- Serie4[which(Serie4$out_event2=="Transferred" | Serie4$in_event2=="Transferred"),]
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
    
    Serie44 <- match_within(Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=34)
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
    write.table(df_final, paste0(wd_ER, "/", Sys.Date(), "Emancipation_Register_Linked.txt", sep="/"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    
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
    write.table(Serie22, paste0("Within/", Sys.Date(), "/2-2 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.table(Serie11, paste0("Within/", Sys.Date(), "/1-1 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
   #excel
    write.xlsx(Serie44, paste0("Within/", Sys.Date(), "/4-4 matches.xlsx"), overwrite=T)
    write.xlsx(Serie22, paste0("Within/", Sys.Date(), "/2-2 matches.xlsx"), overwrite=T)
    write.xlsx(Serie11, paste0("Within/", Sys.Date(), "/1-1 matches.xlsx"), overwrite=T)
    
    
    
  ##########################################################
  #### section 3: load program to filter unique matches ####
  ##########################################################
    
    source(paste(wd_scripts, "filtering + closure.R", sep=""))
    #MATCH SERIE4 EMANCIPATION TOEVOEGEN!!!!!!
    
  #################################################################
  ### section 4: assign grouped certificates same id_person ####
  ###############################################################
    
   #generate SR id 
    reconstitution$Id_person <- paste("SR", sprintf("%06d", 1:length(reconstitution$Source_order_1)), sep="-")
    reconstitution <- melt(setDT(reconstitution), id.vars = c("Id_person"), value.name = "source_order")
    reconstitution <- reconstitution[!is.na(reconstitution$source_order), c("Id_person", "source_order")]
    
    
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
                 "year_birth", "month_birth", "day_birth", "B_year_min", "B_year_min")]
   #rename variables
    colnames(SR2) <- c("Id_person", "Id_source", "Inventarisnummer", "Folionummer",
                      "Sex", "Source_series", "Source_type",
                      "Name_enslaved", "Name_enslaved_extra", "Name_mother", "Name_mother_extra", "Name_owner",
                      "StartEntryYear", 
                      "LastEntryYear", "LastEntryMonth", "LastEntryDay", "LastEntryEvent", "LastEntryEventDetailed", "LastEntryExtraInfo",
                      "B_year", "B_month", "B_day", "B_year_min", "B_year_max")
   #reorder dataset
    SR2 <- SR2 %>% arrange(Id_person, Source_series, StartEntryYear, StartEntryMonth, StartEntryDay)
   #write outfiles
    write.table(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.xlsx(SR2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.xlsx"), overwrite=T)
    
    
  #################################
  #### check length long table ####
  #################################
    
    length(SR[,1])==length(SR2[,1])
    
    
    
    
    
    
    
  #######################################################
  #### section 5: make excerpt in wide format series ####
  #######################################################
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    ###############################################
    # 7 Append to Reconstituted slave registers   #
    ###############################################
    
    ### REWORK THIS SECTION LATER
    
    sl_recon <-  read.xlsx("//cnas.ru.nl/U709207/Documents/HDS and Curacao/Emancipatieregisters/2022-08-01SR life courses.xlsx") %>%
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
    
    
    
    
    
    
    