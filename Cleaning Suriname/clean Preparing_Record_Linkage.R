  
  
  #######################CLEAN REGISTRIES 1-4#####################
  ####  This script cleans the names of:                      ####
  ####    1. Naam                                             ####
  ####    2. Moeder                                           ####
  ####    3. Eigenaar                                         ####
  ####  Recodifies the event_tags                             ####
  ####    4. In_events & Out_events                           ####
  ####  Removes duplicated entries from Series 3 & 4          ####
  ####    5. Series 3 >1851                                   ####
  ####       Series 4 <1851                                   ####
  ################################################################
  
  #load packages
  library("haven")
  library("dplyr")
  library("openxlsx")
  library("stringdist")
  
  #clean environment
  rm(list=ls())
  
  #open dataset
  setwd("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Voorwerk HDS/Most Important Files/Stata")
  df <- read_dta("Dataset_Standardization.dta")
  
  #import standardized entries Eigenaar
  write.xlsx(df[,c("source_order", "Eigenaar"),], "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/Eigenaren particulier/Eigenaren - input.xlsx", overwrite=T)
  #cleaned:
  # 1. de weduwe -> weduwe (if not part of surname OR remark)
  # 2. kinderen van -> kinderen (if not part of surname OR remark)
  # 2. - replaced by ,
  # 3. changes in owners marked with separator -
  # 4. mark vertegenwoordiger (nom ux, voor, door, qq) with ", door ... qq"
  # 5. connect surnames with multiple names with _
  
  #check data
  as.data.frame(table(df$Typeregister))
  as.data.frame(table(df$Serieregister))
  
  #number series
  df$Serieregister_nr <- ifelse(df$Serieregister=="1830-1838", 1,
                                ifelse(df$Serieregister=="1838-1848", 2,
                                       ifelse(df$Serieregister=="1848-1851", 3, 4)))
  as.data.frame(table(df$Serieregister_nr))
  
  #backup original names
  df$Naam_original <- df$Naam
  
  #split inventarisnummer into NAS inventaris & inventarisnummer
  df$Inventaris <- ifelse(grepl("NAS inventarisnummer", df$Inventarisnummer), "NAS", "")
  df$Inventarisnummer <- gsub("NAS inventarisnummer ", "", df$Inventarisnummer)
  
  #########################################################################################
  #### section -1: correct certain entry and exit events that were defined incorrectly ####
  #########################################################################################
  vendu <- read.xlsx("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/Correct entry and exit events/Vendu_aanpassing.xlsx") %>%
    select(source_order, in_event_new, out_event_new)
  executie <- read.xlsx("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/Correct entry and exit events/Executie_aanpassing.xlsx") %>%
    select(source_order, day_entry_new, month_entry_new, year_entry_new, in_event_new, in_event_general_new, out_event_new, out_event_general_new, day_exit_new, month_exit_new, year_exit_new, Aanvullendeinformatieuitschrij_new)   
  verpand <- read.xlsx("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/Correct entry and exit events/Verpand_aanpassing.xlsx")  %>%
    select(source_order, day_entry_new, month_entry_new, year_entry_new, in_event_new, in_event_general_new, year_exit_new, Aanvullendeinformatieuitschrij_new)
 
  
  df <- left_join(df, vendu) %>%
    mutate(in_event = replace(in_event, !(is.na(in_event_new)), in_event_new[!(is.na(in_event_new))]),
           out_event = replace(out_event, !(is.na(out_event_new)), out_event_new[!(is.na(out_event_new))])) %>%
    select(-out_event_new, -in_event_new)
  
  df <- left_join(df, executie) %>%
    mutate(day_entry = replace(day_entry, !(is.na(day_entry_new)), day_entry_new[!(is.na(day_entry_new))]),
           month_entry = replace(month_entry, !(is.na(month_entry_new)), month_entry_new[!(is.na(month_entry_new))]),
           year_entry = replace(year_entry, !(is.na(year_entry_new)), year_entry_new[!(is.na(year_entry_new))]),
           in_event = replace(in_event, !(is.na(in_event_new)), in_event_new[!(is.na(in_event_new))]),
           in_event_general = replace(in_event_general, !(is.na(in_event_general_new)), in_event_general_new[!(is.na(in_event_general_new))]),
           out_event = replace(out_event, !(is.na(out_event_new)), out_event_new[!(is.na(out_event_new))]),
           out_event_general = replace(out_event_general, !(is.na(out_event_general_new)), out_event_general_new[!(is.na(out_event_general_new))]),
           day_exit = replace(day_exit, !(is.na(day_exit_new)), day_exit_new[!(is.na(day_exit_new))]),
           month_exit = replace(month_exit, !(is.na(month_exit_new)), month_exit_new[!(is.na(month_exit_new))]),
           year_exit = replace(year_exit, !(is.na(year_exit_new)), year_exit_new[!(is.na(year_exit_new))]),
           Aanvullendeinformatieuitschrij = replace(Aanvullendeinformatieuitschrij, !(is.na(Aanvullendeinformatieuitschrij_new)), Aanvullendeinformatieuitschrij_new[!(is.na(Aanvullendeinformatieuitschrij_new))])) %>%
    select(-day_entry_new, -month_entry_new, -year_entry_new, -in_event_new, -in_event_general_new, -out_event_new, -out_event_general_new, -day_exit_new, -month_exit_new, -year_exit_new, -Aanvullendeinformatieuitschrij_new)
  
  df <- left_join(df, verpand) %>%
    mutate(day_entry = replace(day_entry, !(is.na(day_entry_new)), day_entry_new[!(is.na(day_entry_new))]),
           month_entry = replace(month_entry, !(is.na(month_entry_new)), month_entry_new[!(is.na(month_entry_new))]),
           year_entry = replace(year_entry, !(is.na(year_entry_new)), year_entry_new[!(is.na(year_entry_new))]),
           in_event = replace(in_event, !(is.na(in_event_new)), in_event_new[!(is.na(in_event_new))]),
           in_event_general = replace(in_event_general, !(is.na(in_event_general_new)), in_event_general_new[!(is.na(in_event_general_new))]),
           year_exit = replace(year_exit, !(is.na(year_exit_new)), year_exit_new[!(is.na(year_exit_new))]),
           Aanvullendeinformatieuitschrij = replace(Aanvullendeinformatieuitschrij, !(is.na(Aanvullendeinformatieuitschrij_new)), Aanvullendeinformatieuitschrij_new[!(is.na(Aanvullendeinformatieuitschrij_new))])) %>%
    select(-day_entry_new, -month_entry_new, -year_entry_new, -in_event_new, -in_event_general_new, -year_exit_new, -Aanvullendeinformatieuitschrij_new)
  
  df <- df %>%
    mutate(in_event = replace(in_event, in_event == "Verpand" & month_entry == -1, "Start Series"),
           in_event_general = replace(in_event_general, in_event == "Verpand" & month_entry == -1, "Beginning"),
           in_event = replace(in_event, source_order == "011159b14705", "Unknown"),
           in_event_general = replace(in_event_general, source_order == "011159b14705", "Unknown"),
           in_event = replace(in_event, source_order == "112056a23534", "Birth"),
           in_event_general = replace(in_event_general, source_order == "112056a23534", "Beginning"),
           in_event = replace(in_event, in_event == "Verpand", "Acquired/Transferred"),)
  
  rm(vendu, executie, verpand)
  
  #####################################################################################
  #### section 0: basic number cleaning + removing unintelligable characters (...) ####
  #####################################################################################
    
   #rules matthias Naam
    df$Naam <- gsub(" de 1e", " I", df$Naam)
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("1e", " I", df$Naam),   gsub("1e", "I", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("N1", "I", df$Naam),   gsub("N1", "I", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("21", " II", df$Naam),  gsub("21", "II", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("21", " II", df$Naam),  gsub("21", "II", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("22", " II", df$Naam),  gsub("22", "II", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("2n", " II", df$Naam),  gsub("2n", "II", df$Naam))
    df$Naam <- gsub(" de 2e", " 2e", df$Naam)
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("2e", " II", df$Naam),  gsub("2e", "II", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("3e", " III", df$Naam), gsub("3e", "III", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("4e", " IV", df$Naam),  gsub("4e", "IV", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("5e", " V", df$Naam),   gsub("5e", "V", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("1", " I", df$Naam),   gsub("1", "I", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("2", " II", df$Naam),  gsub("2", "II", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("3", " III", df$Naam), gsub("3", "III", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("4", " IV", df$Naam),  gsub("4", "IV", df$Naam))
    df$Naam <- ifelse(grepl("[a-zA-Z][0-9]", df$Naam), gsub("5", " V", df$Naam),   gsub("5", "V", df$Naam))
    
   #rules matthias Moeder
    df$Moeder <- gsub("1e", "I", df$Moeder)
    df$Moeder <- gsub("1 e", "I", df$Moeder)
    df$Moeder <- gsub("2e", "II", df$Moeder)
    df$Moeder <- gsub("2 e", "II", df$Moeder)
    df$Moeder <- gsub("2-E", "II", df$Moeder)
    
   #correct entry
    df$Naam_original[df$source_order=="011062a332"] <- "Laurens"
    df$Naam[df$source_order=="011062a332"] <- "Laurens"
    df$Naam[df$source_order=="22300287896"] <- "Christiaan NS"
    df$Naam_original[df$source_order=="22300287896"] <- "Christiaan NS"
    df$Naam_original[df$source_order=="101807a21022"] <- "Louis of Barend Juriaan Louis"
    df$Naam[df$source_order=="101807a21022"] <- "Louis of Barend Juriaan Louis"
    
   #correct Inventarisnummer
    df$Inventaris[df$Inventarisnummer=="4 juni 1861"] <- "33"
    df$Inventarisnummer[df$Inventarisnummer=="4 juni 1861"] <- 33
    
    
   #flag (...)
    df$Naam_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Naam) & nchar(gsub("\\(\\.\\.\\.)", "", df$Naam))<=3 |
                                       grepl("\\([a-zA-Z0-9])", df$Naam)==F & grepl("\\(.)", df$Naam) & nchar(gsub("\\(.)", "", df$Naam))<=3 |
                                       grepl("\\([a-zA-Z0-9]..)", df$Naam)==F & grepl("\\(...)", df$Naam) & nchar(gsub("\\(...)", "", df$Naam))<=3 |
                                       grepl("\\([a-zA-Z0-9])", df$Naam)==F & grepl("\\(.)", df$Naam) & nchar(gsub("\\(.)", "", df$Naam))<=3, 
                                     "fully", "")
    df$Naam_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Naam) & nchar(gsub("\\(\\.\\.\\.)", "", df$Naam))>3 |
                                       grepl("\\([a-zA-Z0-9])", df$Naam)==F & grepl("\\(.)", df$Naam) & nchar(gsub("\\(.)", "", df$Naam))>3 |
                                       grepl("\\([a-zA-Z0-9]..)", df$Naam)==F & grepl("\\(...)", df$Naam) & nchar(gsub("\\(...)", "", df$Naam))>3 |
                                       grepl("\\([a-zA-Z0-9])", df$Naam)==F & grepl("\\(.)", df$Naam) & nchar(gsub("\\(.)", "", df$Naam))>3, 
                                     "partly", df$Naam_unintelligible)
    df$Moeder_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Moeder) & nchar(gsub("\\(\\.\\.\\.)", "", df$Moeder))<=3 |
                                         grepl("\\([a-zA-Z0-9])", df$Moeder)==F & grepl("\\(.)", df$Moeder) & nchar(gsub("\\(.)", "", df$Moeder))<=3 |
                                         grepl("\\([a-zA-Z0-9]..)", df$Moeder)==F & grepl("\\(...)", df$Moeder) & nchar(gsub("\\(...)", "", df$Moeder))<=3 |
                                         grepl("\\([a-zA-Z0-9])", df$Moeder)==F & grepl("\\(.)", df$Moeder) & nchar(gsub("\\(.)", "", df$Moeder))<=3, 
                                       "fully", "")
    df$Moeder_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Moeder) & nchar(gsub("\\(\\.\\.\\.)", "", df$Moeder))>3 |
                                         grepl("\\([a-zA-Z0-9])", df$Moeder)==F & grepl("\\(.)", df$Moeder) & nchar(gsub("\\(.)", "", df$Moeder))>3 |
                                         grepl("\\([a-zA-Z0-9]..)", df$Moeder)==F & grepl("\\(...)", df$Moeder) & nchar(gsub("\\(...)", "", df$Moeder))>3 |
                                         grepl("\\([a-zA-Z0-9])", df$Moeder)==F & grepl("\\(.)", df$Moeder) & nchar(gsub("\\(.)", "", df$Moeder))>3, 
                                       "partly", df$Moeder_unintelligible)
    df$Moeder_2_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Moeder_2) & nchar(gsub("\\(\\.\\.\\.)", "", df$Moeder_2))<=3 |
                                           grepl("\\([a-zA-Z0-9])", df$Moeder_2)==F & grepl("\\(.)", df$Moeder_2) & nchar(gsub("\\(.)", "", df$Moeder_2))<=3 |
                                           grepl("\\([a-zA-Z0-9]..)", df$Moeder_2)==F & grepl("\\(...)", df$Moeder_2) & nchar(gsub("\\(...)", "", df$Moeder_2))<=3 |
                                           grepl("\\([a-zA-Z0-9])", df$Moeder_2)==F & grepl("\\(.)", df$Moeder_2) & nchar(gsub("\\(.)", "", df$Moeder_2))<=3, 
                                         "fully", "")
    df$Moeder_2_unintelligible <- ifelse(grepl("\\(\\.\\.\\.)", df$Moeder_2) & nchar(gsub("\\(\\.\\.\\.)", "", df$Moeder_2))>3 |
                                           grepl("\\([a-zA-Z0-9])", df$Moeder_2)==F & grepl("\\(.)", df$Moeder_2) & nchar(gsub("\\(.)", "", df$Moeder_2))>3 |
                                           grepl("\\([a-zA-Z0-9]..)", df$Moeder_2)==F & grepl("\\(...)", df$Moeder_2) & nchar(gsub("\\(...)", "", df$Moeder_2))>3 |
                                           grepl("\\([a-zA-Z0-9])", df$Moeder_2)==F & grepl("\\(.)", df$Moeder_2) & nchar(gsub("\\(.)", "", df$Moeder_2))>3, 
                                         "partly", df$Moeder_2_unintelligible)
    
   #export (...)
    #Naam
    x1 <- df[df$Naam_unintelligible!="",c("Naam", "Eigenaar", "source_order")]
    x1$Rol <- "Naam"
    colnames(x1) <- c("Naam", "Eigenaar", "Source_order", "Rol")
    x1 <- x1[c("Rol", "Naam", "Eigenaar", "Source_order")]
    #Moeder
    x2 <- df[df$Moeder_unintelligible!="",c("Moeder", "Eigenaar", "source_order")]
    x2$Rol <- "Moeder"
    colnames(x2) <- c("Naam", "Eigenaar", "Source_order", "Rol")
    x2 <- x2[c("Rol", "Naam", "Eigenaar", "Source_order")]
    #Moeder_2
    x3 <- df[df$Moeder_2_unintelligible!="",c("Moeder_2", "Eigenaar", "source_order")]
    x3$Rol <- "Moeder_entry"
    colnames(x3) <- c("Naam", "Eigenaar", "Source_order", "Rol")
    x3 <- x3[c("Rol", "Naam", "Eigenaar", "Source_order")]
    #bind and save
    x <- rbind(x1, x2, x3) %>% arrange(Source_order)
    write.xlsx(x, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Unintelligible.xlsx")
    #clean environment
    rm(x, x1, x2, x3)
    
   #replace (...)
    #Naam
    df$Naam <- gsub("\\(\\.\\.\\.)", "", df$Naam)
    df$Naam <- ifelse(grepl("\\([a-zA-Z0-9])", df$Naam)==F, gsub("\\(.)", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("\\(...)", df$Naam_original) & nchar(df$Naam)<=3 |
                        grepl("\\(.)", df$Naam_original) & nchar(df$Naam)<=3, "", df$Naam)
    #Moeder
    df$Moeder <- gsub("\\(\\.\\.\\.)", "", df$Moeder)
    df$Moeder <- gsub("\\(.)", "", df$Moeder)
    df$Moeder_2 <- gsub("\\(\\.\\.\\.)", "", df$Moeder_2)
    df$Moeder_2 <- gsub("\\(.)", "", df$Moeder_2)
    
    

  ################################################
  #### section 1: allign start and exit dates ####
  ################################################
    
    #add previous and next name
    df_x <- df %>% group_by(Serieregister, Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                              Naam_volgende=lead(Naam)) %>% ungroup()
    #add source_page
    df_x$source_page <- paste(df$Inventarisnummer, df$Folionummer, sep="_")
    
    #check for duplicates
    length(which(duplicated(df_x[,c("Serieregister", "Naam", "Naam_vorige", "Naam_volgende")])))
    #number of possibly duplicated folios
    df_folios <- df_x %>% 
      filter(in_event=="Start Series") %>%
      select(Serieregister, Naam_vorige, Naam, Naam_volgende, source_order, source_page) %>% 
      group_by(Serieregister, Naam_vorige, Naam, Naam_volgende) %>% 
      filter(n()>1 & !is.na(Naam_vorige) & !is.na(Naam_volgende)) %>% ungroup() %>% 
      group_by(source_page) %>%
      summarise(n=n()) %>%
      arrange(-n)
    as.data.frame(table(df_folios$n))
    
    #split series
    df1 <- df_x %>% 
      filter(in_event=="Start Series") %>%
      select(Serieregister, Naam_vorige, Naam, Naam_volgende, source_order) %>% 
      group_by(Serieregister, Naam_vorige, Naam, Naam_volgende) %>% 
      filter(n()>1 & !is.na(Naam_vorige) & !is.na(Naam_volgende)) %>% ungroup() %>% 
      arrange(Serieregister, Naam_vorige, Naam, Naam_volgende)
    df2 <- df_x %>% 
      filter(in_event=="Start Series") %>%
      select(Serieregister, Naam_vorige, Naam, Naam_volgende, source_order) %>% 
      group_by(Serieregister, Naam_vorige, Naam, Naam_volgende) %>% 
      filter(n()>1 & is.na(Naam_vorige) & !is.na(Naam_volgende)) %>% ungroup() %>% 
      arrange(Serieregister, Naam_vorige, Naam, Naam_volgende)
    df3 <- df_x %>% 
      filter(in_event=="Start Series") %>%
      select(Serieregister, Naam_vorige, Naam, Naam_volgende, source_order) %>% 
      group_by(Serieregister, Naam_vorige, Naam, Naam_volgende) %>% 
      filter(n()>1 & !is.na(Naam_vorige) & is.na(Naam_volgende)) %>% ungroup() %>% 
      arrange(Serieregister, Naam_vorige, Naam, Naam_volgende)
    
    
  ############################################################
  #### section 1a: splitting multiple names into families ####
  ############################################################
  
   #folio 1754 uit inventaris 4
    df[df$Inventarisnummer==4 & df$Folionummer==1754, c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar")]
    df$Naam[df$source_order=="041754a7731"] <- "Asetta"
    df$Moeder <- ifelse(df$source_order=="041754a7732" | df$source_order=="041754a7733" | df$source_order=="041754a7734", "Asetta", df$Moeder)
    df$Moeder <- ifelse(df$source_order=="041754a7735", "Betje", df$Moeder)
    
   #Fannij met hare kinderen: Adriaan en Zemire
    df[df$Naam=="Fannij met hare kinderen: Adriaan en Zemire", c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar")]
    #define Adriaan
    Adriaan <- df[df$Naam=="Fannij met hare kinderen: Adriaan en Zemire", ]
    Adriaan$source_order <- paste0(Adriaan$source_order, "_Y")
    Adriaan$Geslacht <- "Mannelijk"
    Adriaan$sex <- "male"
    #define Zemire
    Zemire <- df[df$Naam=="Fannij met hare kinderen: Adriaan en Zemire", ]
    Zemire$source_order <- paste0(Zemire$source_order, "_Z")
    #bind to df
    df <- rbind(df, Adriaan, Zemire) %>% arrange(source_order)
    rm(Adriaan, Zemire)
    #standardise record
    df$Naam[df$Naam=="Fannij met hare kinderen: Adriaan en Zemire"] <- "Fannij"
    
    
  ##########################################
  #### section 1b: remove false entries ####
  ##########################################
    
   #Remove Eigenaar: Kootwijk / HJ / a
    df[df$Inventarisnummer==41 & df$Folionummer==4818, c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar")]
    df <- df[df$source_order!="414818a143869",]
    
   #Combined: America (bij den heil doop gend ) & Jacoba Antoinetta
    df[df$Inventarisnummer==37 & df$Folionummer==786, c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar")]
    df <- df[df$source_order!="370786138660",]
    df$Naam <- ifelse(df$source_order=="370786138661", "America bij den heil doop genmd Jacoba Antoinetta", df$Naam)
    
   #Folio 05_2133 is a copy of 05_2037
   #Correct 05_2037
    df$Naam[df$source_order=="052037a16267"] <- "Charles"
   #Add Jacob
    df$source_order[df$source_order=="052133a9117"] <- "052037a16279"
   #Remove 05_2133
    df <- df[!(df$Inventarisnummer==5 & df$Folionummer==2133 & df$Folionummer!="052037a16279"),]
    
   #Folio 02_1589 contains pre-1830 data for 19_3129-3130
    #transfer age & birth_year_age_based
    df$year_birth_age_based[df$source_order=="193130b52314"] <- 1800
    df$age[df$source_order=="193130b52314"] <- 30
    df$year_birth_age_based[df$source_order=="193130b52315"] <- 1805
    df$age[df$source_order=="193130b52315"] <- 25
    df$year_birth_age_based[df$source_order=="193130b52316"] <- 1800
    df$age[df$source_order=="193130b52316"] <- 30
    df$year_birth_age_based[df$source_order=="193130b52317"] <- 1798
    df$age[df$source_order=="193130b52317"] <- 32
    df$year_birth_age_based[df$source_order=="193130b52318"] <- 1800
    df$age[df$source_order=="193130b52318"] <- 30
    df$year_birth_age_based[df$source_order=="193130b52319"] <- 1802
    df$age[df$source_order=="193130b52319"] <- 28
    df$year_birth_age_based[df$source_order=="193131a72017"] <- 1808
    df$age[df$source_order=="193131a72017"] <- 22
    df$year_birth_age_based[df$source_order=="193131a72018"] <- 1795
    df$age[df$source_order=="193131a72018"] <- 35
    df$year_birth_age_based[df$source_order=="193131a72019"] <- 1810
    df$age[df$source_order=="193131a72019"] <- 20
    df$year_birth_age_based[df$source_order=="193131a72020"] <- 1808
    df$age[df$source_order=="193131a72019"] <- 22
    df$year_birth_age_based[df$source_order=="193131a72021"] <- 1805
    df$age[df$source_order=="193131a72021"] <- 25
    df$year_birth_age_based[df$source_order=="193131a72022"] <- 1828
    df$age[df$source_order=="193131a72022"] <- 2
    df$year_birth_age_based[df$source_order=="193131a72023"] <- 1829
    df$age[df$source_order=="193131a72023"] <- 1
    df$year_birth_age_based[df$source_order=="193131a72024"] <- 1830
    df$age[df$source_order=="193131a72024"] <- 0
    #add former owner
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52314"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52315"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52316"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52317"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52318"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193130b52319"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72017"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72018"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72019"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72020"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72021"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72022"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72023"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    df$Aanvullendeinformatieinschrijv[df$source_order=="193131a72024"] <- "Op den 1e december 1828 aan plantage Kent geaccrocheerd"
    #delete 02_1589
    df <- df[!(df$Inventarisnummer=="2" & df$Folionummer==1589),]
    
  #merge Jan & Cornelius on 06_137 into Jan Cornelius
    df <- df[df$source_order!="060137a11292", ]
    df$Naam[df$source_order=="060137a11291"] <- "Jan Cornelius"
    
    
  ###########################################
  #### section 1c: correct starting date ####
  ###########################################
    
  #Folio 39_2363 are transferred from 27_1497
    df[df$Inventarisnummer=="27" & df$Folionummer==1497, c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar", "in_event", "year_entry", "out_event", "year_exit")]
    df[df$Inventarisnummer=="39" & df$Folionummer==2363, c("Serieregister", "Inventarisnummer", "Folionummer", "source_order", "Naam", "Moeder", "Eigenaar", "in_event", "year_entry", "out_event", "year_exit")]
    
    df$Aanvullendeinformatieinschrijv[df$Inventarisnummer=="39" & df$Folionummer==2363] <- "alhier van het Hoofd van J H F Worsdell pr. en n ux overgebragt, volgens voorstaande dd 20 Maart 1855 Zie Fo 1497"
    df$in_event[df$Inventarisnummer=="39" & df$Folionummer==2363] <- "Acquired/Transferred"
    df$day_entry[df$Inventarisnummer=="39" & df$Folionummer==2363] <- 20
    df$month_entry[df$Inventarisnummer=="39" & df$Folionummer==2363] <- 3
    df$year_entry[df$Inventarisnummer=="39" & df$Folionummer==2363] <- 1855
    
  #Folio 5_2206 are transferred from 1_902
    df$in_event[df$Inventarisnummer==5 & df$Folionummer==2206] <- "Acquired/Inherited For Freedom"
    df$year_entry[df$Inventarisnummer==5 & df$Folionummer==2206] <- (-1)
    
  #Folio 12_2565-1266 are transferred from 9_1351
    df$in_event[df$Inventarisnummer==12 & df$Folionummer==2565 & df$primary_key>=40336 & df$primary_key<=40346] <- "Acquired/Transferred"
    df$year_entry[df$Inventarisnummer==12 & df$Folionummer==2565 & df$primary_key>=40336 & df$primary_key<=40346] <- (-1)
    
    
  #Folios 42_5139-5140 are transferred from 19_3087-3089
    #set in_event 42_5139-42_5140 to inherited
    df$in_event <- ifelse(df$Inventarisnummer=="42" & df$Folionummer==5139 & df$in_event!="Birth" |
                            df$Inventarisnummer=="42" & df$Folionummer==5140 & df$in_event!="Birth", "Inherited", df$in_event)
    df$in_event <- ifelse(df$Inventarisnummer=="42" & df$Folionummer==5139 & df$in_event=="Birth" & df$year_entry<1834 |
                            df$Inventarisnummer=="42" & df$Folionummer==5140 & df$in_event=="Birth" & df$year_entry<1834, "Inherited", df$in_event)
    #set date of entry to 1834
    df$day_entry <- ifelse(df$Inventarisnummer=="42" & df$Folionummer==5139 & df$in_event=="Inherited" |
                             df$Inventarisnummer=="42" & df$Folionummer==5140 & df$in_event=="Inherited", -1, df$day_entry)
    df$month_entry <- ifelse(df$Inventarisnummer=="42" & df$Folionummer==5139 & df$in_event=="Inherited" |
                               df$Inventarisnummer=="42" & df$Folionummer==5140 & df$in_event=="Inherited", -1, df$month_entry)
    df$year_entry <- ifelse(df$Inventarisnummer=="42" & df$Folionummer==5139 & df$in_event=="Inherited" |
                              df$Inventarisnummer=="42" & df$Folionummer==5140 & df$in_event=="Inherited", 1834, df$year_entry)
    #transfer sold enslaved >=1834
    df$out_event[df$source_order=="425140a147652"] <- "Sold/Given for Freedom"
    df$year_exit[df$source_order=="425140a147652"] <- 1835
    df$month_exit[df$source_order=="425140a147652"] <- 4
    df$day_exit[df$source_order=="425140a147652"] <- 9
    #transfer deaths >=1834
    #Fransisko
    df$out_event[df$source_order=="425139a148299"] <- "Death"
    df$year_exit[df$source_order=="425139a148299"] <- 1837
    df$month_exit[df$source_order=="425139a148299"] <- 6
    df$day_exit[df$source_order=="425139a148299"] <- 17
    #Jantje
    df$out_event[df$source_order=="425139b147459"] <- "Death"
    df$year_exit[df$source_order=="425139b147459"] <- 1834
    df$month_exit[df$source_order=="425139b147459"] <- 6
    df$day_exit[df$source_order=="425139b147459"] <- 31
    #Bonheur
    df$out_event[df$source_order=="425139b147460"] <- "Death"
    df$year_exit[df$source_order=="425139b147460"] <- 1837
    df$month_exit[df$source_order=="425139b147460"] <- 3
    df$day_exit[df$source_order=="425139b147460"] <- 27
    #Cicilia
    df$out_event[df$source_order=="425139b147472"] <- "Death"
    df$year_exit[df$source_order=="425139b147472"] <- 1837
    df$month_exit[df$source_order=="425139b147472"] <- 8
    df$day_exit[df$source_order=="425139b147472"] <- 22
    #Saratje
    df$out_event[df$source_order=="425139b147474"] <- "Death"
    df$year_exit[df$source_order=="425139b147474"] <- 1838
    df$month_exit[df$source_order=="425139b147474"] <- 6
    df$day_exit[df$source_order=="425139b147474"] <- 13
    #Flora
    df$out_event[df$source_order=="425139b147478"] <- "Death"
    df$year_exit[df$source_order=="425139b147478"] <- 1834
    df$month_exit[df$source_order=="425139b147478"] <- 10
    df$day_exit[df$source_order=="425139b147478"] <- 27
    #Princessa
    df$out_event[df$source_order=="425139b147481"] <- "Death"
    df$year_exit[df$source_order=="425139b147481"] <- 1834
    df$month_exit[df$source_order=="425139b147481"] <- 8
    df$day_exit[df$source_order=="425139b147481"] <- 10
    #Amimba
    df$out_event[df$source_order=="425139b147483"] <- "Death"
    df$year_exit[df$source_order=="425139b147483"] <- 1834
    df$month_exit[df$source_order=="425139b147483"] <- 12
    df$day_exit[df$source_order=="425139b147483"] <- 20
    #transfer birth >=1834
    df$out_event[df$source_order=="193089a50937"] <- "End Series"
    df$out_event[df$source_order=="193089a50938"] <- "End Series"
    df$source_order[df$source_order=="193089a50937"] <- "425140a147668"
    df$source_order[df$source_order=="193089a50938"] <- "425140a147669"
    #set year_exit 19_3087-3089 to 1834
    #Overgeschreven
    df$day_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Overgeschreven" |
                            df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Overgeschreven" |
                            df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Overgeschreven", -1, df$day_exit)
    df$month_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Overgeschreven" |
                              df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Overgeschreven" |
                              df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Overgeschreven", -1, df$month_exit)
    df$year_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Overgeschreven" |
                             df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Overgeschreven" |
                             df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Overgeschreven", 1834, df$year_exit)
    #Death
    df$day_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Death" & df$year_exit>=1834 |
                            df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Death" & df$year_exit>=1834 |
                            df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Death" & df$year_exit>=1834, -1, df$day_exit)
    df$month_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Death" & df$year_exit>=1834 |
                              df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Death" & df$year_exit>=1834 |
                              df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Death" & df$year_exit>=1834, -1, df$month_exit)
    df$year_exit <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Death" & df$year_exit>=1834 |
                             df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Death" & df$year_exit>=1834 |
                             df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Death" & df$year_exit>=1834, 1834, df$year_exit)
    df$out_event <- ifelse(df$Inventarisnummer==19 & df$Folionummer==3087 & df$out_event=="Death" & df$year_exit>=1834 |
                             df$Inventarisnummer==19 & df$Folionummer==3088 & df$out_event=="Death" & df$year_exit>=1834 |
                             df$Inventarisnummer==19 & df$Folionummer==3089 & df$out_event=="Death" & df$year_exit>=1834, "Overgeschreven", df$out_event)
    
    
   #Folios 29_515-521 are transferred from 32_1860-1866
    #set in_event 29_515-521 to Acquired/Transferred
    df$in_event[df$Inventarisnummer==29 & df$Folionummer==515 & df$primary_key>=118534 |
                  df$Inventarisnummer==29 & df$Folionummer==516 |
                  df$Inventarisnummer==29 & df$Folionummer==517 |
                  df$Inventarisnummer==29 & df$Folionummer==518 |
                  df$Inventarisnummer==29 & df$Folionummer==519 |
                  df$Inventarisnummer==29 & df$Folionummer==520 |
                  df$Inventarisnummer==29 & df$Folionummer==521 & df$primary_key<=118656] <- "Acquired/Transferred"
    #set date of entry to 1834
    df$day_entry[df$Inventarisnummer==29 & df$Folionummer==515 & df$primary_key>=118534 |
                   df$Inventarisnummer==29 & df$Folionummer==516 |
                   df$Inventarisnummer==29 & df$Folionummer==517 |
                   df$Inventarisnummer==29 & df$Folionummer==518 |
                   df$Inventarisnummer==29 & df$Folionummer==519 |
                   df$Inventarisnummer==29 & df$Folionummer==520 |
                   df$Inventarisnummer==29 & df$Folionummer==521 & df$primary_key<=118656] <- 11
    df$month_entry[df$Inventarisnummer==29 & df$Folionummer==515 & df$primary_key>=118534 |
                     df$Inventarisnummer==29 & df$Folionummer==516 |
                     df$Inventarisnummer==29 & df$Folionummer==517 |
                     df$Inventarisnummer==29 & df$Folionummer==518 |
                     df$Inventarisnummer==29 & df$Folionummer==519 |
                     df$Inventarisnummer==29 & df$Folionummer==520 |
                     df$Inventarisnummer==29 & df$Folionummer==521 & df$primary_key<=118656] <- 3
    df$year_entry[df$Inventarisnummer==29 & df$Folionummer==515 & df$primary_key>=118534 |
                    df$Inventarisnummer==29 & df$Folionummer==516 |
                    df$Inventarisnummer==29 & df$Folionummer==517 |
                    df$Inventarisnummer==29 & df$Folionummer==518 |
                    df$Inventarisnummer==29 & df$Folionummer==519 |
                    df$Inventarisnummer==29 & df$Folionummer==520 |
                    df$Inventarisnummer==29 & df$Folionummer==521 & df$primary_key<=118656] <- 1851
    
   #Multiple enslaved from folios 38_1868-1869 are transferred from 30_1079-1080
    #set in_event 38_1868-1869 to Acquired/Transferred
    df$in_event[df$Inventarisnummer==38 & df$Folionummer==1868 |
                  df$Inventarisnummer==38 & df$Folionummer==1869 & df$primary_key<=158388] <- "Acquired/Transferred"
    #set date of entry to 1834
    df$day_entry[df$Inventarisnummer==38 & df$Folionummer==1868 |
                   df$Inventarisnummer==38 & df$Folionummer==1869 & df$primary_key<=158387] <- 6
    df$month_entry[df$Inventarisnummer==38 & df$Folionummer==1868 |
                     df$Inventarisnummer==38 & df$Folionummer==1869 & df$primary_key<=158387] <- 1
    df$year_entry[df$Inventarisnummer==38 & df$Folionummer==1868 |
                    df$Inventarisnummer==38 & df$Folionummer==1869 & df$primary_key<=158387] <- 1852
    
  #Multiple enslaved from folio 38_1913 are transferred from 24_353
    #set in_event 38_1913 to Acquired/Transferred
    df$in_event[df$source_order=="381913146620" |
                  df$source_order=="381913146621" |
                  df$source_order=="381913146622" |
                  df$source_order=="381913146623" |
                  df$source_order=="381913146624"] <- "Acquired/Transferred"
    #set date 38_1913 to 31 dec 1851
    df$day_entry[df$source_order=="381913146620" |
                   df$source_order=="381913146621" |
                   df$source_order=="381913146622" |
                   df$source_order=="381913146623" |
                   df$source_order=="381913146624"] <- 31
    df$month_entry[df$source_order=="381913146620" |
                     df$source_order=="381913146621" |
                     df$source_order=="381913146622" |
                     df$source_order=="381913146623" |
                     df$source_order=="381913146624"] <- 12
    df$year_entry[df$source_order=="381913146620" |
                    df$source_order=="381913146621" |
                    df$source_order=="381913146622" |
                    df$source_order=="381913146623" |
                    df$source_order=="381913146624"] <- 1851
    
  #Multiple enslaved from folio 38_1919 are transferred from 27_1522
    #set in_event 38_1919 to Acquired/Transferred
    df$in_event[df$source_order=="381919157604" |
                  df$source_order=="381919157605" |
                  df$source_order=="381919157606" |
                  df$source_order=="381919157607"] <- "Acquired/Transferred"
    #set date 38_1913 to 31 jan 1852
    df$day_entry[df$source_order=="381919157604" |
                   df$source_order=="381919157605" |
                   df$source_order=="381919157606" |
                   df$source_order=="381919157607"] <- 31
    df$month_entry[df$source_order=="381919157604" |
                     df$source_order=="381919157605" |
                     df$source_order=="381919157606" |
                     df$source_order=="381919157607"] <- 1
    df$year_entry[df$source_order=="381919157604" |
                    df$source_order=="381919157605" |
                    df$source_order=="381919157606" |
                    df$source_order=="381919157607"] <- 1852
    
  #Enslaved from folio 38_1905 are transferred from 38_1696
    #set in_event 38_1905 to Acquired/Transferred
    df$in_event[df$Inventarisnummer==38 & df$Folionummer==1905] <- "Acquired/Transferred"
    #set date 38_1905 to 6 dec 1851
    df$day_entry[df$Inventarisnummer==38 & df$Folionummer==1905] <- 6
    df$month_entry[df$Inventarisnummer==38 & df$Folionummer==1905] <- 12
    df$year_entry[df$Inventarisnummer==38 & df$Folionummer==1905] <- 1851
    
  #Multiple enslaved from folio 38_1891 are transferred from 26_1139
    #set in_event 38_1891 to Acquired/Transferred
    df$in_event[df$source_order=="381891140242" |
                  df$source_order=="381891140243" |
                  df$source_order=="381891140244"] <- "Acquired/Transferred"
    #set date 38_1913 4 nov 1851
    df$day_entry[df$source_order=="381891140242" |
                   df$source_order=="381891140243" |
                   df$source_order=="381891140244"] <- 4
    df$month_entry[df$source_order=="381891140242" |
                     df$source_order=="381891140243" |
                     df$source_order=="381891140244"] <- 11
    df$year_entry[df$source_order=="381891140242" |
                    df$source_order=="381891140243" |
                    df$source_order=="381891140244"] <- 1851
    
    
    
  ##################################
  #### section 2: cleaning Naam ####
  ##################################
  
  #clean random patterns & specific entries
   #remove remark entry
    df <- df[df$Naam!="Zie Fo IV999",]
   #...De in names
    df$Naam <- ifelse(grepl("[a-z]De", df$Naam), gsub("De", "de", df$Naam), df$Naam)
   #...La in names
    df$Naam <- ifelse(grepl("[a-z]La", df$Naam), gsub("La", "la", df$Naam), df$Naam)
   #fix nieuwjaar
    df$Naam <- ifelse(grepl("n", tolower(df$Naam)) &  grepl("jaar", tolower(df$Naam)), "Nieuwjaar", df$Naam)
   #5Anthonette
    df$Naam <- gsub("5Anthonette", "Anthonette", df$Naam)
   #Venus IIvberleden
    df$Naam <- gsub("Venus IIvberleden", "Venus II", df$Naam)
    
    
 #explore Naam
  #numericals
   #I-IV
    as.data.frame(table(df[grepl(" I", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("/I", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("/ I", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("\\(I", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("\\( I", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
   #V-VIII
    as.data.frame(table(df[grepl(" V", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("/V", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("/ V", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("\\(V", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
    as.data.frame(table(df[grepl("\\( V", df$Naam) & grepl(" V[a-zA-z]", df$Naam)==F, "Naam"]))
   #1-9
    as.data.frame(table(df[grepl("[0-9]", df$Naam), "Naam"]))
   #written-out numericals
    as.data.frame(table(df[grepl("eerst", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("twee", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("drie", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("derde", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("vier", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("vijf", tolower(df$Naam)), "Naam"]))
  #variations on "no"
   #no
    as.data.frame(table(df[grepl(" no", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/no", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/ no", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("\\(no", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("\\( no", tolower(df$Naam)), "Naam"]))
   #nr
    as.data.frame(table(df[grepl(" nr", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/nr", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/ nr", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("\\(nr", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("\\( nr", tolower(df$Naam)), "Naam"]))
   #n
    as.data.frame(table(df[grepl(" n ", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl(" ni", tolower(df$Naam)), "Naam"]))
  #descriptions
   #dik
    as.data.frame(table(df[grepl("dik", tolower(df$Naam)), "Naam"]))
   #mooi
    as.data.frame(table(df[grepl("mooi", tolower(df$Naam)), "Naam"]))
   #klein
    as.data.frame(table(df[grepl("klein [a-zA-z]", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl(" klein", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("kl. ", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("kl ", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/kl", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("kl", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("kn", tolower(df$Naam)), "Naam"]))
   #kort
    as.data.frame(table(df[grepl("kort", tolower(df$Naam)), "Naam"]))
   #groot
    as.data.frame(table(df[grepl("groot", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("gr. ", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("gr ", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("/gr", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("gt", tolower(df$Naam)), "Naam"]))
   #lange
    as.data.frame(table(df[grepl("lang", tolower(df$Naam)), "Naam"]))
   #long
    as.data.frame(table(df[grepl("big", tolower(df$Naam)), "Naam"]))
   #little
    as.data.frame(table(df[grepl("lit", tolower(df$Naam)), "Naam"]))
   #long
    as.data.frame(table(df[grepl("long", tolower(df$Naam)), "Naam"]))
   #junior
    as.data.frame(table(df[grepl("junior", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl(" jr", tolower(df$Naam)), "Naam"]))
   #senior
    as.data.frame(table(df[grepl("senior", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl(" sr", tolower(df$Naam)), "Naam"]))
   #jonge
    as.data.frame(table(df[grepl("jong", tolower(df$Naam)), "Naam"]))
   #oude
    as.data.frame(table(df[grepl("ouw", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("oud", tolower(df$Naam)), "Naam"]))
   #nieuw
    as.data.frame(table(df[grepl("nieuw", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("nw", tolower(df$Naam)), "Naam"]))
   #roode
    as.data.frame(table(df[grepl("rood", tolower(df$Naam)), "Naam"]))
   #swart / zwart
    as.data.frame(table(df[grepl("swart", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("sw", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("zwart", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl("zw", tolower(df$Naam)), "Naam"]))
   #black
    as.data.frame(table(df[grepl("black", tolower(df$Naam)), "Naam"]))
   #neger
    as.data.frame(table(df[grepl("neger", tolower(df$Naam)), "Naam"]))
   #mulat
    as.data.frame(table(df[grepl("mulat", tolower(df$Naam)), "Naam"]))
   #creool
    as.data.frame(table(df[grepl("creo", tolower(df$Naam)), "Naam"]))
   #remaining / (
    as.data.frame(table(df[grepl("/", tolower(df$Naam)), "Naam"]))
   #/m/
    as.data.frame(table(df[grepl("/m/", tolower(df$Naam)), "Naam"]))
   #twee namen
    as.data.frame(table(df[grepl(" alias", tolower(df$Naam)), "Naam"]))
    as.data.frame(table(df[grepl(" ook", tolower(df$Naam)), "Naam"]))
    
   
  #standardise entries
   #0f -> of
    df$Naam <- gsub("0f", "of", df$Naam)
   #fix /  /
    df$Naam <- ifelse(grepl("[a-zA-Z]/I", df$Naam), gsub("/I", " I", df$Naam), df$Naam) #change Louis/II/ to Louis II/
    df$Naam <- gsub(" /I", " I", df$Naam) #change Louis /II/ to Louis II/
    df$Naam <- ifelse(grepl(" / I[a-z]", df$Naam)==F, gsub( " / I", " I", df$Naam), df$Naam) #change Louis / II / to Louis II /
    df$Naam <- gsub("/ I", " I", df$Naam) #change Louis/ II/ to Louis II
    df$Naam <- gsub("I /", "I", df$Naam)
    df$Naam <- gsub("I/", "I", df$Naam)
    df$Naam <- gsub("V /", "V", df$Naam)
    df$Naam <- gsub("V/", "V", df$Naam)
   #fix ()
    df$Naam <- ifelse(grepl("[a-zA-Z]\\(I", df$Naam), gsub("\\(I", " I", df$Naam), df$Naam) #change Louis(II) to Louis II)
    df$Naam <- gsub(" \\(I", " I", df$Naam) #change Louis (II) to Louis II)
    df$Naam <- gsub(" \\( I", " I", df$Naam) #change Louis ( II ) to Louis II )
    df$Naam <- gsub("\\( I", " I", df$Naam) #change Louis( II ) to Louis II )
    df$Naam <- gsub("I)", "I", df$Naam)
   #I?
    df$Naam <- gsub("I\\?", "I", df$Naam)
   #I,
    df$Naam <- gsub("I,", "I", df$Naam)
   #I.
    df$Naam <- gsub("I\\.", "I", df$Naam)
   #Io
    df$Naam <- gsub(" Io", " I", df$Naam)
   #fix I-E
    df$Naam <- gsub("I-E", "I", df$Naam)
    df$Naam <- gsub("V-E", "V", df$Naam)
    df$Naam <- gsub("Iden", "I", df$Naam)
    df$Naam <- gsub("Vden", "V", df$Naam)
    df$Naam <- gsub("Ide", "I", df$Naam)
    df$Naam <- gsub("Vde", "V", df$Naam)
    df$Naam <- ifelse(grepl("Ie", df$Naam) & grepl("Ie[a-zA-Z]", df$Naam)==F, gsub("Ie", "I", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Ve", df$Naam) & grepl("Ve[a-zA-Z]", df$Naam)==F, gsub("Ve", "V", df$Naam), df$Naam)
   #set l to I
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))==" ll", gsub(" ll", " II", df$Naam), df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" l", gsub(" l", " I", df$Naam), df$Naam)
   #remove no
    df$Naam <- ifelse(grepl(" noI", df$Naam), gsub("no", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" no ", df$Naam), gsub(" no", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" NoI", df$Naam), gsub("No", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" No ", df$Naam), gsub(" No", "", df$Naam), df$Naam)
   #remove /no
    df$Naam <- ifelse(grepl(" /noI", df$Naam), gsub("/no", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /no ", df$Naam), gsub(" /no", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /NoI", df$Naam), gsub("/No", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /No ", df$Naam), gsub(" /No", "", df$Naam), df$Naam)
   #remove nr
    df$Naam <- ifelse(grepl(" nrI", df$Naam), gsub("nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" nr ", df$Naam), gsub(" nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" NrI", df$Naam), gsub("Nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" Nr ", df$Naam), gsub(" Nr", "", df$Naam), df$Naam)
   #remove /nr
    df$Naam <- ifelse(grepl(" /nrI", df$Naam), gsub("/nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /nr ", df$Naam), gsub(" /nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /NrI", df$Naam), gsub("/Nr", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" /Nr ", df$Naam), gsub(" /Nr", "", df$Naam), df$Naam)
   #remove n
    df$Naam <- ifelse(grepl(" n I", df$Naam), gsub(" n", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" NI", df$Naam), gsub(" N", " ", df$Naam), df$Naam)
   #standardise dik
    df$Naam <- gsub(" dikke", " dik", df$Naam)
    df$Naam <- gsub("Willemdik", "Willem dik", df$Naam)
   #standardise klein
    df$Naam <- gsub("Klijn", "klein", df$Naam)
    df$Naam <- gsub("klijn", "klein", df$Naam)
    df$Naam <- ifelse(df$Naam=="Klein" | df$Naam=="Kleintje", df$Naam, gsub("Klein", "klein", df$Naam))
    df$Naam <- gsub(", klein", " klein", df$Naam)
    df$Naam <- gsub(",klein", " klein", df$Naam)
    df$Naam <- gsub("/ klein", " klein", df$Naam)
    df$Naam <- gsub("/klein", " klein", df$Naam)
    df$Naam <- gsub("\\( klein", " klein", df$Naam)
    df$Naam <- gsub("\\(klein", " klein", df$Naam)
    df$Naam <- gsub(" KL", " klein ", df$Naam)
    df$Naam <- gsub("Kl. ", "klein ", df$Naam)
    df$Naam <- gsub("kl. ", "klein ", df$Naam)
    df$Naam <- gsub("Kl ", "klein ", df$Naam)
    df$Naam <- gsub("kl ", "klein ", df$Naam)
    df$Naam <- gsub("/Kl", " klein", df$Naam)
    df$Naam <- gsub("/kl", " klein", df$Naam)
    df$Naam <- gsub("\\(kl:)", "klein", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))=="Kl", gsub("Kl", "klein", df$Naam), df$Naam) #blabla Kl
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))=="kl", gsub("kl", "klein", df$Naam), df$Naam) #blabla kl
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Kl:", gsub("Kl:", "klein", df$Naam), df$Naam) #blabla Kl:
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="kl:", gsub("kl:", "klein", df$Naam), df$Naam) #blabla kl:
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Kl.", gsub("Kl\\.", "klein", df$Naam), df$Naam) #blabla Kl.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="kl.", gsub("kl\\.", "klein", df$Naam), df$Naam) #blabla kl.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Kl/", gsub("Kl/", "klein", df$Naam), df$Naam) #blabla Kl.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="kl/", gsub("kl/", "klein", df$Naam), df$Naam) #blabla kl.
    df$Naam <- gsub("Kn ", "klein ", df$Naam)
   #groot
    df$Naam <- gsub("Gr:", "groote ", df$Naam)
    df$Naam <- gsub("gr:", "groote ", df$Naam)
    df$Naam <- gsub("Gr ", "groote ", df$Naam)
    df$Naam <- gsub("gr ", "groote ", df$Naam)
    df$Naam <- gsub("\\(groot", " groote", df$Naam)
    df$Naam <- gsub("Groote", "groote", df$Naam)
    df$Naam <- gsub("Groot ", "groote ", df$Naam)
    df$Naam <- gsub("/groote", " groote", df$Naam)
    df$Naam <- gsub("Gr. ", "groote ", df$Naam)
    df$Naam <- gsub("gr. ", "groote ", df$Naam)
    df$Naam <- gsub("gr ", "groote ", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))=="Gr", gsub("Gr", "groote", df$Naam), df$Naam) #blabla Gr
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))=="gr", gsub("gr", "groote", df$Naam), df$Naam) #blabla gr
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Gr:", gsub("Gr:", "groote", df$Naam), df$Naam) #blabla Gr:
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="gr:", gsub("gr:", "groote", df$Naam), df$Naam) #blabla gr:
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Gr.", gsub("Gr\\.", "groote", df$Naam), df$Naam) #blabla Gr.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="gr.", gsub("gr\\.", "groote", df$Naam), df$Naam) #blabla gr.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Gr/", gsub("Gr/", "groote", df$Naam), df$Naam) #blabla Gr.
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="gr/", gsub("gr/", "groote", df$Naam), df$Naam) #blabla gr.
    df$Naam <- gsub("/groote", " groote", df$Naam)
    df$Naam <- gsub("Gt", " groote", df$Naam)
    df$Naam <- gsub(" gt", " groote", df$Naam)
    df$Naam <- gsub("qt", " groote", df$Naam)
   #lange
    df$Naam <- gsub("Lange", "lange", df$Naam)
   #little
    df$Naam <- gsub("Little", "little", df$Naam)
    df$Naam <- gsub("Littel", "little", df$Naam)
    df$Naam <- gsub("littel", "little", df$Naam)
   #junior
    df$Naam <- gsub("Jr", "junior", df$Naam)
   #senior
    df$Naam <- gsub("Senior", "senior", df$Naam)
    df$Naam <- ifelse(grepl("Sr[a-z]", df$Naam)==F, gsub("Sr", "senior", df$Naam), df$Naam)
   #jonge
    df$Naam <- ifelse(grepl("Jonge[a-zA-Z]", df$Naam)==F, gsub("Jonge", "jonge", df$Naam), df$Naam)
    df$Naam <- gsub(" /J/", " jonge", df$Naam)
    df$Naam <- gsub("/jonge", "jonge", df$Naam)
    df$Naam <- gsub(" / jonge", " jonge", df$Naam)
    df$Naam <- gsub("/ jonge", " jonge", df$Naam)
   #oude
    df$Naam <- gsub("Oude Charlotte", "oude Charlotte", df$Naam)
    df$Naam <- gsub("Oude", "oude", df$Naam)
    df$Naam <- gsub("\\(o)", "oude", df$Naam)
    df$Naam <- gsub(" / o /", " oude", df$Naam)
    df$Naam <- gsub("/ oud", "oud", df$Naam)
    df$Naam <- gsub("/oud", "oud", df$Naam)
   #nieuw
    df$Naam <- gsub("Nw", "nieuw", df$Naam)
   #swart
    df$Naam <- ifelse(grepl("sw[a-zA-Z]", tolower(df$Naam))==F, gsub("sw", "zwart", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("sw[a-zA-Z]", tolower(df$Naam))==F, gsub("Sw", "zwart", df$Naam), df$Naam)
   #zwart
    df$Naam <- ifelse(grepl("zw[a-zA-Z]", tolower(df$Naam))==F, gsub("zw", "zwart", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("zw[a-zA-Z]", tolower(df$Naam))==F, gsub("Zw", "zwart", df$Naam), df$Naam)
   #mulat
    df$Naam <- gsub(" Mulat", " mulat", df$Naam)
    df$Naam <- gsub("mulatt", "mulat", df$Naam)
  #deal with abbreviations
    as.data.frame(table(df[grepl(":", df$Naam), "Naam"]))
    as.data.frame(table(df[grepl("\\.", df$Naam), "Naam"]))
   #plaatsen
    #Resolutie
    df$Naam <- gsub("Res:", "Resolutie", df$Naam)
    df$Naam <- gsub("Res\\.", "Resolutie", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))=="Res", gsub("Res", "Resolutie", df$Naam), df$Naam)
    df$Naam <- gsub("Resol:", "Resolutie", df$Naam)
    df$Naam <- gsub("Resol\\.", "Resolutie", df$Naam)
    df$Naam <- gsub("Revolutie", "Resolutie", df$Naam)
    #Stolkwijk
    df$Naam <- gsub("Stolkw:", "Stolkwijk", df$Naam)
    df$Naam <- gsub("Stolkw\\.", "Stolkwijk", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-5, nchar(df$Naam))=="Stolkw", gsub("Stolkw", "Stolkwijk", df$Naam), df$Naam)
    df$Naam <- gsub("Stolw", "Stolkwijk", df$Naam)
    df$Naam <- gsub("Stw", "Stolkwijk", df$Naam)
    #Zeewijk
    df$Naam <- gsub("Zeew:", "Zeewijk", df$Naam)
    df$Naam <- gsub("Zeew\\.", "Zeewijk", df$Naam)
    df$Naam <- gsub("Zn", "Zeewijk", df$Naam)
    df$Naam <- gsub("Zu\\.", "Zeewijk", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-3, nchar(df$Naam))=="Zeew", gsub("Zeew", "Zeewijk", df$Naam), df$Naam)
    #L en R
    df$Naam <- gsub("L R", "LR", df$Naam)
    df$Naam <- gsub("L en R", "LR", df$Naam)
   #namen
    df$Naam <- gsub("Alex\\.", "Alexander", df$Naam)
    df$Naam <- gsub("Carol:", "Carolina", df$Naam)
    df$Naam <- gsub("Carol\\.", "Carolina", df$Naam)
    df$Naam <- gsub("Charl:", "Charlotte", df$Naam)
    df$Naam <- gsub("Charl\\.", "Charlotte", df$Naam)
    df$Naam <- gsub("Elis:", "Elisabeth", df$Naam)
    df$Naam <- gsub("Elis\\.", "Elisabeth", df$Naam)
    df$Naam <- gsub("Els:", "Elisabeth", df$Naam)
    df$Naam <- gsub("Els\\.", "Elisabeth", df$Naam)
    df$Naam <- gsub("Hendr:", "Hendrika", df$Naam)
    df$Naam <- gsub("Hendr\\.", "Hendrika", df$Naam)
    df$Naam[df$Naam=="John / Robn. /"] <- "John of Robijn"
    df$Naam <- gsub("Theod\\.", "Theodorus", df$Naam)
    df$Naam <- gsub("V: d: Meer", "v d Meer", df$Naam)
    df$Naam <- gsub("Wilhelmina Barth:", "Wilhelmina Bartholomina", df$Naam)
    df$Naam <- gsub("Wilhelmina Barth\\.", "Wilhelmina Bartholomina", df$Naam)
    #geen afkortingen
    df$Naam <- gsub("Jan\\.", "Jan", df$Naam)
    df$Naam <- gsub("Louisa\\.", "Louisa", df$Naam)
    df$Naam[df$Naam=="John. Bull"] <- "John Bull"
    #Namen + info
    df$Naam[df$Naam=="Lodewijk. BDHD. Johs. Dd-Lodewijk"] <- "Lodewijk"
    df$Naam[df$Naam=="Prinkes Van De Wed. Lemmers"] <- "Prinkes"
    df$Naam[df$Naam=="Roos / P.J. Van /"] <- "PJ van Roos"
   #rendundante tekens
    df$Naam <- gsub("Cato:", "Cato", df$Naam)
    df$Naam <- gsub("N\\.assau", "Nassau", df$Naam)
    df$Naam <- gsub("Primo\\.", "Primo", df$Naam)
    df$Naam <- gsub("Simika . Simika", "Simika", df$Naam)
    df$Naam <- gsub("Mk:", "Mc", df$Naam)
    df$Naam <- gsub("Mk\\.", "Mc", df$Naam)
    df$Naam <- gsub("mk:", "Mc", df$Naam)
    df$Naam <- gsub("mk\\.", "Mc", df$Naam)
    df$Naam <- gsub(" . idem", "", df$Naam)
   #voorletters
    #St
    df$Naam <- gsub("St\\.\\.", "St", df$Naam)
    df$Naam <- gsub("St\\.", "St", df$Naam)
    #v.
    df$Naam <- gsub("v\\. ", "v ", df$Naam)
   #delete .
    df$Naam <- gsub("\\.", "", df$Naam)
   #bij den
    df$Naam <- gsub("bij den Doop genaamd", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den Doop", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den H D genmd", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den H D", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den doop genaamd", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den H doop", "gedoopt", df$Naam)
    df$Naam <- gsub("bij den heil doop genmd", "gedoopt", df$Naam)
    df$Naam <- ifelse(grepl(" bij ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
  #tweede naam
    df$Naam <- ifelse(grepl("of[A-Z]", df$Naam), gsub("of", "of ", df$Naam), df$Naam)
    df$Naam <- gsub("/ Alias", "of", df$Naam)
    df$Naam <- gsub("/ alias", "of", df$Naam)
    df$Naam <- gsub("Alias ", "of ", df$Naam)
    df$Naam <- gsub("alias", "of", df$Naam)
    df$Naam <- gsub("bijgenaamd", "of", df$Naam)
    df$Naam <- gsub("/ genaamd", "of", df$Naam)
    df$Naam <- gsub(" n ", "of", df$Naam)
    df$Naam <- gsub("ofwel", "of", df$Naam)
    df$Naam <- gsub("ook bekend als", "of", df$Naam)
    df$Naam <- gsub("ook genaamd", "of", df$Naam)
    df$Naam <- gsub("ook gend", "of", df$Naam)
    df$Naam <- gsub(" pf", " of", df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-3, nchar(df$Naam))==" of ", gsub(" of ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))==" of", gsub(" of", "", df$Naam), df$Naam)
  #remaining / (
   #/m/
    as.data.frame(table(df[grepl("/m/", tolower(df$Naam)), "Naam"]))
   #/ vr
    df$Naam <- gsub("/ vr", "privé", df$Naam)
    
    
  #split naam & number into separate variables
   #make separate variable
    df$Naam_number <- ""
   #filter abbreviations
    as.data.frame(table(df[grepl("[A-Z][A-Z]", df$Naam) & grepl("II", df$Naam)==F & grepl("IV", df$Naam)==F & grepl("IJ", df$Naam)==F, "Naam"]))
    df$Naam_number <- ifelse(grepl("Dorothea P", df$Naam), "P", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Dorothea R", df$Naam), "R", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Francina P", df$Naam), "P", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Francina R", df$Naam), "R", df$Naam_number)
    df$Naam_number <- ifelse(grepl("AcB", df$Naam), "AcB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("ACB", df$Naam), "AcB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("A C-B", df$Naam), "AcB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("A C B", df$Naam), "AcB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("AkB", df$Naam), "AkB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("AKB", df$Naam), "AkB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("BW", df$Naam), "BW", df$Naam_number)
    df$Naam_number <- ifelse(grepl("DB", df$Naam), "DB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("D G", df$Naam), "DG", df$Naam_number)
    df$Naam_number <- ifelse(grepl("DG", df$Naam), "DG", df$Naam_number)
    df$Naam_number <- ifelse(grepl("D P", df$Naam), "DP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("DP", df$Naam), "DP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("E D", df$Naam), "FD", df$Naam_number)
    df$Naam_number <- ifelse(grepl("F A", df$Naam), "FA", df$Naam_number)
    df$Naam_number <- ifelse(grepl("FB", df$Naam), "FB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("FD", df$Naam), "FD", df$Naam_number)
    df$Naam_number <- ifelse(grepl("IB", df$Naam), "IB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Ns", df$Naam), "NS", df$Naam_number)
    df$Naam_number <- ifelse(grepl("NS", df$Naam), "NS", df$Naam_number)
    df$Naam_number <- ifelse(grepl("N S", df$Naam), "NS", df$Naam_number)
    df$Naam_number <- ifelse(grepl("pe", df$Naam), "pe", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" p w", df$Naam), "PW", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" P W", df$Naam), "PW", df$Naam_number)
    df$Naam_number <- ifelse(grepl("S P", df$Naam), "SP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("SP", df$Naam), "SP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("VCIP", df$Naam), "VCIP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("V C I P", df$Naam), "VCIP", df$Naam_number)
    df$Naam_number <- ifelse(grepl("vz", df$Naam), "VZ", df$Naam_number)
    df$Naam_number <- ifelse(grepl("VZ", df$Naam), "VZ", df$Naam_number)
    df$Naam_number <- ifelse(grepl("\\(zne )", df$Naam), "zne", df$Naam_number)
    df$Naam_number <- ifelse(grepl("z b", df$Naam), "ZB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Z B", df$Naam), "ZB", df$Naam_number)
    df$Naam_number <- ifelse(grepl("ZB", df$Naam), "ZB", df$Naam_number)
    #single letter
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-2, nchar(df$Naam))==" Br" & df$Naam_number=="", "Br", df$Naam_number)
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" b" & df$Naam_number=="", "b", df$Naam_number)
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" D" & df$Naam_number=="", "D", df$Naam_number)
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" P" & df$Naam_number=="", "P", df$Naam_number)
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" R" & df$Naam_number=="", "R", df$Naam_number)
    df$Naam_number <- ifelse(substr(df$Naam, nchar(df$Naam)-1, nchar(df$Naam))==" s" & df$Naam_number=="", "s", df$Naam_number)
   #remove variations
    df$Naam <- gsub("CSeba", "Seba", df$Naam)
    df$Naam <- gsub("CHarmantje", "Charmantje", df$Naam)
    df$Naam <- gsub("DSimon", "Simon", df$Naam)
    df$Naam <- gsub("KCatharina", "Katharina", df$Naam)
    df$Naam <- gsub("KLaas", "Klaas", df$Naam)
    df$Naam <- gsub("LIndor", "Lindor", df$Naam)
    df$Naam <- gsub("LIvia", "Livia", df$Naam)
    df$Naam <- gsub("Paddij ORaffertij", "Paddij O Raffertij", df$Naam)
    df$Naam <- gsub("PAmela", "Pamela", df$Naam)
    df$Naam <- gsub("PIetje", "Pietje", df$Naam)
    df$Naam <- gsub("SIbelle", "Sibelle", df$Naam)
    df$Naam <- gsub("VAnthonette", "Anthonette", df$Naam)
    df$Naam <- gsub("WIlhelmina", "Wilhelmina", df$Naam)
    df$Naam <- gsub("WIlliam", "William", df$Naam)
    df$Naam <- gsub(" AkB", "", df$Naam)
    df$Naam <- gsub(" AKB", "", df$Naam)
    df$Naam <- gsub(" AcB", "", df$Naam)
    df$Naam <- gsub(" ACB", "", df$Naam)
    df$Naam <- gsub(" A C-B", "", df$Naam)
    df$Naam <- gsub(" A C B", "", df$Naam)
    df$Naam <- gsub(" BW", "", df$Naam)
    df$Naam <- gsub(" DB", "", df$Naam)
    df$Naam <- gsub(" D G", "", df$Naam)
    df$Naam <- gsub(" DG", "", df$Naam)
    df$Naam <- gsub(" D P", "", df$Naam)
    df$Naam <- gsub(" DP", "", df$Naam)
    df$Naam <- gsub(" E D", "", df$Naam)
    df$Naam <- gsub(" F A", "", df$Naam)
    df$Naam <- gsub(" FB", "", df$Naam)
    df$Naam <- gsub(" FD", "", df$Naam)
    df$Naam <- gsub(" IB", "", df$Naam)
    df$Naam <- gsub(" Ns", "", df$Naam)
    df$Naam <- gsub(" NS", "", df$Naam)
    df$Naam <- gsub(" N S", "", df$Naam)
    df$Naam <- gsub(" pe", "", df$Naam)
    df$Naam <- gsub(" p w", "", df$Naam)
    df$Naam <- gsub(" P W", "", df$Naam)
    df$Naam <- gsub(" S P", "", df$Naam)
    df$Naam <- gsub(" SP", "", df$Naam)
    df$Naam <- gsub(" VCIP", "", df$Naam)
    df$Naam <- gsub(" V C I P", "", df$Naam)
    df$Naam <- gsub(" vz", "", df$Naam)
    df$Naam <- gsub(" VZ", "", df$Naam)
    df$Naam <- gsub(" z b", "", df$Naam)
    df$Naam <- gsub(" \\(zne )", "", df$Naam)
    df$Naam <- gsub(" Z B", "", df$Naam)
    df$Naam <- gsub(" ZB", "", df$Naam)
    #single letter
    df$Naam <- ifelse(df$Naam_number=="Br", gsub(" Br", "", df$Naam), df$Naam)
    df$Naam <- ifelse(df$Naam_number=="b", gsub(" b", "", df$Naam), df$Naam)
    df$Naam <- ifelse(df$Naam_number=="D", gsub(" D", "", df$Naam), df$Naam)
    df$Naam <- ifelse(df$Naam_number=="P", gsub(" P", "", df$Naam), df$Naam)
    df$Naam <- ifelse(df$Naam_number=="R", gsub(" R", "", df$Naam), df$Naam)
    df$Naam <- ifelse(df$Naam_number=="s", gsub(" s", "", df$Naam), df$Naam)
   #fix Johannes /II Winst/I
    df$Naam_number <- ifelse(df$Naam=="Johannes  II Winst I", "II / I", df$Naam_number)
    df$Naam <- gsub("Johannes  II Winst I", "Johannes of Winst", df$Naam)
   #transfer roman numericals
    df$Naam_number <- ifelse(grepl(" V", df$Naam)   &  grepl(" V.", df$Naam)==F,    "V",   df$Naam_number)
    df$Naam_number <- ifelse(grepl(" IV", df$Naam)  &  grepl(" IV.", df$Naam)==F,   "IV",  df$Naam_number)
    df$Naam_number <- ifelse(grepl(" IIII", df$Naam),  "IV", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" III", df$Naam) &  grepl(" III.", df$Naam)==F,  "III", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" II", df$Naam)  &  grepl(" II.", df$Naam)==F,   "II",  df$Naam_number)
    df$Naam_number <- ifelse(grepl(" I", df$Naam)   &  grepl(" I.", df$Naam)==F,    "I",   df$Naam_number)
   #remove roman numericals
    df$Naam <- ifelse(grepl(" V", df$Naam)   &  grepl(" V.", df$Naam)==F,    gsub(" V", "", df$Naam),   df$Naam)
    df$Naam <- ifelse(grepl(" IV", df$Naam)  &  grepl(" IV.", df$Naam)==F,   gsub(" IV", "", df$Naam),  df$Naam)
    df$Naam <- ifelse(grepl(" IIII", df$Naam),  gsub(" IIII", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" III", df$Naam) &  grepl(" III.", df$Naam)==F,  gsub(" III", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" II", df$Naam)  &  grepl(" II.", df$Naam)==F,   gsub(" II", "", df$Naam),  df$Naam)
    df$Naam <- ifelse(grepl(" I", df$Naam)   &  grepl(" I.", df$Naam)==F,    gsub(" I", "", df$Naam),   df$Naam)
   #transfer numericals when multiple names are listed 
    df$Naam_number <- ifelse(grepl(" V of", df$Naam), "IV", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" IV of", df$Naam), "IV", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" III of", df$Naam), "III", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" II of", df$Naam), "II", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" I of", df$Naam), "I", df$Naam_number)
   #remove numericals when multiple names are listed 
    df$Naam <- gsub(" V of", " of", df$Naam)
    df$Naam <- gsub(" IV of", " of", df$Naam)
    df$Naam <- gsub(" III of", " of", df$Naam)
    df$Naam <- gsub(" II of", " of", df$Naam)
    df$Naam <- gsub(" I of", " of", df$Naam)
   #transfer roman numericals + description
    df$Naam_number <- ifelse(grepl(" V ", df$Naam), sub(".*? ", "", df$Naam) , df$Naam_number)
    df$Naam_number <- ifelse(grepl(" IV ", df$Naam), sub(".*? ", "", df$Naam) , df$Naam_number)
    df$Naam_number <- ifelse(grepl(" III ", df$Naam), sub(".*? ", "", df$Naam) , df$Naam_number)
    df$Naam_number <- ifelse(grepl(" II ", df$Naam), sub(".*? ", "", df$Naam) , df$Naam_number)
    df$Naam_number <- ifelse(grepl(" I ", df$Naam), sub(".*? ", "", df$Naam) , df$Naam_number)
   #remove roman numericals + description
    df$Naam <- ifelse(grepl(" V ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" IV ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" III ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" II ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" I ", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
   #numericals
   #transfer written-out numericals
    df$Naam_number <- ifelse(grepl("eerst", tolower(df$Naam)), "I", df$Naam_number)
    df$Naam_number <- ifelse(grepl("tweede", tolower(df$Naam)), "II", df$Naam_number)
    df$Naam_number <- ifelse(grepl("derde", tolower(df$Naam)), "III", df$Naam_number)
    df$Naam_number <- ifelse(grepl("vierde", tolower(df$Naam)), "IV", df$Naam_number)
    df$Naam_number <- ifelse(grepl("vijfde", tolower(df$Naam)), "V", df$Naam_number)
   #remove written-out numericals
    df$Naam <- ifelse(grepl("eerst", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("tweede", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("derde", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("vierde", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("vijfde", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- gsub("/tweede.*", "", df$Naam)
  #name parts
   #transfer dik
    df$Naam_number <- ifelse(grepl(" dik", df$Naam), "dik", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Dikke ", df$Naam), "dik", df$Naam_number)
   #remove dik
    df$Naam <- ifelse(grepl(" dik", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- gsub("Dikke ", "", df$Naam)
   #transfer mooi
    df$Naam_number <- ifelse(grepl(" mooi", df$Naam), "mooi", df$Naam_number)
   #remove mooi
    df$Naam <- gsub(" mooi", "", df$Naam)
   #transfer klein
    df$Naam_number <- ifelse(grepl("klein [a-zA-z]", df$Naam), "klein", df$Naam_number)
    df$Naam_number <- ifelse(grepl("kleine ", df$Naam), "klein", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" klein", df$Naam), "klein", df$Naam_number)
    df$Naam_number <- ifelse(grepl("\\(klein", df$Naam), "klein", df$Naam_number)
   #remove klein
    df$Naam <- ifelse(grepl("klein [a-zA-z]", tolower(df$Naam)), gsub("klein ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" klein", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("\\(klein", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("kleine ", tolower(df$Naam)), gsub("kleine ", "", df$Naam), df$Naam)
   #transfer korte
    df$Naam_number <- ifelse(grepl("korte", df$Naam), "korte", df$Naam_number)
   #remove korte
    df$Naam <- ifelse(grepl("korte", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer groote
    df$Naam_number <- ifelse(grepl("groote [a-zA-Z]", df$Naam), "groote", df$Naam_number)
    df$Naam_number <- ifelse(grepl("groote  ", df$Naam), "groote", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" groote", df$Naam), "groote", df$Naam_number)
   #remove groote
    df$Naam <- ifelse(grepl("groote [a-zA-z]", tolower(df$Naam)), gsub("groote ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("groote  ", tolower(df$Naam)), gsub("groote ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" groote", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer lange
    df$Naam_number <- ifelse(grepl("lange [a-zA-z]", df$Naam), "lange", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" lange", df$Naam), "lange", df$Naam_number)
    df$Naam_number <- ifelse(grepl("\\(lange)", df$Naam), "lange", df$Naam_number)
   #remove lange
    df$Naam <- ifelse(grepl("lange [a-zA-z]", tolower(df$Naam)), gsub("lange ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" lange", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- gsub("\\(lange)", "", df$Naam)
   #transfer big
    df$Naam_number <- ifelse(grepl("Big ", df$Naam), "big", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" \\(big)", df$Naam), "big", df$Naam_number)
   #remove big
    df$Naam <- gsub("Big ", "", df$Naam)
    df$Naam <- gsub(" \\(big)", "", df$Naam)
   #transfer little
    df$Naam_number <- ifelse(grepl("little [a-zA-z]", df$Naam), "little", df$Naam_number)
    df$Naam_number <- ifelse(grepl("little", df$Naam), "little", df$Naam_number)
   #remove little
    df$Naam <- ifelse(grepl("little [a-zA-z]", tolower(df$Naam)), gsub("little ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("little", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer long
    df$Naam_number <- ifelse(grepl("\\(long)", df$Naam), "long", df$Naam_number)
   #remove long
    df$Naam <- gsub("\\(long)", "", df$Naam)
   #transfer jonge
    df$Naam_number <- ifelse(grepl("jonge", df$Naam), "jonge", df$Naam_number)
   #remove jonge
    df$Naam <- ifelse(grepl("jonge [a-zA-z]", tolower(df$Naam)), gsub("jonge ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("jonge", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer junior
    df$Naam_number <- ifelse(grepl(" junior", df$Naam), "junior", df$Naam_number)
   #remove junior
    df$Naam <- gsub(" junior", "", df$Naam)
   #transfer oude
    df$Naam_number <- ifelse(grepl("oude [a-zA-z]", tolower(df$Naam)), "oude", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" oud", tolower(df$Naam)), "oude", df$Naam_number)
   #remove oude 
    df$Naam <- ifelse(grepl("oude [a-zA-z]", tolower(df$Naam)), gsub("oude ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" oud", tolower(df$Naam)) & grepl("l esperanke", tolower(df$Naam)), "l Esperanke", df$Naam)
    df$Naam <- ifelse(grepl(" oud", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer senior
    df$Naam_number <- ifelse(grepl(" senior", df$Naam), "senior", df$Naam_number)
   #remove senior
    df$Naam <- gsub(" senior", "", df$Naam)
   #transfer nieuw
    df$Naam_number <- ifelse(grepl(" nieuw", tolower(df$Naam)), "nieuw", df$Naam_number)
    df$Naam_number <- ifelse(grepl("nieuw ", tolower(df$Naam)), "nieuw", df$Naam_number)
   #remove nieuw
    df$Naam <- ifelse(grepl(" nieuw", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- gsub("nieuw ", "", df$Naam)
   #transfer roode
    df$Naam_number <- ifelse(grepl("rood", tolower(df$Naam)), "roode", df$Naam_number)
   #remove roode
    df$Naam <- ifelse(grepl("rood", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer swart/zwart
    df$Naam_number <- ifelse(grepl("swart", tolower(df$Naam)), "zwart", df$Naam_number)
    df$Naam_number <- ifelse(grepl("zwart", tolower(df$Naam)), "zwart", df$Naam_number)
   #remove swart/zwart
    df$Naam <- ifelse(grepl("swart [a-zA-z]", tolower(df$Naam)), gsub(".* ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("swart", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("zwart [a-zA-z]", tolower(df$Naam)), gsub(".* ", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("zwart", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer neger
    df$Naam_number <- ifelse(grepl("neger", tolower(df$Naam)), "neger", df$Naam_number)
   #remove neger
    df$Naam <- ifelse(grepl("neger", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer creool
    df$Naam[df$Naam=="Creolen geluk"] <- "Creolengeluk"
    df$Naam_number <- ifelse(grepl(" creo", tolower(df$Naam)), "creool", df$Naam_number)
    df$Naam_number <- ifelse(grepl("creole ", tolower(df$Naam)), "creool", df$Naam_number)
   #remove creool
    df$Naam <- ifelse(grepl(" creo", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("creole ", tolower(df$Naam)), gsub(".* ", "", df$Naam), df$Naam)
   #transfer karboeger
    df$Naam_number <- ifelse(grepl("karboeg", tolower(df$Naam)), "carboeger", df$Naam_number)
    df$Naam_number <- ifelse(grepl("carboeg", tolower(df$Naam)), "carboeger", df$Naam_number)
   #remove mulat
    df$Naam <- ifelse(grepl("karboeg", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("carboeg", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer mulat
    df$Naam_number <- ifelse(grepl(" mulat", tolower(df$Naam)), "mulat", df$Naam_number)
   #remove mulat
    df$Naam <- ifelse(grepl(" mulat", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer pokdalig
    df$Naam_number <- ifelse(grepl(" pokdalig", tolower(df$Naam)), "pokdalig", df$Naam_number)
   #remove mulat
    df$Naam <- ifelse(grepl(" pokdalig", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer gedoopt
    df$Naam_number <- ifelse(grepl("gedoopt", tolower(df$Naam)), sub(".*? ", "", df$Naam), df$Naam_number)
    df$Naam <- ifelse(df$Naam_number=="of Fanie gedoopt Gijsbertina", "Tannij of Fanie", df$Naam)
    df$Naam_number <- ifelse(df$Naam_number=="of Fanie gedoopt Gijsbertina", "gedoopt Gijsbertina", df$Naam_number)
   #remove gedoopt
    df$Naam <- ifelse(grepl("gedoopt", tolower(df$Naam)), gsub(" .*", "", df$Naam), df$Naam)
   #transfer plantagenaam
    df$Naam_number <- ifelse(grepl("/Bn/", df$Naam), "Bn", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/Br/", df$Naam), "Br", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Brouwerslust", df$Naam) & df$Naam!="Brouwerslust", "Brouwerslust", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" groningen", df$Naam), "Groningen", df$Naam_number)
    df$Naam_number <- ifelse(grepl("LR", df$Naam), "Land en Rust", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Resolutie", df$Naam), "Resolutie", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Standvastigheid", df$Naam), "Standvastigheid", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Stolk", df$Naam), "Stolkwijk", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Zeewijk", df$Naam), "Zeewijk", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/S", df$Naam) & grepl("/S[a-z]", df$Naam)==F, "S", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/ S", df$Naam) & grepl("/ S[a-z]", df$Naam)==F, "S", df$Naam_number)
    df$Naam_number[df$Naam=="Dorinde (S"] <- "S"
    df$Naam_number <- ifelse(grepl("/ Vr M", df$Naam), "privé M", df$Naam_number)
   #remove plantagenaam
    df$Naam <- ifelse(grepl("/Bn/", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("/Br/", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Brouwerslust", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" groningen", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("L en R", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("LR", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Resolutie", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Standvastigheid", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Stolk", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("Zeewijk", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("/S", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("/ S", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
    df$Naam[df$Naam=="Dorinde (S"] <- "Dorinde"
    df$Naam <- ifelse(grepl("/ Vr M", df$Naam), gsub(" .*", "", df$Naam), df$Naam)
   #remove /m/
    df$Naam_number <- ifelse(grepl("/M/", df$Naam), "M", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/ m /", df$Naam), "M", df$Naam_number)
    df$Naam <- gsub("/M/", "", df$Naam)
    df$Naam <- gsub("/ m /", "", df$Naam)
   #transfer privé
    df$Naam_number <- ifelse(grepl("privé", df$Naam), "privé", df$Naam_number)
   #remove privé
    df$Naam <- gsub(" privé", "", df$Naam)
   #transfer beroepen
    df$Naam_number <- ifelse(grepl("/offikier", df$Naam), "officier", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/delver", df$Naam), "delver", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/wees", df$Naam), "wees", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/ kuiper", df$Naam), "kuiper", df$Naam_number)
    df$Naam_number <- ifelse(grepl("/Kuiper", df$Naam), "kuiper", df$Naam_number)
    df$Naam_number <- ifelse(grepl("Herculeskuiper", df$Naam), "kuiper", df$Naam_number)
    df$Naam_number <- ifelse(grepl("timmerbaas", df$Naam), "veldwerk", df$Naam_number)
    df$Naam_number <- ifelse(grepl("timmerknecht ", df$Naam), "veldwerk", df$Naam_number)
    df$Naam_number <- ifelse(grepl("timmerman", df$Naam), "timmerman", df$Naam_number)
    df$Naam_number <- ifelse(grepl("veldwerk", df$Naam), "veldwerk", df$Naam_number)
   #remove beroepen
    df$Naam <- gsub("/offikier", "", df$Naam)
    df$Naam <- gsub("/delver", "", df$Naam)
    df$Naam <- gsub("/wees", "", df$Naam)
    df$Naam <- gsub("/ kuiper /", "", df$Naam)
    df$Naam <- gsub("/Kuiper/", "", df$Naam)
    df$Naam <- gsub("timmerbaas", "", df$Naam)
    df$Naam <- gsub("timmerknecht", "", df$Naam)
    df$Naam <- gsub("timmerman", "", df$Naam)
    df$Naam <- gsub("veldwerk", "", df$Naam)
   #replace / with of
    df$Naam <- gsub("/  /", "", df$Naam)
    df$Naam[df$Naam=="William  /"] <- "William"
    df$Naam <- sub("/", " of ", df$Naam)
    df$Naam <- gsub("/", "", df$Naam)
    df$Naam <- gsub("  ", " ", df$Naam)
    df$Naam <- trimws(df$Naam)
    df$Naam[df$Naam=="Roos of PJ van"] <- "Roos / PJ van /"
   #()
    df$Naam <- ifelse(grepl("of", df$Naam), gsub(")", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("of", df$Naam), gsub("\\(", "", df$Naam), df$Naam)
    df$Naam <- gsub("\\(a)", "of", df$Naam)
    df$Naam <- sub("\\(", " of ", df$Naam)
    df$Naam <- gsub(")", "", df$Naam)
    df$Naam <- gsub("  ", " ", df$Naam)
    df$Naam <- trimws(df$Naam)
   #-
    df$Naam <- ifelse(grepl("of", df$Naam), gsub("-", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl("-.", df$Naam)==F, gsub("-", "", df$Naam), df$Naam)
    df$Naam <- gsub(" - ", " ", df$Naam)
    df$Naam <- gsub("-", " ", df$Naam)
   #=
    df$Naam <- gsub(" = ", " of ", df$Naam)
    df$Naam <- gsub("=", " of ", df$Naam)
   #?
    df$Naam <- gsub("Abicha\\?l", "Abichael", df$Naam)
   #;
    df$Naam <- gsub(";", " of", df$Naam)
   #,
    df$Naam <- gsub(", of", " of", df$Naam)
    df$Naam <- gsub("Minoke of Afie, Louise", "Minoke of Afie of Louise", df$Naam)
    df$Naam <- gsub("Suzanna, Henriette", "Suzanna of Henriette", df$Naam)
    df$Naam <- gsub(",", "", df$Naam)
   #'
    df$Naam[df$Naam=="Bebe'"] <- "Bebe"
    df$Naam <- gsub("a'", "a", df$Naam)
    df$Naam <- gsub("D'", "d'", df$Naam)
    df$Naam <- gsub("L' ", "l'", df$Naam)
    df$Naam <- gsub("L'", "l'", df$Naam)
    df$Naam <- ifelse(grepl("l Esperance", df$Naam_original), "l'Esperance", df$Naam)
    df$Naam <- ifelse(grepl("l Argent", df$Naam_original), "l'Argent", df$Naam)
   #de le la
    df$Naam <- gsub("De ", "de ", df$Naam)
    df$Naam <- gsub("La ", "la ", df$Naam)
    df$Naam <- gsub("Le ", "le ", df$Naam)
   #enNaam
    df$Naam <- ifelse(grepl("en[A-Z]", substr(df$Naam,1,3)), gsub("en[A-Z]", "", df$Naam), df$Naam)
   #abusief
    df$Naam <- gsub(" abusiefe", "", df$Naam)
   #blijft
    df$Naam <- gsub(" blijft", "", df$Naam)
   #blijft
    df$Naam <- gsub("root ", "", df$Naam)
    
   #fix white spaces
    df$Naam <- trimws(df$Naam)
    df$Naam <- gsub("  ", " ", df$Naam)
    
   #give overview
    as.data.frame(table(df$Naam_number))
    df$Naam_number <- gsub(" )", "", df$Naam_number)
    df$Naam_number <- gsub(")", "", df$Naam_number)
    df$Naam_number <- gsub("\\(", "", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" /", df$Naam_number) & grepl(" /.", df$Naam_number)==F, gsub(" /", "", df$Naam_number), df$Naam_number)
    as.data.frame(table(df$Naam_number))
    
    
   #save look-up tables
    #unique Naam_original variants + standardisation
    write.xlsx(df[!duplicated(df$Naam_original),c("Naam_original", "Naam", "Naam_number", "source_order")] %>% arrange(trimws(Naam_original)), "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Slaafgemaakten - Conversietabel.xlsx")
    #frequency table of Naam
    x <- as.data.frame(table(df$Naam)) %>% arrange(Var1)
    colnames(x) <- c("Naam", "Frequentie")
    write.xlsx(x, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Slaafgemaakten.xlsx")
    rm(x)
    
    
  
  #################################
  #### section 3: cleaning sex ####
  #################################
    
    Sex <- df[df$Naam!="", c("Naam", "sex")]
    Sex <- Sex %>% group_by(Naam, sex) %>% summarise(n=n()) %>% ungroup()
    Sex <- Sex %>% group_by(Naam) %>% filter(n()>1) %>% ungroup() 
    
   #save look-up table
    #filter names per sex
    female <- Sex[Sex$sex=="female", c("Naam", "n")]; colnames(female) <- c("Naam", "n_female")
    male <- Sex[Sex$sex=="male", c("Naam", "n")]; colnames(male) <- c("Naam", "n_male")
    unknown <- Sex[Sex$sex=="unknown", c("Naam", "n")]; colnames(unknown) <- c("Naam", "n_unknown")
    #set to wide format
    Sex <- merge(female, male, by="Naam", all=T)
    Sex <- merge(Sex, unknown, by="Naam", all=T)
    Sex[is.na(Sex)] <- 0
    #calculate disagreement
    Sex$error_margin <- ifelse(Sex$n_female>=Sex$n_male, 
                               round(1-Sex$n_female/(Sex$n_female+Sex$n_male),3), 
                               round(1-Sex$n_male/(Sex$n_female+Sex$n_male),3) )
    Sex$recodable <- ifelse(Sex$n_female>=Sex$n_male, 
                            Sex$n_male+Sex$n_unknown, 
                            Sex$n_female+Sex$n_unknown )
    Sex <- Sex %>% arrange(-error_margin)
    #save outfile
    write.xlsx(Sex, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Correctie sekse - slaafgemaakten.xlsx")
    rm(female, male, unknown)
    
   #standardise names if error_margin <= 0.25
    Sex <- Sex[Sex$error_margin<=0.25,] #demarcation point determinated by CvG 12-04-2022
    Sex$sex2 <- ifelse(Sex$n_female>Sex$n_male, "female", "male")
    Sex <- Sex[Sex$Naam!="Cato" &
                 Sex$Naam!="Minosabi" &
                 Sex$Naam!="Minosabie" &
                 Sex$Naam!="Jantje" &
                 Sex$Naam!="Jannie" &
                 Sex$Naam!="Toontje" &
                 Sex$Naam!="Pietje", c("Naam", "sex2")]
    #add info to df
    df <- merge(df, Sex, by="Naam", all=T)
    length(which(df$sex!=df$sex2)) #873
    df$sex <- ifelse(is.na(df$sex2), df$sex, df$sex2)
    
    #save outfile
    Sex <- df %>% group_by(Naam, sex) %>% summarise(n=n()) %>% ungroup()
    Sex <- Sex %>% group_by(Naam) %>% mutate(n=n()) %>% ungroup()
    Sex$sex <- ifelse(Sex$n>1,"unknown", Sex$sex)
    Sex <- Sex[!duplicated(Sex$Naam), c("Naam", "sex")]
    write.xlsx(Sex, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Sekse naar naam - slaafgemaakten.xlsx")
    
    
    
  ####################################
  #### section 4: cleaning Moeder ####
  ####################################
    
  #remove dots
    df$Moeder[df$Moeder=="."] <- ""
    df$Moeder_2[df$Moeder_2=="."] <- ""
    df$Moeder[df$Moeder=="Rose of Ross [onbekend]"] <- "Rose of Ross"
    df$Moeder[grepl("Onb", df$Moeder)] <- ""
    df$Moeder_2[grepl("Onb", df$Moeder_2)] <- ""
    df$Moeder[grepl("kend", df$Moeder)] <- ""
    df$Moeder_2[grepl("kend", df$Moeder_2)] <- ""
    
  #combine information on Moeder from Moeder, Naam & Events
   #Moeder, entered
    df$Moeder_1_Entry <- df$Moeder
   #rename Moeder_2 into Moeder_Naam
    df$Moeder_2_Naam <- df$Moeder_2
    df$Moeder_2 <- NULL
   #combine
    df$Moeder <- ifelse(df$Moeder_1_Entry!="", df$Moeder_1_Entry, df$Moeder_2_Naam)
    df$Moeder_original <- trimws(df$Moeder)
   #flag errors
    df$Moeder_incongruence <- ifelse(df$Moeder_1_Entry=="" | df$Moeder_2_Naam=="", "",
                                     ifelse(df$Moeder_1_Entry==df$Moeder_2_Naam, 0, 1))
   #make separate variable for Moeder_number
    df$Moeder_number <- ""
    
  #report verwijzingen 
    df$Moeder[df$Moeder=="Gratia. Naam moeder: Martina. Zie Gouv: res: dd 14 Augs 1851 No 1010"] <- "Martina"
   #explore patterns
    as.data.frame(table(df[grepl("dochter", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("[0-9][0-9][0-9][0-9]", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("geb", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("gb", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("sie", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("zie", tolower(df$Moeder)), "Moeder"]))
    
   #transfer geboortejaar
    df$Moeder <- gsub("1850 2 Decemb", "", df$Moeder)
    df$Moeder <- gsub("1860 2 Junij", "", df$Moeder)
    df$Moeder_birthyear <- ifelse(grepl("[0-9][0-9][0-9][0-9]", tolower(df$Moeder)) & grepl("over", tolower(df$Moeder))==F,  gsub("\\D", "", df$Moeder), "")
    df$Moeder_birthyear <- ifelse(grepl("geb", tolower(df$Moeder)),  gsub("\\D", "", df$Moeder), df$Moeder_birthyear)
    df$Moeder_birthyear <- ifelse(grepl("gb", tolower(df$Moeder)),  gsub("\\D", "", df$Moeder), df$Moeder_birthyear)
    as.data.frame(table(df$Moeder_birthyear))
    df$Moeder_birthyear <- ifelse(nchar(df$Moeder_birthyear)==5, substr(df$Moeder_birthyear, 2, 5), df$Moeder_birthyear)
    df$Moeder_birthyear[df$Moeder_birthyear=="120"] <- 1820
   #remove geboortejaar
    df$Moeder[df$Moeder=="Diana 2? geboren in 1817"] <- "Diana II"
    df$Moeder[df$Moeder=="Louisa NSGeb in 1824"] <- "Lousia NS"
    df$Moeder <- ifelse(grepl("geb", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("geb", tolower(df$Moeder)), gsub("geb.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("gb.", tolower(df$Moeder)), gsub("gb.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("[a-z]geb", tolower(df$Moeder)), gsub("Geb.*", "", df$Moeder), df$Moeder)
    
   #transfer 4 digits to birthyear
    df$Moeder_birthyear <- ifelse(grepl("[0-9][0-9][0-9][0-9]", tolower(df$Moeder)),  gsub("\\D", "", df$Moeder), df$Moeder_birthyear)
   #remove 4 digits
    df$Moeder <- ifelse(grepl("[0-9][0-9][0-9][0-9]", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    
   #transfer dochter van
    as.data.frame(table(df[grepl("dochter", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl("dochter", df$Moeder) & grepl("[0-9]", df$Moeder), "Moeder"]))
    df$Moeder <- gsub("Judithdochter", "Judith dochter", df$Moeder)
    df$Moeder[df$Moeder=="Floortje II, dochter van Martina"] <- "Floortje II dochter van Martina"
    df$Moeder[df$Moeder=="Doortje 3 dochter van Philippina"] <- "Doortje III dochter van Philippina"
    df$Moeder[df$Moeder=="Frederika II dochter van Louisa I"] <- "Frederika II dochter van Louisa I"
    df$Moeder_number <- ifelse(grepl("dochter", df$Moeder), sub(".*? ", "", df$Moeder), df$Moeder_number)
    df$Moeder_number <- gsub(",", " ", df$Moeder_number)
    df$Moeder_number <- gsub("\\(", "", df$Moeder_number)
    df$Moeder_number <- gsub(")", "", df$Moeder_number)
    df$Moeder_number <- gsub("  ", " ", df$Moeder_number)
    df$Moeder_number <- trimws(df$Moeder_number)
   #remove dochter van
    df$Moeder <- ifelse(grepl("dochter", df$Moeder), gsub(",", " ", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("dochter", df$Moeder), gsub(" .*", " ", df$Moeder), df$Moeder)
   #transfer Sie No / Zie No
    df$Moeder_number <- ifelse(grepl("grietje kl", tolower(df$Moeder)), "klein", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("louisa 1", tolower(df$Moeder)), "I", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("louisa d", tolower(df$Moeder)), "DG", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("louisa moor", tolower(df$Moeder)), "moor", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("louisa n", tolower(df$Moeder)), "NS", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("mietje s", tolower(df$Moeder)), "SP", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("petronella d", tolower(df$Moeder)), "DG", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("sophia kl", tolower(df$Moeder)), "klein", df$Moeder_number)
    df$Moeder_reference <- ifelse(grepl("zie no", tolower(df$Moeder)), gsub("\\D", "", df$Moeder), "")
    df$Moeder_reference <- ifelse(grepl("sie fo", tolower(df$Moeder)), gsub("\\D", "", df$Moeder), df$Moeder_reference)
    df$Moeder_reference <- ifelse(grepl("sie no", tolower(df$Moeder)), gsub("\\D", "", df$Moeder), df$Moeder_reference)
    as.data.frame(table(df$Moeder_reference))
   #remove Sie No / Zie No
    df$Moeder <- ifelse(grepl(" sie fo", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" sie no", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" gr zie no", tolower(df$Moeder)), gsub(" .*", " groot", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" zie no", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" moorzie no", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" szie no", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("sie fo", tolower(df$Moeder)), gsub("Sie.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("sie no", tolower(df$Moeder)), gsub("Sie.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("zie no", tolower(df$Moeder)), gsub("Zie.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("sie fo", tolower(df$Moeder)), gsub("sie.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("sie no", tolower(df$Moeder)), gsub("sie.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("zie no", tolower(df$Moeder)), gsub("zie.*", "", df$Moeder), df$Moeder)
    
   #transfer 3 digits to Moeder_reference
    df$Moeder_reference <- ifelse(grepl("[0-9][0-9][0-9]", tolower(df$Moeder)),  gsub("\\D", "", df$Moeder), df$Moeder_reference)
   #remove 3 digits
    df$Moeder <- ifelse(grepl("[0-9][0-9][0-9]", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    
    
  #explore Moeder
  #numericals
   #I-IV
    as.data.frame(table(df[grepl(" I", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl(" I", df$Moeder) & grepl(" I[a-z]", df$Moeder)==F, "Moeder"]))
   #V-VIII
    as.data.frame(table(df[grepl(" V", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl(" V", df$Moeder) & grepl(" V.", df$Moeder)==F, "Moeder"]))
   #1-9
    as.data.frame(table(df[grepl("[0-9]", df$Moeder), "Moeder"]))
   #written-out numericals
    as.data.frame(table(df[grepl("eerst", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("twee", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("drie", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("derde", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("vier", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("vijf", tolower(df$Moeder)), "Moeder"]))
  #descriptions
   #vrij
    as.data.frame(table(df[grepl("vrij", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("verkocht", tolower(df$Moeder)), "Moeder"]))
   #klein
    as.data.frame(table(df[grepl("klein", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("kl. ", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("kl ", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("/kl", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("kl", tolower(df$Moeder)), "Moeder"]))
   #kort
    as.data.frame(table(df[grepl("kort", tolower(df$Moeder)), "Moeder"]))
   #groot
    as.data.frame(table(df[grepl("groot", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("gr. ", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("gr ", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl(" gr", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("/gr", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("gt", tolower(df$Moeder)), "Moeder"]))
   #lange
    as.data.frame(table(df[grepl("lang", tolower(df$Moeder)), "Moeder"]))
   #little
    as.data.frame(table(df[grepl("lit", tolower(df$Moeder)), "Moeder"]))
   #long
    as.data.frame(table(df[grepl("long", tolower(df$Moeder)), "Moeder"]))
   #junior
    as.data.frame(table(df[grepl("junior", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl(" jr", tolower(df$Moeder)), "Moeder"]))
   #senior
    as.data.frame(table(df[grepl("senior", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl(" sr", tolower(df$Moeder)), "Moeder"]))
   #jonge
    as.data.frame(table(df[grepl("jong", tolower(df$Moeder)), "Moeder"]))
   #oude
    as.data.frame(table(df[grepl("ouw", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("oud", tolower(df$Moeder)), "Moeder"]))
   #nieuw
    as.data.frame(table(df[grepl("nieuw", tolower(df$Moeder)), "Moeder"]))
   #roode
    as.data.frame(table(df[grepl("rod", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("rood", tolower(df$Moeder)), "Moeder"]))
   #swart / zwart
    as.data.frame(table(df[grepl("swart", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("sw", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("zwart", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl("zw", tolower(df$Moeder)), "Moeder"]))
   #neger
    as.data.frame(table(df[grepl("neger", tolower(df$Moeder)), "Moeder"]))
   #mulat
    as.data.frame(table(df[grepl("mulat", tolower(df$Moeder)), "Moeder"]))
   #creool
    as.data.frame(table(df[grepl("creo", tolower(df$Moeder)), "Moeder"]))
   #remaining / (
    as.data.frame(table(df[grepl("/", tolower(df$Moeder)), "Moeder"]))
   #/m/
    as.data.frame(table(df[grepl("/m/", tolower(df$Moeder)), "Moeder"]))
   #twee namen
    as.data.frame(table(df[grepl(" alias", tolower(df$Moeder)), "Moeder"]))
    as.data.frame(table(df[grepl(" ook", tolower(df$Moeder)), "Moeder"]))
   #vrij
    as.data.frame(table(df[grepl("vrij", tolower(df$Moeder_original)), "Moeder"]))
    
    
  #standardise entries
   #fix I
    df$Moeder <- gsub("Ie", "I", df$Moeder)
    df$Moeder <- gsub("I:", "I", df$Moeder)
    df$Moeder <- gsub("I-", "I", df$Moeder)
    df$Moeder <- gsub("II /", " II", df$Moeder)
    df$Moeder <- ifelse(grepl("[a-z]I", df$Moeder), gsub("I", " I", df$Moeder), df$Moeder)
    df$Moeder <- gsub("AmeliaII", "Amelia II", df$Moeder)
    df$Moeder <- gsub("EstherI", "Esther I", df$Moeder)
    df$Moeder <- gsub("SantjeI", "Santje I", df$Moeder)
   #fix arabical numericals
    df$Moeder <- ifelse(grepl("9", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder) #typo's. Aktes vermelden geen nummer, maar overl:
    df$Moeder <- ifelse(grepl("8", df$Moeder), gsub(" .*", " VIII", df$Moeder), df$Moeder) #typo's. Aktes vermelden geen nummer, maar overl:
    df$Moeder <- gsub("3e", "III", df$Moeder)
    df$Moeder <- gsub("3", "III", df$Moeder)
    df$Moeder <- gsub("2den", "II", df$Moeder)
    df$Moeder <- gsub("\\(2e)", "II", df$Moeder)
    df$Moeder <- trimws(gsub("2.", " II ", df$Moeder))
    df$Moeder <- trimws(gsub("2", " II ", df$Moeder))
    df$Moeder <- trimws(gsub("1.", " I ", df$Moeder))
    df$Moeder <- gsub("1", " I", df$Moeder)
   #standardise klein
    df$Moeder <- gsub("Klein", "klein", df$Moeder)
    df$Moeder <- ifelse(grepl("[a-z]klein", df$Moeder), gsub("klein", " klein", df$Moeder), df$Moeder)
    df$Moeder <- gsub(", klein", " klein", df$Moeder)
    df$Moeder <- gsub("Kl. ", "klein ", df$Moeder)
    df$Moeder <- gsub("kl. ", "klein ", df$Moeder)
    df$Moeder <- gsub("Kl ", "klein ", df$Moeder)
    df$Moeder <- gsub("kl ", "klein ", df$Moeder)
    df$Moeder <- ifelse(grepl("Kl", df$Moeder) & grepl("Kl[a-z]", df$Moeder)==F, gsub("Kl", " klein", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("kl", df$Moeder) & grepl("kl[a-z]", df$Moeder)==F, gsub("kl", " klein", df$Moeder), df$Moeder)
   #groot
    df$Moeder <- gsub("Groot", "groot", df$Moeder)
    df$Moeder <- gsub("Gr. ", "groot ", df$Moeder)
    df$Moeder <- ifelse(grepl("Lagre", df$Moeder)==F, gsub("gr. ", "groot ", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Gr", df$Moeder) & grepl("Gr[a-z]", df$Moeder)==F, trimws(gsub("Gr", " groot ", df$Moeder)), df$Moeder)
    df$Moeder <- ifelse(grepl("gr", df$Moeder) & grepl("gr[a-z]", df$Moeder)==F, trimws(gsub("gr", " groot ", df$Moeder)), df$Moeder)
   #lange
    df$Moeder <- gsub("Lange", "lange", df$Moeder)
   #little
    df$Moeder <- gsub("Little", "little", df$Moeder)
    df$Moeder <- gsub("Littel", "little", df$Moeder)
    df$Moeder <- gsub("littel", "little", df$Moeder)
   #jonge
    df$Moeder <- gsub("Jonge ", "jonge ", df$Moeder)
   #oude
    df$Moeder <- gsub("Oude", " oude", df$Moeder)
   #nieuwe
    df$Moeder <- gsub("Nieuwe", "nieuw", df$Moeder)
    df$Moeder <- gsub("Nw\\.", "nieuw", df$Moeder)
    df$Moeder <- gsub("Nw", "nieuw", df$Moeder)
   #swart
    df$Moeder <- ifelse(grepl("sw[a-zA-Z]", tolower(df$Moeder))==F, gsub("sw", "zwart", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("sw[a-zA-Z]", tolower(df$Moeder))==F, gsub("Sw", "zwart", df$Moeder), df$Moeder)
   #zwart
    df$Moeder <- ifelse(grepl("zw[a-zA-Z]", tolower(df$Moeder))==F, gsub("zw", "zwart", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("zw[a-zA-Z]", tolower(df$Moeder))==F, gsub("Zw", "zwart", df$Moeder), df$Moeder)
   #mulat
    df$Moeder <- gsub("Mulat", " mulat", df$Moeder)
   #deal with abbreviations
    as.data.frame(table(df[grepl(":", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl("\\.", df$Moeder), "Moeder"]))
   #plaatsen
    #Botervl
    df$Moeder <- gsub("Botervl\\.", "Botervl:", df$Moeder)
    #Resolutie
    df$Moeder <- gsub("Res:", "Resolutie", df$Moeder)
    df$Moeder <- gsub("Res\\.", " Resolutie", df$Moeder)
    df$Moeder <- ifelse(grepl("Res[a-z]", df$Moeder)==F, gsub("Res", " Resolutie", df$Moeder), df$Moeder)
    df$Moeder <- gsub("/Resol\\.", " Resolutie", df$Moeder)
    df$Moeder <- gsub("Resol\\.", " Resolutie", df$Moeder)
    df$Moeder <- ifelse(grepl("Resol[a-z]", df$Moeder)==F, gsub("Resol", " Resolutie", df$Moeder), df$Moeder)
    df$Moeder <- gsub("Resolutie", " Resolutie", df$Moeder)
    #S /
    df$Moeder <- gsub("S /", " Scheveningen", df$Moeder)
    #la Simplicite
    df$Moeder <- gsub("/la Simp\\./", " la Simplicite", df$Moeder)
    df$Moeder <- gsub(" of Simplikite", " la Simplicite", df$Moeder)
    #Standvastigheid
    df$Moeder <- gsub("Staadv:", " Standvastigheid", df$Moeder)
    df$Moeder <- gsub("Standv:", " Standvastigheid", df$Moeder)
    df$Moeder <- gsub("Standv", " Standvastigheid", df$Moeder)
    df$Moeder <- gsub("Standi\\.", " Standvastigheid", df$Moeder)
    df$Moeder <- gsub("Standv\\.", " Standvastigheid", df$Moeder)
    #Stolkwijk
    df$Moeder <- ifelse(grepl("Stolkw[a-z]", df$Moeder)==F, gsub("Stolkw", " Stolkwijk", df$Moeder), df$Moeder)
    df$Moeder <- gsub("Stolkwijk", " Stolkwijk", df$Moeder)
    df$Moeder <- gsub("Stolwijk", " Stolkwijk", df$Moeder)
    df$Moeder <- gsub("Stokwijk", " Stolkwijk", df$Moeder)
    df$Moeder <- gsub("Stokw", " Stolkwijk", df$Moeder)
    df$Moeder <- gsub("Stw", " Stolkwijk", df$Moeder)
    #Zeewijk
    df$Moeder <- gsub("Zeew:", "Zeewijk", df$Moeder)
    df$Moeder <- gsub("Zeew\\.", "Zeewijk", df$Moeder)
    df$Moeder <- ifelse(grepl("Zeew[a-z]", df$Moeder)==F, gsub("Zeew", " Zeewijk", df$Moeder), df$Moeder)
    df$Moeder <- gsub("/Zeewijk", " Zeewijk", df$Moeder)
    df$Moeder <- gsub("Zeewjk", "Zeewijk", df$Moeder)
    df$Moeder <- gsub("Zeewijk", " Zeewijk", df$Moeder)
    #L en R
    df$Moeder <- ifelse(grepl("[a-z]L", df$Moeder), gsub("L ", " L ", df$Moeder), df$Moeder)
    df$Moeder <- gsub("LR", " Land en Rust", df$Moeder)
    df$Moeder <- gsub("L R", " Land en Rust", df$Moeder)
    df$Moeder <- gsub("L en R", " Land en Rust", df$Moeder)
    df$Moeder <- gsub("LsR", " Land en Rust", df$Moeder)
    #namen
    df$Moeder <- gsub("Carb:", "Carboeg", df$Moeder)
    df$Moeder <- gsub("Christ:", "Christina", df$Moeder)
    df$Moeder <- gsub("Christ\\.", "Christina", df$Moeder)
    df$Moeder <- gsub("Elis:", "Elisabeth", df$Moeder)
    df$Moeder <- gsub("Elis\\.", "Elisabeth", df$Moeder)
    df$Moeder <- gsub("El Bertha", "Elisabeth Bertha", df$Moeder)
    df$Moeder <- gsub("Els\\. Bertha", "Elisabeth Bertha", df$Moeder)
    df$Moeder <- gsub("H Wilh:", "Henrietta Wilhelmina", df$Moeder)
    df$Moeder <- gsub("Henr:", "Henrietta", df$Moeder)
    df$Moeder <- gsub("Hendr\\.", "Hendrina", df$Moeder)
    df$Moeder <- gsub("Th:", "Theodora", df$Moeder)
    df$Moeder <- gsub("Theod:", "Theodora", df$Moeder)
    df$Moeder <- gsub("Theod\\.", "Theodora", df$Moeder)
    df$Moeder <- gsub("Wilh:", "Wilhelmina", df$Moeder)
    df$Moeder <- gsub("Wilh\\.", "Wilhelmina", df$Moeder)
   #overleden
    df$Moeder <- gsub("Premiere/ over", "Premiere overleden", df$Moeder)
    df$Moeder <- gsub("over.l", "overleden", df$Moeder)
    df$Moeder <- gsub("over\\.", "overleden", df$Moeder)
    df$Moeder <- gsub("/verl/", "overleden", df$Moeder)
    df$Moeder <- gsub("/verl/", "overleden", df$Moeder)
   #delete .
    df$Moeder <- gsub(" /\\.", "", df$Moeder)
    df$Moeder <- gsub("\\.", "", df$Moeder)
   #tweede Naam
    df$Moeder <- gsub("bijgent", "of", df$Moeder)
    df$Moeder <- gsub("bijgenaamd", "of", df$Moeder)
    df$Moeder <- gsub("\\(ook genaamd Alijda)", "of Alijda", df$Moeder)
    df$Moeder <- gsub("ook genaamd Alijde", "of Alijde", df$Moeder)
    df$Moeder <- gsub("ook", "of", df$Moeder)
   #overleden
    df$Moeder_overleden <- ifelse(grepl("overl", df$Moeder), "overleden", "")
    df$Moeder <- gsub(" \\(overleden))", "", df$Moeder)
    df$Moeder <- gsub(" \\(overleden)", "", df$Moeder)
    df$Moeder <- gsub("\\(overleden)", "", df$Moeder)
    df$Moeder <- gsub("\\(overled)", "", df$Moeder)
    df$Moeder <- gsub("overleden", "", df$Moeder)
    df$Moeder <- gsub("overled", "", df$Moeder)
    df$Moeder <- gsub("overlijden", "", df$Moeder)
   #remaining / (
    
    
  #split Moeder & number into separate variables
   #filter abbreviations
    df$Moeder_number <- ifelse(grepl("AkB", df$Moeder), "AkB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("AcB", df$Moeder), "AcB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("A C B", df$Moeder), "AcB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("ACB", df$Moeder), "AcB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("IB", df$Moeder), "IB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("DG", df$Moeder), "DG", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("D G", df$Moeder), "DG", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("DP", df$Moeder), "DP", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("D P", df$Moeder), "DP", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("NS", df$Moeder), "NS", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Premiere", df$Moeder) & grepl("O", df$Moeder), "O", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("ZB", df$Moeder), "ZB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Z B", df$Moeder), "ZB", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("jd", df$Moeder) & grepl("ijd", df$Moeder)==F, "jd", df$Moeder_number)
   #remove variations
    df$Moeder <- gsub(" AkB", "", df$Moeder)
    df$Moeder <- gsub(" AcB", "", df$Moeder)
    df$Moeder <- gsub(" A C B", "", df$Moeder)
    df$Moeder <- gsub(" ACB", "", df$Moeder)
    df$Moeder <- gsub(" IB", "", df$Moeder)
    df$Moeder <- gsub(" DG", "", df$Moeder)
    df$Moeder <- gsub(" D G", "", df$Moeder)
    df$Moeder <- gsub(" DP", "", df$Moeder)
    df$Moeder <- gsub(" D P", "", df$Moeder)
    df$Moeder <- gsub(" NS", "", df$Moeder)
    df$Moeder <- ifelse(grepl("Premiere", df$Moeder) & grepl("O", df$Moeder), "Premiere", df$Moeder)
    df$Moeder <- gsub(" S P", "", df$Moeder)
    df$Moeder <- gsub(" ZB", "", df$Moeder)
    df$Moeder <- gsub(" Z B", "", df$Moeder)
    df$Moeder <- gsub(" \\(jd)", "", df$Moeder)
   #transfer roman numericals
    df$Moeder_number <- ifelse(grepl(" VIII", df$Moeder),    "VIII",   df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" V", df$Moeder)   &  grepl(" V.", df$Moeder)==F,    "V",   df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" IV", df$Moeder)  &  grepl(" IV.", df$Moeder)==F,   "IV",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" III", df$Moeder) &  grepl(" III.", df$Moeder)==F,  "III", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" II", df$Moeder)  &  grepl(" II.", df$Moeder)==F,   "II",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" I", df$Moeder)   &  grepl(" I.", df$Moeder)==F,    "I",   df$Moeder_number)
   #remove roman numericals
    df$Moeder[df$Moeder=="Bebe of Elisabeth II"] <- "Bebe of Elisabeth"
    df$Moeder <- ifelse(grepl(" VIII", df$Moeder),                                 gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" V", df$Moeder)   &  grepl(" V.", df$Moeder)==F,    gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" IV", df$Moeder)  &  grepl(" IV.", df$Moeder)==F,   gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" III", df$Moeder) &  grepl(" III.", df$Moeder)==F,  gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" II", df$Moeder)  &  grepl(" II.", df$Moeder)==F,   gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" I", df$Moeder)   &  grepl(" I.", df$Moeder)==F,    gsub(" .*", "", df$Moeder), df$Moeder)
   #name parts
   #transfer klein
    df$Moeder_number <- ifelse(grepl("klein", df$Moeder), "klein", df$Moeder_number)
   #remove klein
    df$Moeder <- ifelse(grepl("klein [a-zA-z]", tolower(df$Moeder)), gsub("klein ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("kleine ", tolower(df$Moeder)), gsub("kleine ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("klein", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer groote
    df$Moeder_number <- ifelse(grepl("groot", df$Moeder), "groote", df$Moeder_number)
   #remove groote
    df$Moeder <- ifelse(grepl("groot ", tolower(df$Moeder)), gsub("groot ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("groote ", tolower(df$Moeder)), gsub("groote ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" groot", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer lange
    df$Moeder_number <- ifelse(grepl("lange [a-zA-z]", df$Moeder), "lange", df$Moeder_number)
   #remove lange
    df$Moeder <- ifelse(grepl("lange [a-zA-z]", tolower(df$Moeder)), gsub("lange ", "", df$Moeder), df$Moeder)
   #transfer little
    df$Moeder_number <- ifelse(grepl("little", df$Moeder), "little", df$Moeder_number)
   #remove little
    df$Moeder <- ifelse(grepl("little [a-zA-z]", tolower(df$Moeder)), gsub("little ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("little", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer jonge
    df$Moeder_number <- ifelse(grepl("jonge ", df$Moeder), "jonge", df$Moeder_number)
   #remove jonge
    df$Moeder <- ifelse(grepl("jonge [a-zA-z]", tolower(df$Moeder)), gsub("jonge ", "", df$Moeder), df$Moeder)
   #transfer oude
    df$Moeder_number <- ifelse(grepl("oud", tolower(df$Moeder)), "oude", df$Moeder_number)
   #remove oude 
    df$Moeder <- ifelse(grepl("oud", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer nieuw
    df$Moeder_number <- ifelse(grepl("nieuw", tolower(df$Moeder)), "nieuw", df$Moeder_number)
   #remove nieuw
    df$Moeder <- ifelse(grepl("nieuw ", tolower(df$Moeder)), gsub("nieuw ", "", df$Moeder), df$Moeder)
   #transfer swart/zwart
    df$Moeder_number <- ifelse(grepl("swart", tolower(df$Moeder)) & grepl("swart[a-z]", tolower(df$Moeder))==F, "zwart", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("swarte", tolower(df$Moeder)) & grepl("swarte[a-z]", tolower(df$Moeder))==F, "zwart", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("zwart", tolower(df$Moeder)) & grepl("zwart[a-z]", tolower(df$Moeder))==F, "zwart", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("zwarte", tolower(df$Moeder)) & grepl("zwarte[a-z]", tolower(df$Moeder))==F, "zwart", df$Moeder_number)
    #remove swart/zwart
    df$Moeder <- ifelse(grepl("zwarte [a-zA-Z]", tolower(df$Moeder)), gsub(".* ", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("zwart", tolower(df$Moeder)) & grepl("zwart[a-z]", tolower(df$Moeder))==F, gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("swart", tolower(df$Moeder)) & grepl("swart[a-z]", tolower(df$Moeder))==F, gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- gsub("/zwart", "", df$Moeder)
   #transfer de laatste
    df$Moeder_number <- ifelse(grepl("de laatste", tolower(df$Moeder)), "de laatste", df$Moeder_number)
   #remove de laatste
    df$Moeder <- ifelse(grepl("de laatste", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer neger
    df$Moeder_number <- ifelse(grepl("neger", tolower(df$Moeder)), "neger", df$Moeder_number)
   #remove neger
    df$Moeder <- ifelse(grepl("neger", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer karboeger
    df$Moeder_number <- ifelse(grepl("karboeg", tolower(df$Moeder)), "carboeger", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("carboeg", tolower(df$Moeder)), "carboeger", df$Moeder_number)
   #remove karboeger
    df$Moeder <- ifelse(grepl("karboeg", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("carboeg", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer mulat
    df$Moeder_number <- ifelse(grepl(" mulat", tolower(df$Moeder)), "mulat", df$Moeder_number)
   #remove mulat
    df$Moeder <- ifelse(grepl(" mulat", tolower(df$Moeder)), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer plantagenaam
    df$Moeder_number <- ifelse(grepl("Botervl:", df$Moeder), "Botervl:", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Brouwlust", df$Moeder), "Brouwerslust", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Brouwerslust", df$Moeder), "Brouwerslust", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Houttuin", df$Moeder), "Houttuin", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Land en Rust", df$Moeder), "Land en Rust", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Resolutie", df$Moeder), "Resolutie", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("van Saramacca", df$Moeder), "Saramacca", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Scheveningen", df$Moeder), "S", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("la Simplicite", df$Moeder), "la Simplicite", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Standvastigheid", df$Moeder), "Standvastigheid", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Stolk", df$Moeder), "Stolkwijk", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("Zeewijk", df$Moeder), "Zeewijk", df$Moeder_number)
    #remove plantagenaam
    df$Moeder <- ifelse(grepl("Botervl:", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Brouwlust", df$Moeder), gsub("\\(.*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Brouwerslust", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Houttuin", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Land en Rust", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Resolutie", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("van Saramacca", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Scheveningen", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("la Simplicite", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Standvastigheid", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Stolk", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("Zeewijk", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer vrij / vrijdom
    df$Moeder_number <- ifelse(grepl("vrij", tolower(df$Moeder)) & grepl("vrijdag", tolower(df$Moeder))==F, "vrij", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl("vrijdom", tolower(df$Moeder)), "voor den vrijdom verkocht", df$Moeder_number)
   #remove (de) vrije / vrijdom
    df$Moeder <- gsub("voor den vrijdom verkocht", " voor den vrijdom verkocht", df$Moeder)
    df$Moeder <- gsub("voor den Vrijdom verkocht", " voor den vrijdom verkocht", df$Moeder)
    df$Moeder <- ifelse(grepl("voor den vrijdom verkocht", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- gsub("De vrije ", "", df$Moeder)
    df$Moeder <- gsub("de vrije ", "", df$Moeder)
    df$Moeder <- gsub("devrije ", "", df$Moeder)
    df$Moeder <- gsub("vrije ", "", df$Moeder)
    df$Moeder <- gsub("de Vrije ", "", df$Moeder)
    df$Moeder <- gsub("De Vrije ", "", df$Moeder)
    df$Moeder <- gsub("Vrije ", "", df$Moeder)
    df$Moeder <- gsub(" / Vrij /", "", df$Moeder)
    df$Moeder <- gsub(" /Vrij/", "", df$Moeder)
    df$Moeder <- gsub("Vrij /", "", df$Moeder)
    df$Moeder <- gsub(" \\(Vrij)", "", df$Moeder)
    df$Moeder <- ifelse(grepl("vrij[a-z]", df$Moeder)==F, gsub("vrij", " vrijx", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl("vrijx", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
   #transfer gemanumd
    df$Moeder_number <- ifelse(grepl("gemanumd", df$Moeder), "gemanumiteerd", df$Moeder_number)
   #remove gemanumd
    df$Moeder <- gsub(" / gemanumd", "", df$Moeder)
    df$Moeder <- gsub("gemanum", "", df$Moeder)
   #transfer besmet
    df$Moeder_number <- ifelse(grepl("besmet", df$Moeder), "besmet", df$Moeder_number)
    df$Moeder <- ifelse(df$Moeder_number=="besmet", "Zwaantje", df$Moeder)
    
   #replace / with of
    as.data.frame(table(df[grepl("/", df$Moeder), "Moeder"]))
    df$Moeder <- ifelse(grepl("Bettie", df$Moeder) & grepl("Blacka", df$Moeder), "Bettie of Blacka", df$Moeder)
    df$Moeder <- ifelse(grepl("Bettie", df$Moeder) & grepl("Loango", df$Moeder), "Bettie of Loango", df$Moeder)
    df$Moeder <- ifelse(grepl("Dora", df$Moeder) & grepl("Batavia", df$Moeder), "Dora of Batavia", df$Moeder)
    df$Moeder <- ifelse(grepl("Julianna", df$Moeder) & grepl("Truitje", df$Moeder), "Julianna of Truitje", df$Moeder)
    df$Moeder <- ifelse(grepl("Wilhelmina", df$Moeder) & grepl("Affiba", df$Moeder), "Wilhelmina of Affiba", df$Moeder)
    df$Moeder <- ifelse(grepl("Wilhelmina", df$Moeder) & grepl("Jaba", df$Moeder), "Wilhelmina of Jaba", df$Moeder)
    df$Moeder_number <- ifelse(grepl("/.*/", df$Moeder), "J", df$Moeder_number)
    df$Moeder <- gsub(" /J/", "", df$Moeder)
    df$Moeder <- gsub("/", "", df$Moeder)
    df$Moeder <- trimws(df$Moeder)
   #()
    as.data.frame(table(df[grepl("\\(.*)", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl("\\(", df$Moeder), "Moeder"]))
    as.data.frame(table(df[grepl(")", df$Moeder), "Moeder"]))
    df$Moeder_number <- ifelse(grepl("Afrikaan", df$Moeder), "Afrikaan", df$Moeder_number)
    df$Moeder <- ifelse(grepl("Afrikaan", df$Moeder), gsub(" .*", "", df$Moeder), df$Moeder)
    df$Moeder <- gsub("Gla\\()", "Clao", df$Moeder)
    df$Moeder <- ifelse(grepl("Catharina", df$Moeder) & grepl("Anna)", df$Moeder), "Catharina of Anna", df$Moeder)
    df$Moeder <- ifelse(grepl("Affiba", df$Moeder) & grepl("Diana", df$Moeder), "Affiba of Diana", df$Moeder)
    df$Moeder <- ifelse(grepl("Christina", df$Moeder) & grepl("Margaretha", df$Moeder), "Christina of Margaretha", df$Moeder)
    df$Moeder <- ifelse(grepl("Lena", df$Moeder) & grepl("Lokenia", df$Moeder), "Lena of Lokenia", df$Moeder)
    df$Moeder <- ifelse(grepl("Trobelina", df$Moeder) & grepl("Minerva", df$Moeder), "Trobelina of Minerva", df$Moeder)
    df$Moeder <- ifelse(df$Moeder=="Magdalena Mietje", "Magdalena of Mietje", df$Moeder)
    df$Moeder <- ifelse(df$Moeder=="Mari ( Siribo", "Mari of Siribo", df$Moeder)
    df$Moeder <- ifelse(df$Moeder=="Mari  S", "Mari of Siribo", df$Moeder)
    df$Moeder <- ifelse(df$Moeder=="Mari (S", "Mari of Siribo", df$Moeder)
    df$Moeder <- gsub("Pima\\()", "Pimas", df$Moeder)
    df$Moeder <- gsub("\\(", "", df$Moeder)
    df$Moeder <- gsub(")", "", df$Moeder)
    df$Moeder <- trimws(df$Moeder)
   #-
    as.data.frame(table(df[grepl("-", df$Moeder), "Moeder"]))
    df$Moeder <- gsub("I-dem", "Bebe", df$Moeder)
    df$Moeder <- gsub("ij- Elisabeth", "Elisabeth", df$Moeder)
    df$Moeder <- gsub("-", " ", df$Moeder)
    df$Moeder <- trimws(df$Moeder)
   #onbekend
    df$Moeder <- gsub(" \\[onbekend]", "", df$Moeder)
    df$Moeder <- ifelse(grepl("onbekend", tolower(df$Moeder)), "", df$Moeder)
   #remove double spaces
    df$Moeder <- gsub("  ", " ", df$Moeder)
    df$Moeder <- gsub("  ", " ", df$Moeder)
   #remove ,
    df$Moeder <- gsub(",", "", df$Moeder)
   
   #namen
    df$Moeder <- gsub("Elis ", "Elisabeth ", df$Moeder)
    df$Moeder <- gsub("Elisabth", "Elisabeth", df$Moeder)
    #samentrekkingen
    df[grepl("[a-z][A-Z]", df$Moeder),c("Naam", "Moeder", "source_order")]
    
    
    
    #give overview
    as.data.frame(table(df$Moeder_number))
    
    
    
   #save look-up table
    #unique Naam_original variants + standardisation
    write.xlsx(df[!duplicated(df$Moeder_original),c("Moeder_original", "Moeder", "Moeder_number", "Moeder_birthyear", "Moeder_reference", "Moeder_overleden", "Moeder_1_Entry", "Moeder_2_Naam", "Moeder_incongruence", "source_order")] %>% arrange(trimws(Moeder_original)), "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Moeders - Conversietabel.xlsx")
    #frequency table of Naam
    x <- as.data.frame(table(df$Moeder)) %>% arrange(Var1)
    colnames(x) <- c("Naam", "Frequentie")
    write.xlsx(x, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Moeders.xlsx")
    rm(x)
    
    
    
  ####################################
  #### section 5: clean Eigenaren ####
  ####################################
    
   #change Avondrust into Arendrust
    df$Eigenaar <- gsub("Avondrust", "Arendrust", df$Eigenaar)
    df$plantation_name <- gsub("Avondrust", "Arendrust", df$plantation_name)
    
  #change from Mon Affaire to Akkerboom  
    df <- df %>% mutate(Eigenaar = replace(Eigenaar, Inventarisnummer == 21 & Folionummer == 2437 , "plantage Akkerboom divisie beneden Commewijne"),
                        plantation_name = replace(plantation_name, Inventarisnummer == 21 & Folionummer == 2437, "Akkerboom"))
  
  #Dubbel plantations  
   df <-  df %>% mutate(Eigenaar = replace(Eigenaar, Eigenaar == "plantage Nieuwe Hoop Saramacca" , "plantage Nieuw Hoop divisie Saramacca"), 
                        Eigenaar = replace(Eigenaar, Eigenaar == "plantage Nieuwe Hoop divisie Saramacca" , "plantage Nieuw Hoop divisie Saramacca"),
                        plantation_name = replace(plantation_name, plantation_name == "Nieuwe Hoop", "Nieuw Hoop"))
  
   df <-  df %>% mutate(Eigenaar = replace(Eigenaar, Eigenaar == "plantage Saint Germain beneden Commewijne" , "plantage St. Germain divisie beneden Commewijne"), 
                        plantation_name = replace(plantation_name, plantation_name == "Saint Germain", "St. Germain")) 
   
  #change from Montresor to Etablissement Montresor
   df <- df %>% mutate(plantation_name = replace(plantation_name, Inventarisnummer == 20 & Folionummer == 4562, "Etablissement Montresor"),
                       plantation_name = replace(plantation_name, Inventarisnummer == 31 & Folionummer == 1244, "Etablissement Montresor"))
   
    
   #filter privé-eigenaren
    Index <- df[df$Typeregister=="Particulieren", c("Eigenaar", "source_order")]
   #add corrected titles
    Index <- merge(Index, Eigenaren_standardized, by="source_order", all.x=T)
   #deduplicate Eigenaar
    Index <- Index[which(!duplicated(Index[,c("Eigenaar")])), ]
    
    
    #####################################################################
    ###   remove dots and redundant whitespaces + set to lower case   ###
    #####################################################################
    
    #check number of unique name entries
    length(Index$Eigenaar_standardised) #7,177
    #check number of unique name entries after:
    #remove double whitespace
    length(which(!duplicated(gsub("  ", " ", Index$Eigenaar_standardised)))) #7,063
    #remove leading whitespace
    length(which(!duplicated(trimws(Index$Eigenaar_standardised, "left")))) #7,065
    #remove trailing whitespace
    length(which(!duplicated(trimws(Index$Eigenaar_standardised, "right")))) #7,056
    #remove dots
    length(which(!duplicated(gsub("\\.", "", Index$Eigenaar_standardised)))) #6,837
    #delete all whitespaces
    length(which(!duplicated(gsub(" ", "", Index$Eigenaar_standardised)))) #6,975
    #set to lower case
    length(which(!duplicated(tolower(Index$Eigenaar_standardised)))) #6,973
    
    #set to lower & delete dots, double whitespace, and leading and trailing whitespace
    Index$Eigenaar_original <- Index$Eigenaar
    Index$Eigenaar <- Index$Eigenaar_standardised
    Index$Eigenaar <- tolower(Index$Eigenaar)
    Index$Eigenaar <- gsub("\\.,", ",", Index$Eigenaar)
    Index$Eigenaar <- gsub("\\.", " ", Index$Eigenaar)
    Index$Eigenaar <- gsub("  ", " ", Index$Eigenaar)
    Index$Eigenaar <- gsub("  ", " ", Index$Eigenaar)
    Index$Eigenaar <- trimws(Index$Eigenaar)
    length(which(!duplicated(Index$Eigenaar))) #6,584
    
    
    
  ###########################
  ###   Split last name   ###
  ###########################
    
  #attach mac to name
    Index[grepl("mac ", Index$Eigenaar),"Eigenaar"]
    Index[grepl("mc ", Index$Eigenaar),"Eigenaar"]
    Index$Eigenaar <- gsub("mac ", "mac", Index$Eigenaar)
    Index$Eigenaar <- gsub("mc ", "mc", Index$Eigenaar)
    
  #separate last name
    Index$Last_name <- sub(" .*", "", Index$Eigenaar)
    Index$temp <- sub(".*? ", "", Index$Eigenaar)
    
  #split comments
    Index$Eigenaar_mutatie <- ifelse(grepl("-", Index$temp), sub(".*? - ", "", Index$temp), "")
    Index$temp <- sub("-.*", "", Index$temp)
    Index$temp <- trimws(Index$temp)
    
  #filter representative
    head(Index[grepl("door ", Index$temp) & grepl("qq", Index$temp), c("source_order", "Last_name", "temp")])
    Index$Eigenaar_qq <- ifelse(grepl(", door", Index$temp), sub(".*?, door ", "", Index$temp), "")
    Index$Eigenaar_qq <- sub("qq.*", "", Index$Eigenaar_qq)
    Index$temp <- sub(", door.*qq,", "", Index$temp)
    Index$temp <- sub(", door.*qq", "", Index$temp)
    
  #filter straatvoogd
    head(Index[grepl(", als", Index$temp), c("source_order", "Last_name", "temp")])
    Index$Straatvoogd <- ifelse(grepl(", als ", Index$temp), sub(".*?, als ", "", Index$temp), "")
    Index$temp <- gsub(", als.*", "", Index$temp)
    
  #make variables
    Index$Person_description <- NA
    
  #filter rechtspersonen
   #set function
    rm_rp <- function(Last_name, Temp, Input1, Input2){
      Index$Last_name <<- ifelse(Index$Last_name==Last_name & Index$temp==Temp, Input1, Index$Last_name)
      Index$Person_description <<- ifelse(Index$Last_name==Last_name & Index$temp==Temp, "rechtspersoon", Index$Person_description)
      Index$temp <<- ifelse(Index$Last_name==Input1 & Index$temp==Temp, Input2, Index$temp)
    }
   #key words rechtspersoon
    Index[grepl(" en co", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl(" en zn", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl(" en zo", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("comp", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("erven", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("bank", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("firma", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("fonds", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("gemeente", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("lands grond", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("maatschappij", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("plantage", Index$Last_name),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
    Index[grepl("respect", Index$Eigenaar),c("source_order", "Last_name", "temp")] %>% arrange(Last_name)
   #en co
    #box nicolaas en co
    rm_rp("box", "nicolaas en co", "box en co", "nicolaas")
    #box n en co
    rm_rp("box", "n en co", "box en co", "n")
    #carbin willem en co
    rm_rp("carbin", "willem en co", "carbin en co", "willem")
    #colin campbell dent en co
    rm_rp("colin", "campbell dent en co", "colin campbell dent en co", "")
    #de geexisteerd hebbende firma van william leckie en co
    rm_rp("de", "geexisteerd hebbende firma van william leckie en co", "leckie en co", "geexisteerd hebbende firma william")
    #de geexisteerd hebbende firma van ferrier parrij en co
    rm_rp("de", "geexisteerd hebbende firma van ferrier parrij en co", "ferrier en co", "geexisteerd hebbende firma parrij")
    #de jonge en co
    rm_rp("de", "jonge en co", "jonge en co", "de")
    #huidekoper en co
    rm_rp("huidekoper", "en co", "huidekoper en co", "")
    #insenger en co en j j van_de_poll
    rm_rp("insenger", "en co en j j van_de_poll", "insenger en co", "en j j van_de_poll")
    #insinger en co
    rm_rp("insinger", "en co", "insinger en co", "")
    #insinger en comp en j j van_de_poll
    rm_rp("insinger", "en comp en j j van_de_poll", "insinger en co", "en j j van_de_poll")
    #kersten c en co
    rm_rp("kersten", "c en co", "kersten en co", "c")
    #macintosch a en co
    rm_rp("macintosch", "a en co", "macintosch en co", "a")
    #macintosh a en co
    rm_rp("macintosh", "a en co", "macintosh en co", "a")
    #poel pieter en zoon, abraham broen en co, constatijn benelle vereul
    rm_rp("poel", "pieter en zoon, abraham broen en co, constatijn benelle vereul", "poel en zoon", "pieter, abraham broen en co, constatijn benelle vereul")
    #queckenberg en co en a scheuten
    rm_rp("queckenberg", "en co en a scheuten", "queckenberg en co", "en a scheuten")
    #richards mcintosh en co
    rm_rp("richards", "mcintosh en co", "richards mcintosh en co", "")
    #richards mcintosh en co
    rm_rp("richards", "macintosh en co", "richards macintosh en co", "")
    #rickards mcintosh en co
    rm_rp("rickards", "macintosh en co", "richards macintosh en co", "")
    #t fonds onder administratie van insinger en co
    rm_rp("t", "fonds onder administratie van insinger en co", "t fonds onder administratie van insinger en co", "")
    #thijm rothuijs en co
    rm_rp("thijm", "rothuijs en co", "thijm rothuijs en co", "")
    #west en co van
    rm_rp("west", "en co van", "west en co", "van")
   #en zn
    #jager uitlandige jan de en zn
    rm_rp("jager", "uitlandige jan de en zn", "jager en co", "uitlandige jan de")
   #en zo
    #charbon en zoon
    rm_rp("charbon", "en zoon", "charbon en co", "")
    #poel en zonen te amsterdam, weduwe c e van_doeveren, weduwe a b en pieter van_daalen
    rm_rp("poel", "en zonen te amsterdam, weduwe c e van_doeveren, weduwe a b en pieter van_daalen", "poel en zoon", "te amsterdam, weduwe c e van_doeveren, weduwe a b en pieter van_daalen")
    #poncelet j j en zoon
    rm_rp("poncelet", "j j en zoon", "poncelet en zoon", "j j")
    #thijm weduwe lambertus en zoon te amsterdam
    rm_rp("thijm", "weduwe lambertus en zoon te amsterdam", "thijm en zoon", "weduwe lambertus en zoon te amsterdam")
    #thijm weduwe lambs en zoon te amsterdam
    rm_rp("thijm", "weduwe lambs en zoon te amsterdam", "thijm en zoon", "weduwe lambs te amsterdam")
   #comp
    #ferrier parrij compe
    rm_rp("ferrier", "parrij compe", "ferrie en co", "parrij")
   #erven
    #de erven mr becker en spiering
    rm_rp("de", "erven mr becker en spiering", "erven mr becker en spiering", "de")
   #bank
    #hoofd directie der particuliere west indische bank
    rm_rp("hoofd", "directie der particuliere west indische bank", "hoofd directie der particuliere west indische bank", "")
   #firma
    #firma van swijt en ries
    rm_rp("de", "firma van swijt en ries", "swijt en ries", "firma")
    #moore boedel john en de firma van martin en dupont
    rm_rp("moore", "boedel john en de firma van martin en dupont", "moore", "boedel john en firma martin en dupont")
   #fonds
    #de r c kerk en liefdefonds daaraan geattacheerd
    rm_rp("de", "r c kerk en liefdefonds daaraan geattacheerd", "roomsch catholijke kerk en liefdefonds", "")
    #de roomsch catholijke kerk en liefdefonds
    rm_rp("de", "roomsch catholijke kerk en liefdefonds", "roomsch catholijke kerk en liefdefonds", "")
    #t fonds g a de graaf
    rm_rp("t", "fonds g a de graaff", "t fonds g a de graaf", "")
    #'t fonds g a de graaf
    rm_rp("'t", "fonds g a de graaf", "t fonds g a de graaf", "")
    #t fonds w g deutz
    rm_rp("t", "fonds w g deutz", "t fonds w g deutz", "")
    #'t fonds w g deutz
    rm_rp("'t", "fonds w g deutz", "t fonds w g deutz", "")
    #t fonds onder administratie van insinger en co
    rm_rp("t", "fonds onder administratie van insinger en co", "t fonds onder administratie van insinger en co", "")
   #gemeente
    #evangelische broeder gemeente
    rm_rp("evangelische", "broeder gemeente", "evangelische broeder gemeente", "")
    #gemeente roomsch catholijke
    rm_rp("gemeente", "roomsch catholijke", "roomsch catholijke gemeente", "")
    #kerk der hervormde gemeente
    rm_rp("kerk", "der hervormde gemeente", "hervormde gemeente", "")
    #n i gemeente
    rm_rp("n", "i gemeente", "nederlands israelitische gemeente", "")
    #nederl israelitische gemeente
    rm_rp("nederl", "israelitische gemeente", "nederlands israelitische gemeente", "")
    #nederlands israelitische gemeente
    rm_rp("nederlands", "israelitische gemeente", "nederlands israelitische gemeente", "")
    #nederlandsch israelitische gemeente
    rm_rp("nederlandsch", "israelitische gemeente", "nederlands israelitische gemeente", "")
   #landgrond
    #s lands grond boniface
    rm_rp("s", "lands grond boniface", "s lands grond boniface", "")
   #maatschappij
    #maatschappij tot uitbreiding van het christendom etc
    rm_rp("maatschappij", "tot uitbreiding van het christendom etc", "maatschappij tot uitbreiding van het christendom", "")
    #maatschappij tot uitbreiding van het christendom etc
    rm_rp("maatschappij", "tot uitbreiding van het christendom", "maatschappij tot uitbreiding van het christendom", "")
    #maatschappij tot uitbreiding van het christendom etc
    rm_rp("maatschappij", "ter uitbreiding van het christendom", "maatschappij tot uitbreiding van het christendom", "")
   #plantage
    #plantage leliendaal
    rm_rp("plantage", "leliendaal", "plantage leliendaal", "")
    #plantage cromelinsgift
    rm_rp("plantage", "cromelinsgift", "plantage cromelinsgift", "")
    #plantage scheveningen
    rm_rp("plantage", "scheveningen", "plantage scheveningen", "")
   #respect
    #respect van den raad commissaris voor de inlandsche bevolking voor slaven die geen meester hebben
    rm_rp("t", "respect van den raad commissaris voor de inlandsche bevolking voor slaven die geen meester hebben", "t respect van den raad commissaris voor de inlandsche bevolking voor slaven die geenen meester hebben", "")
    #respect van den raad commissaris voor de inlandsche bevolking voor slaven die geenen meester hebben
    rm_rp("t", "respect van den raad commissaris voor de inlandsche bevolking voor slaven die geenen meester hebben", "t respect van den raad commissaris voor de inlandsche bevolking voor slaven die geenen meester hebben", "")
   #overig
    #van west en de hart 
    rm_rp("van", "west en de hart", "van west en de hart", "")
    #van west en de hart te amsterdam
    rm_rp("van", "west en de hart te amsterdam", "van west en de hart", "te amsterdam")
    
  #separate prefixes
   #make variable
    Index$Prefix <- NA
   #set function
    rm_prefix <- function(combinatie){
      Index$Prefix <<- ifelse(substr(Index$temp, nchar(Index$temp)-nchar(combinatie), nchar(Index$temp))==paste0(" ", combinatie), #if last 6 characters are whitespace + combination
                              substr(Index$temp, nchar(Index$temp)-nchar(combinatie)+1, nchar(Index$temp)), Index$Prefix) #save last 5 characters as prefix
      Index$temp <<- ifelse(substr(Index$temp, nchar(Index$temp)-nchar(combinatie), nchar(Index$temp))==paste0(" ", combinatie), #if last 6 characters are whitespace + combination
                            substr(Index$temp, 1, nchar(Index$temp)-nchar(combinatie)-1), Index$temp) #drop last 5 characters + leading whitespace from temp
    }
   #explore
    Index[which(substr(Index$temp, nchar(Index$temp)-5, nchar(Index$temp))==" bo de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-8, nchar(Index$temp))==" bueno de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" da"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" du"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" d'"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" del"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" der"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" des"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" het"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" 't"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" la"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" le"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" l'"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" ter"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" van"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" von"),"temp"]
    #bueno de mesquita
    Index$temp <- gsub(" bo de", " bueno de", Index$temp)
    Index$temp <- gsub("d'abm", "d'abraham", Index$temp)
    Index$temp <- gsub("d' abm", "d'abraham", Index$temp)
    Index$temp <- ifelse(grepl("d'ab[a-z]", Index$temp)==F, gsub("d'ab", "d'abraham", Index$temp), Index$temp)
    Index$temp <- gsub("de abm", "de abraham", Index$temp)
    rm_prefix("d'abraham bueno de")
    rm_prefix("de abraham bueno de")
    rm_prefix("bueno de")
    Index$Last_name[grepl("abraham bueno", Index$Prefix)] <- "abraham bueno de mesquita"
    Index$Prefix[Index$Last_name=="abraham bueno de mesquita"] <- "de"
    Index$Last_name[Index$Prefix=="bueno de"] <- "bueno de mesquita"
    Index$Prefix[Index$Last_name=="bueno de mesquita"] <- ""
    #split prefixes
    rm_prefix("d'")
    rm_prefix("da")
    rm_prefix("de")
    rm_prefix("del")
    rm_prefix("des")
    rm_prefix("de la")
    rm_prefix("d' la")
    rm_prefix("de l'")
    rm_prefix("du")
    rm_prefix("l'")
    rm_prefix("la")
    rm_prefix("le")
    rm_prefix("ter")
    rm_prefix("van van")
    rm_prefix("van")
    rm_prefix("van de")
    rm_prefix("van du")
    rm_prefix("van den")
    rm_prefix("van der")
    rm_prefix("van het")
    rm_prefix("van 't")
    rm_prefix("van la")
    rm_prefix("von")
    
    
  #samengestelde namen
    Index$Prefix <- ifelse(grepl("van ", Index$temp), paste("van", sub(".*?van ", "", Index$temp), Index$Prefix, sep=" "), Index$Prefix)
    Index$Prefix <- trimws(gsub("NA", "", Index$Prefix))
    Index$temp <- trimws(gsub("van .*", "", Index$temp))
    
    
  #rm _
    Index$temp <- gsub("_", " ", Index$temp)
    Index$Last_name <- gsub("_", " ", Index$Last_name)
   
    
  #Postfix
   #set function
   #sr
   #jr
   #mr
   #douariere
    
  #personal information
   #boedel
   #uitlandige
   #te
   #de vrije
    
  #relaties standaardiseren
   #de erven
   #erfgenamen
   #prive & nom ux
   #prive & minderjarige 
   #prive, nom ux & kinderen
   #prive
   #nom ux
   #minderjarige / kinderen van
   #weduwe
   #kinderen
    
    
  #voormalige naam
    Index[which(grepl("geb", Index$temp) & grepl("geboren", Index$temp)==F),"temp"]
    Index$temp <- gsub("geb ", "geboren ", Index$temp)
    Index$temp <- gsub("gebr ", "geboren ", Index$temp)
    Index$temp <- gsub("gebs ", "geboren ", Index$temp)
    Index$temp <- gsub("gebn ", "geboren ", Index$temp)
    
   #
    
   #add to df
    Index$First_name <- Index$temp
    Index <- Index[,c("source_order", "Eigenaar_original", "Prefix", "Last_name", "First_name", "Straatvoogd", "Eigenaar_qq")]
    colnames(Index) <- c("source_order", 
                         "Eigenaar", 
                         "Eigenaar_Prefix", "Eigenaar_Last_name", "Eigenaar_First_name",
                         "Eigenaar_Straatvoogd",
                         "Eigenaar_qq")
   #save look-up table
    write.xlsx(Index %>% arrange(trimws(Eigenaar)), "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Eigenaren - particulieren.xlsx")
    write.xlsx(df[!duplicated(df$Eigenaar) & df$Typeregister=="Plantages", c("Eigenaar", "plantation_name", "plantation_district")], "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Eigenaren - plantages.xlsx")
   #add to df
    df <- merge(df, Index[,c("Eigenaar", "Eigenaar_Prefix", "Eigenaar_Last_name", "Eigenaar_First_name", "Eigenaar_Straatvoogd", "Eigenaar_qq")], by="Eigenaar", all=T)
    
    
    
  #####################################
  #### section 6: clean rare names ####
  #####################################
    
    #Naam
    x <- df %>% arrange(Naam) %>% group_by(Naam) %>% filter(n()<=5) %>% ungroup()
    x$Eigenaar <- ifelse(x$Typeregister=="Plantages", x$plantation_name, x$Eigenaar_Last_name)
    x <- x[,c("Naam", "sex", "Naam_number", "Eigenaar", "year_birth", "source_order")]
    x$role <- "Naam"
    #Moeder
    y <- df %>% arrange(Moeder) %>% group_by(Moeder) %>% filter(n()<=5) %>% ungroup()
    y$Eigenaar <- ifelse(y$Typeregister=="Plantages", y$plantation_name, y$Eigenaar_Last_name)
    y <- y[,c("Moeder", "sex", "Moeder_number", "Eigenaar", "year_birth", "source_order")]
    colnames(y) <- c("Naam", "sex", "Naam_number", "Eigenaar", "year_birth", "source_order")
    y$role <- "Moeder"
    #combine
    x <- rbind(x,y)
    x <- x[,c("role", "sex", "Naam", "Naam_number", "Eigenaar", "year_birth", "source_order")] %>% arrange(Naam)
    #export
    write.xlsx(x, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Namenlijsten/Overzicht niet-courante namen.xlsx")
    rm(x,y)
    
    
    
  ################################################
  #### section 7: Recode In_event & Out_event ####
  ################################################
    
  #recode events
    df$out_event2 <- ifelse(df$out_event=="End Series" | 
                              df$out_event=="End Series/Freedom", "Ended",
                            ifelse(df$out_event=="Exchanged" |
                                     df$out_event=="Given away" |
                                     df$out_event=="Given away by Inheritance" |
                                     df$out_event=="Overgeschreven" |
                                     df$out_event=="Sold" |
                                     df$out_event=="Sold (executie)" |
                                     df$out_event=="Sold (publieke veiling)" |
                                     df$out_event=="Sold (vendu)" |
                                     df$out_event=="Sold/Given for Freedom", "Transferred",
                                   ifelse(df$out_event=="Death" |
                                            df$out_event=="Drowned" |
                                            df$out_event=="Killed", "Death",
                                          ifelse(df$out_event=="Unknown", "Unknown", "Other"))))
    df$in_event2 <- ifelse(df$in_event=="Start Series", "Beginning", 
                           ifelse(df$in_event=="Acquired (executie)" |
                                    df$in_event=="Acquired (publieke veiling)" |
                                    df$in_event=="Acquired (vendu)" |
                                    df$in_event=="Acquired/Inherited For Freedom" |
                                    df$in_event=="Acquired/Transferred" |
                                    df$in_event=="Exchanged" |
                                    df$in_event=="Inherited" |
                                    df$in_event=="Verpand", "Transferred",
                                  ifelse(df$in_event=="Birth", "Birth",
                                         ifelse(df$in_event=="Unknown", "Unknown", "Other"))))
    df$in_event2 <- ifelse(df$in_event=="Verpand" & df$year_entry==1830 & df$month_entry==-1 |
                             df$in_event=="Verpand" & df$year_entry==1838 & df$month_entry==-1, "Beginning", df$in_event2)
    
    
  
    ######################################
    #### section 8: Impute birth year ####
    ######################################
    
    #retrieve years from aanvullende informatie inschrijving
    #retrieve entries with four consecutive numbers
    #df$year_birth3 <- ifelse(grepl("[0-9][0-9][0-9][0-9]", df$Aanvullendeinformatieinschrijv) & df$in_event2=="Birth",
    #                         gsub("([0-9][0-9][0-9][0-9]).*", "\\1", df$Aanvullendeinformatieinschrijv),
    #                         NA)
    #df$year_birth3 <- gsub(".*([0-9][0-9][0-9][0-9])", "\\1", df$year_birth3)
    #filter dates
    #df$year_birth3 <- as.numeric(df$year_birth3)
    #df$year_birth3 <- ifelse(df$year_birth3>=1830 & df$year_birth3<=1863, df$year_birth3, NA)
    
    
    
    df$year_birth2 <- ifelse(df$year_birth!=-1, df$year_birth,
                             ifelse(df$in_event2=="Birth" & df$year_entry!=-1, df$year_entry, df$year_birth_age_based))
    df$year_birth_flag <- ifelse(df$year_birth!=-1, "year_birth",
                                 ifelse(df$in_event2=="Birth" & df$year_entry!=-1, "in_event", 
                                        ifelse(df$year_birth_age_based!=-1, "year_birth_age_based", "")))

    ####################################################################################
    #### section 9: Append Pages From Serie 4 Reconstructed by Coen in 2022         ####
    ####################################################################################    
    

    #Inlezen  
    df_recon <- read.xlsx("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/Reconstructie Serie 4/Controlelijst_aanwezige_folios_serie 4_final.xlsx") %>%
      filter(!(is.na(Betrouwbaarheid)))
    
    #Set to character to enable appending
    df_recon$plantation_remarks <- as.character(df_recon$plantation_remarks)
    df_recon$Moeder_reference <- as.character(df_recon$Moeder_reference)
    df_recon$Eigenaar_Straatvoogd <- as.character(df_recon$Eigenaar_Straatvoogd)
    df_recon$Naam_unintelligible <- as.character(df_recon$Naam_unintelligible)
    
    #Append and remove half-empty pages from df
    df <- bind_rows (df, df_recon) %>%
      filter(primary_key < 112312 | primary_key > 112323 | is.na(primary_key)) %>%
      filter(primary_key < 111622 | primary_key > 111633 | is.na(primary_key)) %>%
      filter(primary_key < 112214 | primary_key > 112231 | is.na(primary_key)) %>%
      filter(primary_key != 95961 | is.na(primary_key))
    
    #Remove edited entries
    df <- df %>%
      group_by(primary_key) %>%
      filter(!(n() ==2 & is.na(Betrouwbaarheid))) %>%
      arrange(primary_key) %>%
      ungroup() %>%
      select(-primary_key) %>%
      mutate(primary_key = row_number())
    
    #Correct few exit and entry event
    df <- df %>% mutate(out_event = replace(out_event, out_event =="Sold (publieke veiling)", "Sold (vendu)"),
                        in_event = replace(in_event, in_event =="Acquired (publieke veiling)", "Acquired (vendu)"))
  ##########################################################################
  #### section 10: Clean some additional stuff that we came across      ####
  ##########################################################################  
  
  #Fanny en haar kinderen Adriaan en Zemire  
  df <- df %>% mutate(Naam = replace(Naam, source_order == "122729b46132_Y", "Adriaan"),
                        Naam = replace(Naam, source_order == "122729b46132_Z", "Zemire"),
                        Moeder = replace(Moeder, source_order == "122729b46132_Y", "Fanny"),
                        Moeder = replace(Moeder, source_order == "122729b46132_Z", "Fanny"))  
    
  #Edit two Plantation names
  df<- df %>% 
    filter(source_order != "364003136047") %>% # Unknown plantation
    mutate(plantation_name = replace(plantation_name, plantation_name == "Lot", "Lot Litt. K"),
           Eigenaar = replace(Eigenaar, Eigenaar == "Lot Nickerie", "Lot K")) # Rename "Lot" as "Lot K"
    
    
  # Wrong Aanvullendeinformatie inschrijving for plantation Visserszorg  
  df <- df %>%
    mutate(month_entry = replace(month_entry, Aanvullendeinformatieinschrijv == "Afgeschreven van den naam van den Uitlandigen F. Taunaij ingevolg aangifte d.d. 28 November 1848 No. 8 (Zie daar 1848. Journaal Numero No 8)." & plantation_name == "Visserszorg", 4),
           Aanvullendeinformatieinschrijv = replace(Aanvullendeinformatieinschrijv, Aanvullendeinformatieinschrijv == "Afgeschreven van den naam van den Uitlandigen F. Taunaij ingevolg aangifte d.d. 28 November 1848 No. 8 (Zie daar 1848. Journaal Numero No 8)." & plantation_name == "Visserszorg",
                                                    "Afgeschreven van de plantage Hamburg in boven Cottica volgend Gouvernments Resolutie 28 April 1848 No 850"))
           
  
    
  ##############################################################
  #### section 11: save cleaned outfile, ready for matching ####
  ##############################################################
    
    colnames(df)
    df <- df[, c("source_order", "primary_key", 
                 "Inventarisnummer", "Folionummer", "Serieregister", "Serieregister_nr", "Anno", "Typeregister", "Scan",
                 "Geslacht", "sex",
                 "Naam", "Naam_number", "Extrainformatiebijnaam", "Naam_original",
                 "day_birth", "month_birth", "year_birth", "year_birth_age_based", "year_birth2", "year_birth_flag",
                 "day_death", "month_death", "year_death",
                 "age",
                 "Moeder", "Moeder_number", "Moeder_birthyear", "Moeder_reference", "Moeder_overleden", "Moeder_1_Entry", "Moeder_2_Naam", "Moeder_incongruence", "Moeder_original",
                 "Eigenaar", "plantation_name", "plantation_district", "plantation_remarks",
                 "Eigenaar_Prefix", "Eigenaar_Last_name", "Eigenaar_First_name", "Eigenaar_Straatvoogd", "Eigenaar_qq",
                 "day_entry", "month_entry", "year_entry", 
                 "in_event", "in_event_general", "in_event2", "Aanvullendeinformatieinschrijv",
                 "out_event", "out_event_general", "out_event2", "day_exit", "month_exit", "year_exit", "Aanvullendeinformatieuitschrij", "Reconstructiebron", "Betrouwbaarheid")]
    
    
   #write outfiles
    write.table(df, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/cleaned slave register ", Sys.Date(), ".txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding = 'UTF-8')
    write.xlsx(df, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry/cleaned slave register ", Sys.Date(), ".xlsx"))
    
    
   
    
    
    
