  
  #load packages
  library("data.table")
  library("dplyr")
  library("stringdist")
  library("openxlsx")
  library("tidyr")
  
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
  
  #open dataset
  setwd("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching")
  df <- fread("Cleaned Registry/cleaned slave register 2022-09-17.txt", encoding="UTF-8")

  #set max lev dist for matching procedure
  set_lv_dist <- 3
  
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
  source("U:/Surfdrive/GitHub/Slavenregisters/within matching.R")
  source("U:/Surfdrive/GitHub/Slavenregisters/between matching.R")
  source("U:/Surfdrive/GitHub/Slavenregisters/split_names.R")
  
  ######################################
  #### section 0: standardise names ####
  ######################################
  
  #select name owner & year of birth
    df$Eigenaar_original <- df$Eigenaar
    df$Eigenaar <- ifelse(df$Typeregister=="Plantages", df$plantation_name, df$Eigenaar_Last_name)
    df$year_birth_original <- df$year_birth
    df$year_birth <- df$year_birth2
    
  #standardise names
    df$Naam_original <- df$Naam
    df$Naam <- tolower(df$Naam)
    df$Naam <- gsub(" of ", " ", df$Naam)
    df$Naam <- gsub("ç", "c", df$Naam)
    df$Naam <- gsub("é", "e", df$Naam)
    df$Naam <- gsub("kw", "qu", df$Naam)
    df$Naam <- gsub("ph", "f", df$Naam)
    
  #standardise moeder
    df$Moeder_original <- df$Moeder
    df$Moeder <- tolower(df$Moeder)
    df$Moeder <- gsub(" of ", " ", df$Moeder)
    df$Moeder <- gsub("ç", "c", df$Moeder)
    df$Moeder <- gsub("é", "e", df$Moeder)
    df$Moeder <- gsub("kw", "qu", df$Moeder)
    df$Moeder <- gsub("ph", "f", df$Moeder)
    
  #split naam
    df <- split_names(df, "Naam")
    df <- split_names(df, "Moeder")
    
  #remove white spaces
    df$Naam <- gsub(" ", "", df$Naam)
    df$Moeder <- gsub(" ", "", df$Moeder)
    
    
    
  #####################################################
  #### section 1a: retrieve matches BETWEEN series ####
  #####################################################
    
  #match serie 3 & 4
   #select series
    Serie3 <- df[which(df$Serieregister_nr==3), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie34 <- match_between(Serie3, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=3, NUMMER2=4)
    
  #match serie 2 & 3
   #select series
    Serie2 <- df[which(df$Serieregister_nr==2 & df$out_event2=="Ended" |
                         df$Serieregister_nr==2 & df$year_entry>=1848) , c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3 & df$in_event2=="Beginning" |
                   df$Serieregister_nr==3 & df$year_entry==1848, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie23 <- match_between(Serie2, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=3)
    
  #match serie 1 & 2
   #select series
    Serie1 <- df[which(df$Serieregister_nr==1 & df$out_event2=="Ended" |
                         df$Serieregister_nr==1 & df$year_entry>=1838), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie2 <- df[df$Serieregister_nr==2 & df$in_event2=="Beginning" |
                   df$Serieregister_nr==2 & df$year_entry==1838, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie12 <- match_between(Serie1, Serie2, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=2)
    
  #match serie 2 & 4
   #select series
    Serie2 <- df[df$Serieregister_nr==2 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=1848 |
                   df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth==-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie24 <- match_between(Serie2, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=2, NUMMER2=4)
    
  #match serie 1 & 4
   #select series
    Serie1 <- df[df$Serieregister_nr==1 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=1838 |
                   df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie14 <- match_between(Serie1, Serie4, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=4)
    
  #match serie 1 & 3
   #select series
    Serie1 <- df[df$Serieregister_nr==1 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3 & df$in_event2=="Beginning" & df$year_birth<=1838 |
                   df$Serieregister_nr==3 & df$in_event2=="Beginning" & df$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie13 <- match_between(Serie1, Serie3, lev_dist_naam=set_lv_dist, lev_dist_moeder=set_lv_dist, lev_dist_eigenaar=set_lv_dist, lev_dist_laglead=set_lv_dist, NUMMER1=1, NUMMER2=3)
    
    
  ####################################################
  #### section 1b: retrieve matches WITHIN series ####
  ####################################################
    
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth", "month_birth", "day_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie4 <- Serie4[which(Serie4$out_event2=="Transferred" | Serie4$in_event2=="Transferred"),]
    Serie3 <- df[df$Serieregister_nr==3, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth", "month_birth", "day_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie3 <- Serie3[which(Serie3$out_event2=="Transferred" | Serie3$in_event2=="Transferred"),]
    Serie2 <- df[df$Serieregister_nr==2, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth", "month_birth", "day_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie2 <- Serie2[which(Serie2$out_event2=="Transferred" | Serie2$in_event2=="Transferred"),]
    Serie1 <- df[df$Serieregister_nr==1, c("source_order", 
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
    
  #select Alkmaar & Barbados
    Alkmaar <- Serie34[grepl("Alkmaar", Serie34$Eigenaar_3) | grepl("Alkmaar", Serie34$Eigenaar_4),]
    Barbados <- Serie34[grepl("Barbados", Serie34$Eigenaar_3) | grepl("Barbados", Serie34$Eigenaar_4),]
    
  #make directories
    if (file.exists(paste0("Between/", Sys.Date()))){
      getwd()
    } else {
      dir.create(paste0(getwd(), "/Between/", Sys.Date()))
      dir.create(paste0(getwd(), "/Between/", Sys.Date(), "/Particulieren"))
      dir.create(paste0(getwd(), "/Between/", Sys.Date(), "/Plantages"))
      dir.create(paste0(getwd(), "/Within/", Sys.Date()))
    }
    
    
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
    
    
    
  ##########################################################
  #### section 2: load program to filter unique matches ####
  ##########################################################
    
    filter_unique <- function(df1, threshold, NUMMER1, NUMMER2){
      
      #make temporary variable to replace source_order in series 1 and 2
      df1$Order_1 <- df1[[paste("Source_order", NUMMER1, sep="_")]]
      df1$Order_2 <- df1[[paste("Source_order", NUMMER2, sep="_")]]
      #filter unreliable matches
      Serie_unique <- df1[df1$Match_score>=threshold,]
      #filter best match from serie 1
      Serie_unique <- Serie_unique %>% group_by(Order_1) %>% filter(is.na(Match_score) | Match_score==max(Match_score)) %>% arrange(Order_1) %>% ungroup()
      #filter best match from serie 2
      Serie_unique <- Serie_unique %>% group_by(Order_2) %>% filter(is.na(Match_score) | Match_score==max(Match_score)) %>% arrange(Order_1) %>% ungroup()
      #filter unclear matches
      Serie_unique <- Serie_unique %>% group_by(Order_1) %>% filter(is.na(Match_score) | n()==1) %>% ungroup() %>% arrange(Order_1)
      Serie_unique <- Serie_unique %>% group_by(Order_2) %>% filter(is.na(Match_score) | n()==1) %>% ungroup() %>% arrange(Order_1)
      #restore deleted entries cases
      #from serie3
      x <- df1[!(df1$Order_1 %in% Serie_unique$Order_1),]
      x <- x[!duplicated(x$Order_1), ]
      x[, which(colnames(x) %in% c("Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", 
                                   "Match_volgende_adaptive"))] <- 0
      x[, which(colnames(x) %in% paste(c("Match_score", "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv",
                                         "Typeregister", "In_event", "Sex", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "Naam_vorige", "Naam_volgende",
                                         "Source_order", "Year_birth"), NUMMER2, sep="_"))] <- NA
      #from serie4
      y <- df1[!(df1$Order_2 %in% Serie_unique$Order_2),]
      y <- y[!duplicated(y$Order_2), ]
      y[, which(colnames(y) %in% c("Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", 
                                   "Match_volgende_adaptive"))] <- 0
      y[, which(colnames(y) %in% paste(c("Match_score", "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv",
                                         "Typeregister", "In_event", "Sex", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "Naam_vorige", "Naam_volgende",
                                         "Source_order", "Year_birth"), NUMMER1, sep="_"))] <- NA
      #bind
      Serie_unique <- rbind(Serie_unique, x, y) %>% arrange(Order_1, Order_2)
      rm(x, y)
      Serie_unique[,which(colnames(Serie_unique)!=c("Order_1", "Order_2"))]
    }
    
    
  ##############################################################
  #### section 3a: group matches between consecutive series ####
  ##############################################################
    
   #filter unique matches
    Serie34 <- filter_unique(Serie34, threshold34, 3, 4)
    Serie23 <- filter_unique(Serie23, threshold23, 2, 3)
    Serie12 <- filter_unique(Serie12, threshold12, 1, 2)
    
   #set Source_order_3 to character
    Serie34$Source_order_3 <- as.character(Serie34$Source_order_3) 
    Serie23$Source_order_3 <- as.character(Serie23$Source_order_3) 
    
   #merge consecutive series
   #3-4
    reconstitution <- Serie34[, c("Source_order_3", "Source_order_4")]
   #2-3
    #matched cases
    reconstitution1 <- Serie23[!is.na(Serie23$Source_order_3), c("Source_order_2", "Source_order_3")]
    reconstitution1 <- merge(reconstitution, reconstitution1, by="Source_order_3", all=T)
    #unmatched cases
    reconstitution2 <- Serie23[is.na(Serie23$Source_order_3),  c("Source_order_2", "Source_order_3")]
    reconstitution2 <- merge(reconstitution[!is.na(reconstitution$Source_order_3),], reconstitution2, by="Source_order_3", all.y=T)
    #bind
    reconstitution <- rbind(reconstitution1, reconstitution2)
   #1-2
    #matched cases
    reconstitution1 <- Serie12[!is.na(Serie12$Source_order_2), c("Source_order_1", "Source_order_2")]
    reconstitution1 <- merge(reconstitution, reconstitution1, by="Source_order_2", all=T)
    #unmatched cases
    reconstitution2 <- Serie12[is.na(Serie12$Source_order_2),  c("Source_order_1", "Source_order_2")]
    reconstitution2 <- merge(reconstitution[!is.na(reconstitution$Source_order_2),], reconstitution2, by="Source_order_2", all.y=T)
    #bind
    reconstitution <- rbind(reconstitution1, reconstitution2)
   
   #clean environment
    rm(reconstitution1, reconstitution2)
    
    
  ##################################################################
  #### section 3b: group matches between non-consecutive series ####
  ##################################################################
    
   #filter unique matches
    Serie24 <- filter_unique(Serie24, threshold24, 2, 4)
    Serie14 <- filter_unique(Serie14, threshold14, 1, 4)
    Serie13 <- filter_unique(Serie13, threshold13, 1, 3)
    
   #set Source_order_3 to character
    Serie13$Source_order_3 <- as.character(Serie13$Source_order_3) 
    
  #add info from non-consecutive series to consecutive matches
   #2-4
    reconstitution1 <- Serie24[!is.na(Serie24$Source_order_2) & !is.na(Serie24$Source_order_4), c("Source_order_2", "Source_order_4")]
    colnames(reconstitution1) <- c("Source_order_24", "Source_order_4")
    reconstitution <- merge(reconstitution, reconstitution1, by="Source_order_4", all.x=T)
   #1-4
    reconstitution1 <- Serie14[!is.na(Serie14$Source_order_1) & !is.na(Serie14$Source_order_4), c("Source_order_1", "Source_order_4")]
    colnames(reconstitution1) <- c("Source_order_14", "Source_order_4")
    reconstitution <- merge(reconstitution, reconstitution1, by="Source_order_4", all.x=T)
   #1-3
    reconstitution1 <- Serie13[!is.na(Serie13$Source_order_1) & !is.na(Serie13$Source_order_3), c("Source_order_1", "Source_order_3")]
    colnames(reconstitution1) <- c("Source_order_13", "Source_order_3")
    reconstitution <- merge(reconstitution, reconstitution1, by="Source_order_3", all.x=T)
    #clean environment
    rm(reconstitution1, reconstitution2)
   #check for incongruencies
    length(which(reconstitution$Source_order_2==reconstitution$Source_order_24)); length(which(reconstitution$Source_order_2!=reconstitution$Source_order_24)) #8763 / 12 | 99.9% / 0.01%
    length(which(reconstitution$Source_order_1==reconstitution$Source_order_14)); length(which(reconstitution$Source_order_1!=reconstitution$Source_order_14)) #344 / 16 | 95.6% / 4.4% 
    length(which(reconstitution$Source_order_1==reconstitution$Source_order_13)); length(which(reconstitution$Source_order_1!=reconstitution$Source_order_13)) #439 / 16 | 96.5% / 3.5%
  #drop inconsistencies
    reconstitution$Source_order_24 <- ifelse(reconstitution$Source_order_2!=reconstitution$Source_order_24, NA, reconstitution$Source_order_24) #12 (0.01%)
    reconstitution$Source_order_14 <- ifelse(reconstitution$Source_order_1!=reconstitution$Source_order_14, NA, reconstitution$Source_order_14) #16 (4.4%)
    reconstitution$Source_order_13 <- ifelse(reconstitution$Source_order_1!=reconstitution$Source_order_13, NA, reconstitution$Source_order_13) #16 (3.5%)
    
    
  #################################################
  #### section 3c: group matches WITHIN series ####
  #################################################
    
  #filter unique matches
    Serie44 <- filter_unique(Serie44, threshold44, 1, 2)
    Serie33 <- filter_unique(Serie33, threshold33, 1, 2)
    Serie22 <- filter_unique(Serie22, threshold22, 1, 2)
    Serie11 <- filter_unique(Serie11, threshold11, 1, 2)
    
   #set Source_order_3 to character
    Serie33$Source_order_1 <- as.character(Serie33$Source_order_1) 
    Serie33$Source_order_2 <- as.character(Serie33$Source_order_2) 
    
  #load program to merge consecutive matches into a chain (1-2 to 2-3, 2-3 to 3-4, etc)
    chain_within <- function(df1, df2, NUMMER1, NUMMER2){
      df2 <- df2[!is.na(df2$Source_order_1) & !is.na(df2$Source_order_2), c("Source_order_1", "Source_order_2")]
      colnames(df2) <- c(paste("Source_order", NUMMER1, sep="_"), paste("Source_order", NUMMER2, sep="_"))
      df1 <- merge(df1, df2, by=paste("Source_order", NUMMER1, sep="_"), all.x=T)
      df1
    }
    
  #load program to chain matches and remove redundant rows 
    group_within <- function(df1){
      #matched cases
      df1_linked <- df1[!is.na(df1$Source_order_1) & !is.na(df1$Source_order_2), c("Source_order_1", "Source_order_2")]
      #compile matches
      df1_linked <- chain_within(df1_linked, df1, 2, 3)
      df1_linked$Source_order_3 <- ifelse(df1_linked$Source_order_1==df1_linked$Source_order_3, NA, df1_linked$Source_order_3) #prevents loops
      df1_linked <- chain_within(df1_linked, df1, 3, 4)
      df1_linked <- chain_within(df1_linked, df1, 4, 5)
      df1_linked <- chain_within(df1_linked, df1, 5, 6)
      df1_linked <- chain_within(df1_linked, df1, 6, 7)
      #remove overlap
      df1_linked <- df1_linked[!(df1_linked$Source_order_2 %in% df1_linked$Source_order_7 |
                                           df1_linked$Source_order_2 %in% df1_linked$Source_order_6 |
                                           df1_linked$Source_order_2 %in% df1_linked$Source_order_5 |
                                           df1_linked$Source_order_2 %in% df1_linked$Source_order_4 |
                                           df1_linked$Source_order_2 %in% df1_linked$Source_order_3), c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4", "Source_order_5", "Source_order_6", "Source_order_7")]
      #unmatched cases
      df1_unlinked <- df1[is.na(df1$Source_order_1) | is.na(df1$Source_order_2), c("Source_order_1", "Source_order_2")]
      df1_unlinked$Source_order_7 <- df1_unlinked$Source_order_6 <- df1_unlinked$Source_order_5 <- df1_unlinked$Source_order_4 <- df1_unlinked$Source_order_3 <- NA
      #bind
      df1_linked <- rbind(df1_linked, df1_unlinked)
      #print
      df1_linked
    }
    
    Serie44_linked <- group_within(Serie44)
    Serie33_linked <- group_within(Serie33)
    Serie22_linked <- group_within(Serie22)
    Serie11_linked <- group_within(Serie11)
    
  
  ##########################################################################
  #### section 3d: add grouped WITHIN matches to BETWEEN reconstitution ####
  ##########################################################################
    
  #load program to mark last entry WITHIN series
    mark_lastentry <- function(df1){
      df1$LastEntry <- ifelse(is.na(df1$Source_order_2), NA,
                                         ifelse(is.na(df1$Source_order_3), df1$Source_order_2,
                                                ifelse(is.na(df1$Source_order_4), df1$Source_order_3,
                                                       ifelse(is.na(df1$Source_order_5), df1$Source_order_4,
                                                              ifelse(is.na(df1$Source_order_6), df1$Source_order_5,
                                                                     ifelse(is.na(df1$Source_order_7), df1$Source_order_6, df1$Source_order_7))))))
      df1$LastEntry
    }
    
  #load program to add grouped matches 
    add_within <- function(df1, df2, source_order){
      #matched cases
      reconstitution1 <- df2[!is.na(df2[[source_order]]), ]
      reconstitution1 <- merge(reconstitution1, df1, by=source_order, all=T)
      #unmatched cases
      reconstitution2 <- df2[is.na(df2[[source_order]]), ]
      #add empty columns to reconstitution2
      if(df1[1,1]==Serie44_linked[1,1]){
        reconstitution2$Source_order_44_2 <- reconstitution2$Source_order_44_3 <- reconstitution2$Source_order_44_3 <- reconstitution2$Source_order_44_4 <- reconstitution2$Source_order_44_5 <- reconstitution2$Source_order_44_6 <- reconstitution2$Source_order_44_7 <- NA
      }
      if(df1[1,1]==Serie33_linked[1,1]){
        reconstitution2$Source_order_33_1 <- reconstitution2$Source_order_33_2 <- reconstitution2$Source_order_33_3 <- reconstitution2$Source_order_33_3 <- reconstitution2$Source_order_33_4 <- reconstitution2$Source_order_33_5 <- reconstitution2$Source_order_33_6 <- reconstitution2$Source_order_33_7 <- NA
      }
      if(df1[1,1]==Serie22_linked[1,1]){
        reconstitution2$Source_order_22_1 <- reconstitution2$Source_order_22_2 <- reconstitution2$Source_order_22_3 <- reconstitution2$Source_order_22_3 <- reconstitution2$Source_order_22_4 <- reconstitution2$Source_order_22_5 <- reconstitution2$Source_order_22_6 <- reconstitution2$Source_order_22_7 <- NA
      }
      if(df1[1,1]==Serie11_linked[1,1]){
        reconstitution2$Source_order_11_1 <- reconstitution2$Source_order_11_2 <- reconstitution2$Source_order_11_3 <- reconstitution2$Source_order_11_3 <- reconstitution2$Source_order_11_4 <- reconstitution2$Source_order_11_5 <- reconstitution2$Source_order_11_6 <- reconstitution2$Source_order_11_7 <- NA
      }
      #bind
      df2 <- rbind(reconstitution1, reconstitution2)
      df2
    }
    
    
  #1. mark last entry WITHIN series
   #Serie 44 not necessary, as end is manumission
    
  #4-4
  #match to next series
    #not possible, as end is manumission 
  #match to start series
    #rename grouped matches
    colnames(Serie44_linked) <- c("Source_order_4", "Source_order_44_2", "Source_order_44_3", "Source_order_44_4", "Source_order_44_5", 
                                  "Source_order_44_6", "Source_order_44_7")
    #add grouped matches to Series 4
    reconstitution <- add_within(Serie44_linked, reconstitution, "Source_order_4")
   
  #3-3
  #match to next series
    #mark last entry
    Serie33_linked$LastEntry <- mark_lastentry(Serie33_linked)
    #rename grouped matches
    colnames(Serie33_linked) <- c("Source_order_33_1", "Source_order_33_2", "Source_order_33_3", "Source_order_33_4", "Source_order_33_5", 
                                  "Source_order_33_6", "Source_order_33_7", "Source_order_4")
    #add grouped matches to Series 4
    reconstitution <- add_within(Serie33_linked, reconstitution, "Source_order_4")
  #match to start series
    #1. drop duplicated unmatched within matches
    reconstitution <- reconstitution[!(reconstitution$Source_order_33_1 %in% reconstitution$Source_order_3 & is.na(reconstitution$Source_order_33_2) & !is.na(reconstitution$Source_order_33_1)),]
    #2. join to preceding series
    x <- reconstitution[reconstitution$Source_order_3 %in% reconstitution$Source_order_33_1 & !is.na(reconstitution$Source_order_3),]
    x <- x[,which(grepl("33", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_33_1 %in% reconstitution$Source_order_3 & !is.na(reconstitution$Source_order_33_1),]
    y <- y[,which(grepl("33", colnames(y)))]
    y$Source_order_3 <- y$Source_order_33_1
    x <- merge(x, y, by="Source_order_3", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_33_1 %in% x$Source_order_33_1) &
                                       !(reconstitution$Source_order_3 %in% x$Source_order_3),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_33_1 <- ifelse(reconstitution$Source_order_33_1==reconstitution$Source_order_3, NA, reconstitution$Source_order_33_1)
    
  #2-2
  #match to next series
    #mark last entry
    Serie22_linked$LastEntry <- mark_lastentry(Serie22_linked)
    #rename grouped matches
    colnames(Serie22_linked) <- c("Source_order_22_1", "Source_order_22_2", "Source_order_22_3", "Source_order_22_4", "Source_order_22_5", 
                                  "Source_order_22_6", "Source_order_22_7", "Source_order_3")
    #add grouped matches to Series 3
    reconstitution <- add_within(Serie22_linked, reconstitution, "Source_order_3")
  #match to start series
    #1. drop duplicated unmatched within matches
    reconstitution <- reconstitution[!(reconstitution$Source_order_22_1 %in% reconstitution$Source_order_2 & is.na(reconstitution$Source_order_22_2) & !is.na(reconstitution$Source_order_22_1)),]
    #2. join to preceding series
    x <- reconstitution[reconstitution$Source_order_2 %in% reconstitution$Source_order_22_1 & !is.na(reconstitution$Source_order_2),]
    x <- x[,which(grepl("22", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_22_1 %in% reconstitution$Source_order_2 & !is.na(reconstitution$Source_order_22_1),]
    y <- y[,which(grepl("22", colnames(y)))]
    y$Source_order_2 <- y$Source_order_22_1
    x <- merge(x, y, by="Source_order_2", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_22_1 %in% x$Source_order_22_1) &
                                       !(reconstitution$Source_order_2 %in% x$Source_order_2),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_22_1 <- ifelse(reconstitution$Source_order_22_1==reconstitution$Source_order_2, NA, reconstitution$Source_order_22_1)
    
  #1-1
  #match to next series
    #mark last entry
    Serie11_linked$LastEntry <- mark_lastentry(Serie11_linked)
    #rename grouped matches
    colnames(Serie11_linked) <- c("Source_order_11_1", "Source_order_11_2", "Source_order_11_3", "Source_order_11_4", "Source_order_11_5", 
                                  "Source_order_11_6", "Source_order_11_7", "Source_order_2")
    #add grouped matches to Series 2
    reconstitution <- add_within(Serie11_linked, reconstitution, "Source_order_2")
  #match to start series
    #1. drop duplicated unmatched within matches
    reconstitution <- reconstitution[!(reconstitution$Source_order_11_1 %in% reconstitution$Source_order_1 & is.na(reconstitution$Source_order_11_2) & !is.na(reconstitution$Source_order_11_1)),]
    #2. join to preceding series
    x <- reconstitution[reconstitution$Source_order_1 %in% reconstitution$Source_order_11_1 & !is.na(reconstitution$Source_order_1),]
    x <- x[,which(grepl("11", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_11_1 %in% reconstitution$Source_order_1 & !is.na(reconstitution$Source_order_11_1),]
    y <- y[,which(grepl("11", colnames(y)))]
    y$Source_order_1 <- y$Source_order_11_1
    x <- merge(x, y, by="Source_order_1", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_11_1 %in% x$Source_order_11_1) &
                                       !(reconstitution$Source_order_1 %in% x$Source_order_1),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_11_1 <- ifelse(reconstitution$Source_order_11_1==reconstitution$Source_order_1, NA, reconstitution$Source_order_11_1)
    
    
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
    
   #select relevant variables from df
    df2 <- df[, c("source_order", "Inventarisnummer", "Folionummer",
                 "sex", "Serieregister", "Typeregister",
                 "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
                 "year_entry", "month_entry", "day_entry", "in_event2", "in_event", "Aanvullendeinformatieinschrijv",
                 "year_exit", "month_exit", "day_exit", "out_event2", "out_event", "Aanvullendeinformatieuitschrij",
                 "year_birth", "month_birth", "day_birth", "year_birth_age_based")]
   #set -1 to NA
    df2[df2=="-1"] <- NA
   #generate B_year_min and B_year_max
    df2$B_year_min <- df2$year_birth_age_based-1
    df2$B_year_max <- df2$year_birth_age_based
   #add Id_person
    df2 <- merge(reconstitution, df2, by="source_order")
    df2 <- df2[!duplicated(df2[,c("Id_person", "source_order")])]
   #reallign dataset
    df2 <- df2[, c("Id_person", "source_order", "Inventarisnummer", "Folionummer",
                 "sex", "Serieregister", "Typeregister",
                 "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar_original",
                 "year_entry", "month_entry", "day_entry", "in_event2", "in_event", "Aanvullendeinformatieinschrijv",
                 "year_exit", "month_exit", "day_exit", "out_event2", "out_event", "Aanvullendeinformatieuitschrij",
                 "year_birth", "month_birth", "day_birth", "B_year_min", "B_year_min")]
   #rename variables
    colnames(df2) <- c("Id_person", "Id_source", "Inventarisnummer", "Folionummer",
                      "Sex", "Source_series", "Source_type",
                      "Name_enslaved", "Name_enslaved_extra", "Name_mother", "Name_mother_extra", "Name_owner",
                      "StartEntryYear", "StartEntryMonth", "StartEntryDay", "StartEntryEvent", "StartEntryEventDetailed", "StartEntryExtraInfo",
                      "LastEntryYear", "LastEntryMonth", "LastEntryDay", "LastEntryEvent", "LastEntryEventDetailed", "LastEntryExtraInfo",
                      "B_year", "B_month", "B_day", "B_year_min", "B_year_max")
   #reorder dataset
    df2 <- df2 %>% arrange(Id_person, Source_series, StartEntryYear, StartEntryMonth, StartEntryDay)
   #write outfiles
    write.table(df2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.xlsx(df2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.xlsx"), overwrite=T)
    
    
  #################################
  #### check length long table ####
  #################################
    
    length(df[,1])==length(df2[,1])
    
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    