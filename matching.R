  
  
  ########################MATCHING WITHIN SERIES 4######################
  ####  This script matches records of the enslaved BETWEEN series  ####
  ####  We use 3 conditions to establish matches:                   ####
  ####    1. Levenshtein distance name enslaved                     ####
  ####    2. Levenshtein distance name owner                        ####
  ####    3. Levenshtein distance name mother                       ####
  ####  Further filtering is done based on the:                     ####
  ####    1. Date of birth                                          ####
  ####    2. Name enslaved in the preceding registration            ####
  ####    3. Name enslaved in the following registration            ####
  ####  Multiple matches are solved probabilistically               ####
  ######################################################################
  
  
  #load packages
  library("data.table")
  library("dplyr")
  library("stringdist")
  library("openxlsx")
  
  #clean environment
  rm(list=ls())
  
  #open dataset
  setwd("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Cleaned Registry")
  df <- fread("cleaned slave register 2021-11-29.txt", encoding="UTF-8")
  
  #check data
  as.data.frame(table(df$Typeregister))
  as.data.frame(table(df$Serieregister))
  
  
  ##########################################
  #### section 0: load matching program ####
  ##########################################
    
  match_between <- function(df1, df2, lev_dist_naam, lev_dist_moeder, lev_dist_eigenaar, lev_dist_laglead, NUMMER1, NUMMER2){
    
   #### step 1: determine Levenshtein distance for all NAAM combinations ####

   #select unique names
    Slave_names_1 <- df1[!duplicated(df1$Naam) & df1$Naam!="",]
    Slave_names_2 <- df2[!duplicated(df2$Naam) & df2$Naam!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names_1$Naam, Slave_names_2$Naam, method = "lv")
   #match names with Levenshtein distance <= LEV_DIST_NAAM (currently 3) using repeat loop
    x <- 0 #starting value, looped until LEV_DIST_NAAM
    repeat{
     #filter LEV_DIST_NAAM == X from LV_matrix
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #filter corresponding names + add  
      l <- data.frame(Naam1 = Slave_names_1$Naam[l[,1]],
                      Naam2 = Slave_names_2$Naam[l[,2]])
     #add Levenshtein distance x as metadata
      l$LV <- x
     #store in dataframe named Slave_names_matched
      if("Slave_names_matched" %in% ls() ){
        Slave_names_matched <- rbind(Slave_names_matched, l)
      } else{
        Slave_names_matched <- l
      }
     #repeat loop until max LEV_DIST_NAAM is reached, then break
      if(x==lev_dist_naam) {
        break
      }
     #prepare repeat
      x <- x+1
    }
   #rename columns
    colnames(Slave_names_matched)[1:3] <- c("Naam_1", "Naam_2", "Naam_lv")
   #clean environment
    rm(l, LV_matrix, Slave_names_1, Slave_names_2, x)
    
    
   #### step 2: add metadata and select relevant columns in data frames ####

   #add preceding and proceeding NAAM to data frams
    df1 <- df1 %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                                                 Naam_volgende=lead(Naam)) %>% ungroup()
    df2 <- df2 %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                                                 Naam_volgende=lead(Naam)) %>% ungroup()
   #set NA on NAAM_VORIGE + NAAM_VOLGENDE to ""
    df1$Naam_vorige[is.na(df1$Naam_vorige)] <- ""
    df1$Naam_volgende[is.na(df1$Naam_volgende)] <- ""
    df2$Naam_vorige[is.na(df2$Naam_vorige)] <- ""
    df2$Naam_volgende[is.na(df2$Naam_volgende)] <- ""
    
   #rename variables df1
    colnames(df1) <- paste(c("source_order", 
                              "Out_event", 
                              "Naam", "Naam_number", 
                              "Moeder", "Moeder_number", 
                              "year_birth",
                              "Eigenaar",
                              "sex",
                              "Naam_vorige", "Naam_volgende"),
                            1, sep="_")
   #rename variables df2
    colnames(df2) <- paste(c("source_order", 
                              "In_event", 
                              "Naam", "Naam_number", 
                              "Moeder", "Moeder_number", 
                              "year_birth",
                              "Eigenaar",
                              "sex",
                              "Naam_vorige", "Naam_volgende"),
                            2, sep="_")
    
   #add df1 and df2 to SLAVE_NAMES_MATCHED
    df_matched <- merge(df1, Slave_names_matched, by="Naam_1", all=F)
    df_matched <- merge(df_matched, df2, by="Naam_2", all=F )
    
    
   #### step 3: rule-based filtering of matches ####

  #filter rows
   #same sex OR sex = "u"
    df_matched <- df_matched[which(df_matched$sex_1==df_matched$sex_2 | 
                                     df_matched$sex_1=="u" | df_matched$sex_2=="u"), ]
   #same owner: Levenshtein distance <= LEV_DIST_EIGENAAR
    df_matched <- df_matched[stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2) <= lev_dist_eigenaar, ]
   #Moeder with: 
     # 1 unknown entry OR 
     # Levenshtein distance <= LEV_DIST_MOEDER
    df_matched$Moeder_lv <- stringdist(df_matched$Moeder_1, df_matched$Moeder_2)
    df_matched <- df_matched[which(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | 
                                     df_matched$Moeder_1=="" | df_matched$Moeder_2=="" | 
                                     df_matched$Moeder_lv<=lev_dist_moeder), ]
   #entries with EITHER: 
     # 1 unknown date OR 
     # 2 corresponding birth years OR 
     # same lagging and leading names
    df_matched <- df_matched[which(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) |
                                     df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                     stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                     stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)<=lev_dist_laglead), ]
    
    
  #### step 4: add metadata ####

  #compute Levenshtein distances
   #Eigenaar
    df_matched$Eigenaar_lv <- stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)
   #Moeder
    df_matched$Moeder_lv <- stringdist(df_matched$Moeder_1, df_matched$Moeder_2)
   #Vorige
    df_matched$Naam_vorige_lv <- stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)
   #Volgende
    df_matched$Naam_volgende_lv <- stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)
    
  #add flags
   #matched
    df_matched$Match <- ifelse(is.na(df_matched$Naam_1) | is.na(df_matched$Naam_2) | df_matched$Naam_1=="" | df_matched$Naam_2=="", 0, 1)
   #naam_number
    df_matched$Match_naam_number <- ifelse(is.na(df_matched$Naam_number_1) | is.na(df_matched$Naam_number_2) | df_matched$Naam_number_1=="" & df_matched$Naam_number_2=="" | df_matched$Naam_number_1!=df_matched$Naam_number_2, 0, 1)
   #mother
    df_matched$Match_moeder <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | df_matched$Moeder_1=="" | df_matched$Moeder_2=="" | df_matched$Moeder_lv>lev_dist_moeder, 0, 1)
   #moeder_number
    df_matched$Match_moeder_number <- ifelse(is.na(df_matched$Moeder_number_1) | is.na(df_matched$Moeder_number_2) | df_matched$Moeder_number_1=="" & df_matched$Moeder_number_2=="" | df_matched$Moeder_number_1!=df_matched$Moeder_number_2, 0, 1)
   #year
    df_matched$Match_year <- ifelse(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) | df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | df_matched$year_birth_1!=df_matched$year_birth_2, 0, 1) 
   #previous entry
    df_matched$Match_vorige <- ifelse(df_matched$Naam_vorige_1=="" & df_matched$Naam_vorige_2=="" | df_matched$Naam_vorige_lv<=lev_dist_naam, 1, 0)
   #next entry
    df_matched$Match_volgende <- ifelse(df_matched$Naam_volgende_lv<=lev_dist_naam, 1, 0)
   #out_event
    df_matched$Out_unended <- ifelse(df_matched$Out_event_1!="Ended", 1, 0)
    
  #compute match score
    df_matched$Match_score <- 2.5*df_matched$Match_moeder + #twice as important + tie-breaker
                                df_matched$Match_naam_number + df_matched$Match_moeder_number + 
                                2*df_matched$Match_year + 
                                df_matched$Match_vorige + df_matched$Match_volgende -
                                df_matched$Out_unended
    
    
   #### step 5: filter best match ####

  #selections
   #if match score <4, only accept if Naam == almost identical
    df_matched <- df_matched[which(df_matched$Match_score>=4 |
                                     df_matched$Match_score<4 & df_matched$Naam_lv<=1),]
   #mark best match source_order_1
    x <- df_matched %>% group_by(source_order_1) %>% filter(Match_score==max(Match_score)) %>% ungroup()
    x <- x %>% group_by(source_order_1) %>% filter(Moeder_lv==min(Moeder_lv)) %>% ungroup()
    x <- x %>% group_by(source_order_1) %>% filter(Naam_lv==min(Naam_lv)) %>% ungroup()
    x <- x %>% group_by(source_order_1) %>% filter(Eigenaar_lv==min(Eigenaar_lv)) %>% ungroup()
    x$Match_lv <- paste(x$Naam_lv, x$Moeder_lv, x$Eigenaar_lv, sep="-")
    df_matched <- merge(df_matched, x[,c("source_order_1", "source_order_2", "Match_lv")], all.x=T)
    

  #### step 6: add unmatched cases ####

  #add unmatched cases
   #df1
    colnames(df1)[2] <- "Out_event_x"
    df_full <- merge(df1[,1:9], df_matched, by=paste(c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 1, sep="_"), all=T)
    df_full$Out_event_1 <- ifelse(is.na(df_full$Out_event_1), df_full$Out_event_x, df_full$Out_event_1)
    df_full$Out_event_x <- NULL
   #Series 4
    colnames(df2)[2] <- "In_event_x"
    df_full <- merge(df2[,1:9], df_full, by=paste(c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 2, sep="_"), all=T)
    df_full$In_event_2 <- ifelse(is.na(df_full$In_event_2), df_full$In_event_x, df_full$In_event_2)
    df_full$In_event_x <- NULL
    
  #add flags
   #matched
    df_full$Match <- ifelse(is.na(df_full$Naam_1) | is.na(df_full$Naam_2) | df_full$Naam_1=="" | df_full$Naam_2=="", 0, 1)
   #naam_number
    df_full$Match_naam_number <- ifelse(is.na(df_full$Naam_number_1) | is.na(df_full$Naam_number_2) | df_full$Naam_number_1=="" & df_full$Naam_number_2=="" | df_full$Naam_number_1!=df_full$Naam_number_2, 0, 1)
   #mother
    df_full$Match_moeder <- ifelse(is.na(df_full$Moeder_1) | is.na(df_full$Moeder_2) | df_full$Moeder_1=="" | df_full$Moeder_2=="" | df_full$Moeder_lv>lev_dist_moeder, 0, 1)
   #moeder_number
    df_full$Match_moeder_number <- ifelse(is.na(df_full$Moeder_number_1) | is.na(df_full$Moeder_number_2) | df_full$Moeder_number_1=="" & df_full$Moeder_number_2=="" | df_full$Moeder_number_1!=df_full$Moeder_number_2, 0, 1)
   #year
    df_full$Match_year <- ifelse(is.na(df_full$year_birth_1) | is.na(df_full$year_birth_2) | df_full$year_birth_1=="-1" | df_full$year_birth_2=="-1" | df_full$year_birth_1!=df_full$year_birth_2, 0, 1) 
   #previous entry
    df_full$Match_vorige <- ifelse(is.na(df_full$Naam_vorige_1) | is.na(df_full$Naam_vorige_2) | df_full$Naam_vorige_1=="" & df_full$Naam_vorige_2!="" | df_full$Naam_vorige_1!="" & df_full$Naam_vorige_2=="" | df_full$Naam_vorige_lv>lev_dist_naam, 0, 1)
   #next entry
    df_full$Match_volgende <- ifelse(is.na(df_full$Naam_volgende_1) | is.na(df_full$Naam_volgende_2) | df_full$Naam_volgende_1=="" & df_full$Naam_volgende_2!="" | df_full$Naam_volgende_1!="" & df_full$Naam_volgende_2=="" | df_full$Naam_volgende_lv>lev_dist_naam, 0, 1)
   #sex
    df_full$Match_sex <- ifelse(df_full$sex_1==df_full$sex_2, 1, 0)
    
    
   #### step 7: structure and store data frame ####

   #add type register
    #source_order_1
    x <- df[,c("source_order", "Typeregister")]
    colnames(x) <- c("source_order_1", "Typeregister_1")
    df_full <- merge(df_full, x, by="source_order_1", all.x=T)
    #source_order_1
    colnames(x) <- c("source_order_2", "Typeregister_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
   
   #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_sex", "Match_naam_number", "Match_eigenaar", "Match_moeder", "Match_moeder_number", "Match_year", "Match_vorige", "Match_volgende", "Match_score",
                          "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", "Match_lv",
                          paste(c("Out_event", "In_event"), 1:2, sep="_"), 
                          paste(c("source_order", "source_order"), 1:2, sep="_"), 
                          paste(c("sex", "sex"), 1:2, sep="_"), 
                          paste(c("Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                          paste(c("Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                          paste(c("Eigenaar", "Eigenaar"), 1:2, sep="_"), 
                          paste(c("year_birth", "year_birth"), 1:2, sep="_"), 
                          paste(c("Naam_vorige", "Naam_vorige"), 1:2, sep="_"),
                          paste(c("Naam_volgende", "Naam_volgende"), 1:2, sep="_"))] %>% arrange(source_order_1, source_order_2)
    df_full <- df_full %>% arrange(source_order_1, -Match_score)
    
   #rename
    colnames(df_full) <- c(paste("Typeregister", NUMMER1, sep="_"), paste("Typeregister", NUMMER2, sep="_"),
                           "Match", "Match_sex", "Match_naam_number", "Match_moeder", "Match_moeder_number", "Match_year", "Match_vorige", "Match_volgende", "Match_score",
                           "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", "Match_lv",
                           paste("Out_event", NUMMER1, sep="_"), paste("In_event", NUMMER2, sep="_"),
                           paste("Source_order", NUMMER1, sep="_"), paste("Source_order", NUMMER2, sep="_"),
                           paste("Sex", NUMMER1, sep="_"), paste("Sex", NUMMER2, sep="_"),
                           paste("Naam", NUMMER1, sep="_"), paste("Naam", NUMMER2, sep="_"), paste("Naam_number", NUMMER1, sep="_"), paste("Naam_number", NUMMER2, sep="_"),
                           paste("Moeder", NUMMER1, sep="_"), paste("Moeder", NUMMER2, sep="_"), paste("Moeder_number", NUMMER1, sep="_"), paste("Moeder_number", NUMMER2, sep="_"),
                           paste("Eigenaar", NUMMER1, sep="_"), paste("Eigenaar", NUMMER2, sep="_"),
                           paste("Year_birth", NUMMER1, sep="_"), paste("Year_birth", NUMMER2, sep="_"),
                           paste("Naam_vorige", NUMMER1, sep="_"), paste("Naam_vorige", NUMMER2, sep="_"),
                           paste("Naam_volgende", NUMMER1, sep="_"), paste("Naam_volgende", NUMMER2, sep="_"))
    df_full
    
  }
  
  
  ##########################################
  #### section 0: load matching program ####
  ##########################################
  
  match_within <- function(df1, lev_dist_naam, lev_dist_moeder, NUMMER1){
    
   #### step 1: determine Levenshtein distance for all NAAM combinations ####
    
   #select unique names
    Slave_names <- df1[!duplicated(df1$Naam) & df1$Naam!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names$Naam, Slave_names$Naam, method = "lv")
   #match names with Levenshtein distance <= LEV_DIST_NAAM (currently 3) using repeat loop
    x <- 0 #starting value, looped until LEV_DIST_NAAM
    repeat{
     #filter LEV_DIST_NAAM == X from LV_matrix
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #filter corresponding names + add  
      l <- data.frame(Naam1 = Slave_names$Naam[l[,1]],
                      Naam2 = Slave_names$Naam[l[,2]])
     #add Levenshtein distance x as metadata
      l$LV <- x
     #store in dataframe named Slave_names_matched
      if("Slave_names_matched" %in% ls() ){
        Slave_names_matched <- rbind(Slave_names_matched, l)
      } else{
        Slave_names_matched <- l
      }
     #repeat loop until max LEV_DIST_NAAM is reached, then break
      if(x==lev_dist_naam) {
        break
      }
     #prepare repeat
      x <- x+1
    }
   #rename columns
    colnames(Slave_names_matched)[1:3] <- c("Naam_1", "Naam_2", "Naam_lv")
   #clean environment
    rm(l, LV_matrix, Slave_names, x)
    
    
   #### step 2: select relevant columns in data frames ####
    
   #rename variables df1
    colnames(df1) <- paste(c("source_order", 
                             "in_event2", "out_event2"),
                             "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                           1, sep="_")
   #add df1 to SLAVE_NAMES_MATCHED
    df_matched <- merge(df1, Slave_names_matched, by="Naam_1", all=F)
   #rename variables df1
    colnames(df1) <- paste(c("source_order", 
                             "in_event2", "out_event2"),
                              "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                           2, sep="_")
   #add df1 to DF_MATCHED
    df_matched <- merge(df_matched, df1, by="Naam_2", all=F )
    
    
   #### step 3: rule-based filtering of matches ####
    
   #filter rows
    #same sex OR sex = "u"
    df_matched <- df_matched[which(df_matched$sex_1==df_matched$sex_2 | 
                                     df_matched$sex_1=="u" | df_matched$sex_2=="u"), ]
    #select matches where both names highlight a mutation
    df <- df[which(df$out_event2_1=="Transferred" & df$in_event2_2=="Transferred" |
                     df$out_event2_1=="Transferred" & df$in_event2_2=="Unknown" |
                     df$out_event2_1=="Unknown" & df$in_event2_2=="Transferred"), ]
   #select Moeder with: 
     # 1 unknown entry OR 
     # Levenshtein distance <= LEV_DIST_MOEDER
    df_matched$Moeder_lv <- stringdist(df_matched$Moeder_1, df_matched$Moeder_2)
    df_matched <- df_matched[which(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | 
                                     df_matched$Moeder_1=="" | df_matched$Moeder_2=="" | 
                                     df_matched$Moeder_lv<=lev_dist_moeder), ]
   #select entries with EITHER: 
     # 1 unknown date OR 
     # 2 corresponding birth years
    df_matched <- df_matched[which(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) |
                                     df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                     df_matched$year_birth_1==df_matched$year_birth_2), ]
  #select entries with EITHER: 
    # 1 mutation date OR 
    # 2 corresponding mutation years
    df_matched <- df_matched[which(is.na(df_matched$year_exit_1) | is.na(df_matched$year_entry_2) |
                                     df_matched$year_exit_1=="-1" | df_matched$year_entry_2=="-1" | 
                                     df_matched$year_exit_1==df_matched$year_entry_2), ]
    
    
  #### step 4: add metadata ####
    
  #compute Levenshtein distances
   #Moeder
    df_matched$Moeder_lv <- stringdist(df_matched$Moeder_1, df_matched$Moeder_2)
    
  #add flags
   #matched
    df_matched$Match <- ifelse(is.na(df_matched$Naam_1) | is.na(df_matched$Naam_2) | df_matched$Naam_1=="" | df_matched$Naam_2=="", 0, 1)
   #naam_number
    df_matched$Match_naam_number <- ifelse(is.na(df_matched$Naam_number_1) | is.na(df_matched$Naam_number_2) | df_matched$Naam_number_1=="" & df_matched$Naam_number_2=="" | df_matched$Naam_number_1!=df_matched$Naam_number_2, 0, 1)
   #mother
    df_matched$Match_moeder <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | df_matched$Moeder_1=="" | df_matched$Moeder_2=="" | df_matched$Moeder_lv>lev_dist_moeder, 0, 1)
   #moeder_number
    df_matched$Match_moeder_number <- ifelse(is.na(df_matched$Moeder_number_1) | is.na(df_matched$Moeder_number_2) | df_matched$Moeder_number_1=="" & df_matched$Moeder_number_2=="" | df_matched$Moeder_number_1!=df_matched$Moeder_number_2, 0, 1)
   #year_birth
    df_matched$Match_year <- ifelse(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) | df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | df_matched$year_birth_1!=df_matched$year_birth_2, 0, 1) 
   #year_event
    df_matched$Match_year_event <- ifelse(is.na(df_matched$year_exit_1) | is.na(df_matched$year_entry_2) | df_matched$year_exit_1=="-1" | df_matched$year_entry_2=="-1" | df_matched$year_exit_1!=df_matched$year_entry_2, 0, 1) 
    
  #compute match score
    df_matched$Match_score <- 2.5*df_matched$Match_moeder + #twice as important + tie-breaker
      df_matched$Match_naam_number + df_matched$Match_moeder_number + 
      2*df_matched$Match_year + 
      2*df_matched$Match_year_event
    
    
  #### step 5: filter best match ####
    
  #selections
   #if match score <4, only accept if Naam == almost identical
    df_matched <- df_matched[which(df_matched$Match_score>=4 |
                                     df_matched$Match_score<4 & df_matched$Naam_lv<=1),]
   #mark best match source_order_1
    x <- df_matched %>% group_by(source_order_1) %>% filter(Match_score==max(Match_score)) %>% ungroup()
    x <- x %>% group_by(source_order_1) %>% filter(Moeder_lv==min(Moeder_lv)) %>% ungroup()
    x <- x %>% group_by(source_order_1) %>% filter(Naam_lv==min(Naam_lv)) %>% ungroup()
    x$Match_lv <- paste(x$Naam_lv, x$Moeder_lv, x$Eigenaar_lv, sep="-")
    df_matched <- merge(df_matched, x[,c("source_order_1", "source_order_2", "Match_lv")], all.x=T)
    
    
  #### step 6: add unmatched cases ####
    
  #add unmatched cases
   #df1
    colnames(df1)[2] <- "Out_event_x"
    df_full <- merge(df1[,1:9], df_matched, by=paste(c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 1, sep="_"), all=T)
    df_full$Out_event_1 <- ifelse(is.na(df_full$Out_event_1), df_full$Out_event_x, df_full$Out_event_1)
    df_full$Out_event_x <- NULL
   #df2
    colnames(df2)[2] <- "In_event_x"
    df_full <- merge(df2[,1:9], df_full, by=paste(c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 2, sep="_"), all=T)
    df_full$In_event_2 <- ifelse(is.na(df_full$In_event_2), df_full$In_event_x, df_full$In_event_2)
    df_full$In_event_x <- NULL
    
  #add flags
   #matched
    df_full$Match <- ifelse(is.na(df_full$Naam_1) | is.na(df_full$Naam_2) | df_full$Naam_1=="" | df_full$Naam_2=="", 0, 1)
   #naam_number
    df_full$Match_naam_number <- ifelse(is.na(df_full$Naam_number_1) | is.na(df_full$Naam_number_2) | df_full$Naam_number_1=="" & df_full$Naam_number_2=="" | df_full$Naam_number_1!=df_full$Naam_number_2, 0, 1)
   #mother
    df_full$Match_moeder <- ifelse(is.na(df_full$Moeder_1) | is.na(df_full$Moeder_2) | df_full$Moeder_1=="" | df_full$Moeder_2=="" | df_full$Moeder_lv>lev_dist_moeder, 0, 1)
   #moeder_number
    df_full$Match_moeder_number <- ifelse(is.na(df_full$Moeder_number_1) | is.na(df_full$Moeder_number_2) | df_full$Moeder_number_1=="" & df_full$Moeder_number_2=="" | df_full$Moeder_number_1!=df_full$Moeder_number_2, 0, 1)
   #year
    df_full$Match_year <- ifelse(is.na(df_full$year_birth_1) | is.na(df_full$year_birth_2) | df_full$year_birth_1=="-1" | df_full$year_birth_2=="-1" | df_full$year_birth_1!=df_full$year_birth_2, 0, 1) 
   #year_event
    df_full$Match_year_event <- ifelse(is.na(df_full$year_exit_1) | is.na(df_full$year_entry_2) | df_full$year_exit_1=="-1" | df_full$year_entry_2=="-1" | df_full$year_exit_1!=df_full$year_entry_2, 0, 1)
    
    
    #### step 7: structure and store data frame ####
    
    #add type register
    #source_order_1
    x <- df[,c("source_order", "Typeregister")]
    colnames(x) <- c("source_order_1", "Typeregister_1")
    df_full <- merge(df_full, x, by="source_order_1", all.x=T)
    #source_order_1
    colnames(x) <- c("source_order_2", "Typeregister_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
    
    #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_naam_number", "Match_moeder", "Match_moeder_number", "Match_year", "Match_year_event", "Match_score",
                          "Naam_lv", "Moeder_lv", "Match_lv",
                          paste(c("source_order", "source_order"), 1:2, sep="_"),
                          paste(c("out_event2", "in_event2"), 1:2, sep="_"),
                          paste(c("year_exit", "year_entry"), 1:2, sep="_"), 
                          paste(c("sex", "sex"), 1:2, sep="_"), 
                          paste(c("Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                          paste(c("Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                          paste(c("year_birth", "year_birth"), 1:2, sep="_"))] %>% arrange(source_order_1, source_order_2)
    df_full <- df_full %>% arrange(source_order_1, -Match_score)
    
    #rename
    colnames(df_full) <- c("Typeregister_1", "Typeregister_2",
                           "Match", "Match_naam_number", "Match_moeder", "Match_moeder_number", "Match_year_birth", "Match_year_transfer", "Match_score",
                           "Naam_lv", "Moeder_lv", "Match_lv",
                           paste(c("source_order", "source_order"), 1:2, sep="_"),
                           paste(c("out_event", "in_event"), 1:2, sep="_"),
                           paste(c("year_exit", "year_entry"), 1:2, sep="_"), 
                           paste(c("sex", "sex"), 1:2, sep="_"), 
                           paste(c("Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                           paste(c("Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                           paste(c("year_birth", "year_birth"), 1:2, sep="_"))
    df_full
    
  }
  
  
  ##############################
  #### section 1: retrieval ####
  ##############################
  
    df$Eigenaar_original <- df$Eigenaar
    df$Eigenaar <- ifelse(df$Typeregister=="Plantages", df$plantation_name, df$Eigenaar_Last_name)
  
  #match serie 3 & 4
   #select series
    Serie3 <- df[df$Serieregister_nr==3, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie34 <- match_between(Serie3, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=3, NUMMER2=4)
    
  #match serie 2 & 3
   #select series
    Serie2 <- df[df$Serieregister_nr==2, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie23 <- match_between(Serie2, Serie3, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=2, NUMMER2=3)
    
  #match serie 1 & 2
   #select series
    Serie1 <- df[df$Serieregister_nr==1, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie2 <- df[df$Serieregister_nr==2, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie12 <- match_between(Serie1, Serie2, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=2)
    
  #match serie 2 & 4
   #select series
    Serie2 <- df[df$Serieregister_nr==2, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie24 <- match_between(Serie2, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=2, NUMMER2=4)
    
  #match serie 1 & 4
   #select series
    Serie1 <- df[df$Serieregister_nr==1, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie14 <- match_between(Serie1, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=4)
    
  #match serie 1 & 3
   #select series
    Serie1 <- df[df$Serieregister_nr==1, c("source_order", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3, c("source_order", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie13 <- match_between(Serie1, Serie3, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=3)
    
    
  #select duplicates
    #dubbel <- Serie34 %>% group_by(source_order_3) %>% filter(n()>1 & !is.na(source_order_3)) %>% ungroup()
  
  #select best match source_order_3
    #Serie34 <- Serie34 %>% group_by(source_order_3) %>% filter(Match_score==max(Match_score) | is.na(source_order_3) | is.na(source_order_4)) %>% ungroup()
  #select best match source_order_4
    #Serie34 <- Serie34 %>% group_by(source_order_4) %>% filter(Match_score==max(Match_score) | is.na(source_order_3) | is.na(source_order_4)) %>% ungroup()
    
    
  ###############################
  #### section 2: store info ####
  ###############################
    
  #select Alkmaar & Barbados
    Alkmaar <- Serie34[grepl("Alkmaar", Serie34$Eigenaar_3) | grepl("Alkmaar", Serie34$Eigenaar_4),]
    Barbados <- Serie34[grepl("Barbados", Serie34$Eigenaar_3) | grepl("Barbados", Serie34$Eigenaar_4),]
    
  #write outfiles
   #csv
    write.table(Serie34, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/3-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
    write.table(Serie23, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/2-3 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
    write.table(Serie12, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/1-2 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
    write.table(Serie24, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/1-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
    write.table(Serie14, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/2-4 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
    write.table(Serie13, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/1-3 matches.txt"), quote=F, sep ="\t", col.names=T, row.names=F)
   #excel plantages
    Plantages34 <- Serie34[which(Serie34$Typeregister_3=="Plantages" | Serie34$Typeregister_4=="Plantages"),]
    Plantages23 <- Serie23[which(Serie23$Typeregister_2=="Plantages" | Serie23$Typeregister_3=="Plantages"),]
    Plantages12 <- Serie12[which(Serie12$Typeregister_1=="Plantages" | Serie12$Typeregister_2=="Plantages"),]
    Plantages24 <- Serie24[which(Serie24$Typeregister_2=="Plantages" | Serie24$Typeregister_4=="Plantages"),]
    Plantages14 <- Serie14[which(Serie14$Typeregister_1=="Plantages" | Serie14$Typeregister_4=="Plantages"),]
    Plantages13 <- Serie13[which(Serie13$Typeregister_1=="Plantages" | Serie13$Typeregister_3=="Plantages"),]
    write.xlsx(Plantages34, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/3-4 matches.xlsx"), overwrite=T)
    write.xlsx(Plantages23, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/2-3 matches.xlsx"), overwrite=T)
    write.xlsx(Plantages12, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/1-2 matches.xlsx"), overwrite=T)
    write.xlsx(Plantages24, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/2-4 matches.xlsx"), overwrite=T)
    write.xlsx(Plantages14, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/1-4 matches.xlsx"), overwrite=T)
    write.xlsx(Plantages13, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Plantages/1-3 matches.xlsx"), overwrite=T)
   #excel particulieren
    Particulieren34 <- Serie34[which(Serie34$Typeregister_3=="Particulieren" | Serie34$Typeregister_4=="Particulieren"),]
    Particulieren23 <- Serie23[which(Serie23$Typeregister_2=="Particulieren" | Serie23$Typeregister_3=="Particulieren"),]
    Particulieren12 <- Serie12[which(Serie12$Typeregister_1=="Particulieren" | Serie12$Typeregister_2=="Particulieren"),]
    Particulieren24 <- Serie24[which(Serie24$Typeregister_2=="Particulieren" | Serie24$Typeregister_4=="Particulieren"),]
    Particulieren14 <- Serie14[which(Serie14$Typeregister_1=="Particulieren" | Serie14$Typeregister_4=="Particulieren"),]
    Particulieren13 <- Serie13[which(Serie13$Typeregister_1=="Particulieren" | Serie13$Typeregister_3=="Particulieren"),]
    write.xlsx(Particulieren34, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/3-4 matches.xlsx"), overwrite=T)
    write.xlsx(Particulieren23, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/2-3 matches.xlsx"), overwrite=T)
    write.xlsx(Particulieren12, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/1-2 matches.xlsx"), overwrite=T)
    write.xlsx(Particulieren24, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/2-4 matches.xlsx"), overwrite=T)
    write.xlsx(Particulieren14, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/1-4 matches.xlsx"), overwrite=T)
    write.xlsx(Particulieren13, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Particulieren/1-3 matches.xlsx"), overwrite=T)
    
    
  #testsets Serie34
    #write.table(dubbel, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Samples/3-4 dubbel.csv", quote=T, sep =",", col.names=T, row.names=F)
    write.table(Alkmaar, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Samples/3-4 Alkmaar.csv"), quote=T, sep =",", col.names=T, row.names=F)
    write.table(Barbados, paste0("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/", Sys.Date(), "/Samples/3-4 Barbados.csv"), quote=T, sep =",", col.names=T, row.names=F)
    
    