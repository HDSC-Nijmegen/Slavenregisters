  
  
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
  setwd("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching")
  df <- fread("Cleaned Registry/cleaned slave register 2022-06-20.txt", encoding="UTF-8")
  
  #check data
  as.data.frame(table(df$Typeregister))
  as.data.frame(table(df$Serieregister))
  
  
  ##############################################################
  #### section 0a: load program for matching BETWEEN series ####
  ##############################################################
    
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

   #add preceding and proceeding NAAM to data frames
    df1 <- df1 %>% filter() %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
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
                              "Typeregister",
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
                              "Typeregister",
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
    df_matched <- df_matched[which(df_matched$Typeregister_1=="Plantages" & df_matched$Eigenaar_1==df_matched$Eigenaar_2 |
                                     df_matched$Typeregister_1=="Particulieren" & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2) <= lev_dist_eigenaar), ]
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
                                     df_matched$year_birth_1==df_matched$year_birth_2 |
                                     stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                     stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)<=lev_dist_laglead |
                                     stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                     df_matched$Naam_volgende_1==""), ]
   #Type register is identical
    df_matched <- df_matched[df_matched$Typeregister_1==df_matched$Typeregister_2, ]
    
    
    
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
    df_matched$Match <- ifelse(is.na(df_matched$Naam_1) | is.na(df_matched$Naam_2) | 
                                 df_matched$Naam_1=="" | df_matched$Naam_2=="", 0, 1)
   #match adaptive Levenshtein
    df_matched$Match_adaptive <- ifelse(is.na(df_matched$Naam_1) | is.na(df_matched$Naam_2) | df_matched$Naam_1=="" | df_matched$Naam_2=="" |
                                          nchar(df_matched$Naam_1)>=2 & nchar(df_matched$Naam_1)<=3 & stringdist(df_matched$Naam_1, df_matched$Naam_2)>1 |
                                          nchar(df_matched$Naam_1)>=4 & nchar(df_matched$Naam_1)<=8 & stringdist(df_matched$Naam_1, df_matched$Naam_2)>2 |
                                          nchar(df_matched$Naam_1)>=9 & stringdist(df_matched$Naam_1, df_matched$Naam_2)>3, 0, 1)
   #naam_number
    df_matched$Match_naam_number <- ifelse(is.na(df_matched$Naam_number_1) | is.na(df_matched$Naam_number_2) |
                                             df_matched$Naam_number_1=="" & df_matched$Naam_number_2=="", 0,
                                           ifelse(df_matched$Naam_number_1!=df_matched$Naam_number_2, -1, 1))
   #mother
    df_matched$Match_moeder <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | 
                                        df_matched$Moeder_lv>lev_dist_moeder, 0, 1)
    if(NUMMER1!=3 | NUMMER2!=4){
      df_matched$Match_moeder <- ifelse(df_matched$Moeder_1==df_matched$Moeder_2 & df_matched$Moeder_1=="", 0, df_matched$Match_moeder)
    }
   #mother adaptive Levenshtein
    df_matched$Match_moeder_adaptive <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | df_matched$Moeder_1=="" | df_matched$Moeder_2=="" |
                                                 nchar(df_matched$Moeder_1)>=2 & nchar(df_matched$Moeder_1)<=3 & stringdist(df_matched$Moeder_1, df_matched$Moeder_2)>1 |
                                                 nchar(df_matched$Moeder_1)>=4 & nchar(df_matched$Moeder_1)<=8 & stringdist(df_matched$Moeder_1, df_matched$Moeder_2)>2 |
                                                 nchar(df_matched$Moeder_1)>=9 & stringdist(df_matched$Moeder_1, df_matched$Moeder_2)>3, 0, 1)
    if(NUMMER1!=3 | NUMMER2!=4){
      df_matched$Match_moeder_adaptive <- ifelse(df_matched$Moeder_1==df_matched$Moeder_2 & df_matched$Moeder_1=="", 0, df_matched$Match_moeder_adaptive)
    }
   #moeder_number
    df_matched$Match_moeder_number <- ifelse(is.na(df_matched$Moeder_number_1) | is.na(df_matched$Moeder_number_2) |
                                               df_matched$Moeder_number_1=="" & df_matched$Moeder_number_2=="", 0,
                                             ifelse(df_matched$Moeder_number_1!=df_matched$Moeder_number_2, -1, 1))
   #eigenaar adaptive Levenshtein
    df_matched$Match_eigenaar_adaptive <- ifelse(is.na(df_matched$Eigenaar_1) | is.na(df_matched$Eigenaar_2) | df_matched$Eigenaar_1=="" | df_matched$Eigenaar_2=="" |
                                                   nchar(df_matched$Eigenaar_1)>=2 & nchar(df_matched$Eigenaar_1)<=5 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>1 |
                                                   nchar(df_matched$Eigenaar_1)>=6 & nchar(df_matched$Eigenaar_1)<=8 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>2 |
                                                   nchar(df_matched$Eigenaar_1)>=9 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>3, 0, 1)
   #year
    df_matched$Match_year <- ifelse(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) | 
                                      df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                      df_matched$year_birth_1!=df_matched$year_birth_2, 0, 1) 
   #previous entry
    df_matched$Match_vorige <- ifelse(df_matched$Naam_vorige_1 == "" |
                                      df_matched$Naam_vorige_2 == "" |
                                      df_matched$Naam_vorige_lv > lev_dist_laglead, 0, 1)
   #previous entry adaptive Levensthein
    df_matched$Match_vorige_adaptive <- ifelse(is.na(df_matched$Naam_vorige_1) | is.na(df_matched$Naam_vorige_2) | df_matched$Naam_vorige_1=="" | df_matched$Naam_vorige_2=="" |
                                          nchar(df_matched$Naam_vorige_1)>=2 & nchar(df_matched$Naam_vorige_1)<=3 & stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>1 |
                                          nchar(df_matched$Naam_vorige_1)>=4 & nchar(df_matched$Naam_vorige_1)<=8 & stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>2 |
                                          nchar(df_matched$Naam_vorige_1)>=9 & stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>3, 0, 1)
   #next entry
    df_matched$Match_volgende <- ifelse(df_matched$Naam_volgende_1 == "" |
                                      df_matched$Naam_volgende_2 == "" |
                                      df_matched$Naam_volgende_lv > lev_dist_laglead, 0, 1)
   #next entry adaptive Levensthein
    df_matched$Match_volgende_adaptive <- ifelse(is.na(df_matched$Naam_volgende_1) | is.na(df_matched$Naam_volgende_2) | df_matched$Naam_volgende_1=="" | df_matched$Naam_volgende_2=="" |
                                          nchar(df_matched$Naam_volgende_1)>=2 & nchar(df_matched$Naam_volgende_1)<=3 & stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>1 |
                                          nchar(df_matched$Naam_volgende_1)>=4 & nchar(df_matched$Naam_volgende_1)<=8 & stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>2 |
                                          nchar(df_matched$Naam_volgende_1)>=9 & stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>3, 0, 1)
   #out_event
    df_matched$Out_unended <- ifelse(df_matched$Out_event_1=="Ended" & df_matched$In_event_2=="Beginning", 1, 0)
   
  #compute match score
    df_matched$Match_score <- 2.5*df_matched$Match_moeder_adaptive + #twice as important + tie-breaker
                                df_matched$Match_naam_number + df_matched$Match_moeder_number + 
                                2*df_matched$Match_year + 
                                df_matched$Match_vorige_adaptive + df_matched$Match_volgende_adaptive -
                                df_matched$Out_unended
    df_matched$Match_score_plus_naam210 <- ifelse(df_matched$Naam_lv==0, df_matched$Match_score+2,
                                                  ifelse(df_matched$Naam_lv==1, df_matched$Match_score+1, df_matched$Match_score))
    df_matched$Match_score_plus_naam100 <- ifelse(df_matched$Naam_lv==0, df_matched$Match_score+1, df_matched$Match_score)
    
    
   #### step 5: filter best match ####
    
      df_matched <- df_matched[which(df_matched$Match_adaptive==1 & df_matched$Match_moeder_adaptive==1 & df_matched$Match_eigenaar_adaptive==1 |
                                       df_matched$Match_adaptive==1 & df_matched$Moeder_1=="" & df_matched$Match_eigenaar_adaptive==1),]
    

  #### step 6: add unmatched cases ####

  #add unmatched cases
   #df1
    colnames(df1)[3] <- "Out_event_x"
    df_full <- merge(df1[,1:10], df_matched, by=paste(c("source_order", "Typeregister", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 1, sep="_"), all=T)
    df_full$Out_event_1 <- ifelse(is.na(df_full$Out_event_1), df_full$Out_event_x, df_full$Out_event_1)
    df_full$Out_event_x <- NULL
   #Series 4
    colnames(df2)[3] <- "In_event_x"
    df_full <- merge(df2[,1:10], df_full, by=paste(c("source_order", "Typeregister", "Naam", "Naam_number", "Moeder", "Moeder_number", "Eigenaar", "year_birth", "sex"), 2, sep="_"), all=T)
    df_full$In_event_2 <- ifelse(is.na(df_full$In_event_2), df_full$In_event_x, df_full$In_event_2)
    df_full$In_event_x <- NULL
    
  #add flags
   #matched
    df_full$Match <- ifelse(is.na(df_full$Naam_1) | 
                              is.na(df_full$Naam_2) | 
                              df_full$Naam_1=="" | 
                              df_full$Naam_2=="", 0, 1)
   #match adaptive Levenshtein
    df_full$Match_adaptive <- ifelse(is.na(df_full$Naam_1) | is.na(df_full$Naam_2) | df_full$Naam_1=="" | df_full$Naam_2=="" |
                                       nchar(df_full$Naam_1)>=2 & nchar(df_full$Naam_1)<=3 & stringdist(df_full$Naam_1, df_full$Naam_2)>1 |
                                       nchar(df_full$Naam_1)>=4 & nchar(df_full$Naam_1)<=8 & stringdist(df_full$Naam_1, df_full$Naam_2)>2 |
                                       nchar(df_full$Naam_1)>=9 & stringdist(df_full$Naam_1, df_full$Naam_2)>3, 0, 1)
   #naam_number
    df_full$Match_naam_number <- ifelse(is.na(df_full$Naam_number_1) | is.na(df_full$Naam_number_2) |
                                          df_full$Naam_number_1=="" & df_full$Naam_number_2=="", 0, 
                                        ifelse(df_full$Naam_number_1!=df_full$Naam_number_2, -1, 1))
   #mother
    df_full$Match_moeder <- ifelse(is.na(df_full$Moeder_1) | is.na(df_full$Moeder_2) | 
                                     df_full$Moeder_lv>lev_dist_moeder, 0, 1)
    if(NUMMER1!=3 | NUMMER2!=4){
      df_full$Match_moeder <- ifelse(df_full$Moeder_1==df_full$Moeder_2 & df_full$Moeder_1=="", 0, df_full$Match_moeder)
    }
   #mother adaptive Levenshtein
    df_full$Match_moeder_adaptive <- ifelse(is.na(df_full$Moeder_1) | is.na(df_full$Moeder_2) | df_full$Moeder_1=="" | df_full$Moeder_2=="" |
                                              nchar(df_full$Moeder_1)>=2 & nchar(df_full$Moeder_1)<=3 & stringdist(df_full$Moeder_1, df_full$Moeder_2)>1 |
                                              nchar(df_full$Moeder_1)>=4 & nchar(df_full$Moeder_1)<=8 & stringdist(df_full$Moeder_1, df_full$Moeder_2)>2 |
                                              nchar(df_full$Moeder_1)>=9 & stringdist(df_full$Moeder_1, df_full$Moeder_2)>3, 0, 1)
    if(NUMMER1!=3 | NUMMER2!=4){
      df_full$Match_moeder_adaptive <- ifelse(df_full$Moeder_1==df_full$Moeder_2 & df_full$Moeder_1=="", 0, df_full$Match_moeder_adaptive)
    }
   #moeder_number
    df_full$Match_moeder_number <- ifelse(is.na(df_full$Moeder_number_1) | is.na(df_full$Moeder_number_2) | 
                                            df_full$Moeder_number_1=="" & df_full$Moeder_number_2=="", 0, 
                                          ifelse(df_full$Moeder_number_1!=df_full$Moeder_number_2, -1, 1))
   #eigenaar adaptive Levenshtein
    df_full$Match_eigenaar_adaptive <- ifelse(is.na(df_full$Eigenaar_1) | is.na(df_full$Eigenaar_2) | df_full$Eigenaar_1=="" | df_full$Eigenaar_2=="" |
                                                nchar(df_full$Eigenaar_1)>=2 & nchar(df_full$Eigenaar_1)<=4 & stringdist(df_full$Eigenaar_1, df_full$Eigenaar_2)>1 |
                                                nchar(df_full$Eigenaar_1)>=6 & nchar(df_full$Eigenaar_1)<=8 & stringdist(df_full$Eigenaar_1, df_full$Eigenaar_2)>2 |
                                                nchar(df_full$Eigenaar_1)>=9 & stringdist(df_full$Eigenaar_1, df_full$Eigenaar_2)>3, 0, 1)
   #year
    df_full$Match_year <- ifelse(is.na(df_full$year_birth_1) | is.na(df_full$year_birth_2) | 
                                   df_full$year_birth_1=="-1" | df_full$year_birth_2=="-1" | 
                                   df_full$year_birth_1!=df_full$year_birth_2, 0, 1) 
   #previous entry
    df_full$Match_vorige <- ifelse(is.na(df_full$Naam_vorige_1) | is.na(df_full$Naam_vorige_2) , 0, df_full$Match_vorige)
   #previous entry adaptive Levensthein 
    df_full$Match_vorige_adaptive <- ifelse(is.na(df_full$Naam_vorige_1) | is.na(df_full$Naam_vorige_2) , 0, df_full$Match_vorige_adaptive)
   #next entry
    df_full$Match_volgende <- ifelse(is.na(df_full$Naam_volgende_1) | is.na(df_full$Naam_volgende_2), 0, df_full$Match_volgende)
   #next entry adaptive Levensthein 
    df_full$Match_volgende_adaptive <- ifelse(is.na(df_full$Naam_volgende_1) | is.na(df_full$Naam_volgende_2), 0, df_full$Match_volgende_adaptive)

   
    
   #### step 7: structure and store data frame ####
    
   #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score", "Match_score_plus_naam210", "Match_score_plus_naam100",
                          "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", 
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
                           "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score", "Match_score_plus_naam210", "Match_score_plus_naam100",
                           "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", 
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
  
  
  #############################################################
  #### section 0b: load program for matching WITHIN series ####
  #############################################################
  
  match_within <- function(df1, lev_dist_naam, lev_dist_moeder, lev_dist_laglead, NUMMER1){
    
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
    
   #add preceding and proceeding NAAM to data frames
    df1 <- df1 %>% filter() %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                                                        Naam_volgende=lead(Naam)) %>% ungroup()
    #set NA on NAAM_VORIGE + NAAM_VOLGENDE to ""
    df1$Naam_vorige[is.na(df1$Naam_vorige)] <- ""
    df1$Naam_volgende[is.na(df1$Naam_volgende)] <- ""
    
   #rename variables df1
    df1$Eigenaar <- NULL
    colnames(df1) <- paste(c("source_order", 
                             "in_event", "out_event",
                             "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                             "Naam_vorige", "Naam_volgende"),
                           1, sep="_")
   #add df1 to SLAVE_NAMES_MATCHED
    x <- df1[which(df1$out_event_1=="Transferred"),]
    df_matched <- merge(Slave_names_matched, x, by="Naam_1", all=F)
   #rename variables df1
    colnames(df1) <- paste(c("source_order", 
                             "in_event", "out_event",
                             "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                             "Naam_vorige", "Naam_volgende"),
                           2, sep="_")
   #add df1 to DF_MATCHED
    x <- df1[which(df1$in_event_2=="Transferred"),]
    df_matched <- merge(df_matched, x, by="Naam_2", all=F )
    
    
   #### step 3: rule-based filtering of matches ####
    
   #filter rows
    #prevent that entries match themselves
    df_matched <- df_matched[which(df_matched$source_order_1 != df_matched$source_order_2), ]
    
    #same sex OR sex = "u"
    df_matched <- df_matched[which(df_matched$sex_1==df_matched$sex_2 | 
                                     df_matched$sex_1=="u" | df_matched$sex_2=="u"), ]
    
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
    df_matched$Match <- ifelse(is.na(df_matched$Naam_1) | 
                                 is.na(df_matched$Naam_2) | 
                                 df_matched$Naam_1=="" | 
                                 df_matched$Naam_2=="", 0, 1)
   #match adaptive Levenshtein
    df_matched$Match_adaptive <- ifelse(is.na(df_matched$Naam_1) | is.na(df_matched$Naam_2) | df_matched$Naam_1=="" | df_matched$Naam_2=="" |
                                          nchar(df_matched$Naam_1)>=2 & nchar(df_matched$Naam_1)<=3 & stringdist(df_matched$Naam_1, df_matched$Naam_2)>1 |
                                          nchar(df_matched$Naam_1)>=4 & stringdist(df_matched$Naam_1, df_matched$Naam_2)>2, 0, 1)
   #naam_number
    df_matched$Match_naam_number <- ifelse(is.na(df_matched$Naam_number_1) | is.na(df_matched$Naam_number_2), 0,
                                           ifelse(df_matched$Naam_number_1!=df_matched$Naam_number_2, -1, 1))
   #mother
    df_matched$Match_moeder <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | 
                                        df_matched$Moeder_1=="" & df_matched$Moeder_2!="" | 
                                        df_matched$Moeder_1!="" & df_matched$Moeder_2=="" | 
                                        df_matched$Moeder_lv>lev_dist_moeder, 0, 1)
    if(NUMMER1!=3 & NUMMER1!=4){
      df_matched$Match_moeder <- ifelse(df_matched$Moeder_1==df_matched$Moeder_2 & df_matched$Moeder_1=="", 0, df_matched$Match_moeder)
    }
   #mother adaptive Levenshtein
    df_matched$Match_moeder_adaptive <- ifelse(is.na(df_matched$Moeder_1) | is.na(df_matched$Moeder_2) | df_matched$Moeder_1=="" | df_matched$Moeder_2=="" |
                                                 nchar(df_matched$Moeder_1)>=2 & nchar(df_matched$Moeder_1)<=3 & stringdist(df_matched$Moeder_1, df_matched$Moeder_2)>1 |
                                                 nchar(df_matched$Moeder_1)>=4 & stringdist(df_matched$Moeder_1, df_matched$Moeder_2)>2, 0, 1)
    if(NUMMER1!=3 & NUMMER1!=4){
      df_matched$Match_moeder_adaptive <- ifelse(df_matched$Moeder_1==df_matched$Moeder_2 & df_matched$Moeder_1=="", 0, df_matched$Match_moeder_adaptive)
    }
   #moeder_number
    df_matched$Match_moeder_number <- ifelse(is.na(df_matched$Moeder_number_1) | is.na(df_matched$Moeder_number_2), 0,
                                             ifelse(df_matched$Moeder_number_1!=df_matched$Moeder_number_2, -1, 1))
   #year_birth
    df_matched$Match_year <- ifelse(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) | 
                                      df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                      df_matched$year_birth_1!=df_matched$year_birth_2, 0, 1) 
   #year_event
    df_matched$Match_year_event <- ifelse(is.na(df_matched$year_exit_1) | is.na(df_matched$year_entry_2) | 
                                            df_matched$year_exit_1=="-1" | df_matched$year_entry_2=="-1" | 
                                            df_matched$year_exit_1!=df_matched$year_entry_2, 0, 1) 
   #month_event
    df_matched$Match_month_event <- ifelse(is.na(df_matched$month_exit_1) | is.na(df_matched$month_entry_2) | 
                                             df_matched$month_exit_1=="-1" | df_matched$month_entry_2=="-1" | 
                                             df_matched$month_exit_1!=df_matched$month_entry_2, 0, 1) 
   #month_event
    df_matched$Match_day_event <- ifelse(is.na(df_matched$day_exit_1) | is.na(df_matched$day_entry_2) | 
                                             df_matched$day_exit_1=="-1" | df_matched$day_entry_2=="-1" | 
                                             df_matched$day_exit_1!=df_matched$day_entry_2, 0, 1)
   #previous entry
    df_matched$Match_vorige <- ifelse(df_matched$Naam_vorige_1=="" |
                                      df_matched$Naam_vorige_2=="" |
                                      stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>lev_dist_laglead, 0, 1)
   #previous entry adaptive Levensthein
    df_matched$Match_vorige_adaptive <- ifelse(is.na(df_matched$Naam_vorige_1) | is.na(df_matched$Naam_vorige_2) | df_matched$Naam_vorige_1=="" | df_matched$Naam_vorige_2=="" |
                                          nchar(df_matched$Naam_vorige_1)>=2 & nchar(df_matched$Naam_vorige_1)<=3 & stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>1 |
                                          nchar(df_matched$Naam_vorige_1)>=4 & stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)>2, 0, 1)
   #next entry
    df_matched$Match_volgende <- ifelse(df_matched$Naam_volgende_1=="" |
                                      df_matched$Naam_volgende_2=="" |
                                      stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>lev_dist_laglead, 0, 1)
   #next entry adpative Levensthein
    df_matched$Match_volgende_adaptive <- ifelse(is.na(df_matched$Naam_volgende_1) | is.na(df_matched$Naam_volgende_2) | df_matched$Naam_volgende_1=="" | df_matched$Naam_volgende_2=="" |
                                          nchar(df_matched$Naam_volgende_1)>=2 & nchar(df_matched$Naam_volgende_1)<=3 & stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>1 |
                                          nchar(df_matched$Naam_volgende_1)>=4 & stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)>2, 0, 1)
    
  #compute match score
    df_matched$Match_score <- 2.5*df_matched$Match_moeder_adaptive + #twice as important + tie-breaker
      df_matched$Match_naam_number + df_matched$Match_moeder_number + 
      2*df_matched$Match_year + 
      2*df_matched$Match_year_event +
      1*df_matched$Match_month_event +
      1*df_matched$Match_day_event +
      1*df_matched$Match_vorige_adaptive +
      1*df_matched$Match_volgende_adaptive
    df_matched$Match_score <- ifelse(df_matched$Match_month_event==1 & df_matched$Match_day_event==1, df_matched$Match_score+2, df_matched$Match_score)
    df_matched$Match_score_plus_naam210 <- ifelse(df_matched$Naam_lv==0, df_matched$Match_score+2,
                                                  ifelse(df_matched$Naam_lv==1, df_matched$Match_score+1, df_matched$Match_score))
    df_matched$Match_score_plus_naam100 <- ifelse(df_matched$Naam_lv==0, df_matched$Match_score+1, df_matched$Match_score)
    
    
  #### step 5: filter best match ####
    
    df_matched <- df_matched[which(df_matched$Match_adaptive==1 & df_matched$Match_moeder_adaptive==1 |
                                     df_matched$Match_adaptive==1 & df_matched$Moeder_1==""),]
    
    
  #### step 6: add unmatched cases ####
    
  #add unmatched cases
   #out_events
    colnames(df1) <- paste(c("source_order", 
                             "in_event", "out_event",
                             "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                             "Naam_vorige", "Naam_volgende"),
                           1, sep="_")
    colnames(df1)[2:3] <- c("in_event_x", "out_event_x")
    x <- df1[which(df1$out_event_x=="Transferred"),]
    df_full <- merge(x, df_matched, by=paste(c("source_order", 
                                               "Naam", "Naam_number", 
                                               "Moeder", "Moeder_number", 
                                               "year_birth",
                                               "year_entry", "month_entry", "day_entry",
                                               "year_exit", "month_exit", "day_exit",
                                               "sex",
                                               "Naam_vorige", "Naam_volgende"), 1, sep="_"), all=T)
    df_full$out_event_1 <- ifelse(is.na(df_full$out_event_1), df_full$out_event_x, df_full$out_event_1)
    df_full$out_event_x <- NULL
    df_full$in_event_x <- NULL
   #in_events
    colnames(df1) <- paste(c("source_order", 
                             "in_event", "out_event",
                             "Naam", "Naam_number", 
                             "Moeder", "Moeder_number", 
                             "year_birth",
                             "year_entry", "month_entry", "day_entry",
                             "year_exit", "month_exit", "day_exit",
                             "sex",
                             "Naam_vorige", "Naam_volgende"),
                           2, sep="_")
    colnames(df1)[2:3] <- c("in_event_x", "out_event_x")
    x <- df1[which(df1$in_event_x=="Transferred"),]
    df_full <- merge(x, df_full, by=paste(c("source_order", 
                                            "Naam", "Naam_number", 
                                            "Moeder", "Moeder_number", 
                                            "year_birth",
                                            "year_entry", "month_entry", "day_entry",
                                            "year_exit", "month_exit", "day_exit",
                                            "sex",
                                            "Naam_vorige", "Naam_volgende"), 2, sep="_"), all=T)
    df_full$in_event_2 <- ifelse(is.na(df_full$in_event_2), df_full$in_event_x, df_full$in_event_2)
    df_full$in_event_x <- NULL
    df_full$out_event_x <- NULL
    
  #add flags
   #matched
    df_full$Match <- ifelse(is.na(df_full$Naam_1) | is.na(df_full$Naam_2) | df_full$Naam_1=="" | df_full$Naam_2=="", 0, 1)
   #match adaptive Levenshtein
    df_full$Match_adaptive <- ifelse(is.na(df_full$Naam_1) | is.na(df_full$Naam_2) | df_full$Naam_1=="" | df_full$Naam_2=="" |
                                       nchar(df_full$Naam_1)>=2 & nchar(df_full$Naam_1)<=3 & stringdist(df_full$Naam_1, df_full$Naam_2)>1 |
                                       nchar(df_full$Naam_1)>=4 & stringdist(df_full$Naam_1, df_full$Naam_2)>2, 0, 1)
   #naam_number
    df_full$Match_naam_number <- ifelse(is.na(df_full$Naam_number_1) | is.na(df_full$Naam_number_2) | 
                                          df_full$Naam_number_1!=df_full$Naam_number_2, 0, 1)
   #mother
    df_full$Match_moeder <- ifelse(is.na(df_full$Moeder_1) | 
                                     is.na(df_full$Moeder_2) | 
                                     df_full$Moeder_1=="" & df_full$Moeder_2!="" | 
                                     df_full$Moeder_1!="" & df_full$Moeder_2=="" | 
                                     df_full$Moeder_lv>lev_dist_moeder, 0, 1)
    if(NUMMER1!=3 & NUMMER1!=4){
      df_full$Match_moeder <- ifelse(df_full$Moeder_1==df_full$Moeder_2 & df_full$Moeder_1=="", 0, df_full$Match_moeder)
    }
   #match mother adaptive
    df_full$Match_moeder_adaptive <- ifelse(is.na(df_full$Moeder_1) | is.na(df_full$Moeder_2) | df_full$Moeder_1=="" | df_full$Moeder_2=="" |
                                              nchar(df_full$Moeder_1)>=2 & nchar(df_full$Moeder_1)<=3 & stringdist(df_full$Moeder_1, df_full$Moeder_2)>1 |
                                              nchar(df_full$Moeder_1)>=4 & stringdist(df_full$Moeder_1, df_full$Moeder_2)>2, 0, 1)
    if(NUMMER1!=3 & NUMMER1!=4){
      df_full$Match_moeder_adaptive <- ifelse(df_full$Moeder_1==df_full$Moeder_2 & df_full$Moeder_1=="", 0, df_full$Match_moeder_adaptive)
    }
   #moeder_number
    df_full$Match_moeder_number <- ifelse(is.na(df_full$Moeder_number_1) | 
                                            is.na(df_full$Moeder_number_2) | 
                                            df_full$Moeder_number_1!=df_full$Moeder_number_2, 0, 1)
   #year
    df_full$Match_year <- ifelse(is.na(df_full$year_birth_1) | 
                                   is.na(df_full$year_birth_2) | 
                                   df_full$year_birth_1=="-1" | 
                                   df_full$year_birth_2=="-1" | 
                                   df_full$year_birth_1!=df_full$year_birth_2, 0, 1) 
   #year_event
    df_full$Match_year_event <- ifelse(is.na(df_full$year_exit_1) | 
                                         is.na(df_full$year_entry_2) |
                                         df_full$year_exit_1=="-1" | 
                                         df_full$year_entry_2=="-1" | 
                                         df_full$year_exit_1!=df_full$year_entry_2, 0, 1)
   #month_event
    df_full$Match_month_event <- ifelse(is.na(df_full$month_exit_1) | 
                                          is.na(df_full$month_entry_2) |
                                          df_full$month_exit_1=="-1" | 
                                          df_full$month_entry_2=="-1" | 
                                          df_full$month_exit_1!=df_full$month_entry_2, 0, 1)
   #month_event
    df_full$Match_day_event <- ifelse(is.na(df_full$day_exit_1) | 
                                          is.na(df_full$day_entry_2) |
                                          df_full$day_exit_1=="-1" | 
                                          df_full$day_entry_2=="-1" | 
                                          df_full$day_exit_1!=df_full$day_entry_2, 0, 1)
    
    
  #### step 7: structure and store data frame ####
    
  #add type register
   #source_order_1
    x <- df[,c("source_order", "Typeregister", "Eigenaar", "Aanvullendeinformatieinschrijv", "Aanvullendeinformatieuitschrij")]
    colnames(x) <- c("source_order_1", "Typeregister_1", "Eigenaar_1", "Aanvullendeinformatieinschrijving_1", "Aanvullendeinformatieuitschrijving_1")
    df_full <- merge(df_full, x, by="source_order_1", all.x=T)
   #source_order_1
    colnames(x) <- c("source_order_2", "Typeregister_2", "Eigenaar_2", "Aanvullendeinformatieinschrijving_2", "Aanvullendeinformatieuitschrijving_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
    
   #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_year_event", "Match_score", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score_plus_naam210", "Match_score_plus_naam100",
                          "Naam_lv", "Moeder_lv", 
                          paste(c("source_order", "source_order"), 1:2, sep="_"),
                          paste(c("out_event", "in_event"), 1:2, sep="_"),
                          paste(c("year_exit", "year_entry"), 1:2, sep="_"), 
                          paste(c("sex", "sex"), 1:2, sep="_"), 
                          paste(c("Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                          paste(c("Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                          paste(c("year_birth", "year_birth"), 1:2, sep="_"),
                          paste(c("month_exit", "month_entry"), 1:2, sep="_"),
                          paste(c("day_exit", "day_entry"), 1:2, sep="_"),
                          paste(c("Naam_vorige", "Naam_vorige"), 1:2, sep="_"),
                          paste(c("Naam_volgende", "Naam_volgende"), 1:2, sep="_"),
                          paste(c("Eigenaar", "Eigenaar"), 1:2, sep="_"),
                          paste(c("Aanvullendeinformatieinschrijving", "Aanvullendeinformatieinschrijving"), 1:2, sep="_"),
                          paste(c("Aanvullendeinformatieuitschrijving", "Aanvullendeinformatieuitschrijving"), 1:2, sep="_"))] %>% arrange(source_order_1, source_order_2)
    df_full <- df_full %>% arrange(source_order_1, -Match_score)
    
    
   #rename
    colnames(df_full) <- c("Typeregister_1", "Typeregister_2",
                           "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year_birth", "Match_year_transfer", "Match_score", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score_plus_naam210", "Match_score_plus_naam100",
                           "Naam_lv", "Moeder_lv", 
                           paste(c("Source_order", "Source_order"), 1:2, sep="_"),
                           paste(c("Out_event", "In_event"), 1:2, sep="_"),
                           paste(c("Year_exit", "Year_entry"), 1:2, sep="_"), 
                           paste(c("Sex", "Sex"), 1:2, sep="_"), 
                           paste(c("Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                           paste(c("Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                           paste(c("Year_birth", "Year_birth"), 1:2, sep="_"),
                           paste(c("Month_exit", "Month_entry"), 1:2, sep="_"),
                           paste(c("Day_exit", "Day_entry"), 1:2, sep="_"),
                           paste(c("Naam_vorige", "Naam_vorige"), 1:2, sep="_"),
                           paste(c("Naam_volgende", "Naam_volgende"), 1:2, sep="_"),
                           paste(c("Eigenaar", "Eigenaar"), 1:2, sep="_"),
                           paste(c("Aanvullendeinformatieinschrijving", "Aanvullendeinformatieinschrijving"), 1:2, sep="_"),
                           paste(c("Aanvullendeinformatieuitschrijving", "Aanvullendeinformatieuitschrijving"), 1:2, sep="_"))
    df_full
    
  }
  
  
  #####################################################
  #### section 1a: retrieve matches BETWEEN series ####
  #####################################################
  
    df$Eigenaar_original <- df$Eigenaar
    df$Eigenaar <- ifelse(df$Typeregister=="Plantages", df$plantation_name, df$Eigenaar_Last_name)
    df$year_birth_original <- df$year_birth
    df$year_birth <- df$year_birth2
  
  #match serie 3 & 4
   #select series
    Serie3 <- df[which(df$Serieregister_nr==3), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie34 <- match_between(Serie3, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=3, NUMMER2=4)
    
  #match serie 2 & 3
   #select series
    Serie2 <- df[which(df$Serieregister_nr==2 & df$out_event2=="Ended" |
                         df$Serieregister_nr==2 & df$year_entry>=1848) , c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3 & df$in_event2=="Beginning" |
                   df$Serieregister_nr==3 & df$year_entry==1848, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie23 <- match_between(Serie2, Serie3, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=2, NUMMER2=3)
    
  #match serie 1 & 2
   #select series
    Serie1 <- df[which(df$Serieregister_nr==1 & df$out_event2=="Ended" |
                         df$Serieregister_nr==1 & df$year_entry>=1838), c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie2 <- df[df$Serieregister_nr==2 & df$in_event2=="Beginning" |
                   df$Serieregister_nr==2 & df$year_entry==1838, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie12 <- match_between(Serie1, Serie2, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=2)
    
  #match serie 2 & 4
   #select series
    Serie2 <- df[df$Serieregister_nr==2 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=1848 |
                   df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth==-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie24 <- match_between(Serie2, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=2, NUMMER2=4)
    
  #match serie 1 & 4
   #select series
    Serie1 <- df[df$Serieregister_nr==1 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie4 <- df[df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=1838 |
                   df$Serieregister_nr==4 & df$in_event2=="Beginning" & df$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie14 <- match_between(Serie1, Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=4)
    
  #match serie 1 & 3
   #select series
    Serie1 <- df[df$Serieregister_nr==1 & df$out_event2=="Ended", c("source_order", "Typeregister", "out_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
    Serie3 <- df[df$Serieregister_nr==3 & df$in_event2=="Beginning" & df$year_birth<=1838 |
                   df$Serieregister_nr==3 & df$in_event2=="Beginning" & df$year_birth<=-1, c("source_order", "Typeregister", "in_event2", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar", "sex")]
   #match series
    Serie13 <- match_between(Serie1, Serie3, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_eigenaar=3, lev_dist_laglead=3, NUMMER1=1, NUMMER2=3)
    
    
  #select duplicates
    #dubbel <- Serie34 %>% group_by(source_order_3) %>% filter(n()>1 & !is.na(source_order_3)) %>% ungroup()
  
  #select best match source_order_3
    #Serie34 <- Serie34 %>% group_by(source_order_3) %>% filter(Match_score==max(Match_score) | is.na(source_order_3) | is.na(source_order_4)) %>% ungroup()
  #select best match source_order_4
    #Serie34 <- Serie34 %>% group_by(source_order_4) %>% filter(Match_score==max(Match_score) | is.na(source_order_3) | is.na(source_order_4)) %>% ungroup()
    
    
  ####################################################
  #### section 1b: retrieve matches WITHIN series ####
  ####################################################
    
    Serie4 <- df[df$Serieregister_nr==4, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie4 <- Serie4[which(Serie4$out_event2=="Transferred" | Serie4$in_event2=="Transferred"),]
    Serie3 <- df[df$Serieregister_nr==3, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie3 <- Serie3[which(Serie3$out_event2=="Transferred" | Serie3$in_event2=="Transferred"),]
    Serie2 <- df[df$Serieregister_nr==2, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie2 <- Serie2[which(Serie2$out_event2=="Transferred" | Serie2$in_event2=="Transferred"),]
    Serie1 <- df[df$Serieregister_nr==1, c("source_order", 
                                           "in_event2", "out_event2",
                                           "Naam", "Naam_number", 
                                           "Moeder", "Moeder_number", 
                                           "Eigenaar",
                                           "year_birth",
                                           "year_entry", "month_entry", "day_entry",
                                           "year_exit", "month_exit", "day_exit",
                                           "sex")]
    Serie1 <- Serie1[which(Serie1$out_event2=="Transferred" | Serie1$in_event2=="Transferred"),]
    
    Serie44 <- match_within(Serie4, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_laglead = 3, NUMMER1=4)
    Serie33 <- match_within(Serie3, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_laglead = 3, NUMMER1=3)
    Serie22 <- match_within(Serie2, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_laglead = 3, NUMMER1=2)
    Serie11 <- match_within(Serie1, lev_dist_naam=3, lev_dist_moeder=3, lev_dist_laglead = 3, NUMMER1=1)
    
    
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
    
    filter_unique <- function(df1, NUMMER1, NUMMER2){
      
      #make temporary variable to replace source_order in series 1 and 2
      df1$Order_1 <- df1[[paste("Source_order", NUMMER1, sep="_")]]
      df1$Order_2 <- df1[[paste("Source_order", NUMMER2, sep="_")]]
      #filter best match from serie 1
      Serie_unique <- df1 %>% group_by(Order_1) %>% filter(is.na(Match_score) | Match_score==max(Match_score)) %>% arrange(Order_1) %>% ungroup()
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
                                         "Source_order", "Year_birth"), NUMMER2, sep="_"))] <- NA
      #bind
      Serie_unique <- rbind(Serie_unique, x, y) %>% arrange(Order_1, Order_2)
      rm(x, y)
      Serie_unique[,which(colnames(Serie_unique)!=c("Order_1", "Order_2"))]
    }
    
    
  #######################################################
  #### section 3a: group certificates between series ####
  #######################################################
    
   #filter unique matches
    Serie34 <- filter_unique(Serie34, 3, 4)
    Serie23 <- filter_unique(Serie23, 2, 3)
    Serie12 <- filter_unique(Serie12, 1, 2)
    Serie24 <- filter_unique(Serie24, 2, 4)
    Serie14 <- filter_unique(Serie14, 1, 4)
    Serie13 <- filter_unique(Serie13, 1, 3)
    
   #set Source_order_3 to character
    Serie34$Source_order_3 <- as.character(Serie34$Source_order_3) 
    Serie23$Source_order_3 <- as.character(Serie23$Source_order_3) 
    Serie13$Source_order_3 <- as.character(Serie13$Source_order_3) 
    
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
    
  #merge non-consecutive series
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
    
    
  ######################################################
  #### section 3b: group certificates within series ####
  ######################################################
    
  #filter unique matches
    Serie44 <- filter_unique(Serie44, 1, 2)
    Serie33 <- filter_unique(Serie33, 1, 2)
    Serie22 <- filter_unique(Serie22, 1, 2)
    Serie11 <- filter_unique(Serie11, 1, 2)
    
   #set Source_order_3 to character
    Serie33$Source_order_1 <- as.character(Serie33$Source_order_1) 
    Serie33$Source_order_2 <- as.character(Serie33$Source_order_2) 
    
  #load program to add compile matches 
    build_within <- function(df1, df2, NUMMER1, NUMMER2){
      df2 <- df2[!is.na(df2$Source_order_1) & !is.na(df2$Source_order_2), c("Source_order_1", "Source_order_2")]
      colnames(df2) <- c(paste("Source_order", NUMMER1, sep="_"), paste("Source_order", NUMMER2, sep="_"))
      df1 <- merge(df1, df2, by=paste("Source_order", NUMMER1, sep="_"), all.x=T)
      df1
    }
    
   #4-4
    #matched cases
    Serie44_linked <- Serie44[!is.na(Serie44$Source_order_1) & !is.na(Serie44$Source_order_2), c("Source_order_1", "Source_order_2")]
    #compile matches
    Serie44_linked <- build_within(Serie44_linked, Serie44, 2, 3)
    Serie44_linked$Source_order_3 <- ifelse(Serie44_linked$Source_order_1==Serie44_linked$Source_order_3, NA, Serie44_linked$Source_order_3) #prevents loops
    Serie44_linked <- build_within(Serie44_linked, Serie44, 3, 4)
    Serie44_linked <- build_within(Serie44_linked, Serie44, 4, 5)
    Serie44_linked <- build_within(Serie44_linked, Serie44, 5, 6)
    Serie44_linked <- build_within(Serie44_linked, Serie44, 6, 7)
    #remove overlap
    Serie44_linked <- Serie44_linked[!(Serie44_linked$Source_order_2 %in% Serie44_linked$Source_order_7 |
                                         Serie44_linked$Source_order_2 %in% Serie44_linked$Source_order_6 |
                                         Serie44_linked$Source_order_2 %in% Serie44_linked$Source_order_5 |
                                         Serie44_linked$Source_order_2 %in% Serie44_linked$Source_order_4 |
                                         Serie44_linked$Source_order_2 %in% Serie44_linked$Source_order_3), c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4", "Source_order_5", "Source_order_6", "Source_order_7")]
    #unmatched cases
    Serie44_unlinked <- Serie44[is.na(Serie44$Source_order_1) | is.na(Serie44$Source_order_2), c("Source_order_1", "Source_order_2")]
    Serie44_unlinked$Source_order_7 <- Serie44_unlinked$Source_order_6 <- Serie44_unlinked$Source_order_5 <- Serie44_unlinked$Source_order_4 <- Serie44_unlinked$Source_order_3 <- NA
    #bind
    Serie44_linked <- rbind(Serie44_linked, Serie44_unlinked)
    #clean environment
    rm(Serie44_unlinked)
    
   #3-3
    #matched cases
    Serie33_linked <- Serie33[!is.na(Serie33$Source_order_1) & !is.na(Serie33$Source_order_2), c("Source_order_1", "Source_order_2")]
    #compile matches
    Serie33_linked <- build_within(Serie33_linked, Serie33, 2, 3)
    Serie33_linked$Source_order_3 <- ifelse(Serie33_linked$Source_order_1==Serie33_linked$Source_order_3, NA, Serie33_linked$Source_order_3) #prevents loops
    Serie33_linked <- build_within(Serie33_linked, Serie33, 3, 4)
    Serie33_linked <- build_within(Serie33_linked, Serie33, 4, 5)
    Serie33_linked <- build_within(Serie33_linked, Serie33, 5, 6)
    Serie33_linked <- build_within(Serie33_linked, Serie33, 6, 7)
    #remove overlap
    Serie33_linked <- Serie33_linked[!(Serie33_linked$Source_order_2 %in% Serie33_linked$Source_order_5 |
                                         Serie33_linked$Source_order_2 %in% Serie33_linked$Source_order_4 |
                                         Serie33_linked$Source_order_2 %in% Serie33_linked$Source_order_3), c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4", "Source_order_5", "Source_order_6", "Source_order_7")]
    #unmatched cases
    Serie33_unlinked <- Serie33[is.na(Serie33$Source_order_1) | is.na(Serie33$Source_order_2), c("Source_order_1", "Source_order_2")]
    Serie33_unlinked$Source_order_7 <- Serie33_unlinked$Source_order_6 <- Serie33_unlinked$Source_order_5 <- Serie33_unlinked$Source_order_4 <- Serie33_unlinked$Source_order_3 <- NA
    #bind
    Serie33_linked <- rbind(Serie33_linked, Serie33_unlinked)
    #clean environment
    rm(Serie33_unlinked)
    
   #2-2
    #matched cases
    Serie22_linked <- Serie22[!is.na(Serie22$Source_order_1) & !is.na(Serie22$Source_order_2), c("Source_order_1", "Source_order_2")]
    #compile matches
    Serie22_linked <- build_within(Serie22_linked, Serie22, 2, 3)
    Serie22_linked$Source_order_3 <- ifelse(Serie22_linked$Source_order_1==Serie22_linked$Source_order_3, NA, Serie22_linked$Source_order_3) #prevents loops
    Serie22_linked <- build_within(Serie22_linked, Serie22, 3, 4)
    Serie22_linked <- build_within(Serie22_linked, Serie22, 4, 5)
    Serie22_linked <- build_within(Serie22_linked, Serie22, 5, 6)
    Serie22_linked <- build_within(Serie22_linked, Serie22, 6, 7)
    #remove overlap
    Serie22_linked <- Serie22_linked[!(Serie22_linked$Source_order_2 %in% Serie22_linked$Source_order_7 |
                                         Serie22_linked$Source_order_2 %in% Serie22_linked$Source_order_6 |
                                         Serie22_linked$Source_order_2 %in% Serie22_linked$Source_order_5 |
                                         Serie22_linked$Source_order_2 %in% Serie22_linked$Source_order_4 |
                                         Serie22_linked$Source_order_2 %in% Serie22_linked$Source_order_3), c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4", "Source_order_5", "Source_order_6", "Source_order_7")]
    #unmatched cases
    Serie22_unlinked <- Serie22[is.na(Serie22$Source_order_1) | is.na(Serie22$Source_order_2), c("Source_order_1", "Source_order_2")]
    Serie22_unlinked$Source_order_7 <- Serie22_unlinked$Source_order_6 <- Serie22_unlinked$Source_order_5 <- Serie22_unlinked$Source_order_4 <- Serie22_unlinked$Source_order_3 <- NA
    #bind
    Serie22_linked <- rbind(Serie22_linked, Serie22_unlinked)
    #clean environment
    rm(Serie22_unlinked)
    
   #1-1
    #matched cases
    Serie11_linked <- Serie11[!is.na(Serie11$Source_order_1) & !is.na(Serie11$Source_order_2), c("Source_order_1", "Source_order_2")]
    #compile matches
    Serie11_linked <- build_within(Serie11_linked, Serie11, 2, 3)
    Serie11_linked$Source_order_3 <- ifelse(Serie11_linked$Source_order_1==Serie11_linked$Source_order_3, NA, Serie11_linked$Source_order_3) #prevents loops
    Serie11_linked <- build_within(Serie11_linked, Serie11, 3, 4)
    Serie11_linked <- build_within(Serie11_linked, Serie11, 4, 5)
    Serie11_linked <- build_within(Serie11_linked, Serie11, 5, 6)
    Serie11_linked <- build_within(Serie11_linked, Serie11, 6, 7)
    #remove overlap
    Serie11_linked <- Serie11_linked[!(Serie11_linked$Source_order_2 %in% Serie11_linked$Source_order_6 |
                                         Serie11_linked$Source_order_2 %in% Serie11_linked$Source_order_5 |
                                         Serie11_linked$Source_order_2 %in% Serie11_linked$Source_order_4 |
                                         Serie11_linked$Source_order_2 %in% Serie11_linked$Source_order_3), c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4", "Source_order_5", "Source_order_6", "Source_order_7")]
    #unmatched cases
    Serie11_unlinked <- Serie11[is.na(Serie11$Source_order_1) | is.na(Serie11$Source_order_2), c("Source_order_1", "Source_order_2")]
    Serie11_unlinked$Source_order_7 <- Serie11_unlinked$Source_order_6 <- Serie11_unlinked$Source_order_5 <- Serie11_unlinked$Source_order_4 <- Serie11_unlinked$Source_order_3 <- NA
    #bind
    Serie11_linked <- rbind(Serie11_linked, Serie11_unlinked)
    #clean environment
    rm(Serie11_unlinked)
    
  
  #################################################################################
  #### section 3c: combine groupings from between matches and within groupings ####
  ##################################################################################
    
  #1. map first and last entry WITHIN series
   #Serie 44 
    #not necessary, as end is manumission
   #Serie 33
    Serie33_linked$LastEntry <- ifelse(is.na(Serie33_linked$Source_order_2), NA,
                                       ifelse(is.na(Serie33_linked$Source_order_3), Serie33_linked$Source_order_2,
                                              ifelse(is.na(Serie33_linked$Source_order_4), Serie33_linked$Source_order_3,
                                                     ifelse(is.na(Serie33_linked$Source_order_5), Serie33_linked$Source_order_4,
                                                            ifelse(is.na(Serie33_linked$Source_order_6), Serie33_linked$Source_order_5,
                                                                   ifelse(is.na(Serie33_linked$Source_order_7), Serie33_linked$Source_order_6, Serie33_linked$Source_order_7))))))
   #Serie22
    Serie22_linked$LastEntry <- ifelse(is.na(Serie22_linked$Source_order_2), NA,
                                       ifelse(is.na(Serie22_linked$Source_order_3), Serie22_linked$Source_order_2,
                                              ifelse(is.na(Serie22_linked$Source_order_4), Serie22_linked$Source_order_3,
                                                     ifelse(is.na(Serie22_linked$Source_order_5), Serie22_linked$Source_order_4,
                                                            ifelse(is.na(Serie22_linked$Source_order_6), Serie22_linked$Source_order_5,
                                                                   ifelse(is.na(Serie22_linked$Source_order_7), Serie22_linked$Source_order_6, Serie22_linked$Source_order_7))))))
   #Serie11
    Serie11_linked$LastEntry <- ifelse(is.na(Serie11_linked$Source_order_2), NA,
                                       ifelse(is.na(Serie11_linked$Source_order_3), Serie11_linked$Source_order_2,
                                              ifelse(is.na(Serie11_linked$Source_order_4), Serie11_linked$Source_order_3,
                                                     ifelse(is.na(Serie11_linked$Source_order_5), Serie11_linked$Source_order_4,
                                                            ifelse(is.na(Serie11_linked$Source_order_6), Serie11_linked$Source_order_5,
                                                                   ifelse(is.na(Serie11_linked$Source_order_7), Serie11_linked$Source_order_6, Serie11_linked$Source_order_7))))))
    
  #2. add WITHIN matches to BETWEEN matches in reconsitution
    
   #4-4
    colnames(Serie44_linked) <- c("Source_order_4", "Source_order_44_2", "Source_order_44_3", "Source_order_44_4", "Source_order_44_5", 
                                  "Source_order_44_6", "Source_order_44_7")
    #matched cases
    reconstitution1 <- reconstitution[!is.na(reconstitution$Source_order_4), ]
    reconstitution1 <- merge(reconstitution1, Serie44_linked, by="Source_order_4", all=T)
    #unmatched cases
    reconstitution2 <- reconstitution[is.na(reconstitution$Source_order_4), ]
    #bind
    reconstitution2$Source_order_44_2 <- reconstitution2$Source_order_44_3 <- reconstitution2$Source_order_44_3 <- reconstitution2$Source_order_44_4 <- reconstitution2$Source_order_44_5 <- reconstitution2$Source_order_44_6 <- reconstitution2$Source_order_44_7 <- NA
    reconstitution <- rbind(reconstitution1, reconstitution2)
    
   #3-3
   #internal to 4
    colnames(Serie33_linked) <- c("Source_order_33_1", "Source_order_33_2", "Source_order_33_3", "Source_order_33_4", "Source_order_33_5", 
                                  "Source_order_33_6", "Source_order_33_7", "Source_order_4")
    #matched cases
    reconstitution1 <- reconstitution[!is.na(reconstitution$Source_order_4), ]
    reconstitution1 <- merge(reconstitution1, Serie33_linked, by="Source_order_4", all=T)
    #unmatched cases
    reconstitution2 <- reconstitution[is.na(reconstitution$Source_order_4), ]
    #bind
    reconstitution2$Source_order_33_1 <- reconstitution2$Source_order_33_2 <- reconstitution2$Source_order_33_3 <- reconstitution2$Source_order_33_3 <- reconstitution2$Source_order_33_4 <- reconstitution2$Source_order_33_5 <- reconstitution2$Source_order_33_6 <- reconstitution2$Source_order_33_7 <- NA
    reconstitution <- rbind(reconstitution1, reconstitution2)
    
   #internal to 3
    #drop INTERNAL links without match that also occur in BETWEEN
    reconstitution <- reconstitution[!(reconstitution$Source_order_3 %in% reconstitution$Source_order_33_1 & !is.na(reconstitution$Source_order_33_1) & is.na(reconstitution$Source_order_33_2)),]
    #
    #reconstitution1 <- reconstitution[which(reconstitution$Source_order_3 %in% reconstitution$Source_order_33_1 & !is.na(reconstitution$Source_order_3)),]
    #reconstitution2 <- reconstitution[which(!(reconstitution$Source_order_3 %in% reconstitution$Source_order_33_1) & !is.na(reconstitution$Source_order_3)),]
    #reconstitution3 <- reconstitution[is.na(reconstitution$Source_order_3),]
    
    
   #2-2
   #internal to 3
    colnames(Serie22_linked) <- c("Source_order_22_1", "Source_order_22_2", "Source_order_22_3", "Source_order_22_4", "Source_order_22_5", 
                                  "Source_order_22_6", "Source_order_22_7", "Source_order_3")
    #matched cases
    reconstitution1 <- reconstitution[!is.na(reconstitution$Source_order_3), ]
    reconstitution1 <- merge(reconstitution1, Serie22_linked, by="Source_order_3", all=T)
    #unmatched cases
    reconstitution2 <- reconstitution[is.na(reconstitution$Source_order_3), ]
    #bind
    reconstitution2$Source_order_22_1 <- reconstitution2$Source_order_22_2 <- reconstitution2$Source_order_22_3 <- reconstitution2$Source_order_22_3 <- reconstitution2$Source_order_22_4 <- reconstitution2$Source_order_22_5 <- reconstitution2$Source_order_22_6 <- reconstitution2$Source_order_22_7 <- NA
    reconstitution <- rbind(reconstitution1, reconstitution2)
    
   #internal to 2
    #drop INTERNAL links without match that also occur in BETWEEN
    reconstitution <- reconstitution[!(reconstitution$Source_order_2 %in% reconstitution$Source_order_22_1 & !is.na(reconstitution$Source_order_22_1) & is.na(reconstitution$Source_order_22_2)),]
    #
    #reconstitution1 <- reconstitution[which(reconstitution$Source_order_2 %in% reconstitution$Source_order_22_1 & !is.na(reconstitution$Source_order_2)),]
    #reconstitution2 <- reconstitution[which(!(reconstitution$Source_order_2 %in% reconstitution$Source_order_22_1) & !is.na(reconstitution$Source_order_2)),]
    #reconstitution3 <- reconstitution[is.na(reconstitution$Source_order_2),]
    
    
   #1-1
   #internal to 2
    colnames(Serie11_linked) <- c("Source_order_11_1", "Source_order_11_2", "Source_order_11_3", "Source_order_11_4", "Source_order_11_5", 
                                  "Source_order_11_6", "Source_order_11_7", "Source_order_2")
    #matched cases
    reconstitution1 <- reconstitution[!is.na(reconstitution$Source_order_2), ]
    reconstitution1 <- merge(reconstitution1, Serie11_linked, by="Source_order_2", all=T)
    #unmatched cases
    reconstitution2 <- reconstitution[is.na(reconstitution$Source_order_2), ]
    #bind
    reconstitution2$Source_order_11_1 <- reconstitution2$Source_order_11_2 <- reconstitution2$Source_order_11_3 <- reconstitution2$Source_order_11_3 <- reconstitution2$Source_order_11_4 <- reconstitution2$Source_order_11_5 <- reconstitution2$Source_order_11_6 <- reconstitution2$Source_order_11_7 <- NA
    reconstitution <- rbind(reconstitution1, reconstitution2)
    
    
   #internal to 1
    #drop INTERNAL links without match that also occur in BETWEEN
    reconstitution <- reconstitution[!(reconstitution$Source_order_1 %in% reconstitution$Source_order_11_1 & !is.na(reconstitution$Source_order_11_1) & is.na(reconstitution$Source_order_11_2)),]
    #
    #reconstitution1 <- reconstitution[which(reconstitution$Source_order_1 %in% reconstitution$Source_order_11_1 & !is.na(reconstitution$Source_order_1)),]
    #reconstitution2 <- reconstitution[which(!(reconstitution$Source_order_1 %in% reconstitution$Source_order_11_1) & !is.na(reconstitution$Source_order_1)),]
    #reconstitution3 <- reconstitution[is.na(reconstitution$Source_order_1),]
    
   #clean environment
    rm(reconstitution1, reconstitution2)
    
    
  #################################################################
  ### section 3d: assign grouped certificates same id_person ####
  ###############################################################
    
   #generate SR id 
    reconstitution$Id_person <- paste("SR", sprintf("%06d", 1:length(reconstitution$Source_order_1)), sep="-")
    reconstitution <- melt(setDT(reconstitution), id.vars = c("Id_person"), value.name = "source_order")
    reconstitution <- reconstitution[!is.na(reconstitution$source_order), c("Id_person", "source_order")]
    
    
  #####################################################
  #### section 4: store file in long format series ####
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
    df2 <- df2 %>% arrange(Id_person, Source_series, StartEntryYear)
   #write outfiles
    write.table(df2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.txt"), quote=F, sep ="\t", col.names=T, row.names=F, fileEncoding="UTF-8")
    write.xlsx(df2, paste0("Reconstituted registry/", Sys.Date(), "SR life courses.xlsx"), overwrite=T)
    
    
  #######################################################
  #### section 5: make excerpt in wide format series ####
  #######################################################
    
    
    
    
