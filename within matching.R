  
  ########################MATCHING RECORDS SLAVE REGISTERS 4#######################
  ####  This script matches records of the enslaved BETWEEN and WITHIN series  ####
  ####                                                                         ####
  ####  We use 2 conditions to establish matches WITHIN series:                ####
  ####    1. Levenshtein distance name enslaved                                ####
  ####    2. Levenshtein distance name mother                                  ####
  ####  Further filtering is done based on the:                                ####
  ####    1. Sex                                                               ####
  ####    2. Year of birth                                                     ####
  ####    3. Year of transfer                                                  ####
  ####  Matches are scored probabilistically                                   ####
  #################################################################################
  
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
   #adaptive Levenshtein distance
    Slave_names_matched <- Slave_names_matched[nchar(Slave_names_matched$Naam_1)>=2 & nchar(Slave_names_matched$Naam_1)<=3 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2)<=1 |
                                                 nchar(Slave_names_matched$Naam_1)>=4 & nchar(Slave_names_matched$Naam_1)<=8 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2)<=2 |
                                                 nchar(Slave_names_matched$Naam_1)>=9 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2), ]
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
     # 2 corresponding birth dates
    df_matched <- df_matched[which(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) |
                                     df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                     df_matched$month_birth_1==df_matched$month_birth_2 & df_matched$month_birth_1!="-1" &
                                     df_matched$day_birth_1==df_matched$day_birth_2 & df_matched$day_birth_1!="-1"), ]
  #select entries with EITHER: 
    # 1 mutation date OR 
    # 2 corresponding mutation years OR
    # 2 corresponding mutation dates
    df_matched <- df_matched[which(is.na(df_matched$year_exit_1) | is.na(df_matched$year_entry_2) |
                                     df_matched$year_exit_1=="-1" | df_matched$year_entry_2=="-1" | 
                                     df_matched$year_exit_1==df_matched$year_entry_2 | 
                                     df_matched$month_exit_1==df_matched$month_entry_2 & df_matched$month_exit_1!="-1" &
                                     df_matched$day_exit_1==df_matched$day_entry_2 & df_matched$day_exit_1=="-1"), ]
    
    
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
    
    
  #### step 5: apply adaptive Levenshtein distance ####
    
   #make maximum Levenshtein distance dependent on length of the name 
    df_matched <- df_matched[which(df_matched$Match_moeder_adaptive==1 |
                                     df_matched$Moeder_1==""),]
    
    
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
   #source_order_2
    colnames(x) <- c("source_order_2", "Typeregister_2", "Eigenaar_2", "Aanvullendeinformatieinschrijving_2", "Aanvullendeinformatieuitschrijving_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
    
  #add Naam_original and Moeder_original
    x <- df[,c("source_order", "Naam_original", "Moeder_original")]
    colnames(x) <- c("source_order_1", "Naam_original_1", "Moeder_original_1")
    df_full <- merge(df_full, x, by="source_order_1", all.x=T)
    colnames(x) <- c("source_order_2", "Naam_original_2", "Moeder_original_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
    
   #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_year_event", "Match_score", "Match_vorige_adaptive", "Match_volgende_adaptive",
                          "Naam_lv", "Moeder_lv", 
                          paste(c("source_order", "source_order"), 1:2, sep="_"),
                          paste(c("out_event", "in_event"), 1:2, sep="_"),
                          paste(c("year_exit", "year_entry"), 1:2, sep="_"), 
                          paste(c("sex", "sex"), 1:2, sep="_"), 
                          paste(c("Naam_original", "Naam_original", "Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
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
                           "Match", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year_birth", "Match_year_transfer", "Match_score", "Match_vorige_adaptive", "Match_volgende_adaptive",
                           "Naam_lv", "Moeder_lv", 
                           paste(c("Source_order", "Source_order"), 1:2, sep="_"),
                           paste(c("Out_event", "In_event"), 1:2, sep="_"),
                           paste(c("Year_exit", "Year_entry"), 1:2, sep="_"), 
                           paste(c("Sex", "Sex"), 1:2, sep="_"), 
                           paste(c("Naam_original", "Naam_original", "Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
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
  
  
  