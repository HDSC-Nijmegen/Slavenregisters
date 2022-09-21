  
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
  ####  Matches are scored probabilistically                                   ####
  #################################################################################
  
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
    if(NUMMER1==3 & NUMMER2==4){
      df_matched <- df_matched[which(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) |
                                       df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                       df_matched$year_birth_1==df_matched$year_birth_2 |
                                       stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                       stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)<=lev_dist_laglead |
                                       stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                       df_matched$Naam_volgende_1==""), ]
    } else{
      df_matched <- df_matched[which(is.na(df_matched$year_birth_1) | is.na(df_matched$year_birth_2) |
                                       df_matched$year_birth_1=="-1" | df_matched$year_birth_2=="-1" | 
                                       df_matched$year_birth_1-df_matched$year_birth_2<=1 &
                                       df_matched$year_birth_1-df_matched$year_birth_2>= -1 |
                                       stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                       stringdist(df_matched$Naam_volgende_1, df_matched$Naam_volgende_2)<=lev_dist_laglead |
                                       stringdist(df_matched$Naam_vorige_1, df_matched$Naam_vorige_2)<=lev_dist_laglead & 
                                       df_matched$Naam_volgende_1==""), ]
    }
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
   
  #compute match score
    df_matched$Match_score <- 2.5*df_matched$Match_moeder_adaptive + #twice as important + tie-breaker
                                df_matched$Match_naam_number + df_matched$Match_moeder_number + 
                                2*df_matched$Match_year + 
                                df_matched$Match_vorige_adaptive + df_matched$Match_volgende_adaptive
    #add 0.5 for identical name matches between series 1 & 2 (tie-breaker) 
    if(NUMMER1==1 | NUMMER2==2){
      df_matched$Match_score <- ifelse(stringdist(df_matched$Naam_1, df_matched$Naam_2)==0, df_matched$Match_score+0.5, df_matched$Match_score)
    }
    
    
   #### step 5: apply adaptive Levenshtein distance ####
    
    #make maximum Levenshtein distance dependent on length of the name 
      df_matched <- df_matched[which(df_matched$Match_adaptive==1 & df_matched$Match_moeder_adaptive==1 & df_matched$Match_eigenaar_adaptive==1 | #matching names
                                       df_matched$Match_adaptive==1 & df_matched$Moeder_1=="" & df_matched$Match_eigenaar_adaptive==1 | #mother unknown
                                       df_matched$Match_adaptive==1 & df_matched$Moeder_2=="" & df_matched$Match_eigenaar_adaptive==1),] #mother only known in serie 1 (newborns)
    

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
    
   #add original names
    x <- df[,c("source_order", "Naam_original", "Moeder_original")]
    colnames(x) <- c("source_order_1", "Naam_original_1", "Moeder_original_1")
    df_full <- merge(df_full, x, by="source_order_1", all.x=T)
    colnames(x) <- c("source_order_2", "Naam_original_2", "Moeder_original_2")
    df_full <- merge(df_full, x, by="source_order_2", all.x=T)
    
   #order
    df_full <- df_full[,c("Typeregister_1", "Typeregister_2",
                          "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score",
                          "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", 
                          paste(c("Out_event", "In_event"), 1:2, sep="_"), 
                          paste(c("source_order", "source_order"), 1:2, sep="_"), 
                          paste(c("sex", "sex"), 1:2, sep="_"), 
                          paste(c("Naam_original", "Naam_original", "Naam", "Naam", "Naam_number", "Naam_number"), 1:2, sep="_"), 
                          paste(c("Moeder_original", "Moeder_original", "Moeder", "Moeder", "Moeder_number", "Moeder_number"), 1:2, sep="_"), 
                          paste(c("Eigenaar", "Eigenaar"), 1:2, sep="_"), 
                          paste(c("year_birth", "year_birth"), 1:2, sep="_"), 
                          paste(c("Naam_vorige", "Naam_vorige"), 1:2, sep="_"),
                          paste(c("Naam_volgende", "Naam_volgende"), 1:2, sep="_"))] %>% arrange(source_order_1, source_order_2)
    df_full <- df_full %>% arrange(source_order_1, -Match_score)
    
   #rename
    colnames(df_full) <- c(paste("Typeregister", NUMMER1, sep="_"), paste("Typeregister", NUMMER2, sep="_"),
                           "Match", "Match_adaptive", "Match_naam_number", "Match_moeder_adaptive", "Match_moeder_number", "Match_year", "Match_vorige_adaptive", "Match_volgende_adaptive", "Match_score",
                           "Naam_lv", "Moeder_lv", "Eigenaar_lv", "Naam_vorige_lv", "Naam_volgende_lv", 
                           paste("Out_event", NUMMER1, sep="_"), paste("In_event", NUMMER2, sep="_"),
                           paste("Source_order", NUMMER1, sep="_"), paste("Source_order", NUMMER2, sep="_"),
                           paste("Sex", NUMMER1, sep="_"), paste("Sex", NUMMER2, sep="_"),
                           paste("Naam_original", NUMMER1, sep="_"), paste("Naam_original", NUMMER1, sep="_"), paste("Naam", NUMMER1, sep="_"), paste("Naam", NUMMER2, sep="_"), paste("Naam_number", NUMMER1, sep="_"), paste("Naam_number", NUMMER2, sep="_"),
                           paste("Moeder_original", NUMMER1, sep="_"), paste("Moeder_original", NUMMER1, sep="_"), paste("Moeder", NUMMER1, sep="_"), paste("Moeder", NUMMER2, sep="_"), paste("Moeder_number", NUMMER1, sep="_"), paste("Moeder_number", NUMMER2, sep="_"),
                           paste("Eigenaar", NUMMER1, sep="_"), paste("Eigenaar", NUMMER2, sep="_"),
                           paste("Year_birth", NUMMER1, sep="_"), paste("Year_birth", NUMMER2, sep="_"),
                           paste("Naam_vorige", NUMMER1, sep="_"), paste("Naam_vorige", NUMMER2, sep="_"),
                           paste("Naam_volgende", NUMMER1, sep="_"), paste("Naam_volgende", NUMMER2, sep="_"))
    df_full
    
  }
  
  
  