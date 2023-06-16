
########################MATCHING RECORDS SLAVE REGISTERS AND EMANCIPATION REGISTER####################################
####  This script matches records of the enslaved BETWEEN Slave registers Serie 4 and Emancipation registers      ####
####                                                                                                              ####
####  We use 3 conditions to establish matches BETWEEN series:                                                    ####
####    1. Levenshtein distance name enslaved                                                                     ####
####    2. Levenshtein distance name owner (for private owners)                                                   ####
####                                                                                                              ####
####  Further filtering is done based on the:                                                                     ####
####    1. Sex                                                                                                    ####
####    2. Year of birth                                                                                          ####
####                                                                                                              ####
####  Matches are scored probabilistically (higher match score indicates that match is more likley to be correct) ####
######################################################################################################################

match_between_emancipation <- function(df1, df2, lev_dist_naam, lev_dist_eigenaar){
  
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
  #adaptive Levenshtein distance
  Slave_names_matched <- Slave_names_matched[nchar(Slave_names_matched$Naam_1)>=2 & nchar(Slave_names_matched$Naam_1)<=3 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2)<=1 |
                                               nchar(Slave_names_matched$Naam_1)>=4 & nchar(Slave_names_matched$Naam_1)<=8 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2)<=2 |
                                               nchar(Slave_names_matched$Naam_1)>=9 & stringdist(Slave_names_matched$Naam_1, Slave_names_matched$Naam_2)<=lev_dist_naam, ]
  #clean environment
  rm(l, LV_matrix, Slave_names_1, Slave_names_2, x)
  
  
  #### step 2: add metadata and select relevant columns in data frames ####
  
  df1 <- df1 %>% rename(Naam_1 = Naam,
                        Naam_number_1 = Naam_number ) 
  df2 <- df2 %>% rename(Naam_2 = Naam,
                        Naam_number_2 = Naam_number) 
  #add df1 and df2 to SLAVE_NAMES_MATCHED
  df_matched <- merge(df1, Slave_names_matched, by="Naam_1", all=F, allow.cartesian=T)
  df_matched <- merge(df_matched, df2, by="Naam_2", all=F, allow.cartesian=T)
  
  
  #### step 3: rule-based filtering of matches ####
  
  df_matched <- df_matched %>% 
    #filter (Place_name1 == plantation_name) %>% 
    filter (sex_emanc == sex | sex_emanc == "unknown" | sex  == "unknown") 
  
  #filter rows
  #same owner: Levenshtein distance <= LEV_DIST_EIGENAAR
  df_matched <- df_matched[which(df_matched$plantation_match ==1 & df_matched$Place_name1_match ==1 & df_matched$plantation_name==df_matched$Place_name1 |
                                   (df_matched$plantation_match ==0 | df_matched$Place_name1_match ==0) & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2) <= lev_dist_eigenaar), ]
  
  df_matched <- df_matched %>%
    filter(plantation_match == Place_name1_match)
  
  
  #### step 4: add metadata and compute match score####
  
  df_matched$Eigenaar_lv <- stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)
  
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
  
  #eigenaar adaptive Levenshtein
  df_matched$Match_eigenaar_adaptive <- ifelse(is.na(df_matched$Eigenaar_1) | is.na(df_matched$Eigenaar_2) | df_matched$Eigenaar_1=="" | df_matched$Eigenaar_2=="" |
                                                 nchar(df_matched$Eigenaar_1)>=2 & nchar(df_matched$Eigenaar_1)<=5 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>1 |
                                                 nchar(df_matched$Eigenaar_1)>=6 & nchar(df_matched$Eigenaar_1)<=8 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>2 |
                                                 nchar(df_matched$Eigenaar_1)>=9 & stringdist(df_matched$Eigenaar_1, df_matched$Eigenaar_2)>3, 0, 1)
  
  #year
  df_matched$Match_year <- ifelse(is.na(df_matched$B_year) | is.na(df_matched$year_birth) | 
                                    df_matched$B_year=="-1" | df_matched$year_birth=="-1" | 
                                    (df_matched$B_year > df_matched$year_birth +1 | df_matched$B_year < df_matched$year_birth -1), 0, 1) 
  df_matched$Match_year_close <- ifelse(is.na(df_matched$B_year) | is.na(df_matched$year_birth) | 
                                          df_matched$B_year=="-1" | df_matched$year_birth=="-1" | 
                                          df_matched$Match_year == 1 |
                                          (df_matched$B_year > df_matched$year_birth +3 | df_matched$B_year < df_matched$year_birth -3) , 0, 1) 
  df_matched$Match_year_less_close <- ifelse(is.na(df_matched$B_year) | is.na(df_matched$year_birth) | 
                                               df_matched$Match_year == 1 | df_matched$Match_year_close ==1 |
                                               df_matched$B_year=="-1" | df_matched$year_birth=="-1" | 
                                               (df_matched$B_year > df_matched$year_birth +10 | df_matched$B_year < df_matched$year_birth -10) , 0, 1) 
  
  
  df_matched$Match_year2 <- ifelse(is.na(df_matched$B_year2) | is.na(df_matched$year_birth) | 
                                     df_matched$B_year2=="-1" | df_matched$year_birth=="-1" | 
                                     (df_matched$B_year2 > df_matched$year_birth +1 | df_matched$B_year2 < df_matched$year_birth -1) , 0, 1) 
  df_matched$Match_year2_close <- ifelse(is.na(df_matched$B_year2) | is.na(df_matched$year_birth) | 
                                           df_matched$B_year2=="-1" | df_matched$year_birth=="-1" | 
                                           df_matched$Match_year2 == 1 |
                                           (df_matched$B_year2 > df_matched$year_birth +3 | df_matched$B_year2 < df_matched$year_birth -3) , 0, 1) 
  df_matched$Match_year2_less_close <- ifelse(is.na(df_matched$B_year2) | is.na(df_matched$year_birth) | 
                                                df_matched$Match_year2 == 1 | df_matched$Match_year2_close ==1 |
                                                df_matched$B_year2=="-1" | df_matched$year_birth=="-1" | 
                                                (df_matched$B_year2 > df_matched$year_birth +10 | df_matched$B_year2 < df_matched$year_birth -10) , 0, 1) 
  
  #compute match score
  df_matched$Match_score <- 
    3*df_matched$Match_year + 
    3*df_matched$Match_year2 +
    2*df_matched$Match_year_close + 
    2*df_matched$Match_year2_close +
    1*df_matched$Match_year_less_close + 
    1*df_matched$Match_year2_less_close +
    1*df_matched$Match_naam_number +
    2*df_matched$Match_eigenaar_adaptive
  
  
  
  #### step 5: filter highest match and create separate file for matches with identical scores####
  
  #Filter highest matches
  df_matched <- df_matched %>%
    distinct(Id_person, source_order, .keep_all = T) %>%
    group_by(Id_person) %>%
    filter(Match_score == max(Match_score)) %>%
    filter(Naam_lv == min(Naam_lv)) %>%
    ungroup() %>%
    group_by(source_order) %>%
    filter(Match_score == max(Match_score)) %>%
    filter(Naam_lv == min(Naam_lv)) %>%
    ungroup() %>%
    filter(((plantation_name != "" & Match_score > 0) | year_birth == -1 ) | ((plantation_name == "" & Match_score > 1) | year_birth == -1)) %>%
    select(-Match, -Match_adaptive, -Match_naam_number, -Match_year, -Match_year_close, -Match_year_less_close, -Match_year2, -Match_year2_close, -Match_year2_less_close)
  
  #Separate File for identical scores (unclear which one to choose)
  df_identical_scores_1 <- df_matched %>%
    group_by(Id_person) %>%
    filter(n() > 1) %>% 
    ungroup() %>%
    arrange(Id_person) 
  
  df_identical_scores_2 <- df_matched %>%
    group_by(source_order) %>%
    filter(n() > 1) %>% 
    ungroup() %>%
    arrange(source_order)
  
  df_identical <- bind_rows(df_identical_scores_1, df_identical_scores_2) %>%
    distinct(Id_person, source_order, .keep_all = T) %>%
    mutate(identical_score = 1)
  rm(df_identical_scores_1, df_identical_scores_2)
  
  #Separate file for unique matches only
  df_unique <- left_join(df_matched, df_identical) %>%
    filter(is.na(identical_score)) 
  
  df_matched_ER <- rbind(df_unique, df_identical) %>%
    distinct(Id_person, .keep_all = T)
  
  df_matched_SR <- rbind(df_unique, df_identical) %>%
    distinct(source_order, .keep_all = T)
  
  
  #### step 6: add unmatched cases ####
  
  #Emancipation Register
  df_not_matched_ER <- left_join(df1, df_matched_ER, by= c("Id_person", "Naam_1", "Naam_number_1", "B_year", "B_year2", "Place_name1", "plantation_match", "Eigenaar_1","sex_emanc")) %>%
    group_by(Id_person) %>%
    mutate(flag = n()) %>%
    ungroup %>%
    filter(is.na(source_order)) %>%
    group_by(Id_person) %>%
    mutate(flag2 = n()) %>%
    ungroup %>%
    filter(flag == flag2) %>%
    select(-flag, -flag2) %>%
    distinct(Id_person, .keep_all = T)
  
  #Series 4
  df_not_matched_SR <- left_join(df2, df_matched_SR,  by= c("source_order", "Naam_2", "Naam_number_2", "year_birth", "plantation_name", "Place_name1_match", "Eigenaar_2", "sex")) %>%
    group_by(source_order) %>%
    mutate(flag = n()) %>%
    ungroup %>%
    filter(is.na(Id_person)) %>%
    group_by(source_order) %>%
    mutate(flag2 = n()) %>%
    ungroup %>%
    filter(flag == flag2) %>%
    select(-flag, -flag2) %>%
    distinct(source_order, .keep_all = T)
  
  
  list <-  list(df_matched, df_not_matched_ER, df_not_matched_SR, df_unique, df_identical)
  list
}
