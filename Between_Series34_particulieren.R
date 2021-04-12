  
  
  ########################MATCHING WITHIN SERIES 4########################
  ####  This script matches records of the enslaved WITHIN series 4.  ####
  ####  We use 3 conditions to estbalish matches:                     ####
  ####    1. Levenshtein distance name enslaved                       ####
  ####    2. Levenshtein distance name mother                         ####
  ####    3. Birth year                                               ####
  ########################################################################
  
  
  #load packages
  library("haven")
  library("dplyr")
  library("stringdist")
  library("openxlsx")
  
  #clean environment
  rm(list=ls())
  
  #set levenshtein distance of matches
  lev_dist <- 2
  
  #open dataset
  setwd("U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Most Important Files/Stata")
  df <- read_dta("Preparing_Record_Linkage.dta")
  
  #check data
  as.data.frame(table(df$Typeregister))
  as.data.frame(table(df$Serieregister))
  
  #replace (...)
   #naam
    df$Naam <- gsub("\\(...)", "", df$Naam)
    df$Naam <- gsub("\\(.)", "", df$Naam)
   #naam
    df$Moeder <- gsub("\\(...)", "", df$Moeder)
    df$Moeder <- gsub("\\(.)", "", df$Moeder)
    
  #split naam & number
   #fix /  /
    df$Naam <- gsub(" / IV /", " IV", df$Naam)
    df$Naam <- gsub(" / III /", " III", df$Naam)
    df$Naam <- gsub(" / II /", " II", df$Naam)
    df$Naam <- gsub(" / I /", " I", df$Naam)
   #make separate variable
    df$Naam_number <- ""
    df$Naam_number <- ifelse(grepl(" V", df$Naam)   &  grepl(" V.", df$Naam)==F,    "V",   df$Naam_number)
    df$Naam_number <- ifelse(grepl(" IV", df$Naam)  &  grepl(" IV.", df$Naam)==F,   "IV",  df$Naam_number)
    df$Naam_number <- ifelse(grepl(" III", df$Naam) &  grepl(" III.", df$Naam)==F,  "III", df$Naam_number)
    df$Naam_number <- ifelse(grepl(" II", df$Naam)  &  grepl(" II.", df$Naam)==F,   "II",  df$Naam_number)
    df$Naam_number <- ifelse(grepl(" I", df$Naam)   &  grepl(" I.", df$Naam)==F,    "I",   df$Naam_number)
   #remove roman numerical
    df$Naam <- ifelse(grepl(" V", df$Naam)   &  grepl(" V.", df$Naam)==F,    gsub(" V", "", df$Naam),   df$Naam)
    df$Naam <- ifelse(grepl(" IV", df$Naam)  &  grepl(" IV.", df$Naam)==F,   gsub(" IV", "", df$Naam),  df$Naam)
    df$Naam <- ifelse(grepl(" III", df$Naam) &  grepl(" III.", df$Naam)==F,  gsub(" III", "", df$Naam), df$Naam)
    df$Naam <- ifelse(grepl(" II", df$Naam)  &  grepl(" II.", df$Naam)==F,   gsub(" II", "", df$Naam),  df$Naam)
    df$Naam <- ifelse(grepl(" I", df$Naam)   &  grepl(" I.", df$Naam)==F,    gsub(" I", "", df$Naam),   df$Naam)
    
    
  #split moeder & number
   #fix /  /
    df$Moeder <- gsub(" / IV /", " IV", df$Moeder)
    df$Moeder <- gsub(" / III /", " III", df$Moeder)
    df$Moeder <- gsub(" / II /", " II", df$Moeder)
    df$Moeder <- gsub(" / I /", " I", df$Moeder)
   #make separate variable
    df$Moeder_number <- ""
    #roman numericals
    df$Moeder_number <- ifelse(grepl(" V", df$Moeder)   &  grepl(" V.", df$Moeder)==F,    "V",   df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" IV", df$Moeder)  &  grepl(" IV.", df$Moeder)==F,   "IV",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" III", df$Moeder) &  grepl(" III.", df$Moeder)==F,  "III", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" II", df$Moeder)  &  grepl(" II.", df$Moeder)==F,   "II",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" I", df$Moeder)   &  grepl(" I.", df$Moeder)==F,    "I",   df$Moeder_number)
    #arabic numericals
    df$Moeder_number <- ifelse(grepl(" 5", df$Moeder)   &  grepl(" 5.", df$Moeder)==F,    "V",   df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" 4", df$Moeder)   &  grepl(" 4.", df$Moeder)==F,    "IV",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" 3", df$Moeder)   &  grepl(" 3.", df$Moeder)==F,    "III", df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" 2", df$Moeder)   &  grepl(" 2.", df$Moeder)==F,    "II",  df$Moeder_number)
    df$Moeder_number <- ifelse(grepl(" 1", df$Moeder)   &  grepl(" 1.", df$Moeder)==F,    "I",   df$Moeder_number)
   #remove numericals
    #roman
    df$Moeder <- ifelse(grepl(" V", df$Moeder)   &  grepl(" V.", df$Moeder)==F,    gsub(" V", "", df$Moeder),   df$Moeder)
    df$Moeder <- ifelse(grepl(" IV", df$Moeder)  &  grepl(" IV.", df$Moeder)==F,   gsub(" IV", "", df$Moeder),  df$Moeder)
    df$Moeder <- ifelse(grepl(" III", df$Moeder) &  grepl(" III.", df$Moeder)==F,  gsub(" III", "", df$Moeder), df$Moeder)
    df$Moeder <- ifelse(grepl(" II", df$Moeder)  &  grepl(" II.", df$Moeder)==F,   gsub(" II", "", df$Moeder),  df$Moeder)
    df$Moeder <- ifelse(grepl(" I", df$Moeder)   &  grepl(" I.", df$Moeder)==F,    gsub(" I", "", df$Moeder),   df$Moeder)
    #arabic
    df$Moeder <- ifelse(grepl(" 5", df$Moeder)   &  grepl(" 5.", df$Moeder)==F,    gsub(" 5", "", df$Moeder),   df$Moeder)
    df$Moeder <- ifelse(grepl(" 4", df$Moeder)   &  grepl(" 4.", df$Moeder)==F,    gsub(" 4", "", df$Moeder),   df$Moeder)
    df$Moeder <- ifelse(grepl(" 3", df$Moeder)   &  grepl(" 3.", df$Moeder)==F,    gsub(" 3", "", df$Moeder),   df$Moeder)
    df$Moeder <- ifelse(grepl(" 2", df$Moeder)   &  grepl(" 2.", df$Moeder)==F,    gsub(" 2", "", df$Moeder),   df$Moeder)
    df$Moeder <- ifelse(grepl(" 1", df$Moeder)   &  grepl(" 1.", df$Moeder)==F,    gsub(" 1", "", df$Moeder),   df$Moeder)
    
  
  #select serie 3 & 4
  Serie3 <- df[df$Serieregister=="1848-1851" & df$out_event_general=="Ended", c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar")]
  Serie4 <- df[df$Serieregister=="1851-1863" & df$in_event_general=="Beginning", c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "Eigenaar")]
  
  #Match slave names
   #unique names
    Slave_names_3 <- Serie3[!duplicated(Serie3$Naam) & Serie3$Naam!="",]
    Slave_names_4 <- Serie4[!duplicated(Serie4$Naam) & Serie4$Naam!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names_3$Naam, Slave_names_4$Naam, method = "lv")
   #Filter Levenshtein distance == x
    x <- 0
    repeat{
     #filter Lev Dist == x
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #list names + Lev Dist
      l <- data.frame(Naam1 = Slave_names_3$Naam[l[,1]],
                      Naam2 = Slave_names_4$Naam[l[,2]])
      l$LV <- x
      l <- list(l)
      #list LVdist
      if("l_Naam" %in% ls() ){
        l_Naam <- c(l_Naam, l)
      } else{
        l_Naam <- l
      }
     #set break
      if(x==lev_dist) {
        break
      }
     #prepare repeat
      x <- x+1
    }
   #merge into one data frame
    Slave_names_matched <- do.call("rbind", l_Naam)
    colnames(Slave_names_matched)[1:3] <- c("Naam_3", "Naam_4", "Naam_lv")
   #clean environment
    rm(l, l_Naam, LV_matrix, Slave_names_3, Slave_names_4, x)
    
    
  #Match mother names
   #unique names
    Slave_names_3 <- Serie3[!duplicated(Serie3$Moeder) & Serie3$Moeder!="",]
    Slave_names_4 <- Serie4[!duplicated(Serie4$Moeder) & Serie4$Moeder!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names_3$Moeder, Slave_names_4$Moeder, method = "lv")
   #Filter Levenshtein distance == x
    x <- 0
    repeat{
     #filter Lev Dist == x
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #list names + Lev Dist
      l <- data.frame(Naam1 = Slave_names_3$Moeder[l[,1]],
                      Naam2 = Slave_names_4$Moeder[l[,2]])
      l$LV <- x
      l <- list(l)
     #list LVdist
      if("l_Naam" %in% ls() ){
        l_Naam <- c(l_Naam, l)
      } else{
        l_Naam <- l
      }
     #set break
      if(x==lev_dist) {
        break
      }
     #prepare repeat
      x <- x+1
    }
   #merge into one data frame
    Moeder_names_matched <- do.call("rbind", l_Naam)
    colnames(Moeder_names_matched)[1:3] <- c("Moeder_3", "Moeder_4", "Moeder_lv")
   #clean environment
    rm(l, l_Naam, LV_matrix, Slave_names_3, Slave_names_4, x)
    
    
  #Match names owners
   #unique names
    Slave_names_3 <- Serie3[!duplicated(Serie3$Eigenaar) & Serie3$Eigenaar!="",]
    Slave_names_4 <- Serie4[!duplicated(Serie4$Eigenaar) & Serie4$Eigenaar!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names_3$Eigenaar, Slave_names_4$Eigenaar, method = "lv")
   #Filter Levenshtein distance == x
    x <- 0
    repeat{
     #filter Lev Dist == x
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #list names + Lev Dist
      l <- data.frame(Naam1 = Slave_names_3$Eigenaar[l[,1]],
                      Naam2 = Slave_names_4$Eigenaar[l[,2]])
      l$LV <- x
      l <- list(l)
     #list LVdist
      if("l_Naam" %in% ls() ){
        l_Naam <- c(l_Naam, l)
      } else{
        l_Naam <- l
      }
     #set break
      if(x==lev_dist) {
        break
      }
     #prepare repeat
      x <- x+1
    }
    #merge into one data frame
    Eigenaar_matched <- do.call("rbind", l_Naam)
    colnames(Eigenaar_matched)[1:3] <- c("Eigenaar_3", "Eigenaar_4", "Eigenaar_lv")
    #clean environment
    rm(l, l_Naam, LV_matrix, Slave_names_3, Slave_names_4, x)
    
    
    
  #add lagging & leading names of slaves with same owner 
    Serie3 <- Serie3 %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                                                 Naam_volgende=lead(Naam)) %>% ungroup()
    Serie4 <- Serie4 %>% arrange(source_order) %>% group_by(Eigenaar) %>% mutate(Naam_vorige=lag(Naam),
                                                                                 Naam_volgende=lead(Naam)) %>% ungroup()
    
     
  #combine matches
   #find matches
    colnames(Serie3) <- c("source_order_3", 
                          "Naam_3", "Naam_number_3", 
                          "Moeder_3", "Moeder_number_3", 
                          "year_birth_3",
                          "Eigenaar_3",
                          "Naam_vorige_3", "Naam_volgende_3")
    Serie34_matched <- merge(Serie3, Slave_names_matched, by="Naam_3", all=F)
    Serie34_matched <- merge(Serie34_matched, Moeder_names_matched, by="Moeder_3", all=F)
    Serie34_matched_Eigenaar <- merge(Serie34_matched, Eigenaar_matched, by="Eigenaar_3", all=F)
   #add source_order, in-event, out-event, & birth year
    colnames(Serie4) <- c("source_order_4", 
                          "Naam_4", "Naam_number_4", 
                          "Moeder_4", "Moeder_number_4", 
                          "year_birth_4",
                          "Eigenaar_4",
                          "Naam_vorige_4", "Naam_volgende_4")
    Serie34_matched <- merge(Serie34_matched, Serie4, by=c("Naam_4", "Moeder_4"), all=F )
    Serie34_matched_Eigenaar <- merge(Serie34_matched_Eigenaar, Serie4, by=c("Naam_4", "Moeder_4", "Eigenaar_4"), all=F )
    
    
    
    
  #order columns
    Serie34_matched <- Serie34_matched[,c("source_order_3", "source_order_4",
                                          "Naam_lv", "Moeder_lv", 
                                          "Naam_3", "Naam_number_3", "Naam_4", "Naam_number_4",
                                          "Moeder_3", "Moeder_number_3", "Moeder_4", "Moeder_number_4",
                                          "Eigenaar_3", "Eigenaar_4",
                                          "year_birth_3", "year_birth_4",
                                          "Naam_vorige_3", "Naam_vorige_4",
                                          "Naam_volgende_3", "Naam_volgende_3")]
    Serie34_matched_Eigenaar <- Serie34_matched_Eigenaar[,c("source_order_3", "source_order_4",
                                                            "Naam_lv", "Moeder_lv", "Eigenaar_lv",
                                                            "Naam_3", "Naam_number_3", "Naam_4", "Naam_number_4",
                                                            "Moeder_3", "Moeder_number_3", "Moeder_4", "Moeder_number_4",
                                                            "Eigenaar_3", "Eigenaar_4",
                                                            "year_birth_3", "year_birth_4",
                                                            "Naam_vorige_3", "Naam_vorige_4",
                                                            "Naam_volgende_3", "Naam_volgende_4")]
    
    
    
  #check difference
   #combine source orders into 1 var
    Serie34_matched_Eigenaar$id <- paste(Serie34_matched_Eigenaar$source_order_3, Serie34_matched_Eigenaar$source_order_4)
    Serie34_matched$id <- paste(Serie34_matched$source_order_3, Serie34_matched$source_order_4)
   #select dropped
    Serie34_matched_diff <- Serie34_matched[!(Serie34_matched$id %in% Serie34_matched_Eigenaar$id),]
   #clean environment
    Serie34_matched_Eigenaar$id <- NULL
    Serie34_matched$id <- NULL
    
    
    write.xlsx(Serie34_matched, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/3-4.xlsx")
    write.xlsx(Serie34_matched_Eigenaar, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/3-4_Eigenaar.xlsx")
    write.xlsx(Serie34_matched_diff, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Between/3-4_diff.xlsx")
    
    
    