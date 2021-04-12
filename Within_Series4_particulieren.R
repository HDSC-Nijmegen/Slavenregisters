  
  
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
    
  
  #select serie 4
  Serie4 <- df[df$Serieregister=="1851-1863", c("source_order", "Naam", "Naam_number", "Moeder", "Moeder_number", "year_birth", "in_event_general", "out_event_general")]
  Serie4_done <- Serie4[Serie4$in_event_general=="Beginning" & Serie4$out_event_general=="Ended",]
  Serie4 <- Serie4[!(Serie4$in_event_general=="Beginning" & Serie4$out_event_general=="Ended"),]
  
  #Match slave names
   #unique names
    Slave_names <- Serie4[!duplicated(Serie4$Naam) & Serie4$Naam!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names$Naam, Slave_names$Naam, method = "lv")
   #Filter Levenshtein distance == x
    x <- 0
    repeat{
     #filter Lev Dist == x
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #list names + Lev Dist
      l <- data.frame(Naam1 = Slave_names$Naam[l[,1]],
                      Naam2 = Slave_names$Naam[l[,2]])
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
    colnames(Slave_names_matched)[1:3] <- c("Naam", "Naam_variatie", "Naam_lv")
   #clean environment
    rm(l, l_Naam, LV_matrix, Slave_names, x)
    
    
  #Match mother names
   #unique names
    Slave_names <- Serie4[!duplicated(Serie4$Moeder) & Serie4$Moeder!="",]
   #produce matrix with Levenshtein distance
    LV_matrix <- stringdistmatrix(Slave_names$Moeder, Slave_names$Moeder, method = "lv")
   #Filter Levenshtein distance == x
    x <- 0
    repeat{
     #filter Lev Dist == x
      l <- as.data.frame(which(LV_matrix==x, arr.ind=TRUE))
     #list names + Lev Dist
      l <- data.frame(Naam1 = Slave_names$Moeder[l[,1]],
                      Naam2 = Slave_names$Moeder[l[,2]])
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
    colnames(Moeder_names_matched)[1:3] <- c("Moeder", "Moeder_variatie", "Moeder_lv")
   #clean environment
    rm(l, l_Naam, LV_matrix, Slave_names, x)
    
     
  #combine matches
   #find matches
    Serie4_matched <- merge(Serie4, Slave_names_matched, by="Naam", all=F)
    Serie4_matched <- merge(Serie4_matched, Moeder_names_matched, by="Moeder", all=F)
   #add source_order, in-event, out-event, & birth year
    Serie4_variatie <- Serie4
    colnames(Serie4_variatie) <- c("source_order_variatie", 
                                   "Naam_variatie", "Naam_number_variatie", 
                                   "Moeder_variatie", "Moeder_number_variatie", 
                                   "year_birth_variatie",
                                   "in_event_variatie", "out_event_variatie")
    Serie4_matched <- merge(Serie4_matched, Serie4_variatie, by=c("Naam_variatie", "Moeder_variatie"), all=F )
   #drop identical records
    Serie4_matched <- Serie4_matched[Serie4_matched$source_order!=Serie4_matched$source_order_variatie,]
   #make certain that certificates are possible
    Serie4_matched <- Serie4_matched[Serie4_matched$out_event_general!="Ended",]
    Serie4_matched <- Serie4_matched[Serie4_matched$in_event_variatie!="Beginning",]
    
  #add out & in date
   #filter mutations
    entryexit <- df[,c("source_order", "year_entry", "year_exit")]
   #add entry + exit original
    colnames(entryexit) <- c("source_order", "year_entry", "year_exit")
    Serie4_matched <- merge(Serie4_matched, entryexit, by="source_order", all=F)
   #add entry + exit variation
    colnames(entryexit) <- c("source_order_variatie", "year_entry_variatie", "year_exit_variatie")
    Serie4_matched <- merge(Serie4_matched, entryexit, by="source_order_variatie", all=F)
   #clear environment
    rm(entryexit)
    
  #order columns
    Serie4_matched <- Serie4_matched[,c("source_order", "source_order_variatie",
                                        "year_entry", "year_exit",
                                        "year_entry_variatie", "year_exit_variatie",
                                        "in_event_general", "out_event_general", "in_event_variatie", "out_event_variatie", 
                                        "Naam_lv", "Moeder_lv",  
                                        "Naam", "Naam_number", "Naam_variatie", "Naam_number_variatie",
                                        "Moeder", "Moeder_number", "Moeder_variatie", "Moeder_number_variatie",
                                        "year_birth", "year_birth_variatie")]
    
    
    
   #filter identical year
    #Serie4_matched <- Serie4_matched[Serie4_matched$year_birth==Serie4_matched$year_birth_variatie,]
    #Serie4_matched <- Serie4_matched[substr(Serie4_matched$year_birth,2,3)==substr(Serie4_matched$year_birth_variatie,2,3) |
    #                                   substr(Serie4_matched$year_birth,2,2)==substr(Serie4_matched$year_birth_variatie,2,2) & 
    #                                     substr(Serie4_matched$year_birth,4,4)==substr(Serie4_matched$year_birth_variatie,4,4) |
    #                                   substr(Serie4_matched$year_birth,3,4)==substr(Serie4_matched$year_birth_variatie,3,4),]

    
    
    write.xlsx(Serie4_matched, "U:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Matching/Within/4.xlsx")
    
    
    