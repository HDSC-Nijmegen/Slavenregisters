    
  ##########################################################
  #### section 1: load program to filter unique matches ####
  ##########################################################
    
    filter_unique <- function(df1, threshold, NUMMER1, NUMMER2){
      
      #make temporary variable to replace source_order in series 1 and 2
      df1$Order_1 <- df1[[paste("Source_order", NUMMER1, sep="_")]]
      df1$Order_2 <- df1[[paste("Source_order", NUMMER2, sep="_")]]
      #filter reliable matches
      Serie_unique <- df1[df1$Match_score>=threshold,]
      #filter best match from serie 1
       #prioritize matches on complete name for folios with multiple names
      
       #filter highest match score  
        Serie_unique <- Serie_unique %>% group_by(Order_1) %>% filter(is.na(Match_score) | Match_score==max(Match_score)) %>% arrange(Order_1) %>% ungroup()
      #filter best match from serie 2
       #prioritize matches on complete name for folios with multiple names
        
       #filter highest match score  
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
      Serie_unique %>% select(-c("Order_1", "Order_2"))
    }
    
    
  #############################################################
  #### section 2: group matches between consecutive series ####
  #############################################################
    
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
    reconstitution <- reconstitution[!(is.na(reconstitution$Source_order_1) & is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4)),]
   
   #clean environment
    rm(reconstitution1, reconstitution2)
    
    
  ##################################################################
  #### section 3: group matches between non-consecutive series ####
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
    rm(reconstitution1)
   #check for incongruencies
    length(which(reconstitution$Source_order_2==reconstitution$Source_order_24)); length(which(reconstitution$Source_order_2!=reconstitution$Source_order_24)) #9083 / 10 | 99.99% / 0.01%
    length(which(reconstitution$Source_order_1==reconstitution$Source_order_14)); length(which(reconstitution$Source_order_1!=reconstitution$Source_order_14)) #451 / 29 | 94.4% / 6.0%% 
    length(which(reconstitution$Source_order_1==reconstitution$Source_order_13)); length(which(reconstitution$Source_order_1!=reconstitution$Source_order_13)) #572 / 32 | 96.5% / 5.1%
   #check possible gains
    length(which(is.na(reconstitution$Source_order_2) & !is.na(reconstitution$Source_order_24))) #2181
    length(which(is.na(reconstitution$Source_order_1) & !is.na(reconstitution$Source_order_14))) #1491
    length(which(is.na(reconstitution$Source_order_1) & !is.na(reconstitution$Source_order_13))) #1004
   #add to indirect match to df
    reconstitution$Source_order_24 <- ifelse(is.na(reconstitution$Source_order_2), reconstitution$Source_order_24, NA)
    reconstitution$Source_order_14 <- ifelse(is.na(reconstitution$Source_order_1), reconstitution$Source_order_14, NA)
    reconstitution$Source_order_13 <- ifelse(is.na(reconstitution$Source_order_1), reconstitution$Source_order_13, NA)
    #2-4
    x <- reconstitution[is.na(reconstitution$Source_order_1) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4),"Source_order_2"]
    x <- x[x %in% reconstitution$Source_order_24]
    reconstitution <- reconstitution[!(reconstitution$Source_order_2 %in% x),]
    reconstitution$Source_order_2 <- ifelse(is.na(reconstitution$Source_order_2) & reconstitution$Source_order_24 %in% x, reconstitution$Source_order_24, reconstitution$Source_order_2)
    #1-4
    x <- reconstitution[is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4),"Source_order_1"]
    x <- x[x %in% reconstitution$Source_order_14]
    reconstitution <- reconstitution[!(reconstitution$Source_order_1 %in% x),]
    reconstitution$Source_order_1 <- ifelse(is.na(reconstitution$Source_order_1) & reconstitution$Source_order_14 %in% x, reconstitution$Source_order_14, reconstitution$Source_order_1)
    #1-3
    x <- reconstitution[is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4),"Source_order_1"]
    x <- x[x %in% reconstitution$Source_order_13]
    reconstitution <- reconstitution[!(reconstitution$Source_order_1 %in% x),]
    reconstitution$Source_order_1 <- ifelse(is.na(reconstitution$Source_order_1) & reconstitution$Source_order_13 %in% x, reconstitution$Source_order_13, reconstitution$Source_order_1)
   #reorder
    reconstitution <- reconstitution[,c("Source_order_1", "Source_order_2", "Source_order_3", "Source_order_4")]
    
    
  ################################################
  #### section 4: group matches WITHIN series ####
  ################################################
    
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
      
      #filter unique unmatched cases
      df1_unlinked <- df1[is.na(df1$Source_order_1) | is.na(df1$Source_order_2), c("Source_order_1", "Source_order_2")]
      df1_unlinked$Source_order_1 <- ifelse(is.na(df1_unlinked$Source_order_1), df1_unlinked$Source_order_2, df1_unlinked$Source_order_1)
      df1_unlinked$Source_order_2 <- ifelse(df1_unlinked$Source_order_1==df1_unlinked$Source_order_2, NA, df1_unlinked$Source_order_2)
      df1_unlinked <- df1_unlinked[!duplicated(df1_unlinked$Source_order_1) & !is.na(df1_unlinked$Source_order_1), ]
      df1_unlinked <- df1_unlinked[!(df1_unlinked$Source_order_1 %in% df1_linked$Source_order_7 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_6 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_5 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_4 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_3 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_2 |
                                       df1_unlinked$Source_order_1 %in% df1_linked$Source_order_1), ]
      #add rows
      df1_unlinked$Source_order_7 <- df1_unlinked$Source_order_6 <- df1_unlinked$Source_order_5 <- df1_unlinked$Source_order_4 <- 
        df1_unlinked$Source_order_3 <- df1_unlinked$Source_order_2 <- NA
      #bind
      df1_linked <- rbind(df1_linked, df1_unlinked)
      #remove overlap
      #print
      df1_linked
    }
    
    Serie44_linked <- group_within(Serie44)
    Serie33_linked <- group_within(Serie33)
    Serie22_linked <- group_within(Serie22)
    Serie11_linked <- group_within(Serie11)
    
  
  #########################################################################
  #### section 5: add grouped WITHIN matches to BETWEEN reconstitution ####
  #########################################################################
    
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
      df3 <- rbind(reconstitution1, reconstitution2)
      df3
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
  #drop singleton BETWEEN matches that also occur in WITHIN
    reconstitution <- reconstitution[!(is.na(reconstitution$Source_order_1) & is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_44_2) & 
                                         reconstitution$Source_order_4 %in% c(reconstitution$Source_order_44_2, reconstitution$Source_order_44_3, reconstitution$Source_order_44_4, reconstitution$Source_order_44_5, reconstitution$Source_order_44_6, reconstitution$Source_order_44_7)),]
    
  #3-3
  #match to next series
    #mark last entry
    Serie33_linked$LastEntry <- mark_lastentry(Serie33_linked)
    #rename grouped matches
    colnames(Serie33_linked) <- c("Source_order_33_1", "Source_order_33_2", "Source_order_33_3", "Source_order_33_4", "Source_order_33_5", 
                                  "Source_order_33_6", "Source_order_33_7", "Source_order_4")
    #add grouped matches to Series 4
    reconstitution <- add_within(Serie33_linked, reconstitution, "Source_order_4")
  #drop singleton BETWEEN matches that also occur in WITHIN
    reconstitution <- reconstitution[!(is.na(reconstitution$Source_order_1) & is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_4) & is.na(reconstitution$Source_order_33_1) & 
                                         reconstitution$Source_order_3 %in% c(reconstitution$Source_order_33_1, reconstitution$Source_order_33_2, reconstitution$Source_order_33_3, reconstitution$Source_order_33_4, reconstitution$Source_order_33_5, reconstitution$Source_order_33_6, reconstitution$Source_order_33_7)),]
  #join to preceding series
    x <- reconstitution[reconstitution$Source_order_3 %in% reconstitution$Source_order_33_1 & !is.na(reconstitution$Source_order_3),]
    x <- x[,which(grepl("33", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_33_1 %in% reconstitution$Source_order_3 & !is.na(reconstitution$Source_order_33_1),]
    y <- y[,which(grepl("33", colnames(y)))]
    y$Source_order_3 <- y$Source_order_33_1
    x <- merge(x, y, by="Source_order_3", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_33_1 %in% x$Source_order_33_1 &
                                         reconstitution$Source_order_3 %in% x$Source_order_3),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_33_1 <- ifelse(is.na(reconstitution$Source_order_3), reconstitution$Source_order_33_1,
                                               ifelse(reconstitution$Source_order_33_1==reconstitution$Source_order_3, NA, reconstitution$Source_order_33_1))
    
  #2-2
  #match to next series
    #mark last entry
    Serie22_linked$LastEntry <- mark_lastentry(Serie22_linked)
    #rename grouped matches
    colnames(Serie22_linked) <- c("Source_order_22_1", "Source_order_22_2", "Source_order_22_3", "Source_order_22_4", "Source_order_22_5", 
                                  "Source_order_22_6", "Source_order_22_7", "Source_order_3")
    #add grouped matches to Series 3
    reconstitution <- add_within(Serie22_linked, reconstitution, "Source_order_3")
  #drop singleton BETWEEN matches that also occur in WITHIN
    reconstitution <- reconstitution[!(is.na(reconstitution$Source_order_1) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4) & is.na(reconstitution$Source_order_33_1) & is.na(reconstitution$Source_order_22_1) & 
                                         reconstitution$Source_order_2 %in% c(reconstitution$Source_order_22_1, reconstitution$Source_order_22_2, reconstitution$Source_order_22_3, reconstitution$Source_order_22_4, reconstitution$Source_order_22_5, reconstitution$Source_order_22_6, reconstitution$Source_order_22_7)),]
  #join to preceding series
    x <- reconstitution[reconstitution$Source_order_2 %in% reconstitution$Source_order_22_1 & !is.na(reconstitution$Source_order_2),]
    x <- x[,which(grepl("22", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_22_1 %in% reconstitution$Source_order_2 & !is.na(reconstitution$Source_order_22_1),]
    y <- y[,which(grepl("22", colnames(y)))]
    y$Source_order_2 <- y$Source_order_22_1
    x <- merge(x, y, by="Source_order_2", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_22_1 %in% x$Source_order_22_1 & 
                                         reconstitution$Source_order_2 %in% x$Source_order_2),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_22_1 <- ifelse(is.na(reconstitution$Source_order_2), reconstitution$Source_order_22_1,
                                               ifelse(reconstitution$Source_order_22_1==reconstitution$Source_order_2, NA, reconstitution$Source_order_22_1))
    
  #1-1
  #match to next series
    #mark last entry
    Serie11_linked$LastEntry <- mark_lastentry(Serie11_linked)
    #rename grouped matches
    colnames(Serie11_linked) <- c("Source_order_11_1", "Source_order_11_2", "Source_order_11_3", "Source_order_11_4", "Source_order_11_5", 
                                  "Source_order_11_6", "Source_order_11_7", "Source_order_2")
    #add grouped matches to Series 2
    reconstitution <- add_within(Serie11_linked, reconstitution, "Source_order_2")
    #drop singleton BETWEEN matches that also occur in WITHIN
    reconstitution <- reconstitution[!(is.na(reconstitution$Source_order_2) & is.na(reconstitution$Source_order_3) & is.na(reconstitution$Source_order_4) & is.na(reconstitution$Source_order_33_1) & is.na(reconstitution$Source_order_22_1) & is.na(reconstitution$Source_order_11_1) & 
                                         reconstitution$Source_order_1 %in% c(reconstitution$Source_order_11_1, reconstitution$Source_order_11_2, reconstitution$Source_order_11_3, reconstitution$Source_order_11_4, reconstitution$Source_order_11_5, reconstitution$Source_order_11_6, reconstitution$Source_order_11_7)),]
  #join to preceding series
    x <- reconstitution[reconstitution$Source_order_1 %in% reconstitution$Source_order_11_1 & !is.na(reconstitution$Source_order_1),]
    x <- x[,which(grepl("11", colnames(x))==F)]
    y <- reconstitution[reconstitution$Source_order_11_1 %in% reconstitution$Source_order_1 & !is.na(reconstitution$Source_order_11_1),]
    y <- y[,which(grepl("11", colnames(y)))]
    y$Source_order_1 <- y$Source_order_11_1
    x <- merge(x, y, by="Source_order_1", all=F)
    x <- cbind(x[,c("Source_order_4", "Source_order_3", "Source_order_2", "Source_order_1")], x[,5:length(x)])
    reconstitution <- reconstitution[!(reconstitution$Source_order_11_1 %in% x$Source_order_11_1 &
                                         reconstitution$Source_order_1 %in% x$Source_order_1),]
    reconstitution <- rbind(reconstitution, x)
    reconstitution$Source_order_11_1 <- ifelse(is.na(reconstitution$Source_order_1), reconstitution$Source_order_11_1,
                                               ifelse(reconstitution$Source_order_11_1==reconstitution$Source_order_1, NA, reconstitution$Source_order_11_1))
    
    
  #clean environment
    rm(x,y)
    
    
    
    
    
    
    
    
