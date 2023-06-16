split_names <- function(df1, varname){

  
  #filter all variables with a whitespace
  df2 <- df1[grepl(" ", df1[[varname]]), ]
  
  #drop all names with 1 characters
  df2[[varname]] <- gsub(" . ", "", df2[[varname]])
  df2[[varname]] <- ifelse(substr(df2[[varname]], 2, 2)==" ", substr(df2[[varname]], 3, nchar(df2[[varname]])), df2[[varname]])
  df2[[varname]] <- ifelse(substr(df2[[varname]], nchar(df2[[varname]])-1, nchar(df2[[varname]])-1)==" ", substr(df2[[varname]], 1, nchar(df2[[varname]])-2), df2[[varname]])
  
  #drop all names with 2 characters 
  df2[[varname]] <- gsub(" .. ", "", df2[[varname]])
  df2[[varname]] <- ifelse(substr(df2[[varname]], 3, 3)==" ", substr(df2[[varname]], 4, nchar(df2[[varname]])), df2[[varname]])
  
  #drop variables without whitespace
  df2 <- df2[grepl(" ", df2[[varname]]),]
  
  #split names
  df2 <- df2 %>% select("source_order", all_of(varname))
  df2 <- df2 %>% separate(varname, paste(rep(varname,4), 1:4, sep=""), " ")
  
  #drop names that differ 0 to 1 character
  df2[[paste(varname, 2, sep="")]] <- ifelse(is.na(df2[[paste(varname, 2, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 1, sep="")]], df2[[paste(varname, 2, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 2, sep="")]]))
  df2[[paste(varname, 3, sep="")]] <- ifelse(is.na(df2[[paste(varname, 1, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 1, sep="")]], df2[[paste(varname, 3, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 3, sep="")]]))
  df2[[paste(varname, 3, sep="")]] <- ifelse(is.na(df2[[paste(varname, 3, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 2, sep="")]], df2[[paste(varname, 3, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 3, sep="")]]))
  df2[[paste(varname, 4, sep="")]] <- ifelse(is.na(df2[[paste(varname, 4, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 1, sep="")]], df2[[paste(varname, 4, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 4, sep="")]]))
  df2[[paste(varname, 4, sep="")]] <- ifelse(is.na(df2[[paste(varname, 4, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 2, sep="")]], df2[[paste(varname, 4, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 4, sep="")]]))
  df2[[paste(varname, 4, sep="")]] <- ifelse(is.na(df2[[paste(varname, 4, sep="")]]), NA, 
                                             ifelse(stringdist(df2[[paste(varname, 3, sep="")]], df2[[paste(varname, 4, sep="")]])<=1, NA, 
                                                    df2[[paste(varname, 4, sep="")]]))
  
  #put data in long format
  #df2 <- melt(df2, id.vars="source_order", variable.name=varname, variable.factor=F, na.rm=T)
  df2 <- df2 %>% pivot_longer(!source_order, names_to = varname, values_to = "value", values_drop_na = TRUE)
  df2[[varname]] <- NULL
  
  #add variables
  df2 <- merge(df1,df2, by="source_order", all=F)
  df2[[varname]] <- df2$value
  df2$value <- NULL
  
  #bind data frames
  df1 <- rbind(df1, df2)
  
  #return df
  df1
}
