split_names <- function(df1, varname){
  #filter all variables with a whitespace
  df2 <- df1[grepl(" ", df1[[varname]]), ]
  
  #drop all names with 1 characters
  df2$Naam <- gsub(" . ", "", df2$Naam)
  df2$Naam <- ifelse(substr(df2$Naam, 2, 2)==" ", substr(df2$Naam, 3, nchar(df2$Naam)), df2$Naam)
  df2$Naam <- ifelse(substr(df2$Naam, nchar(df2$Naam)-1, nchar(df2$Naam)-1)==" ", substr(df2$Naam, 1, nchar(df2$Naam)-2), df2$Naam)
  
  #drop all names with 2 characters 
  df2$Naam <- gsub(" .. ", "", df2$Naam)
  df2$Naam <- ifelse(substr(df2$Naam, 3, 3)==" ", substr(df2$Naam, 4, nchar(df2$Naam)), df2$Naam)
  
  #drop variables without whitespace
  df2 <- df2[grepl(" ", df2$Naam),]
  
  #split names
  df2 <- df2[, c("source_order", "Naam")]
  df2 <- df2 %>% separate(Naam, c("Naam1", "Naam2", "Naam3", "Naam4"), " ")
  
  #drop names that differ 0 to 1 character
  df2$Naam2 <- ifelse(stringdist(df2$Naam1, df2$Naam2)<=1, NA, df2$Naam2)
  df2$Naam3 <- ifelse(stringdist(df2$Naam1, df2$Naam3)<=1, NA, df2$Naam3)
  df2$Naam4 <- ifelse(stringdist(df2$Naam1, df2$Naam4)<=1, NA, df2$Naam4)
  df2$Naam2 <- ifelse(stringdist(df2$Naam2, df2$Naam3)<=1, NA, df2$Naam3)
  
  #put data in long format
  df2 <- melt(df2, id.vars="source_order", variable.name="Naam", variable.factor=F, na.rm=T)
  df2$Naam <- NULL
  
  #add variables
  df2 <- merge(df,df2, by="source_order", all=F)
  df2$Naam <- df2$value
  df2$value <- NULL
  
  #bind data frames
  df1 <- rbind(df1, df2)
  
  #return df
  df1
}