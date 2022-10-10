

#####################################################################
### Prepare end of Serie4 for matching with Emancipation register ###
#####################################################################

#load packages
library("data.table")
library("dplyr")
library("stringdist")
library("openxlsx")
library("tidyr")

#clean environment
rm(list=ls())

#set working directory
#Home
#setwd("~/HDS/Emancipatieregisters")
#Work
setwd("//cnas.ru.nl/U709207/Documents/Suriname/Emancipatieregisters")

#open data set
df <- read.delim("cleaned slave register 2022-06-20.txt", encoding="UTF-8")

#check data
as.data.frame(table(df$Typeregister))
as.data.frame(table(df$Serieregister))

################################
### 1 Filter end of Serie 4  ###
################################

serie4 <- df %>% filter (Serieregister == "1851-1863" & out_event == "End Series/Freedom") %>%

#Everybody in the filtered data set exits on 1 July 1863
#table(serie4$day_exit)
#table(serie4$month_exit)
#table(serie4$year_exit) 

###################################################################################################################
### 2 Replace plantations names for those that were identified as identical in the file "Identical_Plantations" ###
###################################################################################################################

  mutate(plantation_name = replace(plantation_name, plantation_name =="De Eendragt", "Eendragt"),
       plantation_name = replace(plantation_name, plantation_name =="Alkmaar ( voor het 1/2 aandeel aankomende den Boedel A. Ferrier )", "Alkmaar"),
       plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot", "Concordia"),
       plantation_name = replace(plantation_name, plantation_name =="Johanna Charlotte (1/2 aandeel mevrouw G. C. Henkel geboren Vogt)", "Rustenburg"),
       plantation_name = replace(plantation_name, plantation_name =="Waterwijk", "Hazard boven Commewijne"),
       plantation_name = replace(plantation_name, plantation_name =="Harmonie", "l'Aventure"),
       plantation_name = replace(plantation_name, plantation_name =="Lotland No 34", "Crappahoek nr. 34"),
       plantation_name = replace(plantation_name, plantation_name =="Concordia en Kwart Lot L. L.", "Concordia"))

  

#######################################################################################################################
### 3 Add information on plantation names from Emancipation Register for later comparison and export final document ###
#######################################################################################################################

emanc <- read.csv("Emancipatieregister_cleaned.csv") %>% 
  select(Place_name1) %>% 
  distinct() %>%
  filter(Place_name1 != "") %>%
  mutate(Place_name1_match = 1)


final_s4 <- left_join(serie4, emanc, by = c("plantation_name" = "Place_name1")) %>% 
  mutate(Place_name1_match = ifelse(is.na(Place_name1_match), 0, 1))


#Save

write.csv(final_s4, file = "Serie4_Exit.csv", row.names = FALSE)



