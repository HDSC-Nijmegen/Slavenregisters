
  #install packages
  #install.packages("dplyr"); install.packages("data.table"); install.packages("stringdist"); install.packages("openxlsx")
  #load packages
  library("dplyr"); library("data.table"); library("stringdist"); library("openxlsx")
  
  #clean environment
  rm(list=ls())
  
  #set working directory
  setwd("C:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/All Datawork")
  
  #load files
  Serie1 <- read.xlsx("Serie1/Serie1_V1402_source_order.xlsx")
  Serie2 <- read.xlsx("Serie2/Serie_2_updated_04_09_2019.xlsx")
  Serie3 <- read.xlsx("Serie3/Serie3_updated_04_09_2019.xlsx")
  Serie4 <- read.xlsx("Serie4/Serie4_Updated_04_09_2019.xlsx")
  
  #fix column names
  colnames(Serie1)[colnames(Serie1)=="Type.register"] <- "Typeregister"
  colnames(Serie1)[colnames(Serie1)=="Serie.register"] <- "Serieregister"
  Serie4$Eigenaar_test <- NULL
  
  #Watermark series
  Serie1$Serie <- 1
  Serie2$Serie <- 1
  Serie3$Serie <- 1
  Serie4$Serie <- 1
  
  #filter prive-eigenaren
  Index <- rbind(Serie1[which(Serie1$Typeregister=="Particulieren"),c("Eigenaar", "Serieregister", "Folionummer")],
                 Serie2[which(Serie2$Typeregister=="Particulieren"),c("Eigenaar", "Serieregister", "Folionummer")],
                 Serie3[which(Serie3$Typeregister=="Particulieren"),c("Eigenaar", "Serieregister", "Folionummer")],
                 Serie4[which(Serie4$Typeregister=="Particulieren"),c("Eigenaar", "Serieregister", "Folionummer")]  )

  #deduplicate Eigenaar
  Index <- Index[which(!duplicated(Index[,c("Eigenaar", "Serieregister")])),]
  
  
  #####################################################################
  ###   remove dots and redundant whitespaces + set to lower case   ###
  #####################################################################
  
  #check number of unique name entries
    length(Index$Eigenaar) #8,355
   #check number of unique name entries after:
    #remove double whitespace
    length(which(!duplicated(gsub("  ", " ", Index$Eigenaar)))) #7,498
    #remove leading whitespace
    length(which(!duplicated(trimws(Index$Eigenaar, "left")))) #7,498
    #remove trailing whitespace
    length(which(!duplicated(trimws(Index$Eigenaar, "right")))) #7,316
    #remove dots
    length(which(!duplicated(gsub("\\.", "", Index$Eigenaar)))) #7,202
    #delete all whitespaces
    length(which(!duplicated(gsub(" ", "", Index$Eigenaar)))) #7,245
    #set to lower case
    length(which(!duplicated(tolower(Index$Eigenaar)))) #7,415
    
  #set to lower & delete dots, double whitespace, and leading and trailing whitespace
    Index$Eigenaar <- tolower(Index$Eigenaar)
    Index$Eigenaar <- gsub("\\.", " ", Index$Eigenaar)
    Index$Eigenaar <- gsub("  ", " ", Index$Eigenaar)
    Index$Eigenaar <- gsub("  ", " ", Index$Eigenaar)
    Index$Eigenaar <- trimws(Index$Eigenaar, "both")
    Index <- Index[which(!duplicated(Index$Eigenaar)),]
    length(Index$Eigenaar) #6,700
    
    
    
  ###########################
  ###   Split last name   ###
  ###########################
  
  #fix mac to name
    Index[which(substr(Index$Eigenaar,1,4)=="mac "),"Eigenaar"]
    Index[which(substr(Index$Eigenaar,1,3)=="mc "),"Eigenaar"]
    Index$Eigenaar <- ifelse(substr(Index$Eigenaar,1,4)=="mac ", paste0(substr(Index$Eigenaar,1,3), substr(Index$Eigenaar,5,nchar(Index$Eigenaar))), Index$Eigenaar)
    Index$Eigenaar <- ifelse(substr(Index$Eigenaar,1,3)=="mc ", paste0(substr(Index$Eigenaar,1,2), substr(Index$Eigenaar,4,nchar(Index$Eigenaar))), Index$Eigenaar)
    
  #separate last name
    Index$Last_name <- sub(" .*", "", Index$Eigenaar)
    Index$temp <- sub(".*? ", "", Index$Eigenaar)
  
  #make variables
    Index$Postfix <- NA
    Index$Patroniem <- NA
    Index$Person_description <- NA
    Index$Relation_description <- NA
    Index$Birth_name <- NA
    
    
    
  #filter rechtspersonen
   #set function
    rm_stichting <- function(combinatie){
      Index$Last_name <<- ifelse(grepl(combinatie, Index$Eigenaar), Index$Eigenaar, Index$Last_name)
      Index$Person_description <<- ifelse(grepl(combinatie, Index$Eigenaar), "rechtspersoon", Index$Person_description)
      Index$temp <<- ifelse(grepl(combinatie, Index$Eigenaar), "", Index$temp)
    }
   #key words: en co, gemeente, grond, firma, en zonen
    Index[grepl(" en co", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl(" bank", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("firma", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("gemeente", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("lands grond", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("maatschappij", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("raad", Index$Eigenaar),c("Eigenaar", "temp", "Person_description")]
   #firma
    Index$temp <- gsub("de geexisteerd hebbende firma van", "", Index$temp)
   #en co
    Index$Last_name <- ifelse(grepl(" en co", Index$Eigenaar), paste(sub(" en co.*", "", Index$Eigenaar), "en co"), Index$Last_name)
    Index$Person_description <- ifelse(grepl(" en co", Index$Eigenaar), "rechtspersoon", Index$Person_description)
    Index$temp <- ifelse(grepl(" en co", Index$Eigenaar), trimws(sub(".*en co", "", Index$temp),"both"), Index$temp)
   #bank
    rm_stichting(" bank")
   #gemeente, grond
    rm_stichting("gemeente")
    rm_stichting("lands grond")
   #maatschappij
    rm_stichting("maatschappij")
   #raad
    rm_stichting(" raad")
   #fonds
    Index[grepl("fonds", Index$temp),c("Eigenaar", "temp", "Person_description")]
    #'t fonds w g deutz
    Index$Person_description <- ifelse(grepl("fonds", Index$temp) & grepl("deutz", Index$Eigenaar), "rechtspersoon", Index$Person_description)
    Index$Last_name <- ifelse(grepl("fonds", Index$temp) & grepl("deutz", Index$Eigenaar), "'t fonds w g deutz", Index$Last_name)
    Index$temp <- ifelse(grepl("fonds", Index$temp) & grepl("deutz", Index$Eigenaar), "", Index$temp)
    #g a de graaf
    Index$Person_description <- ifelse(grepl("fonds", Index$temp) & grepl("graaf", Index$Eigenaar), "rechtspersoon", Index$Person_description)
    Index$Last_name <- ifelse(grepl("fonds", Index$temp) & grepl("graaf", Index$Eigenaar), "'t fonds g a de graaf", Index$Last_name)
    Index$temp <- ifelse(grepl("fonds", Index$temp) & grepl("graaf", Index$Eigenaar), "", Index$temp)
    #insinger en co privé en qq en j j van poll qq 't fonds onder administratie van
    Index$Person_description <- ifelse(grepl("fonds", Index$temp) & grepl("insinger", Index$Eigenaar), "rechtspersoon", Index$Person_description)
    Index$Last_name <- ifelse(grepl("fonds", Index$temp) & grepl("insinger", Index$Eigenaar), "'t fonds onder administratie van insinger en co", Index$Last_name)
    Index$temp <- ifelse(grepl("fonds", Index$temp) & grepl("insinger", Index$Eigenaar), "", Index$temp)
   #de erven mr becker en spiering
    Index$Last_name[Index$Eigenaar=="becker en spiering de erven mr"] <- "de erven mr becker en spiering"
    Index$temp[Index$Eigenaar=="becker en spiering de erven mr"] <- ""
    Index$Person_description[Index$Eigenaar=="becker en spiering de erven mr"] <- "rechtspersoon"
   #van west en co
    Index$Last_name[Index$Eigenaar=="west en co van"] <- "van west en co"
    Index$temp[Index$Eigenaar=="west en co van"] <- ""
    Index$Person_description[Index$Eigenaar=="west en co van"] <- "rechtspersoon"
    
    
    
  #separate prefixes
   #make variable
    Index$Prefix <- NA
   #set function
    rm_prefix <- function(combinatie){
      Index$Prefix <<- ifelse(substr(Index$temp, nchar(Index$temp)-nchar(combinatie), nchar(Index$temp))==paste0(" ", combinatie), #if last 6 characters are whitespace + combination
                              substr(Index$temp, nchar(Index$temp)-nchar(combinatie)+1, nchar(Index$temp)), Index$Prefix) #save last 5 characters as prefix
      Index$temp <<- ifelse(substr(Index$temp, nchar(Index$temp)-nchar(combinatie), nchar(Index$temp))==paste0(" ", combinatie), #if last 6 characters are whitespace + combination
                            substr(Index$temp, 1, nchar(Index$temp)-nchar(combinatie)-1), Index$temp) #drop last 5 characters + leading whitespace from temp
      }
   #explore
    Index[which(substr(Index$temp, nchar(Index$temp)-5, nchar(Index$temp))==" bo de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-8, nchar(Index$temp))==" bueno de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" da"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" de"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" du"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" d'"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" der"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" des"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" het"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" 't"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" la"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" le"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-2, nchar(Index$temp))==" l'"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" ter"),"temp"]
    Index[which(substr(Index$temp, nchar(Index$temp)-3, nchar(Index$temp))==" van"),"temp"]
   #replace
    Index$temp <- gsub(" bo de", " bueno de", Index$temp)
    rm_prefix("bueno de")
    rm_prefix("d'")
    rm_prefix("da")
    rm_prefix("de")
    rm_prefix("des")
    rm_prefix("de la")
    rm_prefix("d' la")
    rm_prefix("de l'")
    rm_prefix("du")
    rm_prefix("l'")
    rm_prefix("la")
    rm_prefix("le")
    rm_prefix("ter")
    rm_prefix("van van")
    rm_prefix("van")
    rm_prefix("van de")
    rm_prefix("van du")
    rm_prefix("van den")
    rm_prefix("van der")
    rm_prefix("van het")
    rm_prefix("van 't")
    rm_prefix("van la")
    
    
    
  #Postfix
   #set function
    rm_Postfix <- function(combinatie, output){
      Index$Postfix <<- ifelse(grepl(combinatie, Index$temp), output, Index$Postfix)
      Index$temp <<- gsub(combinatie, "", Index$temp)
    }
   #sr
    Index[grepl("sr", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("senior", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_Postfix("sr ", "senior")
    rm_Postfix("senior ", "senior")
   #jr
    Index[grepl("jr ", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("junior", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_Postfix("jr ", "junior")
    rm_Postfix("junior ", "junior")
   #mr
    Index[grepl("mr ", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("meester ", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_Postfix("mr ", "mr")
    
    
    
  #Patroniemen 
   #set function
    rm_Patroniem <- function(combinatie){
      Index$Patroniem <<- ifelse(grepl(combinatie, Index$temp), trimws(combinatie), Index$Patroniem)
      Index$temp <<- gsub(combinatie, "", Index$temp)
    }
   #patronen
    Index[grepl("sz", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("szoon", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("zn", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_Patroniem("robleszoon ")
    rm_Patroniem("thomaszoon ")
    rm_Patroniem("fczn ")
    rm_Patroniem("czn ")
    rm_Patroniem("hzn ")
    rm_Patroniem("ezn ")
    rm_Patroniem("azn ")
    rm_Patroniem("szn ")
    rm_Patroniem("csz ")
    rm_Patroniem("jos zn ")
    rm_Patroniem("j g zn ")
    
    
    
  #personal information
   #set leading "en " to " en "
    Index$temp <- ifelse(substr(Index$temp,1,3)=="en ", paste0(" ", Index$temp), Index$temp)
   #set function
    rm_person <- function(combinatie, output){
      Index$Person_description <<- ifelse(grepl(combinatie, Index$temp), output, Index$Person_description)
      Index$temp <<- gsub(combinatie, "", Index$temp)
    }
    rm_person2 <- function(combinatie1, combinatie2, output){
      Index$Person_description <<- ifelse(grepl(combinatie1, Index$temp) & grepl(combinatie2, Index$temp)==F, output, Index$Person_description)
      Index$temp <<- ifelse(grepl(combinatie1, Index$temp) & grepl(combinatie2, Index$temp)==F, gsub(combinatie1, "", Index$temp), Index$temp)
    }
   #straatvoogd
    Index[grepl("straatvoogdes", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index[grepl("straatvoogd", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_person(", straatvoogdesse", "straatvoogd")
    rm_person(", straatvoogdesje", "straatvoogd")
    rm_person(", straatvoogdes", "straatvoogd")
    rm_person(", straatvoogd", "straatvoogd")
    rm_person("straatvoogdesse", "straatvoogd")
    rm_person("straatvoogdesje", "straatvoogd")
    rm_person("straatvoogdes", "straatvoogd")
    rm_person("straatvoogd", "straatvoogd")
   #curator
    Index[grepl("curator", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_person(", curator ad hoc", "curator ad hoc")
    rm_person("curator ad hoc", "curator ad hoc")
   #boedel
    Index[grepl("boedel", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index$temp <- gsub("- nu boedel", "boedel", Index$temp)
    Index$temp <- gsub("nu boedel", "boedel", Index$temp)
    rm_person("gemeenschappelijke boedel ", "boedel")
    rm_person("de boedel ", "boedel")
    rm_person("den boedel ", "boedel")
    rm_person2("boedel", " en ", "boedel")
   #wijlen
    Index[grepl("wijlen", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index$temp <- gsub("wijlen ", "", Index$temp)
   #uitlandige
    Index[grepl("uitlandige", Index$temp),c("Eigenaar", "temp", "Person_description")]
    rm_person("uitlandige ", "uitlandige")
   #te
    Index[grepl("te ", Index$temp),c("Eigenaar", "temp", "Person_description")]
   #amsterdam
    rm_person(", te amsterdam", "uitlandige")
    rm_person(" te amsterdam", "uitlandige")
    rm_person("te amsterdam ", "uitlandige")
   #rotterdam
    rm_person(" te rotterdam", "uitlandige")
   #curaçao
    rm_person("te curaçao", "uitlandige")
   #de vrije
    Index[grepl("de vrije", Index$temp),c("Eigenaar", "temp", "Person_description")]
    Index$temp <- gsub("de vrije ", "", Index$temp)
    
    
  #relation information
   #set function
    rm_relation <- function(combinatie, output){
      Index$Relation_description <<- ifelse(grepl(combinatie, Index$temp), output, Index$Relation_description)
      Index$temp <<- gsub(combinatie, "", Index$temp)
    }
    rm_relation2 <- function(combinatie1, combinatie2, output){
      Index$Relation_description <<- ifelse(grepl(combinatie1, Index$temp) & grepl(combinatie2, Index$temp)==F, output, Index$Relation_description)
      Index$temp <<- ifelse(grepl(combinatie1, Index$temp) & grepl(combinatie2, Index$temp)==F, gsub(combinatie1, "", Index$temp), Index$temp)
    }
   #multiple persons
    Index$Relation_description <- ifelse(grepl("/", Index$temp), "multiple persons", Index$Relation_description)
   #de erven
    Index[grepl("erven", Index$temp),c("Eigenaar", "temp", "Relation_description")]
    rm_relation2("de erven ", " en ", "erven")
    rm_relation2("erven ", " en ", "erven")
   #erfgenamen
    Index[grepl("erfgenamen", Index$temp),c("Eigenaar", "temp", "Relation_description")]
    rm_relation("de gezamenlijke erfgenamen van", "erven")
   #qq
    Index$temp <- gsub("q q ", "qq ", Index$temp)
    Index[grepl("qq", Index$temp),c("Eigenaar", "temp", "Relation_description")]
   #prive & nom ux
    Index[grepl("privé", Index$temp),c("Eigenaar", "temp", "Relation_description")]
    rm_relation(", privé en nom ux", "privé en nom ux")
    rm_relation(", privé en n ux", "privé en nom ux")
    rm_relation2("privé en nom ux ", "erven", "privé en nom ux")
    rm_relation2("privé en nom uxoris ", "erven", "privé en nom ux")
    rm_relation2(" privé en nom ux", "erven", "privé en nom ux")
    rm_relation("privé nom ux ", "privé en nom ux")
    rm_relation("privé et nom ux ", "privé en nom ux")
    rm_relation(" privé et nom ux", "privé en nom ux")
    rm_relation("privé en noms ux ", "privé en nom ux")
    rm_relation("privé en n ux ", "privé en nom ux")
    rm_relation("pr en nom ux ", "privé en nom ux")
    rm_relation("nom ux en privé ", "privé en nom ux")
    #prive & minderjarige 
    rm_relation("privé en hare minderjarige dochter ", "privé en minderjarige")
    rm_relation("privé en qq minderjarige zoon", "privé en minderjarige")
    rm_relation("privé en minderjarige zoon ", "privé en minderjarige")
    #prive, nom ux & kinderen
    rm_relation("privé, nom ux en kinderen ", "privé, nom ux en kinderen")
    rm_relation("privé, nom ux en voor zijne minderjarige dochter ", "privé, nom ux en minderjarige")
    #prive
    Index$temp <- gsub(", prive", "", Index$temp)
   #nom ux
    Index[grepl("ux", Index$temp),c("Eigenaar", "temp", "Relation_description")]
    Index$temp <- gsub("nux", "nom ux", Index$temp)
    Index$temp <- gsub("n ux", "nom ux", Index$temp)
    Index$temp <- gsub("n u x", "nom ux", Index$temp)
    Index$temp <- gsub("num ux", "nom ux", Index$temp)
    rm_relation(", en nom ux", "privé en nom ux")
    rm_relation(" en nom ux , ", "privé en nom ux")
    rm_relation2(" en nom ux ", "erven", "privé en nom ux")
    rm_relation2(" en nom ux", "erven", "privé en nom ux")
    Index$Relation_description <- ifelse(grepl("nom ux", Index$temp) & grepl(" en ", Index$temp), "multiple persons", Index$Relation_description)
    rm_relation(", nom ux", "nom ux")
    rm_relation("nom uxs ", "nom ux")
    rm_relation("nom ur ", "nom ux")
    rm_relation("nom uxoris ", "nom ux")
    rm_relation2("nom ux ", " en ", "nom ux")
    rm_relation2(" nom ux", " en ", "nom ux")
   #minderjarige / kinderen van
    Index[grepl("mind", Index$temp),c("Eigenaar", "temp", "Relation_description")]
    rm_relation("de minderjarige kinderen van ", "kinderen van")
    rm_relation("minderjarige kinderen van ", "kinderen van")
    rm_relation(", voor hare minderjarige kinderen", "kinderen van")
    rm_relation(" voor hare minderjarige kinderen", "kinderen van")
    rm_relation("voor hare minderjarige kinderen ", "kinderen van")
    rm_relation(", voor zijne minderjarige kinderen", "kinderen van")
    rm_relation(" voor zijne minderjarige kinderen", "kinderen van")
    rm_relation("voor zijne minderjarige kinderen ", "kinderen van")
    Index$Relation_description <- ifelse(grepl("en de minderjarige", Index$temp), "multiple persons", Index$Relation_description)
    rm_relation("voor de minderjarigen ", "kinderen van")
    rm_relation("voor den minderjarige ", "kinderen van")
    rm_relation(" en minderjarige dochter", "privé en minderjarige dochter")
    rm_relation(", voor hare minderjarige dochters", "kinderen van")
    rm_relation("zijne minderjarige dochter", "kinderen van")
    rm_relation(" voor hare minderjarige dochter", "kinderen van")
    rm_relation(", voor haren minderjarige zoon", "kinderen van")
    rm_relation(" voor zijnen minderjarige zoon", "kinderen van")
    rm_relation(" voor de minderjarige kinderen", "kinderen van")
    rm_relation("en de door p e goede nog te verwekken kinderen, de minderjarigen ", "kinderen van")
    rm_relation("en de minderjarigen ", "privé en minderjarige")
    rm_person("de minderjarigen ", "minderjarige")
    rm_relation(" voor zijnen minderjarige zoon", "kinderen van")
    rm_relation(" voor haren minderjarigen zoon", "kinderen van")
    rm_relation(" voor zijn minderjarige kind const alex en verder door hem in huwelijk te verwekkene kinderen", "kinderen van")
    rm_relation(" privé en qq minderjarige zoon", "privé en minderjarige")
    rm_relation(", en hare minderjarige kinderen", "privé en minderjarige")
    rm_relation("voor deszelfs minderjarige kinderen", "kinderen van")
    Index$Relation_description <- ifelse(grepl("en de minderjarige", Index$temp), "privé en minderjarige", Index$Relation_description)
    Index$Person_description <- ifelse(grepl("de minderjarige", Index$temp) & grepl("en de minderjarige", Index$temp)==F, "minderjarige", Index$Person_description)
    Index$temp <- gsub("de minderjarige", "", Index$temp)
    rm_person("monderjarige ", "minderjarige")
    rm_relation("en minderjarigen ", "privé en minderjarige")
    rm_relation("qq minderjarigen ", "kinderen van")
    rm_relation("qq minderjarige ", "kinderen van")
    rm_person("minderjarigen ", "minderjarige")
    rm_person("minderj ", "minderjarige")
    rm_person("minderjrige ", "minderjarige")
    rm_person("minderjarige ", "minderjarige")
   #weduwe
    Index[grepl("wed", Index$temp),"temp"]
    Index$Relation_description <- ifelse(grepl("de weduwe", Index$temp) & is.na(Index$Relation_description), "weduwe", Index$Relation_description)
    Index$temp <- ifelse(grepl("de weduwe ", Index$temp) & Index$Relation_description!="multiple persons", gsub("de weduwe ", "", Index$temp), Index$temp)
    Index$temp <- ifelse(grepl(" de weduwe", Index$temp) & Index$Relation_description!="multiple persons", gsub(" de weduwe", "", Index$temp), Index$temp)
    Index$temp <- gsub("ezn", "", Index$temp)
    rm_relation(" weduwe", "weduwe")
    Index$Relation_description <- ifelse(grepl("weduwe", Index$temp) & Index$Relation_description=="minderjarige kinderen", "weduwe voor haar minderjarige kinderen", Index$Relation_description)
    Index$Relation_description <- ifelse(grepl("weduwe", Index$temp) & Index$Relation_description=="nom ux", "weduwe & nom ux", Index$Relation_description)
    Index$temp <- ifelse(is.na(Index$Relation_description), Index$temp, gsub("weduwe ", "", Index$temp))
    rm_relation("weduwe ", "weduwe")
    rm_relation("weduew ", "weduwe")
    rm_relation("wed ", "weduwe")
   #douariere van
    Index[grepl("doua", Index$temp),"temp"]
    rm_relation(", douariere", "weduwe")
    rm_relation(" douariere", "weduwe")
   #kinderen
    Index[grepl("kind", Index$temp),"temp"]
    rm_relation(" voor hare reeds verwekte en nog te verwekken kinderen", "kinderen van")
    rm_relation("zijn kind ", "kinderen van")
    rm_relation(" en verder door hem in huwelijk te verwekkene kinderen", "kinderen van")
    rm_relation(" voor zijn reeds hebbende en nog te verwekkene kinderen bij zijne huisvrouw i f thijm", "kinderen van")
    rm_relation(" hare reeds hebbende en nog te verwekken kinderen met name", "kinderen van")
    rm_relation(" en voor hare reeds hebbende en nog te verwekken kinderen", "kinderen van")
    rm_relation(" voor hare reeds hebbende en nog te verwekken kinderen", "kinderen van")
    rm_relation(" , voor zijne reeds hebbende en nog te verwekkene kinderen bij jula frederika straub geboren steinbach", "kinderen van")
    rm_relation(", en hare kinderen", "kinderen van")
    rm_relation(" , en de door j m a salomons bij zijne huisvrouw a j salomons geboren lionarons nader te verwekkene kinderen", "kinderen van")
    rm_relation(" , voor zijne reeds hebbende en nog te verwekken kinderen", "kinderen van")
    rm_relation(", voor hare reeds hebbende en nog te verwekkene kinderen", "kinderen van")
    rm_relation(" voor deszelfs kinderen", "kinderen van")
    rm_relation(" en nog te verwekken kinderen, bij zijne echtgenote s sarqui daniel jessurun", "kinderen van")
    rm_relation("en de door p e goede nog te verwekken kinderen, ", "kinderen van")
    rm_relation(" voor hare reeds hebbende en nog te verwekkene kinderen", "kinderen van")
    rm_relation("voor haar reeds hebbende en nog te verwekkene kinderen ", "kinderen van")
    rm_relation("voor zijne reeds hebbende en nog te verwekkene kinderen ", "kinderen van")
    rm_relation("voor hare hebbende kinderen ", "kinderen van")
    rm_relation(" en nog te verwekken kinderen", "kinderen van")
    rm_relation(" en kinderen", "privé en minderjarige")
    rm_relation("kinderen van ", "kinderen")
    rm_relation(" voor kinderen", "kinderen")
    Index$Relation_description <- ifelse(grepl("nom ux", Index$temp) & grepl("kind", Index$temp), "nom ux en kinderen", Index$Relation_description)
    
    
    
  #voormalige naam
    Index[which(grepl("geb", Index$temp) & grepl("geboren", Index$temp)==F),"temp"]
    Index$temp <- gsub("geb ", "geboren ", Index$temp)
    Index$temp <- gsub("gebr ", "geboren ", Index$temp)
    Index$temp <- gsub("gebs ", "geboren ", Index$temp)
    Index$temp <- gsub("gebn ", "geboren ", Index$temp)
   #set function
    rm_geb <- function(combinatie){
      Index$Birth_name <<- ifelse(grepl(combinatie, Index$temp), sub(" .*", "", sub(paste0(".*?",combinatie), "", Index$temp)), Index$Birth_name)
      Index$temp <<- ifelse(grepl(combinatie, Index$temp)==F, Index$temp,
                            paste0( sub(paste0(combinatie,".*"), "", Index$temp), #plak voor combinatie
                                    sub(".*? ", "", sub(paste0(".*?",combinatie), "", Index$temp)) ) #na combinatie + 1 woord)
      ) 
    }
    rm_geb_prefix <- function(combinatie, prefix){
      Index$Birth_name <<- ifelse(grepl(combinatie, Index$temp), sub(" .*", "", sub(paste0(".*?",combinatie), "", Index$temp)), Index$Birth_name)
      Index$Birth_name <<- ifelse(grepl(combinatie, Index$temp), paste0(prefix, Index$Birth_name), Index$Birth_name)
      Index$temp <<- ifelse(grepl(combinatie, Index$temp)==F, Index$temp,
                            paste0( sub(paste0(combinatie,".*"), "", Index$temp), #plak voor combinatie
                                    sub(".*? ", "", sub(paste0(".*?",combinatie), "", Index$temp)) ) #na combinatie + 1 woord)
                            ) 
    }
   #geboren de
    Index[grepl("geboren le ", Index$temp),"temp"]
    rm_geb_prefix("geboren del ", "del ")
    rm_geb_prefix("geboren de la ", "de la ")
    rm_geb_prefix("geboren de ", "de ")
   #geboren van
    Index[grepl("geboren van ", Index$temp),"temp"]
    rm_geb_prefix("geboren van der ", "van der ")
    rm_geb_prefix("geboren van de ", "van de ")
    rm_geb_prefix("geboren van ", "van ")
   #geboren
    Index[grepl("geboren ", Index$temp),"temp"]
    rm_geb_prefix("geboren ", "van ")
    
    
   #en
    Index[which(grepl(" en ", Index$First_name) & is.na(Index$Relation_description)),"temp"]
    Index$Relation_description[grepl(" en ", Index$First_name) & is.na(Index$Relation_description)] <- "multiple persons"
    
    
    
  #clean remaining prefixes
    Index$temp <- trimws(Index$temp)
   #replace
    rm_prefix("bueno de")
    rm_prefix("d'")
    rm_prefix("da")
    rm_prefix("de")
    rm_prefix("des")
    rm_prefix("de la")
    rm_prefix("d' la")
    rm_prefix("de l'")
    rm_prefix("du")
    rm_prefix("l'")
    rm_prefix("la")
    rm_prefix("le")
    rm_prefix("ter")
    rm_prefix("van van")
    rm_prefix("van")
    rm_prefix("van de")
    rm_prefix("van du")
    rm_prefix("van den")
    rm_prefix("van der")
    rm_prefix("van het")
    rm_prefix("van 't")
    rm_prefix("van la")
    
    
    #fix composite names
    Index$Last_name <- ifelse(is.na(Index$Prefix), Index$Last_name,
                              ifelse(Index$Prefix=="bueno de", paste(Index$Prefix, Index$Last_name), Index$Last_name))
    Index$Prefix <- ifelse(Index$Prefix=="bueno de", NA, Index$Prefix)
    
    
    
    Index$First_name <- Index$temp
    
    Index <- Index[,c("Eigenaar", "Last_name", "First_name", "Prefix", "Birth_name", "Postfix", "Patroniem", "Person_description", "Relation_description")]
    
    write.xlsx(Index,"C:/Surfdrive/HDS/Cleaning Prive-eigenaren/Prive-eigenaren - serie 1-4 - standardisatie - 20210226.xlsx")
    
    
    
    
    
    