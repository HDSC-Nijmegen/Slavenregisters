
#####################################################################
### Prepare Emancipation register for matching with end of Serie4 ###
#####################################################################

#load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringdist)

#clean environment
rm(list=ls())

#set working directory
#Home
#setwd("~/HDS/Emancipatieregisters")
#Work
setwd("//cnas.ru.nl/U709207/Documents/Suriname/Emancipatieregisters")

#open data set
df <- read_excel("Emancipatieregisters totaal na correcties Coen v2.xlsx") %>%
  select(-"...13", -"...14") ## remove unnecessary columns ...13 and ...14


################################################################
### 1 Remove typos from birth year and generate birth date   ###
################################################################

df2 <- df %>% mutate(Geboortejaar = replace(Geboortejaar, Geboortejaar == ",850", "1850"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == ",859", "1859"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1806 )", "1806"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1812 ,", "1812"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1815  ;", "1815"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == ",817", "1817"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1302", "1802"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1319", "1819"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1333", "1833"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1343", "1843"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1357", "1857"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1847  '", "1847"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1959", "1859"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1333/1823", "1833/1823"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "1897/1801", "1797/1801"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "..03.1863", "01.03.1863"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "25.08.58", "25.08.1858"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "08.01.1959", "08.01.1859"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "09.10.1959", "09.10.1859"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "01.09.1961", "01.09.1861"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "17 10.1862", "17.10.1862"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "17.03,1855", "17.03.1855"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "10.09 1859", "10.09.1859"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "76.25", NA),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "(…)", NA),
                     `Verwantschap en Erkenning` = replace(`Verwantschap en Erkenning`, Geboortejaar == "Vermoedelijk verwant aan Marius en Kees Hugo", "Vermoedelijk verwant aan Marius en Kees Hugo"),
                     Geboortejaar = replace(Geboortejaar, Geboortejaar == "Vermoedelijk verwant aan Marius en Kees Hugo", NA)) %>%
              separate(Geboortejaar, c("B_year1", "B_year2"), "/", remove = FALSE) %>% # separate birth year into first and second birth year
              separate(B_year1, c("B_day", "B_month", "B_year"), "\\.", remove = FALSE) %>% # separate birth day, month, and year for first birth year
              mutate(B_year2 = replace(B_year2, B_year2 == "1707", "1807"), #repair some entries
                     B_year2 = replace(B_year2, B_year2 == "1858 1814", "1858"),
                     B_year2 = replace(B_year2, B_year2 == "ca 1808", "1808"),
                     B_year2 = replace(B_year2, B_year2 == "ca.1803", "1803"),
                     B_year2 = replace(B_year2, B_year2 == "ca.1811", "1811"),
                     B_year1 = replace(B_year1, B_year1 > 1863, NA),
                     B_day = replace(B_day, B_day == "00", "01"),
                     B_day = replace(B_day, B_day == "", "01"))

#Make the newly generated variables numeric
df2$B_day <- as.numeric(df2$B_day) 
df2$B_month <- as.numeric(df2$B_month) 
df2$B_year <- as.numeric(df2$B_year) 
df2$B_year1 <- as.numeric(df2$B_year1) 
df2$B_year2 <- as.numeric(df2$B_year2) 

#Generate final birth variables
df2 <- df2 %>% mutate(B_day = replace(B_day, B_day > 31, NA),
                      B_year = replace(B_year, is.na(B_year), B_year1[is.na(B_year)])) %>%
               select(-B_year1, -Geboortejaar) %>%
  

################################################
### 2 Remove typos from plantation variable  ###
################################################
 
  mutate(`Vestiging/ Negernaam` = replace(`Vestiging/ Negernaam`, `Vestiging/ Negernaam` == "Groot Marseille/Jacob/'", "Groot Marseille/Jacobi"),
         `Vestiging/ Negernaam` = replace(`Vestiging/ Negernaam`, `Vestiging/ Negernaam` == "Hooijland/D/Hoo/", "Hooijland/Di Hooi")) %>%
  separate(`Vestiging/ Negernaam`, c("Place_name1", "Place_name2"), "/", remove = FALSE) %>%
  select(-`Vestiging/ Negernaam`) %>%
  mutate(Place_name1 = gsub("’", "'", Place_name1)) %>%
  mutate(Place_name1 = replace(Place_name1, Place_name1 =="Crappahoeknr. 34", "Crappahoek nr. 34"),
         Place_name1 = replace(Place_name1, Place_name1 =="Etablissement MonTresor", "Etablissement Mon Tresor"),
         Place_name1 = replace(Place_name1, Place_name1 =="GuadeIoupe", "Guadeloupe"),
         Place_name1 = replace(Place_name1, Place_name1 =="In 't bosch", "In't bosch"),
         Place_name1 = replace(Place_name1, Place_name1 =="In 'tbosch", "In't bosch"),
         Place_name1 = replace(Place_name1, Place_name1 =="L' Esperance", "L'Esperance"),
         Place_name1 = replace(Place_name1, Place_name1 =="L'inquietude", "L'lnquietude"),
         Place_name1 = replace(Place_name1, Place_name1 =="Ma retraite", "Ma Retraite"),
         Place_name1 = replace(Place_name1, Place_name1 =="Merveiiie", "Merveille"),
         Place_name1 = replace(Place_name1, Place_name1 =="Paramaribo i", "Paramaribo I"),
         Place_name1 = replace(Place_name1, Place_name1 =="Paramaribo li", "Paramaribo II"),
         Place_name1 = replace(Place_name1, Place_name1 =="Rae a Rac", "Rac a Rac"),
         Place_name1 = replace(Place_name1, Place_name1 =="Susanna's Daal", "Susanna'sDaal"),
         Place_name1 = replace(Place_name1, Place_name1 =="t ijland", "t IJland"),
         Place_name1 = replace(Place_name1, Place_name1 =="Vaderszorg enCarelsdeel", "Vaderszorg en Carelsdeel"),
         Place_name1 = replace(Place_name1, Place_name1 =="Wegiooperskamp", "Weglooperskamp"),
         Place_name1 = replace(Place_name1, Place_name1 =="I'Aventure", "l'Aventure"),
         Place_name1 = replace(Place_name1, Place_name1 =="Simplicite", "La Simplicite"),
         Place_name1 = replace(Place_name1, Place_name1 =="Hampton Court", "Hamptoncourt"),
         Place_name1 = replace(Place_name1, Place_name1 =="Guinesche Vriendschap", "Guineesche Vriendschap"),
         Place_name1 = replace(Place_name1, Place_name1 =="Potosie", "Potosi"),
         Place_name1 = replace(Place_name1, Place_name1 =="Roozenburg", "Rozenburg"),
         Place_name1 = replace(Place_name1, Place_name1 =="Susanna'sDaal", "Susanna'sdaal"),
         Place_name1 = replace(Place_name1, Place_name1 =="Leliendal", "Leliendaal"),
         Place_name1 = replace(Place_name1, Place_name1 =="St Eustatius", "St. Eustatius"),
         Place_name1 = replace(Place_name1, Place_name1 =="Vaderszorg en Carelsdeel", "Vaderszorg en Karelsdeel"),
         Place_name1 = replace(Place_name1, Place_name1 =="Paradise", "Paradize"),
         Place_name1 = replace(Place_name1, Place_name1 =="W.C. Frederica", "Wilhelmina Catharina Frederica"),
         Place_name1 = replace(Place_name1, Place_name1 =="La Liberte", "La Liberté"),
         Place_name1 = replace(Place_name1, Place_name1 =="Broederhoop", "Broedershoop"),
         Place_name1 = replace(Place_name1, Place_name1 =="Morgenster", "De Morgenster"),
         Place_name1 = replace(Place_name1, Place_name1 =="Sinabo", "Sinabo en Gelre"),
         Place_name1 = replace(Place_name1, Place_name1 =="Cannawapibo", "Cannawappibo"),
         Place_name1 = replace(Place_name1, Place_name1 =="Bronswijk", "Brunswijk"),
         Place_name1 = replace(Place_name1, Place_name1 =="Wolffs Capoerica", "Wolffscapoerica"),
         Place_name1 = replace(Place_name1, Place_name1 =="Sorgvliet", "Zorgvliet"),
         Place_name1 = replace(Place_name1, Place_name1 =="Kroonenburg", "Kroonenbrug"),
         Place_name1 = replace(Place_name1, Place_name1 =="Mon Tresor", "Montrésor"),
         Place_name1 = replace(Place_name1, Place_name1 =="Overtoom", "Overtoom en Vreeland"),
         Place_name1 = replace(Place_name1, Place_name1 =="St Barbara", "St. Barbara"),
         Place_name1 = replace(Place_name1, Place_name1 =="Toutluifaut", "Tout lui faut"),
         Place_name1 = replace(Place_name1, Place_name1 =="Munnikendam", "Munnickendam"),
         Place_name1 = replace(Place_name1, Place_name1 =="Cocqswoud", "Kocqswoud"),
         Place_name1 = replace(Place_name1, Place_name1 =="d'Alijda", "d'Alijda en Karelsburg"),
         Place_name1 = replace(Place_name1, Place_name1 =="Bleijenhoop", "Bleijenhoop en Bleijenrust"),
         Place_name1 = replace(Place_name1, Place_name1 =="L'lnquietude", "L'Inquietude"),
         Place_name1 = replace(Place_name1, Place_name1 =="Vriendsbeleid en Ouderszorg", "Vriendsbeleid en Ouderzorg"),
         Place_name1 = replace(Place_name1, Place_name1 =="Vier Hendrikken", "De Vier Hendrikken"),
         Place_name1 = replace(Place_name1, Place_name1 =="Merveille", "De Merveille"),
         Place_name1 = replace(Place_name1, Place_name1 =="Ephraimszegen", "Ephraimzegen"),
         Place_name1 = replace(Place_name1, Place_name1 =="Frederici'sgift", "Fridericisgift"),
         Place_name1 = replace(Place_name1, Place_name1 =="De Dankbaarheid", "Dankbaarheid"),
         Place_name1 = replace(Place_name1, Place_name1 =="La Sonnette", "Sonnette"),
         Place_name1 = replace(Place_name1, Place_name1 =="Lot nr. 1", "Lot No 1"),
         Place_name1 = replace(Place_name1, Place_name1 =="Good-lntend", "Good Intent"),
         Place_name1 = replace(Place_name1, Place_name1 =="Rac a Rac", "Rac à Rac"),
         Place_name1 = replace(Place_name1, Place_name1 =="Lot nr. 36", "Lot Land No 36"),
         Place_name1 = replace(Place_name1, Place_name1 =="La Prosperite", "La Prosperité"),
         Place_name1 = replace(Place_name1, Place_name1 =="Perseverance", "La Perseverance Coronie"),
         Place_name1 = replace(Place_name1, Place_name1 =="Vertrouwen", "t Vertrouwen"),
         Place_name1 = replace(Place_name1, Place_name1 =="N.C. Frederica", "Wilhelmina Catharina Frederica"),
         Place_name1 = replace(Place_name1, Place_name1 =="Adrichem" & Divisie == "Matappica", "Adrichem Matappica"),
         Place_name1 = replace(Place_name1, Place_name1 =="Adrichem" & Divisie == "Para beneden", "Adrichem Para"),
         Place_name1 = replace(Place_name1, Place_name1 =="Berlijn" & Divisie == "Cottica beneden", "Berlijn beneden Cottica"),
         Place_name1 = replace(Place_name1, Place_name1 =="Berlijn" & Divisie == "Para boven", "Berlijn Para"),
         Place_name1 = replace(Place_name1, Place_name1 =="Hamburg" & Divisie == "Saramacca beneden", "Hamburg Saramacca"),
         Place_name1 = replace(Place_name1, Place_name1 =="Hazard" & Divisie == "Commewijne boven", "Hazard boven Commewijne"),
         Place_name1 = replace(Place_name1, Place_name1 =="Hazard" & Divisie == "Nickerie", "Hazard Nickerie"),
         Place_name1 = replace(Place_name1, Place_name1 =="Johannesburg" & Divisie == "Cottica beneden", "Johannesburg beneden Cottica"),
         Place_name1 = replace(Place_name1, Place_name1 =="L'Esperance" & Divisie == "Suriname boven", "L'Esperance boven Suriname en Thorarica"),
         Place_name1 = replace(Place_name1, Place_name1 =="Ma Retraite" & Divisie == "Commewijne beneden", "Maretraite beneden Commewijne"),
         Place_name1 = replace(Place_name1, Place_name1 =="Ma Retraite" & Divisie == "Para beneden", "Maretraite Para"),
         Place_name1 = replace(Place_name1, Place_name1 =="Onverwacht" & Divisie == "Para boven", "Onverwacht Para"),
         Place_name1 = replace(Place_name1, Place_name1 =="Waterloo" & Divisie == "Nickerie", "Waterloo Nickerie"),
         Place_name1 = replace(Place_name1, Place_name1 =="Zorg en Hoop" & Divisie == "Para beneden", "Zorg en Hoop Para"),
         Place_name1 = replace(Place_name1, Place_name1 =="Zorg en Hoop" & Divisie == "Cottica beneden", "Zorg en Hoop beneden Cottica")) %>%


###########################################
### 3 Remove typos from owner variable  ###
###########################################
 
  mutate(Eigenaar = gsub(",", ".", Eigenaar), # replace commas with dots
         Eigenaar = replace(Eigenaar, Eigenaar =="0'Ferrall. D. J", "O'Ferrall. D. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="AuloriusJ. van", "Aulorius. J. van"),
         Eigenaar = replace(Eigenaar, Eigenaar =="B randt. C", "Brandt. C"),
         Eigenaar = replace(Eigenaar, Eigenaar =="BasJ. F. de", "Bas. J. F. de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="BennernagelJ. C", "Bennernagel. J. C"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Bijlaa rt. H", "Bijlaart. H"),
         Eigenaar = replace(Eigenaar, Eigenaar =="BronovoJ. J", "Bronovo. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Brantsen v. d Zijp. D. W", "Brantsen. v. d. Zijp. D. W"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Brantsen v. d Zijp. D. W (plantage eigenaar )", "Brantsen. v. d Zijp. D. W (plantage eigenaar )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Brantsen v. d. Zijp. D. W.", "Brantsen. v. d. Zijp. D. W"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Brantsen v. dZijp. D. W", "Brantsen. v. d. Zijp. D. W"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Bueno de Mesquita Jr. J", "Bueno de Mesquita. Jr. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Bueno de Mesquita Jzn. l", "Bueno de Mesquita. Jzn. l"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Bueno de MesquitaJ. J", "Bueno de Mesquita. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Charbon en zn", "Charbon. en zn"),
         Eigenaar = replace(Eigenaar, Eigenaar =="CharbonJ. A en Zn", "Charbon. J. A en Zn"),
         Eigenaar = replace(Eigenaar, Eigenaar =="CharbonJ. ASZn", "Charbon. J. ASZn"),
         Eigenaar = replace(Eigenaar, Eigenaar =="CharbonJ. J", "Charbon. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Cruder). G", "Cruder. G"),
         Eigenaar = replace(Eigenaar, Eigenaar =="d' Hangestd'ijvoijAM en C. M", "d' Hangestd'ijvoij. AM en C. M"),
         Eigenaar = replace(Eigenaar, Eigenaar =="DoerrlebenJ. N", "Doerrleben. J. N"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Emanuels Jzn. E", "Emanuels. Jzn. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="EzechielsJ.", "Ezechiels. J."),
         Eigenaar = replace(Eigenaar, Eigenaar =="FerrierA de erven", "Ferrier. A. de erven"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Fraissinet en v. Baak fonds", "Fraissinet. en v. Baak fonds"),
         Eigenaar = replace(Eigenaar, Eigenaar =="FrouinJ", "Frouin. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="GeffenJ. M. Martini van", "Geffen. J. M. Martini van"),
         Eigenaar = replace(Eigenaar, Eigenaar =="GerdemanAF. de erven", "Gerdeman. AF. de erven"),
         Eigenaar = replace(Eigenaar, Eigenaar =="GodefroijJ. J", "Godefroij. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="GodefroijJJ", "Godefroij. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="HeijmansJ. H", "Heijmans. J. H"),
         Eigenaar = replace(Eigenaar, Eigenaar =="HoltC. M en J. H. van", "Holt. C. M en J. H. van"),
         Eigenaar = replace(Eigenaar, Eigenaar =="HorstJ. D", "Horst. J. D"),
         Eigenaar = replace(Eigenaar, Eigenaar =="ijoungJ", "ijoung. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Insinger en co", "Insinger. en co"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Insinger en co (plantage eigenaar )", "Insinger. en co (plantage eigenaar )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Insinger Tiema en co", "Insinger. Tiema en co"),
         Eigenaar = replace(Eigenaar, Eigenaar =="InsingerTiema en co", "Insinger. Tiema en co"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Jansen Eijken SluijtersJ. M", "Jansen Eijken Sluijters. J. M"),
         Eigenaar = replace(Eigenaar, Eigenaar =="JansonJ. P", "Janson. J. P"),
         Eigenaar = replace(Eigenaar, Eigenaar =="JohanzoonJ. J. H", "Johanzoon. J. J. H"),
         Eigenaar = replace(Eigenaar, Eigenaar =="JudaJ. J", "Juda. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Kaersenhoutjh", "Kaersenhout. jh"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Kerste r. M", "Kerster. M"),
         Eigenaar = replace(Eigenaar, Eigenaar =="KuvelG. E", "Kuvel. G. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="KuveLG. E", "Kuvel. G. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="LierJ. van", "Lier. J. van"),
         Eigenaar = replace(Eigenaar, Eigenaar =="LouzadaJ. B", "Louzada. J. B"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Me ij er. J. T", "Meijer. J. T"),
         Eigenaar = replace(Eigenaar, Eigenaar =="MeijerJ. T", "Meijer. J. T"),
         Eigenaar = replace(Eigenaar, Eigenaar =="MeinertzhagenJ. L", "Meinertzhagen. J. L"),
         Eigenaar = replace(Eigenaar, Eigenaar =="MesquitaJ. J. de", "Mesquita. J. J. de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="MesquitaJJ. de", "Mesquita. J. J. de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Mij uitbr. Chr. dom", "Mij. uitbr. Chr. dom"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Monte Lijon. l. del", "Monte. Lijon. l. del"),
         Eigenaar = replace(Eigenaar, Eigenaar =="MorpurgoJ", "Morpurgo. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NahlopJ. A", "Nahlop. J. A"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Nassij J. E", "Nassij. J. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NassijJ. E", "Nassij. J. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Ned. lsr. Gemeente", "Ned lsr Gemeente"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NegrebJ. C. van", "Negreb. J. C. van"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NobelAC", "Nobel. A. C"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NobelP. C. de erven", "Nobel. P. C. de erven"),
         Eigenaar = replace(Eigenaar, Eigenaar =="NoordberghJ", "Noordbergh. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="O'FerralLD. J", "O'Ferrall. D. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Olivieirajr. A. van Mozes", "Olivieira. jr. A. van Mozes"),
         Eigenaar = replace(Eigenaar, Eigenaar =="OlivieiraJr. A. van Mozes", "Olivieira. jr. A. van Mozes"),
         Eigenaar = replace(Eigenaar, Eigenaar =="OnnaJ. M. van e. a", "Onna. J. M. van e. a"),
         Eigenaar = replace(Eigenaar, Eigenaar =="OrsingaJ. S", "Orsinga. J. S"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Pardo Cardoze. A", "Pardo. Cardoze. A"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Pichotdu Plessis. S. R", "Pichot du Plessis. S. R"),
         Eigenaar = replace(Eigenaar, Eigenaar =="PolLA. J. van de", "Poll. A. J. van de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="PotvillageJ. J", "Potvillage. J. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="PutscherJ. H", "Putscher. J. H"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Raatge. ver. M. E", "Raatgever. M. E"),
         Eigenaar = replace(Eigenaar, Eigenaar =="RandamieJ. de", "Randamie. J. de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Roijer J. C", "Roijer. J. C"),
         Eigenaar = replace(Eigenaar, Eigenaar =="SandickJ. A. vanJe erven", "Sandick. J. A. van. de erven"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Sussenba ch. I. G", "Sussenbach. I. G"),
         Eigenaar = replace(Eigenaar, Eigenaar =="TeltingJ. C", "Telting. J. C"),
         Eigenaar = replace(Eigenaar, Eigenaar =="ThielerJ", "Thieler. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Tijnda!. J. de Veer", "Tijndal. J. de Veer"),
         Eigenaar = replace(Eigenaar, Eigenaar =="TijndalJ. de Veer", "Tijndal. J. de Veer"),
         Eigenaar = replace(Eigenaar, Eigenaar =="TijndaLJ. de Veer", "Tijndal. J. de Veer"),
         Eigenaar = replace(Eigenaar, Eigenaar =="TijndalJ. de Veer (plantage eigenaar )", "Tijndal. J. de Veer (plantage eigenaar )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Tijndalljh", "Tijndall. jh"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Traissinet en v. Baak", "Traissinet. en v. Baak"),
         Eigenaar = replace(Eigenaar, Eigenaar =="UlfhothJ. W", "Ulfhoth. J. W"),
         Eigenaar = replace(Eigenaar, Eigenaar =="VereuLM. J", "Vereul. M. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="VereuLM. J (plantage eigenaar )", "Vereul. M. J (plantage eigenaar )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="VereuLN. J", "Vereul. N. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Vos van SteenwijkJ. A. de", "Vos van Steenwijk. J. A. de"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WeimannJ. P", "Weimann. J. P"),
         Eigenaar = replace(Eigenaar, Eigenaar =="Welcom Luna Lobo. A", "Welcom. Luna Lobo. A"),
         Eigenaar = replace(Eigenaar, Eigenaar =="West-Indische BankfpI. e. )", "West-Indische Bank (pI. e. )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="West-Indische BankIpI. e. )", "West-Indische Bank (pI. e. )"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WijnandJ", "Wijnand. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WolffJ. en Robles de Medina. J", "Wolff. J. en Robles de Medina. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="ZaalJ", "Zaal. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WijnandJ", "Wijnand. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WijnandJ", "Wijnand. J"),
         Eigenaar = replace(Eigenaar, Eigenaar =="WijnandJ", "Wijnand. J")) %>%
  separate(Eigenaar, c("Eigenaar_Lastname"), "\\.", remove = FALSE) %>% # generate column only with owner last name


###############################################
### 4 Remove typos from slave name variable ###
###############################################

 # mutate(`Naam voor 1863`= gsub("ph", "f",`Naam voor 1863`)) %>% # replace "ph" with "f"
  mutate(`Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Le entje", "Leentje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Fi nette", "Finette"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Ca rotje", "Carotje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Charles=Cornelis", "Charles of Cornelis"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "S a raatje", "Saraatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Serjeantgedoopt Julius", "Serjeant gedoopt Julius"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Europa gedooptAzetta", "Europa gedoopt Azetta"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Julius gedooptTheodorus", "Julius gedoopt Theodorus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "An aatje", "Anaatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Lodewijk gedooptHermanus", "Lodewijk gedoopt Hermanus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Lode wijk", "Lodewijk"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Josephgedoopt Jacobus", "Joseph gedoopt Jacobus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Tranquille gedooptRebekka", "Tranquille gedoopt Rebekka"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Ba rtholine", "Bartholine"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Alfred gedooptJohannes", "Alfred gedoopt Johannes"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Chri stoffel", "Christoffel"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Lee ntj e", "Leentje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Adolf of Adclph", "Adolf of Adolph"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "IJatta of Jetta or Jata", "IJatta of Jetta of Jata"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Daphina of Dauphina gedoopt Albe'rtina", "Daphina of Dauphina gedoopt Albertina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Janagedoopt Adolphina", "Jana gedoopt Adolphina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Rudolf gedoopt Gideon", "Rudolf gedoopt Gideon"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "America gedooptRebecca", "America gedoopt Rebecca"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Harlekein of Arlequin gedooptJacob", "Harlekein of Arlequin gedoopt Jacob"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Laurens of Laurence Johanna(?)", "Laurens of Laurence Johannes"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "John of Johangedoopt Jozef", "John of Johan gedoopt Jozef"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Bruinetta of Brui nette", "Bruinetta of Bruinette"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Matje of M aatje", "Matje of Maatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Sa raatje", "Saraatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Ce^ar of Casar", "Cegar of Casar"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Leent je", "Leentje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Jozef gedooptHendrik", "Jozef gedoopt Hendrik"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Betti j", "Bettij"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Chri stoffel", "Christoffel"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Johannagedoopt Lea", "Johanna gedoopt Lea"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Li via", "Livia"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Li cette", "Licette"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Ma rietje", "Marietje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Baron gedooptJosua", "Baron gedoopt Josua"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Amandagedoopt Sabina", "Amanda gedoopt Sabina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "An aatje", "Anaatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Bachus gedooptThomas", "Bachus gedoopt Thomas"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Johangedoopt Lucas", "Johan gedoopt Lucas"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Wijsger of Weisscher gedooptEduard", "Wijsger of Weisscher gedoopt Eduard"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Lotte rie", "Lotterie"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Luc retie", "Lucretie"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Primo gedooptJoseph", "Primo gedoopt Joseph"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Elisa beth of Betsij 3e", "Elisabeth of Betsij 3e"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Sa ratje", "Saratje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Julius gedooptSamuel", "Julius gedoopt Samuel"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Justin a", "Justina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Aria antje", "Ariaantje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "CarolinagedooptVinitia", "Carolina gedoopt Vinitia"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Pate riot", "Pateriot"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Jupiter gedooptAbraham", "Jupiter gedoopt Abraham"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Betti e", "Bettie"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Jangedoopt Johannes", "Jan gedoopt Johannes"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Josefgedoopt Jacobus", "Josef gedoopt Jacobus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Janagedoopt Adolfina", "Jana gedoopt Adolfina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Gu staaf", "Gustaaf"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Marinus gedooptTheodorus", "Marinus gedoopt Theodorus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "D oortje", "Doortje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Francina gedooptAlbertina", "Francina gedoopt Albertina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Louisa gedooptHenriette", "Louisa gedoopt Henriette"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Matthijs gedooptJacob", "Matthijs gedoopt Jacob"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Bergere gedooptJosephina", "Bergere gedoopt Josephina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Salmon gedooptSalomon", "Salmon gedoopt Salomon"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Rudolfgedoopt Gideon", "Rudolf gedoopt Gideon"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Roos gedooptLucas", "Roos gedoopt Lucas"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Minosabiegedoopt William", "Minosabie gedoopt William"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Alij da", "Alijda"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Johangedoopt Josua", "Johan gedoopt Josua"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Johannagedoopt Eva", "Johanna gedoopt Eva"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Sa raatje", "Saraatje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Clasina gedooptTheodora", "Clasina gedoopt Theodora"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Secondogedoopt Jacob", "Secondo gedoopt Jacob"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Anthoin ette of Anthoinetta", "Anthoinette of Anthoinetta"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Amaranth a", "Amarantha"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Madelijnegedoopt Maria", "Madelijne gedoopt Maria"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Lea gedooptSara", "Lea gedoopt Sara"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Vannij gedooptMaria", "Vannij gedoopt Maria"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Susannagedoopt Susanna", "Susanna gedoopt Susanna"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Bien gedooptStephanus", "Bien gedoopt Stephanus"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Floragedoopt Elisabeth", "Flora gedoopt Elisabeth"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Jacobgedoopt Johannes", "Jacob gedoopt Johannes"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Am a ra nth a", "Amarantha"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Margaretha gedooptMargaretha", "Margaretha gedoopt Margaretha"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Johanna gedooptMagdalijntje", "Johanna gedoopt Magdalijntje"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Louisa gedooptGerhardina", "Louisa gedoopt Gerhardina"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Am o u rette of Anth o i n ette", "Amourette of Anthoinette"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "Anth o i n ette of Anth o i n etta", "Anthoinette of Anthoinetta"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "M itri d ate s of M ith ri d ate s", "Mitridates of Mithridates"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "G rati a of G race", "Gratia of Grace"),
         `Naam voor 1863` = replace(`Naam voor 1863`, `Naam voor 1863` == "G rati a of G race", "Gratia of Grace")) %>%
  separate(`Naam voor 1863`, c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6"), "\\s", remove = FALSE) %>% # separate parts of the Name variable (up to 6 parts)
  

################################
### 5 Clean "Geslachtsnaam"  ###
################################ 

  mutate(Geslachtsnaam = tolower(Geslachtsnaam), # all lower case
         Geslachtsnaam = gsub("‘1", "*1", Geslachtsnaam), # solve problems with Lamur's footnotes 
         Geslachtsnaam = gsub("‘2", "*2", Geslachtsnaam),
         Geslachtsnaam = gsub("‘3", "*3", Geslachtsnaam),
         Geslachtsnaam = gsub("‘4", "*4", Geslachtsnaam),
         Geslachtsnaam = gsub("'1", "*1", Geslachtsnaam),
         Geslachtsnaam = gsub("'2", "*2", Geslachtsnaam),
         Geslachtsnaam = gsub("'3", "*3", Geslachtsnaam),
         Geslachtsnaam = gsub("'4", "*4", Geslachtsnaam),
         Geslachtsnaam = gsub("'6", "*6", Geslachtsnaam),
         Geslachtsnaam = gsub("~", "*", Geslachtsnaam)) %>%
  mutate(Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "29977 vander leende", "29977 van der leende"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "30647 veld de*39", "30647 de veld*39"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "32675 wes1e*1o", "32675 weste*10"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "5251 christian!", "5251 christiani"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "21131 noordw1jk*11", "21131 noordwijk*11"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "21135 noordw!jk*11", "21135 noordwijk*11"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "30648 veld de*39", "30648 de veld*39"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "30649 veld de*39", "30649 de veld*39"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "30650 veld de*39", "30650 de veld*39"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "| 197 adnis", "197 adnis"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "| 439 all", "439 all"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "| 447 allas", "447 allas"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "| 473 allin", "473 allin"),
         Geslachtsnaam = replace(Geslachtsnaam, Geslachtsnaam == "3045 block*! 5", "3045 block*5")) %>%
  separate(Geslachtsnaam, c("Part1", "Part2", "Part3", "Part4"), "\\s", remove = FALSE) %>%
  unite("Part2_3",  Part2: Part3, sep = " ", remove = FALSE) %>%
  unite("Part2_3_4",  Part2: Part4, sep = " ", remove = FALSE) %>%
  unite("Part3_2",  Part3: Part2, sep = " ", remove = FALSE) %>%
  mutate(Naam_Family = ifelse(is.na(Part3), Part2, 
                              ifelse((Part2 == "van" | Part2 == "de" |  Part2 == "te" |  Part2 == "ter" | Part2 == "da" | 
                                        Part2 == "caprera" | Part2 == "casjefas" | Part2 == "der" | Part2 == "den" | Part2 == "la" | 
                                        Part2 == "mac" |  Part2 == "ten" | Part2 == "st.") & is.na(Part4), Part2_3, 
                                     ifelse((Part2 == "van" & !(is.na(Part4))), Part2_3_4, 
                                            ifelse(Part3== "van" | Part3 == "de", Part3_2, NA))))) %>%
  unite("Part2_3_new",  Part2: Part3, sep = "", remove = FALSE) %>%
  unite("Part2_3_4_new",  Part2: Part4, sep = "", remove = FALSE) %>%
  mutate(Naam_Family_new = ifelse(is.na(Naam_Family) & is.na(Part4) & Part2 != "info", Part2_3_new, 
                                  ifelse(is.na(Naam_Family) & !(is.na(Part4)) & Part2 != "info", Part2_3_4_new, NA))) %>%
  mutate(Naam_Family = ifelse(is.na(Naam_Family), Naam_Family_new, Naam_Family)) %>%
  select(-Part1, -Part2, -Part3, -Part4, -Part2_3, -Part2_3_4, -Part3_2, -Part2_3_new, -Part2_3_4_new, -Naam_Family_new) %>%
  separate(Naam_Family, c("Naam_Family_1", "Naam_Family_Remarks"), "\\*", remove = FALSE) %>%
  mutate(Naam_Family_1= gsub("0", "o", Naam_Family_1)) %>%
  mutate(Naam_Family_Remarks= gsub("o", "0", Naam_Family_Remarks),
         Naam_Family_Remarks= replace(Naam_Family_Remarks, Naam_Family_Remarks == "a3", "43"),
         Naam_Family_Remarks= replace(Naam_Family_Remarks, Naam_Family_Remarks == "t2", "12"),
         Naam_Family_Remarks= replace(Naam_Family_Remarks, Naam_Family_Remarks == "!", "1")) %>%
  select(-Naam_Family) %>%
  relocate(Naam_Family_1, Naam_Family_Remarks) %>%
  rename(Naam_Family = Naam_Family_1) %>%
  separate(Naam_Family, c("Naam_Family", "Naam_Family_Remarks_help"), "(?<=[a-z]) ?(?=[0-9])") %>%
  mutate(Naam_Family_Remarks = ifelse(is.na(Naam_Family_Remarks), Naam_Family_Remarks_help, Naam_Family_Remarks),
         Naam_Family_Remarks= gsub("o", "0", Naam_Family_Remarks)) %>%
  select(-Naam_Family_Remarks_help) %>%



#############################################################################################
### 6 Split slave name variable into its important parts (First Name, Baptized Name etc.) ###
############################################################################################# 
  unite("Name1_2", Name1: Name2, sep = " ", remove = FALSE) %>%
  unite("Name1_2_3", Name1: Name3, sep = " ", remove = FALSE) %>%
  mutate(Name = ifelse(is.na(Name2), `Naam voor 1863`, 
                       ifelse(is.na(Name3) & !(is.na(Name2)), `Naam voor 1863`, 
                              ifelse((Name2 == "of" & is.na(Name4)) | Name2 == "van" |  Name3 =="of" | Name2 == "de" | Name2 == "alias" | (Name3 != "gedoopt" & Name2 != "gedoopt" & Name4 != "gedoopt") ,  `Naam voor 1863`,
                                     ifelse(Name2 == "gedoopt", Name1,
                                            ifelse(Name3 == "gedoopt", Name1_2,
                                                   ifelse(Name4 == "gedoopt", Name1_2_3, NA))))))) %>%
  unite("Name3_4", Name3: Name4, sep = " ", remove = FALSE) %>%
  unite("Name3_4_5", Name3: Name5, sep = " ", remove = FALSE) %>%
  unite("Name3_4_5_6", Name3: Name6, sep = " ", remove = FALSE) %>%
  unite("Name4_5", Name4: Name5, sep = " ", remove = FALSE) %>%
  unite("Name5_6", Name5: Name6, sep = " ", remove = FALSE) %>%
  mutate(Doopnaam = ifelse(Name2 == "gedoopt" & is.na(Name4), Name3, 
                           ifelse(Name3 == "gedoopt" & is.na(Name5), Name4, 
                                  ifelse(Name4 == "gedoopt" & is.na(Name6), Name5, 
                                         ifelse(Name2 == "gedoopt" & !(is.na(Name4)) & is.na(Name5), Name3_4, 
                                                ifelse(Name2 == "gedoopt" & !(is.na(Name5)) & is.na(Name6), Name3_4_5, 
                                                       ifelse(Name2 == "gedoopt" & !(is.na(Name6)), Name3_4_5_6,
                                                              ifelse(Name4 == "gedoopt" & !(is.na(Name6)), Name5_6,NA)))))))) %>%
  mutate(Naam_deel_1 = ifelse(Name2 == "of" | Name2 == "alias", Name1, 
                              ifelse(Name3 == "of" | Name3 == "alias", Name1_2, 
                                     ifelse(`Naam voor 1863` == "van de Pol of Poll", Name1_2_3, NA)))) %>%
  mutate(Naam_deel_2 = ifelse((Name4 == "gedoopt" | is.na(Name4) | Name4 == "of") &  (Name2 == "of" | Name2 == "alias"), Name3,
                              ifelse(!(Name4 == "gedoopt" | is.na(Name4) | Name4 == "of") & (Name2 == "of" | Name2 == "alias") & Name4 != "of", Name3_4, 
                                     ifelse(Name3 == "of" & is.na(Name5), Name4, 
                                            ifelse(Name3 == "of" & !(is.na(Name5)), Name4_5, 
                                                   ifelse(`Naam voor 1863` == "van de Pol of Poll", Name5, NA)))))) %>%
  mutate(Naam_deel_3 = ifelse((Name2 == "of" & Name4 == "of") | Name5 == "Dwinghof", Name5, NA)) %>%
  mutate(Naam_number = ifelse(Name2 == "I" | Name2 == "1" , "I" , 
                              ifelse(Name2 == "II" | Name2 == "2", "II", 
                                     ifelse(Name2 == "III" | Name2 == "3", "III",NA)))) %>%
  mutate(Name = ifelse(!(is.na(Naam_number)), Name1, Name)) %>%
  mutate(Extrainformatiebijnaam = ifelse(Name1 == "klein" | Name1 == "groot" |  Name1 == "lange" | Name1 == "Black", Name1, 
                                         ifelse(Name2 == "klein" | Name2 == "groot", Name2, 
                                                ifelse(Name2 == "(Groote)" | Name1 == "Gr", "groot", 
                                                       ifelse(Name2 == "(Kleine)", "klein", NA))))) %>%
  mutate(Name = ifelse(!(is.na(Extrainformatiebijnaam)) & (Name1 == "klein" | Name1 == "groot" | Name1 == "Gr" | Name1 == "lange" | Name1 == "Black"), Name2, 
                       ifelse(!(is.na(Extrainformatiebijnaam)) & (Name2 == "klein" | Name2 == "groot" | Name2 == "(Groote)") | Name2 == "(Kleine)", Name1, Name))) %>%
  mutate(Name = ifelse(is.na(Name), `Naam voor 1863`, Name)) %>%
  mutate(Naam_deel_1 = ifelse(is.na(Naam_deel_1) & !(is.na(Name2)) & Name1 != "La" & Name1 != "van" & Name1 != "de" & Name2 != "gedoopt" & Name2 != "of" & Name2 != "alias" & is.na(Extrainformatiebijnaam) & is.na(Naam_number), Name1, Naam_deel_1)) %>%
  mutate(Naam_deel_2 = ifelse(is.na(Naam_deel_2) & !(is.na(Name2)) & Name1 != "La" & Name1 != "van" & Name1 != "de" & Name2 != "gedoopt" & Name2 != "of" & Name2 != "alias" & is.na(Extrainformatiebijnaam) & is.na(Naam_number), Name2, Naam_deel_2)) %>%
  mutate(Naam_deel_3 = ifelse(is.na(Naam_deel_3) & !(is.na(Name3)) & Name1 != "La" & Name1 != "van" & Name1 != "de" & Name2 !="gedoopt" & Name2 != "of" & Name2 != "alias" & Name3 != "gedoopt" & Name3 != "of" & is.na(Extrainformatiebijnaam) & is.na(Naam_number), Name3, Naam_deel_3)) %>%
  select(- Name1_2, -Name1_2_3, -Name3_4, -Name4_5, -Name3_4_5, -Name3_4_5_6, -Name5_6, -Name1, -Name2, -Name3, -Name4, -Name5, -Name6) %>%
  relocate(c("Naam_Family", "Naam_Family_Remarks", "Name", "Naam_number", "Extrainformatiebijnaam", "Doopnaam", "Naam_deel_1", "Naam_deel_2", "Naam_deel_3"), .after = `Naam voor 1863`) %>%


############################################################################
### 7 Change the plantation names according to Lamur's footnotes/remarks ###
############################################################################
  mutate(Place_name1_adjusted = Place_name1) %>%
  mutate(Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 1, "Monbijou"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 2, "La Campagne"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 3, "Bleijenhoop en Bleijenrust"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 4, "Breda"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 5, "Beekvliet"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 6, "Dageraad en Dankbaarheid"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 7, "Cornelia'sburg"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 8, "Purmerend"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 9, "Lust tot Rust en Einde Rust"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 10, "Berkshoven en Oostrust"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 11, "Groot Cuijlenburg"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 12, "Annaszorg"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 13, "Gelderland"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 14, "Corisane"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 15, "Frederikslust"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 16, "Landzigt"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 17, "Nieuw Timotibo"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 18, "Goosen"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 19, "Hanover"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 20, "De Gekroonde Paauw"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 21, "Johanna Charlotte"), # Double Check with Johanna Charlotte (1/2 aandeel mevrouw G. C. Henkel geboren Vogt)
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 22, "Penoribo"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 23, "Nieuwhoop"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 24, "Indigoveld"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 25, "De Goede Hoop"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 26, "De Twee Kinderen"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 27, "Rama"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 28, "Waicoribo"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 29, "Pomona"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 30, "Boomakker"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 31, "Naccarikkibo"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 32, "Longmaij"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 33, "De Vrede"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 34, "Breedevoort"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 35, "La Resource"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 36, "Plaisance 1/3"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 37, "Berthaudslust"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 52, "Alkmaar"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Naam_Family_Remarks == 53, "Ornamibo"),
           Place_name1_adjusted = replace(Place_name1_adjusted, Place_name1_adjusted == "Paramaribo en Nieuwe Hoop" , "Nieuw Hoop")) %>%
  relocate(Place_name1_adjusted, .after = Place_name1) %>%
  rename(Place_name_Original = Place_name1) %>%
  rename(Place_name1 = Place_name1_adjusted) %>%
           
####################################
### 8 Clean Beroep/Aantekeningen ###
####################################
  mutate(`Beroep/ Aanteekeningen`= replace(`Beroep/ Aanteekeningen`, `Beroep/ Aanteekeningen` =="99Ponteneger", "99 Ponteneger" ),
         `Beroep/ Aanteekeningen`= replace(`Beroep/ Aanteekeningen`, `Beroep/ Aanteekeningen` =="124Ingenieur", "124 Ingenieur" ),
         `Beroep/ Aanteekeningen`= replace(`Beroep/ Aanteekeningen`, `Beroep/ Aanteekeningen` =="( 745", "745" ),
         `Beroep/ Aanteekeningen`= gsub("1/2", "0,5",`Beroep/ Aanteekeningen`),
         `Beroep/ Aanteekeningen`= gsub("1/3", "0,33",`Beroep/ Aanteekeningen`),
         `Beroep/ Aanteekeningen`= gsub("2/3", "0,67",`Beroep/ Aanteekeningen`),
         `Beroep/ Aanteekeningen`= gsub("\\| ", "",`Beroep/ Aanteekeningen`),
         `Beroep/ Aanteekeningen`= gsub("^\\w*\\s*", "", `Beroep/ Aanteekeningen`)) %>%
  separate(`Beroep/ Aanteekeningen`, c("occupation", "general_remarks"), "/", remove = TRUE) %>%
  mutate(occupation = replace(occupation, occupation == "...", NA),
         occupation = replace(occupation, occupation == "", NA))
  

####################################
### 9 Generate unique identifier ###
####################################
df2 <-  df2 %>%
  mutate(Id_person = paste("ER", sprintf("%05d", 1:length(df2$Geslachtsnaam)), sep="-")) %>%
  relocate("Id_person") # %>%


#########################################################################################################
### 10 Add information on plantation names from Serie 4 for later comparison and export final document ###
#########################################################################################################

serie4 <- read.csv("Serie4_Exit.csv") %>% 
  select(plantation_name) %>% 
  distinct() %>%
  filter(plantation_name != "") %>%
  mutate(plantation_match = 1)


final <- left_join(df2, serie4, by = c("Place_name1" = "plantation_name")) %>% 
  mutate(plantation_match = ifelse(is.na(plantation_match), 0, 1))

write.csv(final, file = "Emancipatieregister_cleaned.csv", row.names = FALSE)



              
