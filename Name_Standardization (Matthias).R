
library("haven")

setwd("C:/Surfdrive/Shared/shared map slavenregisters/Suriname slavenregisters/Most Important Files/Stata")


#############################Name Standardization###############################

rm(list=ls())
Dataset_Standardization <- read_dta("Dataset_Standardization.dta")

#1.) Remove unnecessary spaces
Dataset_Standardization$Naam_original <- Dataset_Standardization$Naam
Dataset_Standardization$Naam <- trimws(Dataset_Standardization$Naam)
Dataset_Standardization$Moeder <- trimws(Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder_2 <- trimws(Dataset_Standardization$Moeder_2)



#2.) Slave names
Dataset_Standardization$Naam <- gsub("1e", "I", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("2e", "II", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("3e", "III", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("4e", "IV", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("5e", "V", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("1", "I", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("2", "II", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("3", "III", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("4", "IV", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("5", "V", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("cc", "c", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("c", "k", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("z", "s", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("gr.", "gr", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("kl.", "kl", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Z B", "ZB", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("A C B", "ACB", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Gr.", "gr", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Korte", "korte", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lange", "lange", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("?.", "", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("de ", "De ", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("van ", "Van ", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("la ", "La ", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("l'", "L'", Dataset_Standardization$Naam)

Dataset_Standardization$Naam <- gsub("Aarrie", "Aarrij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Aavrij", "Aarrij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Abennie", "Abenie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Albertina Elis:", "Albertina Elisabeth", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Antje of Antje Petronella", "Antje Petronella", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bonkoeur", "Bonkour", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Brampie", "Brampje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Buonaparta", "Buonaparte", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Calestha", "Calesta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Candassie", "Candasie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Christina-Louisa", "Christina Louisa", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Classina", "Clasina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Clementina", "Clementine", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Colletta", "Coletta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Comtessie", "Comtesse", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Dianna", "Diana", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Dibbits", "Dibits", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Duringhoff", "Duringhof", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Farij", "Farie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Favorietje", "Favoriet", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Fortuin (sne )", "Fortuin Zn", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("François", "Frankois", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Gellert", "Gelbert", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jakques", "Jaques", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Gerard", "Gerhard", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Hannah", "Hanna", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Henriette", "Henrietta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Isak", "Isaak", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Constantie", "Constantia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Janie", "Jannie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jansie", "Jansje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jeanette", "Jeannette", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Johannis", "Johannes", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Josef", "Joseph", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lafleur", "La Fleur", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("LaFleur", "La Fleur", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Legaal", "Legaat", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lina", "Lena", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Madeleentje", "Madelijntje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Madeleintje", "Madelijntje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Magdalijntje", "Madelijntje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Maria-Theresia", "Maria Theresia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Marie", "Maria", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Matilda", "Mathilda", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Mathias", "Matthias", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Matthieu", "Mathieu", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Maij", "Mei", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Nanie", "Nannij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Nanij", "Nannij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Neij", "Nei", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Orianne", "Orianna", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Philipina", "Philippina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kwassij", "Kwassie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Porkia", "Portia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Prospeer", "Prosper", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Protesetie", "Protektie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Qua", "Kwa", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Reduktie", "Redaktie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Rosemunda", "Rosamunda", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Rudolph", "Rudolf", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sabanna", "Sabana", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sans Souki", "Sansouki", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sibilla", "Sibelle", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sibille", "Sibelle", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sijbille", "Sibelle", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Skipion", "Skipio", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Soophie", "Sophia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sophie", "Sophia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sussanna", "Susanna", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Troue", "Trone", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Truij", "Trui", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Valentin", "Valentijn", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Afie", "Affie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Wilhelmine", "Wilhelmina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Willemijnte", "Wilhelmijntje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Anna NoI", "Anna I", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Boenhatie", "Bonhati", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Gratia", "Grakia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kastor", "Castor", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kwassiba", "Kwasiba", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Louise", "Louisa", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lukie", "Lukia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Merrie", "Merie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Merij", "Merie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Pietronella", "Pieternella", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Disse", "Diesje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Donquikhot", "Don Quikhot", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bebe of Elisabeth-", "Bebe of Elisabeth", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Belona", "Bellona", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Minverva", "Minerva", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Abigail", "Abigael", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Adam I-E", "Adam I", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Adjuba of Ouroe mama", "Adjuba of Ouroemama", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Agnisia", "Agnesia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Akuba", "Akoeba", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Aldolphina", "Adolphina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Mikhella", "Mikhilla", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Anthoinette", "Antoinetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Antoinete", "Antoinetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Anthoinetta", "Antoinetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Appollonia", "Apollonia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Arabie s b", "Arabie Z B", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Ariijtte", "Arijette", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Astria", "Astrea", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Avantuur", "Avontuur", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Baronnesse", "Baronesse", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("BeBe", "Bebe", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bebe'", "Bebe", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Benskhop", "Bentshope", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bestsij", "Betsij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bien / Mattau /", "Bien Mattau", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bien (Mattau)", "Bien Mattau", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Bien (Mattau", "Bien Mattau", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Boentem", "Boentim", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Bokkij", "Bokkie", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Brunetta", "Brunette", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Cakilia", "Cekilia", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Calista", "Calesta", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Casar", "Caesar", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Catharina Ceilia", "Catharina Cecilia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Cekelia", "Cekilia", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Chagrin", "Chagrijn", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Charles Ie", "Charles I", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Charlote", "Charlotte", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Cobbet", "Cobett", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Corridon", "Corredon", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Driesse", "Driesje", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Dutru", "Dutrie", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Eduard Charles", "Eduard of Charles", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Zilia", "Julia", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Driesse", "Driesje", Dataset_Standardization$Naam) 
Dataset_Standardization$Naam <- gsub("Februarij / Monjo /", "Februarij (Monjo)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Februarij ( Monjo", "Februarij (Monjo)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Ferdinand (Skran", "Ferdinand (Skran)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Ferdinand / Skraw /", "Ferdinand (Skran)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Ferdinand / Skran", "Ferdinand (Skran)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Finette / Babi", "Finette (Babe)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Finette ( Babe )", "Finette (Babe)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frankonee", "Frankonie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frans / Werke /", "Frans Werke", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frans (Werke)", "Frans Werke", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frans (Werke", "Frans Werke", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frederik / Pikien", "Frederik (Pikien)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Frideriki", "Friderike", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Georgeana", "Georgeane", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Gidion", "Gideon", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Hednrik", "Hendrik", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Henrietta A C-B", "Henrietta ACB", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Herkulus", "Herkules", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Herrietta", "Henrietta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Hisse", "Hisje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Interest", "Intrest", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Isodore", "Isidore", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("JanI", "Jan I", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jan_Baas", "Janbaas", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Janetsar", "Janitsar", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jemmoth", "Jemmott", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jetta / sw. /", "Jetta (Zw)", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Joahnnes", "Johannes", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Johhanna III", "Johanna III", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Jolikoeur", "Jolikour", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Julianna", "Juliana", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kaptein", "Kapitein", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Trobie", "Frobie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("kortevreugd", "Kortevreugd", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kwaqoe", "Kwakoe", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Kwasie", "Kwassie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("LaRose", "La Rose", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lakkeij", "Lakeij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lakui", "Lakeij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("LIndor", "Lindor", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Livia-Ann", "Livia Ann", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Lukretie", "Lukretia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("MakCreath", "Mak Creath", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("MakLean", "Mak Lean", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Manniel", "Mannuel", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Good lukk", "Goodlukk", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Monplekier", "Mon Plaisir", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Mortimor", "Mortimer", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Nooitgedagt", "Nooitgedakht", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Norman", "Norma", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Adjoeba", "Adjuba", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Ophelia", "Ophilia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("PIetje", "Pietje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Pamilla", "Pamella", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Petrronella", "Petronella", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Phlipp", "Philipp", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Phoeboe", "Phoebe", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Phoenij", "Phoenix", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Pierro", "Pierre", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Pikunpaij", "Pikienpaij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Primier", "Premier", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Prinses", "Prinkes", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Resolutie", "Revolutie", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Robneij", "Rodneij", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Rodolph", "Rudolph", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Rudoloph", "Rudolph", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Roseta", "Rosetta", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sans souki", "Sanssouki", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("SansSouki", "Sanssouki", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sansouki", "Sanssouki", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sanssourki", "Sanssouki", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sara KL", "Sara kl", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sekonde", "Sekondo", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Selena", "Selina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Solen", "Solon", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sontem", "Sontim", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Stilla", "Stella", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Sumon", "Simon", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Tarlijne", "Tartijne", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Thelemakhus", "Telemakhus", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Theodurus", "Theodorus", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Theresea", "Theresia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Tuitje", "Truitje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Vigilant", "Vigelant", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Washinton", "Washington", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Wellim", "Welliam", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Willliam", "William", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Zaturdag", "Zaterdag", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("ZwaantJe", "Zwaantje", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("VanThol", "Van Thol", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Catharine", "Catharina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Derk", "Dirk", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Abeni of Abena", "Abini of Abena", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Josephine", "Josephina", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Geertruida", "Gertruida", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Garkia", "Grakia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Apolonia", "Appolonia", Dataset_Standardization$Naam)
Dataset_Standardization$Naam <- gsub("Husar", "Husaar", Dataset_Standardization$Naam)


Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 66136] <- "Lukretie of Kwasiba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 69022] <- "Moses of Christiaan" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 66471] <- "Onverwakht of Interest" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 93670] <- "Reinhardus of Washington" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 66943] <- "Stephens of Stephanus" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 68391] <- "Stephens of Stephanus" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 64171] <- "Toetoe of Fortuna" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 62298] <- "Winst of Johannes Gottfredt" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 65694] <- "Hannibal of Kapitein" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 95606] <- "Premiere of Jaba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 83555] <- "Virginie of Johanna" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 97312] <- "Albertina" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 103495] <- "Flinkk" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140501] <- "Gerrit Akroemioe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 153122] <- "Gerrit Akroemioe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137616] <- "Gerrit Akroemioe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140486] <- "Heintje Aransoen" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118234] <- "Heintje Aransoen" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137600] <- "Heintje Aransoen"
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140490] <- "Jakobus Lotto"
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137604] <- "Jakobus Lotto" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 120567] <- "Jansje kl" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 154136] <- "Jansje ZB" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140493] <- "Jantje Kwassi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118238] <- "Jantje Kwassi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137607] <- "Jantje Kwassi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140487] <- "Jaques Asembo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118235] <- "Jaques Asembo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137601] <- "Jaques Asembo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140495] <- "Johannes Dima" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118240] <- "Johannes Dima" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137609] <- "Johannes Dima" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140489] <- "Junij Takroewenti" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137603] <- "Junij Takroewenti" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 149512] <- "Kettie II" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 149495] <- "Kettie I" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 126547] <- "Kleine Verwakhting" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140504] <- "Koba Abramba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 153123] <- "Koba Abramba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137621] <- "Koba Abramba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 111294] <- "L'Esperanke" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 149274] <- "Lena Bosi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 153135] <- "Lena Bosi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137635] <- "Lena Bosi" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140507] <- "Lotje Adjuba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 153125] <- "Lotje Adjuba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137624] <- "Lotje Adjuba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 160271] <- "Maria I" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 155890] <- "Onverwakht I" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140488] <- "Philip Akote" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137602] <- "Philip Akote" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140485] <- "Primo Gienda" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137599]<- "Primo Gienda" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 110717] <- "Wilhelmina Affiba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 155617] <- "Wilhelmina Affiba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 110714] <- "Wilhelmina Jaba" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140496] <- "Willem Peroe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118241] <- "Willem Peroe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137611] <- "Willem Peroe" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140498] <- "William Lopo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118243] <- "William Lopo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137613] <- "William Lopo" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 159445] <- "William Gerhardus" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 118244] <- "Wimpie Aboeka" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 140499] <- "Wimpie Aboeka" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 137614] <- "Wimpie Aboeka" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 159716] <- "Winst II" 
Dataset_Standardization$Naam[Dataset_Standardization$primary_key == 132277] <- "Bebe II" 



##########################
#### 3.) Mother names ####
##########################

Dataset_Standardization$Moeder <- gsub("1e", "1", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("2e", "2", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("2 e", "2", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("2-E", "2", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("1 e", "1", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("cc", "c", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("c", "k", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("z", "s", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("gr.", "gr", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub(" (overleden)", "", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("(overleden)", "", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Vrij", "vrij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("/vrij/", "(vrij)", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Z B", "ZB", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("A C B", "ACB", Dataset_Standardization$Moeder)

Dataset_Standardization$Moeder[Dataset_Standardization$Moeder == "Onbekend"] <- ""
Dataset_Standardization$Moeder[Dataset_Standardization$Moeder == "onbekend"] <- ""
Dataset_Standardization$Moeder[Dataset_Standardization$Moeder == "Onbkend"] <- ""
Dataset_Standardization$Moeder[Dataset_Standardization$Moeder == "Overleden"] <- "" 
Dataset_Standardization$Moeder[Dataset_Standardization$Moeder == "overleden"] <- "" 

Dataset_Standardization$Moeder <- gsub("Marie", "Maria", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Louise", "Louisa", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Hannah", "Hanna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Magdalijntje", "Madelijntje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cateau", "Kato", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Coba", "Jakoba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Boenhatie", "Boenhatti", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sanne", "Sanna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Primiere (overleden)", "Premiere (overleden)", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Calijpson (overleden)", "Calipson (overleden)", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Constantie", "Constantia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Frederika I", "Frederika 1", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Maritie", "Mariatje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jansie", "Jansje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Luxsiana", "Lousiana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Luijsiana", "Lousiana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Theresia", "Theresa", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Mathilde", "Mathilda", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anthoinette", "Antoinetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anna(overleden)", "Anna (overleden)", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Merrie", "Merie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Merij", "Merie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cato", "Kato", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Saraatje", "Saratje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Annette", "Anette", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Charlotte(overleden)", "Charlotte (overleden)", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Doesesie", "Dukhesse", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Fiana(overleden)", "Fiana (overleden)", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Afie", "Affie", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Farie", "Tarie", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Farij", "Tarie", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Philles", "Phillis", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Porkia", "Portia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Kwassiba of Johanna", "Kwasiba of Johanna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Amelia", "Amalia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("L J B Lindveld", "Louisa Jeannette B Lindveld", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Ester", "Esther", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Wilhelmine", "Wilhelmina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Prinkes", "Prinses", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("l'Esperanke", "L'Esperanke", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jebrinha", "Jebrinka", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jebrenka", "Jebrinka", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Clarisje", "Clarisse", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sabanna", "Sabana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anna / vrij /", "Anna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Zephir", "Ziphir", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Henriette", "Henrietta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Patientie /", "Patientie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sallij", "Salij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Gratie", "Gratia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Selvie", "Silvie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Olijmphia", "Olimphia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Rosette Elisabth Haan", "Rosette Elis Haan", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Calestha", "Calista", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Dokesse", "Dukhesse", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Nannij", "Nanie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Laque", "Lague", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Semiere", "Semire", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anette", "Annette", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Serire", "Servie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Trui A C B", "Trui ACB", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anthoinetta", "Antoinetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Simire", "Semire", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betseij", "Betsij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Finisfe", "Finisse", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Quasfiba", "Quassiba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Kwassiba", "Quassiba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Willemijntje", "Wilhelmijntje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Philiva", "Philida", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Marianna", "Mariana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Truijtje", "Truitje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jansje 2 geb in 1824)", "Jansje 2", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Pense", "Panse", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Catherina", "Catharina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jeanette", "Jeannette", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Fransina", "Frankina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Elisabeth Wensum", "Elisabeth Wensina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Premierre", "Premiere", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Brandinaover", "Brandina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Louisa 1", "Louisa I", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("CleopatraKl", "Cleopatra", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Mereij", "Merie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Petronellea", "Petronella", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betije", "Betje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Rhilippina", "Philippina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Abonnie", "Abennie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Luka", "Lukia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Souvenier", "Souvenir", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Simirie", "Semirie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Thousia", "Theresa", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Radina", "Badina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Memie", "Mimie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Margaretta", "Margaretha", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Hoemansie", "Hoemensie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Milobise", "Milobiso", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Luukretie", "Lukretie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Caekilia", "Cekilia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cakilia", "Cekilia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Ciekilia", "Cekilia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Falo", "Palo", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Margarietta", "Margaretha", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Adrianna", "Adriana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Mosoe", "Majoe", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Julianna", "Juliana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Brasserree", "Bresserre", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Mikettie", "Miketta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Xjangie", "Njangie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Hendrina 1", "Hendrina I", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cathanna", "Catharina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anna )", "Anna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("JettaZw", "Jetta Zw", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jetta ( Zw", "Jetta Zw", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jetta (Zw)", "Jetta Zw", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jettasw", "Jetta Zw", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betsiba", "Betseba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betinia", "Betenia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("De vrije Simiere van Valier", "De vrije Semire van Valier", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("de vrije Semire van Valier", "De vrije Semire van Valier", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Truij, ACB", "Truij ACB", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Truij A C B", "Truij ACB", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cossie", "Cossia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Catrentje", "Catreintje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("La Tona", "La Tonna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sabelie", "Jabelie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("AsemiaKl", "Asemia kl", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Asemia ( Kl )", "Asemia kl", Dataset_Standardization$Moeder) 
Dataset_Standardization$Moeder <- gsub("Toenba", "Toemba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Luno", "Luna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Plaisiere", "Plaisiera", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Aspakia", "Aspasia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Tontje", "Fontje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Okane", "Okeane", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Candaki", "Candatie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Clarinda", "Clarina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("WilhelminaAffiba", "Wilhelmina ( Affiba )", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Wilhelmijntjeover.", "Wilhelmijntje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Tannij", "Fannij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cekilia", "Cikilia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Calesta", "Calista", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Celestinal", "Celestina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Pauline", "Paulina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Emeline", "Emelina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Madelientje", "Madeleintje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Onbekend ( Afrikaan)", "Onbekend (Afrikaan)", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Astrea", "Astria", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Rephaela", "Rhephaela", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Olijmpia", "Olimphia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Finissie", "Finette", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Kroebooij", "Kroeboij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Antonette", "Antonetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Silphinie", "Selphinie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Lorentie", "Lorentia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Lukretie", "Lukretia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sijsje", "Lijsje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Antonetta", "Antoinetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Dalie", "Dalia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anklika", "Angelika", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Anelika", "Angelika", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Amerentie", "Amarantie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Katharine", "Katharina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Poulina", "Paulina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Philippa", "Philippina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Classina", "Clasina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Isabelle", "Isabella", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Popolje", "Popotje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betshda", "Bethseba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Assetta", "Asetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Constantia", "Constansia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Adriaantje", "Adriantje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Soresoe", "Sereese", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Dannij", "Fannij", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Besje", "Bessie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Betseba", "Bethseba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Okasee", "Okeane", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Seraphine", "Seraphina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sina /", "Sina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Philppina", "Philippina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Antje )", "Antje", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Caluta", "Calista", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jansje 1", "Jansje I", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Christiana", "Christina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Sabana /", "Sabana", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Phiena", "Thiena", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Catharine", "Catharina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Fredrika", "Frederika", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jaja", "Jaba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Primiere", "Premiere", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Iona", "Jona", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Agatha)", "Agatha", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Amenta", "Aminta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Fuba", "Tuba", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Bandina", "Brandina", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("La Tonna", "La Donna", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Prinses", "Prinsesse", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Aretta", "Asetta", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Foetoe", "Toetoe", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Jabellie", "Jabelie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Wennie", "Winnie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Cosia", "Cossia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Akoeja", "Akoesa", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("patientie", "Patientie", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Garkia", "Grakia", Dataset_Standardization$Moeder)
Dataset_Standardization$Moeder <- gsub("Apolonia", "Appolonia", Dataset_Standardization$Moeder)

Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==95307] <- "Jansje II"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==65299 ] <- "Tarie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==65362 ] <- "Tarie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==65300 ] <- "Tarie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==66186 ] <- "Joha Catharina"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==83557 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==83551 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==83552 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==69035 ] <- "Jaaba of Jacoba"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==68402 ] <- "Petronella 1"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==69645 ] <- "Saratje"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==97312 ] <- "Lukretie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==107288 ] <- "Christina"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==103520 ] <- "Alijda Philippina (ook genaamd Alijda)"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==141483 ] <- "Kaatje"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==150178 ] <- "Charlotte P"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==150179 ] <- "Charlotte P"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==141482 ] <- "Flora"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==107290 ] <- "Christina"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==99409 ] <- "Wilhelmina Barth"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==102078 ] <- "Antje 2"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==141479 ] <- "Martha"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==143345 ] <- "Jansje 2"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==107297 ] <- "Wilhelmina"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==98902 ] <- "Besie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==158603 ] <- "Besie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==98901 ] <- "Besie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==99439 ] <- "Dora Batavia"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==103638 ] <- "Dora Batavia"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==158860 ] <- "Dora Batavia"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==103647 ] <- "Dora Batavia"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==132146 ] <- "Jansje"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==116404 ] <- "Amerika"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==132210 ] <- "Monkie"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==149457 ] <- "Lena"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==149303 ] <- "Doortje"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==132173 ] <- "Jansje"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==110835 ] <- "Amarentha"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==161086 ] <- "Premiere"
Dataset_Standardization$Moeder[Dataset_Standardization$primary_key==99660 ] <- "Premiere"

#4.) Mother names 2

Dataset_Standardization$Moeder_2 <- gsub("1e", "1", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("2e", "2", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("2 e", "2", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("2-E", "2", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("1 e", "1", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("cc", "c", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("c", "k", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("z", "s", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("gr.", "gr", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub(" (overleden)", "", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("(overleden)", "", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Vrij", "vrij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("/vrij/", "(vrij)", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Z B", "ZB", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("A C B", "ACB", Dataset_Standardization$Moeder_2)

replace Moeder_2 = "" if Moeder_2 == "Onbekend"
replace Moeder_2 = "" if Moeder_2 == "onbekend"
replace Moeder_2 = "" if Moeder_2 == "Onbkend"
replace Moeder_2 = "" if Moeder_2 == "Overleden"
replace Moeder_2 = "" if Moeder_2 == "overleden"

Dataset_Standardization$Moeder_2 <- gsub("Marie", "Maria", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Louise", "Louisa", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Hannah", "Hanna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Magdalijntje", "Madelijntje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cateau", "Kato", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Coba", "Jakoba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Boenhatie", "Boenhatti", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sanne", "Sanna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Primiere (overleden)", "Premiere (overleden)", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Calijpson (overleden)", "Calipson (overleden)", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Constantie", "Constantia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Frederika I", "Frederika 1", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Maritie", "Mariatje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jansie", "Jansje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Luxsiana", "Lousiana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Luijsiana", "Lousiana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Theresia", "Theresa", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Mathilde", "Mathilda", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anthoinette", "Antoinetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anna(overleden)", "Anna (overleden)", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Merrie", "Merie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Merij", "Merie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cato", "Kato", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Saraatje", "Saratje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Annette", "Anette", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Charlotte(overleden)", "Charlotte (overleden)", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Doesesie", "Dukhesse", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Fiana(overleden)", "Fiana (overleden)", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Afie", "Affie", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Farie", "Tarie", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Farij", "Tarie", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Philles", "Phillis", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Porkia", "Portia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Kwassiba of Johanna", "Kwasiba of Johanna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Amelia", "Amalia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("L J B Lindveld", "Louisa Jeannette B Lindveld", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Ester", "Esther", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Wilhelmine", "Wilhelmina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Prinkes", "Prinses", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("l'Esperanke", "L'Esperanke", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jebrinha", "Jebrinka", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jebrenka", "Jebrinka", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Clarisje", "Clarisse", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sabanna", "Sabana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anna / vrij /", "Anna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Zephir", "Ziphir", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Henriette", "Henrietta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Patientie /", "Patientie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sallij", "Salij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Gratie", "Gratia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Selvie", "Silvie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Olijmphia", "Olimphia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Rosette Elisabth Haan", "Rosette Elis Haan", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Calestha", "Calista", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Dokesse", "Dukhesse", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Nannij", "Nanie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Laque", "Lague", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Semiere", "Semire", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anette", "Annette", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Serire", "Servie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Trui A C B", "Trui ACB", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Antoinette", "Antoinetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anthoinetta", "Antoinetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Simire", "Semire", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betseij", "Betsij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Finisfe", "Finisse", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Quasfiba", "Quassiba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Kwassiba", "Quassiba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Willemijntje", "Wilhelmijntje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Philiva", "Philida", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Marianna", "Mariana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Truijtje", "Truitje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jansje 2 geb in 1824)", "Jansje 2", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Pense", "Panse", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Catherina", "Catharina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jeanette", "Jeannette", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Fransina", "Frankina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Elisabeth Wensum", "Elisabeth Wensina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Premierre", "Premiere", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Brandinaover", "Brandina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Louisa 1", "Louisa I", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("CleopatraKl", "Cleopatra", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Mereij", "Merie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Petronellea", "Petronella", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betije", "Betje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Rhilippina", "Philippina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Abonnie", "Abennie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Luka", "Lukia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Souvenier", "Souvenir", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Simirie", "Semirie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Thousia", "Theresa", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Radina", "Badina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Memie", "Mimie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Margaretta", "Margaretha", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Hoemansie", "Hoemensie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Milobise", "Milobiso", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Luukretie", "Lukretie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Caekilia", "Cekilia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cakilia", "Cekilia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Ciekilia", "Cekilia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Falo", "Palo", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Margarietta", "Margaretha", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Adrianna", "Adriana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Mosoe", "Majoe", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Julianna", "Juliana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Brasserree", "Bresserre", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Mikettie", "Miketta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Xjangie", "Njangie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Hendrina 1", "Hendrina I", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cathanna", "Catharina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anna )", "Anna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("JettaZw", "Jetta Zw", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jetta ( Zw", "Jetta Zw", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jetta (Zw)", "Jetta Zw", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jettasw", "Jetta Zw", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betsiba", "Betseba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betinia", "Betenia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("De vrije Simiere van Valier", "De vrije Semire van Valier", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("de vrije Semire van Valier", "De vrije Semire van Valier", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Truij, ACB", "Truij ACB", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Truij A C B", "Truij ACB", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cossie", "Cossia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Catrentje", "Catreintje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("La Tona", "La Tonna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sabelie", "Jabelie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("AsemiaKl", "Asemia kl", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Asemia ( Kl )", "Asemia kl", Dataset_Standardization$Moeder_2) 
Dataset_Standardization$Moeder_2 <- gsub("Toenba", "Toemba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Luno", "Luna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Plaisiere", "Plaisiera", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Aspakia", "Aspasia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Tontje", "Fontje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Okane", "Okeane", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Candaki", "Candatie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Clarinda", "Clarina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("WilhelminaAffiba", "Wilhelmina ( Affiba )", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Wilhelmijntjeover.", "Wilhelmijntje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Tannij", "Fannij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cekilia", "Cikilia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Calesta", "Calista", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Celestinal", "Celestina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Pauline", "Paulina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Emeline", "Emelina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Madelientje", "Madeleintje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Onbekend ( Afrikaan)", "Onbekend (Afrikaan)", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Astrea", "Astria", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Rephaela", "Rhephaela", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Olijmpia", "Olimphia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Finissie", "Finette", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Kroebooij", "Kroeboij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Antonette", "Antonetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Silphinie", "Selphinie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Lorentie", "Lorentia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Lukretie", "Lukretia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sijsje", "Lijsje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Antonetta", "Antoinetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Dalie", "Dalia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anklika", "Angelika", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Anelika", "Angelika", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Amerentie", "Amarantie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Katharine", "Katharina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Poulina", "Paulina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Philippa", "Philippina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Classina", "Clasina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Isabelle", "Isabella", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Popolje", "Popotje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betshda", "Bethseba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Assetta", "Asetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Constantia", "Constansia", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Adriaantje", "Adriantje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Soresoe", "Sereese", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Dannij", "Fannij", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Besje", "Bessie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Betseba", "Bethseba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Okasee", "Okeane", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Seraphine", "Seraphina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sina /", "Sina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Philppina", "Philippina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Antje )", "Antje", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Caluta", "Calista", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jansje 1", "Jansje I", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Christiana", "Christina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Sabana /", "Sabana", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Phiena", "Thiena", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Catharine", "Catharina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Fredrika", "Frederika", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jaja", "Jaba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Primiere", "Premiere", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Iona", "Jona", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Agatha)", "Agatha", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Amenta", "Aminta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Fuba", "Tuba", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Bandina", "Brandina", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("La Tonna", "La Donna", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Prinses", "Prinsesse", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Aretta", "Asetta", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Foetoe", "Toetoe", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Jabellie", "Jabelie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Wennie", "Winnie", Dataset_Standardization$Moeder_2)
Dataset_Standardization$Moeder_2 <- gsub("Cosia", "Cossia", Dataset_Standardization$Moeder_2)

Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==95307] <- "Jansje II"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==65299 ] <- "Tarie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==65362 ] <- "Tarie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==65300 ] <- "Tarie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==66186 ] <- "Joha Catharina"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==83557 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==83551 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==83552 ] <- "Virginia of Johanna"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==69035 ] <- "Jaaba of Jacoba"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==68402 ] <- "Petronella 1"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==69645 ] <- "Saratje"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==97312 ] <- "Lukretie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==107288 ] <- "Christina"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==103520 ] <- "Alijda Philippina (ook genaamd Alijda)"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==141483 ] <- "Kaatje"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==150178 ] <- "Charlotte P"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==150179 ] <- "Charlotte P"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==141482 ] <- "Flora"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==107290 ] <- "Christina"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==99409 ] <- "Wilhelmina Barth"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==102078 ] <- "Antje 2"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==141479 ] <- "Martha"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==143345 ] <- "Jansje 2"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==107297 ] <- "Wilhelmina"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==98902 ] <- "Besie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==158603 ] <- "Besie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==98901 ] <- "Besie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==99439 ] <- "Dora Batavia"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==103638 ] <- "Dora Batavia"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==158860 ] <- "Dora Batavia"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==103647 ] <- "Dora Batavia"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==132146 ] <- "Jansje"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==116404 ] <- "Amerika"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==132210 ] <- "Monkie"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==149457 ] <- "Lena"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==149303 ] <- "Doortje"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==132173 ] <- "Jansje"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==110835 ] <- "Amarentha"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==161086 ] <- "Premiere"
Dataset_Standardization$Moeder_2[Dataset_Standardization$primary_key==99660 ] <- "Premiere"


#write_dta("Preparing_Record_Linkage.dta")





