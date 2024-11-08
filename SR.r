library("data.table"); library(dplyr)
rm(list=ls())

setwd("/home/tuinschepje/Surfdrive/CLARIAH/Thunnis/SR")

SR <- fread("Dataset Suriname Slave and Emancipation Registers Version 1.1.csv", encoding="UTF-8")
SR <- SR[order(SR$Id_source),]
Plantations <- fread("Suriname Plantation Dataset Version 1.0.csv", encoding="UTF-8")

SR$Id_source <- ifelse(SR$Typeregister=="Emancipation Register", paste0("ER", SR$Id_person), SR$Id_source)
SR$Id_folio <- ifelse(SR$Typeregister=="Emancipation Register", "ER", paste(SR$Serieregister, SR$Inventory_number, SR$Folio_number, sep="_"))
SR$gender <- ifelse(tolower(SR$Sex)=="male", "sdo:Male",
                    ifelse(tolower(SR$Sex)=="female", "sdo:Female", NA))
SR$Occupation <- gsub("\\\\", "", SR$Occupation)
SR$Name_owner <- gsub("\\\\", "", SR$Name_owner)
SR$birthDate <- ifelse(!is.na(SR$Year_birth) & !is.na(SR$Month_birth) & !is.na(SR$Day_birth), paste0('"', SR$Year_birth, "-", sprintf("%02d", SR$Month_birth), "-", sprintf("%02d", SR$Day_birth), '"^^xsd:date'), NA)
SR$birthYear <- paste0('"', SR$Year_birth, '"^^xsd:gYear')
SR$deathDate <- ifelse(SR$EndEntryEvent=="Death", paste0('"', SR$EndEntryYear, "-", sprintf("%02d", SR$EndEntryMonth), "-", sprintf("%02d", SR$EndEntryDay), '"^^xsd:date'), NA)
SR$deathYear <- paste0('"', SR$Year_death, '"^^xsd:gYear')
SR$startDate <- ifelse(is.na(SR$StartEntryDay) & is.na(SR$StartEntryMonth), 
                       paste0('"', SR$StartEntryYear, '"^^xsd:gYear'),
                       paste0('"', SR$StartEntryYear, "-", sprintf("%02d", SR$StartEntryMonth), "-", sprintf("%02d", SR$StartEntryDay), '"^^xsd:date'))
SR$endDate <- ifelse(is.na(SR$EndEntryDay) & is.na(SR$EndEntryMonth), 
                     paste0('"', SR$EndEntryYear, '"^^xsd:gYear'),
                     paste0('"', SR$EndEntryYear, "-", sprintf("%02d", SR$EndEntryMonth), "-", sprintf("%02d", SR$EndEntryDay), '"^^xsd:date'))
SR$EndEntryEvent2 <- ifelse(SR$Typeregister=="Emancipation Register" | SR$EndEntryEvent=="Freedom", "ed:Q281",
                            ifelse(SR$EndEntryEvent=="Death", "ed:Q148",
                            ifelse(SR$EndEntryEvent=="Diseased", "hdsc:Diseased",
                            ifelse(SR$EndEntryEvent=="Ended", "ed:Q250",
                            ifelse(SR$EndEntryEvent=="Escaped", "ed:Q154",
                            ifelse(SR$EndEntryEvent=="Transferred", "ed:Q153",
                            ifelse(SR$EndEntryEvent=="Unknown", "hdsc:Unknown",
                            ifelse(SR$EndEntryEvent=="Written off", "hdsc:WrittenOff",
                            NA ))))))))
SR$StartEntryEvent2 <- ifelse(SR$Typeregister=="Emancipation Register", "ed:Q281",
                              ifelse(SR$StartEntryEvent=="Birth", "ed:Q147",
                              ifelse(SR$StartEntryEvent=="Start Series", "ed:Q250",
                              ifelse(SR$StartEntryEvent=="Transferred", "ed:Q153",
                              ifelse(SR$StartEntryEvent=="Unknown", "hdsc:Unknown", 
                              NA )))))
#Id_mother
SR$Id_person_mother <- ifelse(SR$Name_mother!="", paste(SR$Id_person, "mother", sep="_"), NA)
SR$Id_source_mother <- ifelse(SR$Name_mother!="", paste(SR$Id_source, "mother", sep="_"), NA)
#Id_owner
SR$Id_owner <- ifelse(SR$Name_owner!=lag(SR$Name_owner) | SR$Name_owner==lag(SR$Name_owner) & SR$Inventory_number!=lag(SR$Inventory_number) | is.na(lag(SR$Name_owner)), 
                      ifelse(SR$Typeregister=="Emancipation Register", paste0("owner_", SR$Id_person), paste0("owner_", SR$Id_folio)),
                      NA)
repeat{
    SR$Id_owner <- ifelse(is.na(SR$Id_owner), lag(SR$Id_owner), SR$Id_owner)
    if(length(which(is.na(SR$Id_owner)))==0){break}
}
#lag & lead
SR$vorige <- ifelse(SR$Name_owner==lag(SR$Name_owner), lag(SR$Id_source), NA)
SR$volgende <- ifelse(SR$Name_owner==lead(SR$Name_owner), lead(SR$Id_source), NA)
#filter unique sources
SR_source <- SR[SR$Typeregister %in% c("Slave register plantation", "Slave register private owner"),]
#filter known mothers
Moeders <- SR[SR$Name_mother!="",]
#filter unique owner observations
SR_owner <- SR[SR$Name_owner!=lag(SR$Name_owner),]
SR_owner_ER <- SR[SR$Typeregister=="Emancipation register",]
SR_owner_private <- SR[SR$Typeregister=="Slave register private owner",]
SR_owner_private <- SR[SR$Typeregister=="Slave register plantation",]
#split registrations by source type
ER <- SR[SR$Typeregister=="Emancipation Register",]
Private <- SR[SR$Typeregister=="Slave register private owner",]
Plantation <- SR[SR$Typeregister=="Slave register plantation",]
#list person observations per person reconstruction
Reconstruction <- SR[, c("Id_person", "Id_source")] %>% group_by(Id_person) %>% filter(row_number()==1)
colnames(Reconstruction) <- c("Id_person", "Id_source1")
for(i in 2:11) {
    ff <- SR[, c("Id_person", "Id_source")] %>% group_by(Id_person) %>% filter(row_number()==i)
    colnames(ff) <- c("Id_person", paste0("Id_source", i))
    Reconstruction <- merge(Reconstruction, ff, by="Id_person", all.x=T)
}
Reconstruction$Id_mother <- ifelse(Reconstruction$Id_person %in% Moeders$Id_person, paste0(Reconstruction$Id_person, "_mother"), NA)
Reconstruction$Id_source_vector <- paste0("hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source1, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source2, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source3, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source4, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source5, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source6, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source7, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source8, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source9, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source10, 
                                          ", hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source11)
Reconstruction$Id_source_vector <- gsub(", hdsc:SR-Suriname\\\\/observation\\\\/NA", "", Reconstruction$Id_source_vector)
#list person observations per person reconstruction for mothers
Reconstruction_mother <- Moeders[, c("Id_person_mother", "Id_source_mother")] %>% group_by(Id_person_mother) %>% filter(row_number()==1)
colnames(Reconstruction_mother) <- c("Id_person_mother", "Id_source_mother1")
for(i in 2:9) {
    ff <- Moeders[, c("Id_person_mother", "Id_source_mother")] %>% group_by(Id_person_mother) %>% filter(row_number()==i)
    colnames(ff) <- c("Id_person_mother", paste0("Id_source_mother", i))
    Reconstruction_mother <- merge(Reconstruction_mother, ff, by="Id_person_mother", all.x=T)
}
Reconstruction_mother$Id_source_vector <- paste0("hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother[,2:10])
Reconstruction_mother$Id_source_vector <- paste0("hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother1, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother2,
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother3,
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother4, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother5, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother6, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother7, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother8, 
                                                 ", hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_mother9)
Reconstruction_mother$Id_source_vector <- gsub(", hdsc:SR-Suriname\\\\/observation\\\\/NA", "", Reconstruction_mother$Id_source_vector)


#namespaces
prefix <- "@prefix ed:    <http://lod.enslaved.org/entity/> .
@prefix hdsc:  <https://www.ru.nl/hdsc/example/> .
@prefix pico:  <https://personsincontext.org/model#> .
@prefix picot: <https://terms.personsincontext.org/> .
@prefix pnv:   <https://w3id.org/pnv#> .
@prefix prov:  <http://www.w3.org/ns/prov#> .
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix sdo:   <https://www.schema.org/> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
"

#sources
source_ttl_SR <- paste0("hdsc:SR-Suriname\\/source\\/", SR_source$Id_folio, " 
    a sdo:ArchiveComponent ; 
    sdo:name \"", SR_source$Typeregister, " ", SR_source$Serieregister, " inv ", SR_source$Inventory_number, " folio ", SR_source$Folio_number, "\" ; 
    sdo:locationCreated \"", "Paramaribo", "\" ; 
    sdo:dateCreated \"", SR_source$Serieregister, "\" ; 
    #sdo:image <", SR_source$Scan, "> ; 
    sdo:holdingArchive <https://www.nationaalarchief.nl> .
")
source_ttl_ER <- paste0("hdsc:SR-Suriname\\/source\\/", "ER", " 
    a sdo:ArchiveComponent ; 
    sdo:name \"", " emancipation register Humphrey Lamur ", "\" ; #pp toevoegen
    sdo:locationCreated \"", "Amsterdam", "\" ; 
    sdo:dateCreated \"", 2004, "\"^^xsd:gYear .
")
source_ttl <- c(source_ttl_SR, source_ttl_ER)
rm(source_ttl_SR, source_ttl_ER)


#person observations
observation_ttl_private <- paste0("hdsc:SR-Suriname\\/observation\\/", Private$Id_source, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", Private$Id_folio, " ; 
    pico:hasRole ", "picot:roles\\/522", " ;  #geregistreerde
    hdsc:lag hdsc:SR-Suriname\\/observation\\/", Private$vorige, " ; 
    hdsc:lead hdsc:SR-Suriname\\/observation\\/", Private$volgende, " ; 
    sdo:gender ", Private$gender, " ; 
    sdo:birthDate ", Private$birthDate, " ; 
    sdo:deathDate ", Private$deathDate, " ; 
    pico:hasAge \"", Private$Age, "\"^^xsd:decimal ; 
    sdo:occupation \"", Private$Occupation, "\" ; 
    sdo:name \"", Private$Name_enslaved, "\" ; 
    sdo:givenName \"", Private$Name_enslaved, "\" ; 
    sdo:additionalName [ 
        a pnv:PersonName ; 
        pnv:literalName \"", Private$Name_enslaved, "\" ; 
        pnv:givenName \"", Private$Name_enslaved, "\" ; ] ; 
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    sdo:startDate [ 
        rdf:value ", Private$startDate, " ; 
        ed:P30 ", Private$StartEntryEvent2, " ; ] ; 
    sdo:endDate [ 
        rdf:value ", Private$endDate, " ; 
        ed:P30 ", Private$EndEntryEvent2, " ; ] ; 
    sdo:parent hdsc:SR-Suriname\\/observation\\/", Private$Id_source_mother, " ; 
    hdsc:isEnslavedBy [ 
        rdf:value hdsc:SR-Suriname\\/owner\\/", Private$Id_owner, " ; 
        sdo:startDate ", Private$startDate, " ; 
        sdo:endDate ", Private$endDate, " ; ] .
")
observation_ttl_plantation <- paste0("hdsc:SR-Suriname\\/observation\\/", Plantation$Id_source, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", Plantation$Id_folio, " ; 
    pico:hasRole ", "picot:roles\\/522", " ;  #geregistreerde
    hdsc:lag hdsc:SR-Suriname\\/observation\\/", Plantation$vorige, " ; 
    hdsc:lead hdsc:SR-Suriname\\/observation\\/", Plantation$volgende, " ; 
    sdo:gender ", Plantation$gender, " ; 
    sdo:birthDate ", Plantation$birthDate, " ; 
    sdo:deathDate ", Plantation$deathDate, " ; 
    pico:hasAge \"", Plantation$Age, "\"^^xsd:decimal ; 
    sdo:occupation \"", Plantation$Occupation, "\" ; 
    sdo:name \"", Plantation$Name_enslaved, "\" ; 
    sdo:givenName \"", Plantation$Name_enslaved, "\" ; 
    sdo:additionalName [ 
        a pnv:PersonName ; 
        pnv:literalName \"", Plantation$Name_enslaved, "\" ; 
        pnv:givenName \"", Plantation$Name_enslaved, "\" ; ] ; 
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person 
    sdo:startDate [ 
        rdf:value ", Plantation$startDate, " ; 
        ed:P30 ", Plantation$StartEntryEvent2, " ; ] ; 
    sdo:endDate [ 
        rdf:value ", Plantation$endDate, " ; 
        ed:P30 ", Plantation$EndEntryEvent2, " ; ] ; 
    sdo:parent hdsc:SR-Suriname\\/observation\\/", Plantation$Id_source_mother, " ; 
    hdsc:isEnslavedBy [ 
        rdf:value hdsc:SR-Suriname\\/owner\\/", Plantation$Id_owner, " ; 
        sdo:startDate ", Plantation$startDate, " ; 
        sdo:endDate ", Plantation$endDate, " ; ] .
")
observation_ttl_ER1 <- paste0("hdsc:SR-Suriname\\/observation\\/", ER$Id_source, "a 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", ER$Id_folio, " ; 
    pico:hasRole ", "picot:roles\\/522", " ;  #geregistreerde
    sdo:gender ", ER$gender, " ; 
    sdo:birthDate ", ER$birthDate, " ; 
    sdo:deathDate ", ER$deathDate, " ; 
    pico:hasAge \"", ER$Age, "\"^^xsd:decimal ; 
    sdo:occupation \"", ER$Occupation, "\" ; 
    sdo:name \"", ER$Name_enslaved, "\" ; 
    sdo:givenName \"", ER$Name_enslaved, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", ER$Name_enslaved, "\" ; 
        pnv:givenName \"", ER$Name_enslaved, "\" ; ] ; 
    ed:P33 ", "ed:Q109", " ;  #hasPersonStatus #enslaved person
    sdo:endDate [ 
        rdf:value ", "\"1863-06-30\"^^xsd:date", " ; 
        ed:P30 ", "ed:Q281", " ; ] ;  #Emancipation or Manumission
    hdsc:isEnslavedBy [ 
        rdf:value hdsc:SR-Suriname\\/owner\\/", ER$Id_owner, "_private", " ; 
        rdf:value hdsc:SR-Suriname\\/owner\\/", ER$Id_owner, "_plantation", " ; 
        sdo:endDate ", "\"1863-06-30\"^^xsd:date", " ; ] .
")
observation_ttl_ER2 <- paste0("hdsc:SR-Suriname\\/observation\\/", ER$Id_source, "b 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", ER$Id_folio, " ; 
    pico:hasRole ", "picot:roles\\/522", " ;  #geregistreerde
    sdo:gender ", ER$gender, " ; 
    sdo:birthDate ", ER$birthDate, " ; 
    sdo:deathDate ", ER$deathDate, " ; 
    pico:hasAge \"", ER$Age, "\"^^xsd:decimal ; 
    sdo:occupation \"", ER$Occupation, "\" ; 
    sdo:name \"", ER$Name_enslaved, "\" ; 
    sdo:givenName \"", ER$Name_enslaved, "\" ; 
    sdo:familyName \"", ER$Name_enslaved, "\" ; 
    sdo:additionalName [ 
        a pnv:PersonName ; 
        pnv:literalName \"", ER$Name_enslaved, "\" ; 
        pnv:givenName \"", ER$Name_enslaved, "\" ; 
        pnv:baseSurname \"", ER$Name_enslaved, "\" ; 
        pnv:surnamePrefix \"", ER$Name_enslaved, "\" ; 
        ] ; 
    ed:P33 ", "ed:Q117", " ;  #hasPersonStatus #free person
    sdo:startDate [ 
        rdf:value ", "\"1863-07-01\"^^xsd:date", " ; 
        ed:P30 ", "ed:Q281", " ; ] .  #Emancipation or Manumission
")
observation_ttl <- c(observation_ttl_private, observation_ttl_plantation, observation_ttl_ER1, observation_ttl_ER2)
rm(observation_ttl_private, observation_ttl_plantation, observation_ttl_ER1, observation_ttl_ER2)
#doopnaam
#ER: family_relations, family_name
observation_ttl <- gsub(";  #geregistreerde
    hdsc:lag hdsc:SR-Suriname\\\\/observation\\\\/NA ", "", observation_ttl)
observation_ttl <- gsub("; 
    hdsc:lead hdsc:SR-Suriname\\\\/observation\\\\/NA ", "", observation_ttl)
observation_ttl <- gsub("; 
    sdo:gender NA ", "", observation_ttl)
observation_ttl <- gsub(";  #geregistreerde
    sdo:gender NA ", "", observation_ttl)
observation_ttl <- gsub("; 
    sdo:birthDate NA ", "", observation_ttl)
observation_ttl <- gsub("; 
    sdo:deathDate NA ", "", observation_ttl)
observation_ttl <- gsub("\"NA\"..xsd:gYear", "\"NA\"", observation_ttl)
observation_ttl <- gsub("; 
    pico:hasAge \"NA\"..xsd:decimal ", "", observation_ttl)
observation_ttl <- gsub("; 
    sdo:occupation \"\" ", "", observation_ttl)
observation_ttl <- gsub("; 
    sdo:parent hdsc:SR-Suriname\\\\/observation\\\\/NA ", "", observation_ttl)


#mothers
mothers_ttl <- paste0("hdsc:SR-Suriname\\/observation\\/", Moeders$Id_source_mother, " 
    a pico:PersonObservation ; 
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", Moeders$Id_folio, " ; 
    sdo:name \"", Moeders$Name_mother, "\" ; 
    sdo:givenName \"", Moeders$Name_mother, "\" ; 
    sdo:additionalName [
        a pnv:PersonName ; 
        pnv:literalName \"", Moeders$Name_mother, "\" ; 
        pnv:givenName \"", Moeders$Name_mother, "\" ; ] ; 
    sdo:gender ", "sdo:Female", " ; 
    sdo:children hdsc:SR-Suriname\\/observation\\/", Moeders$Id_source, " .
")


#owners
owners_ttl_SR_private <- paste0("hdsc:SR-Suriname\\/owner\\/", SR_owner$Id_owner, " 
    a pico:PersonObservation ;
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", SR_owner$Id_folio, " ;
    sdo:name \"", SR_owner$Name_owner, "\" .
")
owners_ttl_SR_plantation <- paste0("hdsc:SR-Suriname\\/owner\\/", SR_owner$Id_owner, " 
    a hdsc:plantation ;
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", SR_owner$Id_folio, " ;
    sdo:name \"", SR_owner$Plantation, "\" .
")
owners_ttl_ER1 <- paste0("hdsc:SR-Suriname\\/owner\\/", SR_owner$Id_owner, "_private", " 
    a pico:PersonObservation ;
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", SR_owner$Id_folio, " ; 
    sdo:name \"", SR_owner$Name_owner, "\" .
")
owners_ttl_ER2 <- paste0("hdsc:SR-Suriname\\/owner\\/", SR_owner$Id_owner, "_plantation", " 
    a hdsc:plantation ;
    prov:hadPrimarySource hdsc:SR-Suriname\\/source\\/", SR_owner$Id_folio, " ;
    sdo:name \"", SR_owner$Plantation, "\" .
")
owners_ttl <- c(owners_ttl_SR_private, owners_ttl_SR_plantation, owners_ttl_ER1, owners_ttl_ER2)
rm(owners_ttl_SR_private, owners_ttl_SR_plantation, owners_ttl_ER1, owners_ttl_ER2)
#qq toevoegen


#person reconstruction
reconstruction_ttl <- paste0("hdsc:SR-Suriname\\/reconstruction\\/", Reconstruction$Id_person, " 
    a pico:PersonReconstruction ; 
    sdo:parent hdsc:SR-Suriname\\/reconstruction\\/", Reconstruction$Id_mother, " ; 
    prov:wasDerivedFrom hdsc:SR-Suriname\\/observation\\/", Reconstruction$Id_source_vector, " ; 
    prov:wasGeneratedBy <https://hdl.handle.net/10622/CSPBHO> .
")
reconstruction_ttl <- gsub("; 
    sdo:parent hdsc:SR-Suriname\\\\/reconstruction\\\\/NA ", "", reconstruction_ttl)


reconstruction_ttl_mother <- paste0("hdsc:SR-Suriname\\/reconstruction\\/", Reconstruction_mother$Id_person_mother, " 
    a pico:PersonReconstruction ; 
    sdo:children hdsc:SR-Suriname\\/reconstruction\\/", gsub("_mother", "", Reconstruction_mother$Id_person_mother), " ; 
    prov:wasDerivedFrom hdsc:SR-Suriname\\/observation\\/", Reconstruction_mother$Id_source_vector, " ; 
    prov:wasGeneratedBy <https://hdl.handle.net/10622/CSPBHO> .
")

#bind
ttl <- c(prefix, source_ttl, observation_ttl, mothers_ttl, owners_ttl, reconstruction_ttl, reconstruction_ttl_mother)
ttl <- gsub(" ,", ",", ttl)


#write ttl
write.table(ttl,
            paste0("SR Paramaribo ", Sys.Date(), ".ttl"),
            quote=F,
            sep = "\t", 
            col.names=F,
            row.names=F,
            fileEncoding="UTF-8")
