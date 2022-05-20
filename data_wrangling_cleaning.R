      #*****************************************************************************
      #      Data wrangling - cleaning
      #******************************************************************************

library(tidyverse)

#**************************************
# Species data base and plot data base
#**************************************

###Importing data from GitHub

#data species = export CBNA - last used is 22 Nov 2021

f <- "https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/export_478_20042022_184348.csv"
data_sp <- read_csv(f, col_names = TRUE)
#View(data_sp)

#data plot = Anais csv plot, with geomorphology on it (for later) + colonne cleaned

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/AlpsAndes_plots.csv"
Alps_plot <- read_csv(f, col_names = TRUE)

#filter Alps data
Alps_plot<-Alps_plot%>%filter(Region=="Alps")
#View(Alps_plot)

unique(Alps_plot$Plot)#405

### Data Cleaning
##Data_sp
#Rename column
data_sp %>%
  rename(
    Plot='code_gps',
  )->data_sp


#Create Site variable (with shorter name than lieudit)
data_sp<-data_sp%>%
  mutate(Site=case_when(
    lieudit=="Glacier Blanc" ~"Glacier Blanc",
    lieudit=="Glacier de Gébroulaz"~"Gebroulaz", 
    lieudit=="Glacier des Pélerins"~"Pelerins",  
    lieudit=="Glacier de Saint-Sorlin"~"Saint Sorlin",
    lieudit=="Glacier du Tour"~"Tour",
    lieudit=="Glacier d'Orny"~"Orny"))

unique(data_sp$Site)

#replace ZZ releve sans vegetation by na
# data_sp %>% mutate(nom_reconnu_ss_auteur=str_replace(nom_reconnu_ss_auteur,'ZZ relevé sans végétation','na'))->data_sp
# data_sp %>% mutate(nom_reconnu_ss_auteur=str_replace(nom_reconnu_ss_auteur,'Zz Mousses','na'))->data_sp #je supprime les MOusses ici, dumoins pour le calcul de richesse, 
# #car on considere que les plantes vasculaires - les mousses sont dans la SBC.
# # data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'ZZ non rattachable','na'))->data_sp
# data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'Zz Taxon à vérifier','Zz Taxon a verifier'))->data_sp
#  
#View(data_sp)

#count plot per glacier in data_sp and Alps_plot

Alps_plot%>%group_by(Site)%>%count()->plot1
plot1
# Site              n
# <chr>         <int>
# 1 Gebroulaz        69
# 2 Glacier Blanc    93
# 3 Orny             52
# 4 Pelerins         70
# 5 Saint Sorlin     55
# 6 Tour             66

data_sp%>%select(Site, Plot)%>%group_by(Site, Plot)%>%count()%>%group_by(Site)%>%count()->plot2
plot2
# Site              n
# <chr>         <int>
# 1 Gebroulaz        69
# 2 Glacier Blanc    92 -> B3-03 dont have floristic data (not surveyed by Cedric's team??) - they only have geomorpho data
# 3 Orny             52 
# 4 Pelerins         70
# 5 Saint Sorlin     55
# 6 Tour             66


data_sp%>%select(Site, id_releve)%>%group_by(Site, id_releve)%>%count()%>%group_by(Site)%>%count()->id_releve_data_sp
id_releve_data_sp
# Site              n
# <chr>         <int>
# 1 Gebroulaz        69
# 2 Glacier Blanc    92 -> B3-03 dont have floristic data (not surveyed by Cedric's team??) - they only have geomorpho data
# 3 Orny             52 
# 4 Pelerins         71 -> erreur avec 12127009 Band 4 sans vegetation (en trop - ne sais pas a quoi ce plot correspond -> SUPRIMER)
# 5 Saint Sorlin     55
# 6 Tour             66

#Deleting plot B3-03 with missing veg data (from AlpsPlot)
Alps_plot<-Alps_plot%>%subset(Plot!="B3-03")

#Deleting id-releve "12127009" from data_sp (Twice plot P07)
data_sp<-data_sp%>%subset(id_releve!="12127009")

#******************************************************************************
# RICHNESS
#******************************************************************************
##Test calcul Richness index to compare with previous export files - RICHNESS INDEX - Gebroulaz - Saint Sorlin - Tour -Pelerins

data_sp %>%filter(nom_reconnu_ss_auteur!="na")%>% group_by(Plot) %>% summarize(Plant_Richness=n()) %>% right_join(Alps_plot, by=c('Plot'='Plot'))->Alps_plot

#  #verif data is ok : comparison between Plant_Richness and Richness (previous data calculated without CBNA export)
# check_Richness_Cover<-Alps_plot%>%select(Plot, Richness, Plant_Richness, Plant_cover, Cover, BSC_cover)
# 
# check_Richness_Cover$Richness<-as.numeric(check_Richness_Cover$Richness)
# check_Richness_Cover$Plant_Richness<-as.numeric(check_Richness_Cover$Plant_Richness)
# check_Richness_Cover %>%mutate(dif=Richness-Plant_Richness)%>%filter(dif!="0")->check_Richness_Cover
# #view(check_Richness_Cover)

##
#GLIAX-07 was sp was missing in previous counting -> OK
#O63-> error in the field data (Saliz herbacea twice)
#OLIA-02 -> Error in field data sheet (Cardus defloratus x2)->OK
#P100/T13-B-> inversé a corriger !!!!!!!!!!!!!!
#PC-05-> error in field data sheet (Salix herbacea twice)
#PA-10-> error in previous doc, richness=11

#after checking na values for richness are not errors, and correspond to no-vegetation plot, replace NA value by 0
Alps_plot$Plant_Richness[is.na(Alps_plot$Plant_Richness)]<-0
#View(Alps_plot)

#****************************************************
# Age of plot
#****************************************************

### Import 'age' data (Antoine Rabatel estimates) from 'Alps_data' to data_sp

Alps_plot$age<-as.numeric(Alps_plot$age)
Alps_plot %>%
  dplyr::select(Plot, age)%>% right_join(data_sp, by=c('Plot'='Plot'))->data_sp

#View(data_sp) # NEED TO VERIFY S1-14, T99 AND P90

data_sp$age<-as.numeric(data_sp$age)


#****************************************************
# data extracted from MNT_5m IGE + Swiss
#****************************************************


#generating a list of name plot = name id_releve
id_releve<-data_sp%>%select(id_releve,Plot)%>%unique
#view(id_releve)

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/data_enviro/data_extract_MNT_5m.csv"
data_extract_MNT_5m <- read_csv(f, col_names = TRUE)

#adding id_releve to Alps plot
str(Alps_plot)


id_releve %>%
  dplyr::select(Plot, id_releve)%>% right_join(Alps_plot, by=c('Plot'='Plot'))->Alps_plot

#joinging data from data_extract_mnt

data_extract_MNT_5m %>% right_join(Alps_plot, by=c('id_releve'='id_releve'))->Alps_plot
#view(Alps_plot)


unique(Alps_plot$id_releve)

#******************************************************************************
# CREATING TRAIT DATA BASE FOR PROGLACIAL SPECIES
#******************************************************************************

###################################

### FILES

## Our list of taxons
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/export_taxons_rec_478_15042022_122148.csv"
taxons <- read_csv(f, col_names = TRUE)
#view(taxons)

##Plant Functional Trait CBNA file
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/PFT_CBNA.csv"
PFT_CBNA <- read_csv(f, col_names = TRUE)
#View(PFT_CBNA)

## Baseflor Julve
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/Baseflor_taxon.csv"
baseflor<-read_csv(f, col_names = TRUE)
#View(baseflor)

## CSR_dissemi_Pollini = doc completed by Sophie (base julve + utilisant les attributs de toutes les esp. du genre)

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/missing_CSR_dispersal.csv"
CSR_dissemi_Pollini <- read_csv(f, col_names = TRUE)
#view(CSR_dissemi_Pollini)

#CSR Strategies from CBNA_Extra doc (with several CSR champs)
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/CSR_CBNA_extra.csv"
SA_CSR<- read_csv(f, col_names = TRUE)
#view(SA_CSR)

############################

### **DISPERSAL**

  ## BASE TRAITS = PFT_CBNA
    #jointure dispersal
PFT_CBNA %>%
  dplyr::select(CD_ref, mode_dispersion_CBNA_VALS)%>% right_join(taxons, by=c('CD_ref'='cd_ref'))->taxon_dispersal
taxon_dispersal %>%rename(dispersal_mode=mode_dispersion_CBNA_VALS)->taxon_dispersal ##rename column "mode_dispersion_CBNA_VALS" to friendlier name
#view(taxon_dispersal)  

  #dividing taxon dispersal into two tables: 1 with dispersal from previous CBNA data base, and 2 with dispersal missing
taxon_dispersal$dispersal_mode[is.na(taxon_dispersal$dispersal_mode)]<-"NA"
taxon_dispersal%>%filter(dispersal_mode!="NA")->taxon_dispersal1
taxon_dispersal%>%filter(dispersal_mode=="NA")->taxon_dispersal2
#view(taxon_dispersal1)
#view(taxon_dispersal2)


  ## Baseflor Julve
    #importning missing taxon from Baseflor Julve

baseflor %>%select(cd_ref,dissémination)%>%right_join(taxon_dispersal2, by=c('cd_ref'='CD_ref'))->taxon_dispersal2 # 19 taxon with dispersal mode still missing

taxon_dispersal2 %>%select(-dispersal_mode)%>%rename(dispersal_mode=dissémination)->taxon_dispersal2

    #select taxon with dispersal mode still missing
taxon_dispersal2$dispersal_mode[is.na(taxon_dispersal2$dispersal_mode)]<-"NA"
taxon_dispersal2%>%filter(dispersal_mode!="NA")->taxon_dispersal2ok
taxon_dispersal2%>%filter(dispersal_mode=="NA")->taxon_dispersal3
#view(taxon_dispersal3)


  ## CSR_dissemi_Pollini doc completed by Sophie (base julve + utilisant les attributs de toutes les esp. du genre)

CSR_dissemi_Pollini %>%
  dplyr::select(CD_REF, dissemination)%>% right_join(taxon_dispersal3, by=c('CD_REF'='cd_ref'))->taxon_dispersal3
taxon_dispersal3 %>%select(-dispersal_mode)%>%rename(dispersal_mode=dissemination)->taxon_dispersal3
#view(taxon_dispersal3)


  ##merge taxon_dispersal1, taxon_dispersal2ok and taxon_dispersal3 into TRAITS_dispersal
TRAITS_dispersal<-merge(taxon_dispersal1,taxon_dispersal2ok, all=TRUE)
TRAITS_dispersal<-merge(TRAITS_dispersal,taxon_dispersal3, all=TRUE)

TRAITS_dispersal%>%mutate(cd_ref=coalesce(cd_ref, CD_REF,CD_ref))%>%unique%>%select(-CD_ref,-CD_REF)->TRAITS_dispersal
TRAITS_dispersal%>%rename(dissemination_compil=dispersal_mode)->TRAITS_dispersal #changement de nom pour comparaison donnees avec base JULVE 
#(dispersion_compil = donnees CBNA + base JULVE pour donnees manquantes)
#view(TRAITS_dispersal)

unique(TRAITS_dispersal$dissemination_compil)



### **POLLINISATION**

## BASE TRAITS = PFT_CBNA
#jointure dispersal
PFT_CBNA %>%
  dplyr::select(CD_ref, phenologie_fecondationpollinisation_type_CBNA)%>% right_join(taxons, by=c('CD_ref'='cd_ref'))->taxons_pollin
taxons_pollin %>%rename(pollinisation1=phenologie_fecondationpollinisation_type_CBNA)->taxon_pollin ##rename column "mode_dispersion_CBNA_VALS" to friendlier name
#view(taxon_pollin)  # 100 taxon missing for pollinisation


#dividing TRAITS_dispersal into two tables: 1 with dissemination from previous CBNA data base, and 2 with pollinisation missing
taxon_pollin$pollinisation1[is.na(taxon_pollin$pollinisation1)]<-"NA"
taxon_pollin%>%filter(pollinisation1!="NA")->taxon_pollin1
taxon_pollin%>%filter(pollinisation1=="NA")->taxon_pollin2
#view(taxon_pollin2)


## Baseflor Julve
#importing missing taxon from Baseflor Julve

baseflor %>%select(cd_ref,pollinisation)%>%right_join(taxon_pollin2, by=c('cd_ref'='CD_ref'))->taxon_pollin2 # 18 taxon with dispersal mode still missing

taxon_pollin2 %>%select(-pollinisation1)%>%rename(pollinisation1=pollinisation)->taxon_pollin2

#select taxon with dispersal mode still missing
taxon_pollin2$pollinisation1[is.na(taxon_pollin2$pollinisation1)]<-"NA"
taxon_pollin2%>%filter(pollinisation1!="NA")->taxon_pollin2ok
taxon_pollin2%>%filter(pollinisation1=="NA")->taxon_pollin3


## CSR_dissemi_Pollini doc completed by Sophie (base julve + utilisant les attributs de toutes les esp. du genre)

CSR_dissemi_Pollini %>%
  dplyr::select(CD_REF, pollinisation)%>% right_join(taxon_pollin3, by=c('CD_REF'='cd_ref'))->taxon_pollin3
taxon_pollin3 %>%select(-pollinisation1)%>%rename(pollinisation1=pollinisation)->taxon_pollin3
#view(taxon_pollin3)

##merge taxon_dispersal1, taxon_dispersal2ok and taxon_dispersal3 into TRAITS_dispersal
TRAITS_pollin<-merge(taxon_pollin1,taxon_pollin2ok, all=TRUE)
TRAITS_pollin<-merge(TRAITS_pollin,taxon_pollin3, all=TRUE)
unique(TRAITS_pollin$pollinisation1)


TRAITS_pollin%>%mutate(cd_ref=coalesce(cd_ref, CD_REF,CD_ref))%>%unique%>%select(-CD_ref,-CD_REF)->TRAITS_pollin
TRAITS_pollin%>%rename(pollinisation_compil=pollinisation1)->TRAITS_pollin #changement de nom pour comparaison donnees avec base JULVE 
#(dispersion_compil = donnees CBNA + base JULVE pour donnees manquantes)
#view(TRAITS_pollin)

## ** JOIN DISPERSAL AND POLLI DATA IN TRAITS**

TRAITS_dispersal %>%select(cd_ref, dissemination_compil)%>% right_join(TRAITS_pollin, by=c('cd_ref'='cd_ref'))->TRAITS
#view(TRAITS)


### **CSR STRATEGIES**

#Join with CSR_CBNA_SA_CSR (work CBNA specialist)

SA_CSR %>%select(CD_REF7, SA_CSR)%>% right_join(TRAITS, by=c('CD_REF7'='cd_ref'))%>%unique->TRAITS
#view(TRAITS)#329 entries (Il doit y avoir deux/trois doublons dans la donnees SA_CSR)


TRAITS %>%select(-SA_CSR)%>%unique%>%count() #326 entries -> doublons avec deux valeurs de CSR differentes pour le meme taxon. A RECHERCHER !!!
TRAITS%>%group_by(CD_REF7)%>%summarize(count_CDREF=n())->TRAITS_check
#view(TRAITS_check)
# duplicates are:
# -81179, Alchemilla transiens (Buser) Buser, 1898 - CSS and CCS -> Le taxon se repete 4 fois dans SA_CSR (3 css et 1 ccs)-> modifier pour CSS
# -133087, Cerastium arvense subsp. strictum Gaudin, 1828 - CRS and CCS (Les deux strategies sont presente une fois pour exactement le meme taxon) ????
# -83528, Arctostaphylos uva-ursi (L.) Spreng., 1825 - same problem

TRAITS$SA_CSR[TRAITS$CD_REF7=="81179"]<-"css"
TRAITS%>%unique ->TRAITS
#view(TRAITS) #328 entries



### **BASEFLOR JULVE** - OTHERS TRAITS

baseflor %>%select(cd_ref,
                   nom_reconnu_ss_auteur,
                   CHOROLOGIE,
                   sexualité,
                   pollinisation,
                   inflorescence,
                   fruit,
                   dissémination,
                   TYPE_BIOLOGIQUE, FORMATION_VEGETALE,
                   CARACT_ECOLOG_HABITAT_OPTI,
                   INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE)%>%right_join(TRAITS, by=c('cd_ref'='CD_REF7'))->TRAITS
#view(TRAITS)


 
#write.csv(TRAITS,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Analyses_flore/data\\TRAITS_notcomplete.csv")


#Concatenation des deux bases de données de dissemination
unique(TRAITS$dissemination_compil)
##DOES NOT WORK
 # TRAITS%>%mutate(dissemination_compil=str_replace(dissemination_compil,"anémochore ? (et dyszoochore)","anémochore et zoochore"))%>%#OK
 #   mutate(dissemination_compil=str_replace(dissemination_compil,"épizoochore ?","zoochore"))%>% #OK
 #   mutate(dissemination_compil=str_replace(dissemination_compil,'endozoochore','zoochore'))%>%#OK
 #   mutate(dissemination_compil=str_replace(dissemination_compil,'myrmécochore','zoochore'))%>%#OK
 #   mutate(dissemination_compil=str_replace(dissemination_compil,"anémochore(et zoochore?)","anémochore et zoochore"))%>%
 #   mutate(dissemination_compil=str_replace(dissemination_compil,"anémochore ?","anémochore"))%>%
 #   mutate(dissemination_compil=str_replace(dissemination_compil,"autochore?","autochore"))%>%
 #   mutate(dissemination_compil=str_replace(dissemination_compil,"anémochore(et zoochore?)","anémochore et zoochore"))->TRAITS_2
 # unique(TRAITS_2$dissemination_compil)
## look at recode function
# regular expression
#\\ before ?


## By hand in excel on TRAITS_not complete:

TRAITS_modif <- read_csv("data/TRAITS_notcomplete.csv")
#View(TRAITS_modif)

TRAITS_modif%>%select(cd_ref,nom_reconnu_ss_auteur, dissemination_compil,dissémination, pollinisation_compil,pollinisation, SA_CSR)->TRAITS_comparison
#view(TRAITS_comparison)

#write.csv(TRAITS,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\TRAITS.csv")


#***********************************************************
#* Ajout donnees BASE ANDROSACE _ Concatenation Brad Carlson
#* **********************************************************

Androsace <- read_csv("files_PFT/Concatenation_BD_Androsace_Brad_Carlson.csv")
Androsace$Taxref<-as.numeric(Androsace$Taxref)
Androsace<-Androsace%>%select(Taxref, CNRATIO, GROWTHF, LDMCst, LDMCp, LDMC, SEEDM, SLA, STRAT)%>%filter(Taxref!="NA")%>%unique()
#View(Androsace)

#duplicate cd_ref in TRAITS_modif to compare with Taxref in Brad doc Androsace
TRAITS_modif$CD_REF<-TRAITS_modif$cd_ref

#Join TRAITS_modif and Androsace

Androsace %>%
  #select(SP_nom, Taxref, CNRATIO, GROWTHF, LDMCst, LDMCp, LDMC, SEEDM, SLA, STRAT)%>%
  right_join(TRAITS_modif, by=c('Taxref'='CD_REF'))->TRAITS_modif2
#view(TRAITS_modif2)

#(verif<-(TRAITS_modif2$Taxref)-(TRAITS_modif2$cd_ref)) OK

#write.csv(TRAITS_modif2,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/CHRONOSEQUENCES/Analyses_flore/data\\baseTRAITS.csv")



#******************************************************************************
# CHRONOSEQUENCE GROUPS
#******************************************************************************

### Chronosequence groups

## c(0,7,12,19,36,100,175) # corresponding to chronosequence (~ homogenized period between sites)
data_sp$ageChronoG<-cut(data_sp$age, c(0,7,12,19,36,100,175))

levels(data_sp$ageChronoG)=c("0-7 yrs","7-12 years","12-19 years", "19-36 yrs","36-100 yrs","LIA - Control")
data_sp
## Count data -> Balanced or unbalanced?
data_sp%>%group_by(Site, Plot, ageChronoG)%>%count()%>%group_by(ageChronoG)%>%count()->count1
count1 #-> unbalanced sampling
# ageChronoG        n
# <fct>         <int>
# 1 0-7 yrs          49
# 2 7-12 years       57
# 3 12-19 years      67
# 4 19-36 yrs        99
# 5 36-100 yrs       47
# 6 LIA - Control    61
# 7 NA               24

## c(0,7,13,20,30,120,175) = 7 years, 6 years, 7 years, 10years, 90 years, 55 years # to follow a balanced sampling between classes.
data_sp$ageChronoG2<-cut(data_sp$age, c(0,7,14,21,32,120,175))

levels(data_sp$ageChronoG2)=c("0-7 yrs","7-13 years","13-20 years", "20-30 yrs","30-120 yrs","LIA - Control")
data_sp
## Count data -> Balanced or unbalanced?
data_sp%>%group_by(Site, Plot, ageChronoG2)%>%count()%>%group_by(ageChronoG2)%>%count()->count2
count2

# ageChronoG2       n
# <fct>         <int>
# 1 0-7 yrs          49
# 2 7-13 years       73
# 3 13-20 years      70
# 4 20-30 yrs        74
# 5 30-120 yrs       76
# 6 LIA - Control    38
# 7 NA               24



#*******************************************************************************************
#*    Deconcatenation donnees recouvrement a la main dans excel and fusion des 6 fichiers
#*******************************************************************************************



# #DO NOT MODIFY OR RUN
# Gebroulaz<-filter(data_sp, Site=="Gebroulaz")
# #write.csv(Gebroulaz,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Gebroulaz_comm_taxon.csv")
# Sorlin<-filter(data_sp, Site=="Saint Sorlin")
# #write.csv(Sorlin,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Sorlin_comm_taxon.csv")
# Pelerins<-filter(data_sp, Site=="Pelerins")
# #write.csv(Pelerins,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Pelerins_comm_taxon.csv")
# Tour<-filter(data_sp, Site=="Tour")
# #write.csv(Tour,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Tour_comm_taxon.csv")
# Orny<-filter(data_sp, Site=="Orny")
# #write.csv(Orny,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Orny_comm_taxon.csv")
# Blanc<-filter(data_sp, Site=="Glacier Blanc")
# #write.csv(Blanc,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Blanc_comm_taxon.csv")
# 

# #Merging the 6 files
# Gebroulaz_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Gebroulaz_comm_taxon.csv")
# Sorlin_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Sorlin_comm_taxon.csv")
# Pelerins_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Pelerins_comm_taxon.csv")
# Tour_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Tour_comm_taxon.csv")
# Orny_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Orny_comm_taxon.csv")
# Blanc_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Blanc_comm_taxon.csv")
# 
# 
# Data_sp_1<-merge(Gebroulaz_comm_taxon,Sorlin_comm_taxon,all=TRUE)
# 
# Data_sp_2<-merge(Data_sp_1,Pelerins_comm_taxon,all=TRUE)
# Data_sp_3<-merge(Data_sp_2,Tour_comm_taxon,all=TRUE)
# Data_sp_4<-merge(Data_sp_3,Orny_comm_taxon,all=TRUE)
# Data_sp_All<-merge(Data_sp_4,Blanc_comm_taxon,all=TRUE)


##Deleting id-releve "12127009" from data_sp (Twice plot P07)
#Data_sp_All<-Data_sp_All%>%subset(id_releve!="12127009")
#view(Data_sp_All)
#write.csv(Data_sp_All,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Analyses_flore/data\\Data_sp_All.csv")


