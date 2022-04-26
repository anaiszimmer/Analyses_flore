####################################################################################
#                ANALYSIS BOTA - Diversity and composition - 6 March 2021
#                               Last modified - Anaïs - 13 March
####################################################################################

#packages
library(readr)
library(tidyverse)
library(gridExtra)
library(ggpubr)
library(ggplot2)

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
data_sp %>% mutate(nom_reconnu_ss_auteur=str_replace(nom_reconnu_ss_auteur,'ZZ relevé sans végétation','na'))->data_sp
data_sp %>% mutate(nom_reconnu_ss_auteur=str_replace(nom_reconnu_ss_auteur,'Zz Mousses','na'))->data_sp #je supprime les MOusses ici, dumoins pour le calcul de richesse, 
#car on considere que les plantes vasculaires - les mousses sont dans la SBC.
# data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'ZZ non rattachable','na'))->data_sp
# data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'Zz Taxon à vérifier','na'))->data_sp
 
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

#Deleting plot B3-03 with missing veg data (from AlpsPlot)
Alps_plot<-Alps_plot%>%subset(Plot!="B3-03")


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

#******************************************************************************
# AGE DATA & CHRONOSEQUENCE GROUPS
#******************************************************************************

### Import 'age' data (Antoine Rabatel estimates) from 'Alps_data' to data_sp

Alps_plot$age<-as.numeric(Alps_plot$age)
Alps_plot %>%
  dplyr::select(Plot, age)%>% right_join(data_sp, by=c('Plot'='Plot'))->data_sp

#View(data_sp) # NEED TO VERIFY S1-14, T99 AND P90

data_sp$age<-as.numeric(data_sp$age)


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



#******************************************************************************
# CREATING TRAIT DATA BASE FOR PROGLACIAL SPECIES
#******************************************************************************

# Our list of taxons
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/export_taxons_rec_478_15042022_122148.csv"
taxons <- read_csv(f, col_names = TRUE)
#view(taxons)

##Plant Functional Trait CBNA file
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/PFT_CBNA.csv"
PFT_CBNA <- read_csv(f, col_names = TRUE)
#View(PFT_CBNA)

#jointure
PFT_CBNA %>%
  dplyr::select(CD_ref, mode_dispersion_CBNA_VALS,phenologie_fecondationpollinisation_type_CBNA)%>% right_join(taxons, by=c('CD_ref'='cd_ref'))->taxon_trait
taxon_trait %>%rename(dispersal_mode=mode_dispersion_CBNA_VALS)%>%rename(pollinisation1=phenologie_fecondationpollinisation_type_CBNA)->taxon_trait ##rename column "mode_dispersion_CBNA_VALS" to friendlier name
#view(taxon_trait)  


### **DISPERSAL**
#dividing taxon trait into two tables: 1 with dispersal from previous CBNA data base, and 2 with dispersal missing
taxon_trait$dispersal_mode[is.na(taxon_trait$dispersal_mode)]<-"NA"
taxon_trait%>%filter(dispersal_mode!="NA")->taxon_trait1
taxon_trait%>%filter(dispersal_mode=="NA")->taxon_trait2
#view(taxon_trait1)

## Import dispersal mode from (2) missing_CSR_dispersal to data_sp
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/missing_CSR_dispersal.csv"
CSR_dissemi_Pollini <- read_csv(f, col_names = TRUE)
#view(CSR_dispersal_2)
CSR_dissemi_Pollini %>%
  dplyr::select(CD_REF, dissemination)%>% right_join(taxon_trait2, by=c('CD_REF'='CD_ref'))->taxon_trait2
view(taxon_trait2)

taxon_trait2 %>%select(-dispersal_mode)%>%rename(dispersal_mode=dissemination)->taxon_trait2

#merge data_trait1 and data Trait2
TRAITS1<-merge(taxon_trait1,taxon_trait2, all=TRUE)
unique(TRAITS1$dispersal_mode)

TRAITS1%>%mutate(cd_ref=coalesce(CD_ref, CD_REF))%>%unique%>%select(-CD_ref,-CD_REF)->TRAITS1
TRAITS1%>%rename(dissemination_compil=dispersal_mode)->TRAITS1 #changement de nom pour comparaison donnees avec base JULVE 
#(dispersion_compil = donnees CBNA + base JULVE pour donnees manquantes)
view(TRAITS1)



### **POLLINISATION**

#dividing TRAITS1 into two tables: 1 with dissemination from previous CBNA data base, and 2 with pollinisation missing
TRAITS1$pollinisation1[is.na(TRAITS1$pollinisation1)]<-"NA"
TRAITS1%>%filter(pollinisation1!="NA")->TRAITS1a
TRAITS1%>%filter(pollinisation1=="NA")->TRAITS1b
#view(TRAITS1a)

# import for completed doc by Sophie from Julve
CSR_dissemi_Pollini %>%
  dplyr::select(CD_REF, pollinisation)%>% right_join(TRAITS1b, by=c('CD_REF'='cd_ref'))->TRAITS1b
view(TRAITS1b)

TRAITS1b %>%select(-pollinisation1)%>%rename(pollinisation1=pollinisation)->TRAITS1b

#merge data_trait1 and data Trait2
TRAITS<-merge(TRAITS1a,TRAITS1b, all=TRUE)
unique(TRAITS$pollinisation1)

TRAITS%>%mutate(cd_ref=coalesce(cd_ref, CD_REF))%>%unique%>%select(-CD_REF)->TRAITS
TRAITS%>%rename(pollinisation_compil=pollinisation1)->TRAITS #changement de nom pour comparaison donnees avec base JULVE 
#(pollinisation_compil = donnees CBNA + base JULVE pour donnees manquantes)
view(TRAITS)



### **CSR STRATEGIES**

#Join with CSR_CBNA_SA_CSR (work CBNA specialist)

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/CSR_CBNA_extra.csv"
SA_CSR<- read_csv(f, col_names = TRUE)
#view(SA_CSR)

SA_CSR %>%select(CD_REF7, SA_CSR)%>% right_join(TRAITS, by=c('CD_REF7'='cd_ref'))%>%unique->TRAITS
#view(TRAITS)#329 entries (Il doit y avoir deux/trois doublons dans la donnees SA_CSR)


TRAITS %>%select(-SA_CSR)%>%unique%>%count() #326 entries -> doublons avec deux valeurs de CSR differentes pour le meme taxon. A RECHERCHER !!!
TRAITS%>%group_by(cd_ref)%>%summarize(count_CDREF=n())->TRAITS_check
#view(TRAITS_check)
# duplicates are:
# -81179, Alchemilla transiens (Buser) Buser, 1898 - CSS and CCS -> Le taxon se repete 4 fois dans SA_CSR (3 css et 1 ccs)-> modifier pour CSS
# -133087, Cerastium arvense subsp. strictum Gaudin, 1828 - CRS and CCS (Les deux strategies sont presente une fois pour exactement le meme taxon) ????
# - 83528, Arctostaphylos uva-ursi (L.) Spreng., 1825 - same problem

TRAITS$SA_CSR[TRAITS$CD_REF7=="81179"]<-"css"
TRAITS%>%unique ->TRAITS
#view(TRAITS) #328 entries




### **BASEFLOR JULVE**

## Jointure avec la liste des 322 taxons concaténée avec les données de Baseflor 32 taxons A COMPLETER


f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/Baseflor_taxon.csv"

baseflor<-read_csv(f, col_names = TRUE)
#View(baseflor)


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



#Concatenation des deux bases de données de dissemination

# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'anémochore (et épizoochore ?)','épizoochore'))->TRAITS
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'anémochore(et épizoochore ?)','épizoochore'))->TRAITS
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'anémochore ?','anémochore'))->TRAITS
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'anémochore et dyszoochore','anémochore'))->TRAITS
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'autochore ?','barochore'))->TRAITS
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'autochore ? (et dyszoochore)','mymécochore'))->TRAITS # A VERIFIER
# TRAITS %>% mutate(dispersal_mode=str_replace(dispersal_mode,'némochore et dyszoochore','anémochore'))->TRAITS
# 
# 
# TRAITS$dispersal_mode[TRAITS$cd_ref=="90863"]<-"barochore"
# TRAITS$dispersal_mode[TRAITS$cd_ref=="997256"]<-"barochore"
# TRAITS$dispersal_mode[TRAITS$cd_ref=="125238"]<-"autochore"


TRAITS%>%select(cd_ref,nom_reconnu_ss_auteur, dissemination_compil,dissémination, pollinisation_compil,pollinisation, SA_CSR)->TRAITS_comparison
#view(TRAITS_comparison)

#write.csv(TRAITS,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\TRAITS_comparison.csv")


#**********************************************************************************************************
### PLOT DISPERSAL 


TRAITS %>%
  dplyr::select(CD_ref, dispersal_mode)%>% right_join(data_sp, by=c('CD_ref'='cd_ref'), na.rm=TRUE)->data_sp


view(data_sp)

data_sp$dispersal_mode<-factor(data_sp$dispersal_mode, levels = c("barochore","anemochore","endozoochoe", "epizoochore", "hydrochore", "zoochore","NA"))

Dplot<-ggplot(filter(data_sp, age!="NA"), aes(dispersal_mode, ageChronoG2), na.rm=TRUE)+
  geom_jitter(aes(color=dispersal_mode),size=1)+
  labs(title="Dispersal mode distribution - Sites: Tour, Pelerins, Gebroulaz, Saint Sorlin, Glacier Blanc",
       subtitle = paste("n = ", nrow(data_sp), "Sp"),
       x="", y="Age Groups")+
  theme(plot.title = element_text(size=11))
Dplot

Dplot<-ggplot(filter(data_sp, age!="NA", dispersal_mode!="NA"), aes(dispersal_mode, ageChronoG2), na.rm=TRUE)+
  geom_jitter(aes(color=dispersal_mode),size=2)+
  labs(title="Dispersal mode distribution - Sites: Tour, Pelerins, Gebroulaz, Saint Sorlin", 
       subtitle = paste("n = ", nrow(filter(data_sp, dispersal.mode!="NA")), "Sp"),
       x="", y="Age Groups")+
  scale_color_brewer(palette="Dark2")
Dplot


DispAge <- data_sp %>%
  count(ageChronoG2, dispersal_mode) %>%
  group_by(ageChronoG2) %>% #change to `group_by(Genotypes) %>%` for alternative approach
  mutate(prop = n / sum(n))

ggplot(data = DispAge, aes(ageChronoG2, prop, fill = dispersal_mode)) + 
  geom_bar(stat = "identity", position = "dodge")



#******************************************************************************
# test RLQ ANALYSIS
#******************************************************************************

# Preparation de la base de donnees

view(data_sp)

library(dplyr)
library(tidyr)
library(stringr)

#THIS DOES NOT WORK
# #duplicating the comm_taxon column
# data_sp$comm_taxon1<-data_sp$comm_taxon
# 
# data_sp_ok<-data_sp%>%separate(comm_taxon1, c('Cover_sp', 'Height_sp','Facilitation', 'Assoc_BSC', 'Compet', 'Nurse_rock', 'Reprod', 'Vielle_Repro_or_Comment','Comment'),";")
# 
# #write.csv(data_sp_ok,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\data_sp_cleaning_comm_taxon.csv")




#DO NOT MODIFY OR RUN
Gebroulaz<-filter(data_sp, Site=="Gebroulaz")
#write.csv(Gebroulaz,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Gebroulaz_comm_taxon.csv")
Sorlin<-filter(data_sp, Site=="Saint Sorlin")
#write.csv(Sorlin,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Sorlin_comm_taxon.csv")
Pelerins<-filter(data_sp, Site=="Pelerins")
#write.csv(Pelerins,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Pelerins_comm_taxon.csv")
Tour<-filter(data_sp, Site=="Tour")
#write.csv(Tour,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Tour_comm_taxon.csv")
Orny<-filter(data_sp, Site=="Orny")
#write.csv(Orny,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Orny_comm_taxon.csv")
Blanc<-filter(data_sp, Site=="Glacier Blanc")
#write.csv(Blanc,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon\\Blanc_comm_taxon.csv")


#Merging the 6 files
Gebroulaz_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Gebroulaz_comm_taxon.csv")
Sorlin_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Sorlin_comm_taxon.csv")
Pelerins_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Pelerins_comm_taxon.csv")
Tour_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Tour_comm_taxon.csv")
Orny_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Orny_comm_taxon.csv")
Blanc_comm_taxon <- read_csv("C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp/cleaning_comm_taxon/Blanc_comm_taxon.csv")


Data_sp_1<-merge(Gebroulaz_comm_taxon,Sorlin_comm_taxon,all=TRUE)

Data_sp_2<-merge(Data_sp_1,Pelerins_comm_taxon,all=TRUE)
Data_sp_3<-merge(Data_sp_2,Tour_comm_taxon,all=TRUE)
Data_sp_4<-merge(Data_sp_3,Orny_comm_taxon,all=TRUE)
Data_sp_All<-merge(Data_sp_4,Blanc_comm_taxon,all=TRUE)
view(Data_sp_All)

write.csv(Data_sp_All,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Analyses_flore/data\\Data_sp_All.csv")



### R matrix - environment matrix

R<-Alps_plot%>%
  filter(Site!="Glacier Blanc")%>%   #remove glacier Blanc data because geomorpho highly incomplete
  select(Site,Plot,SlopeO, Landform_corr, GeoA, Rock,age)
view(R)

 #Import alti calc from export CBNA (data_sp) to R
data_sp %>%
  dplyr::select(Plot, alti_calc)%>%unique()%>% right_join(R, by=c('Plot'='Plot'), na.rm=TRUE)->R
View(R)

### Q matrix - species by trait matrix

Q<-TRAITS

### L matrix - species matrix
L<- dcast(data_sp, Plot~nom_reconnu,value.var= 'cover_sp')
Gebroulaz_spe[is.na(Gebroulaz_spe)]<-0
View(Gebroulaz_spe)

