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

f <- "https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/export_478_13042022_172215.csv?token=GHSAT0AAAAAABTEILHCJ7NIWPO6M2DAMAYMYSXQDTQ"
data_sp <- read_csv(f, col_names = TRUE)
#View(data_sp)

#data plot = Anais csv plot, with geomorphology on it (for later) + colonne cleaned

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/AlpsAndes_plots.csv?token=GHSAT0AAAAAABTEILHCZPXIHSJK3WFICLI6YSXQEZQ"
Alps_plot <- read_csv(f, col_names = TRUE)
#filter Alps data

 Alps_plot<-Alps_plot%>%filter(Region=="Alps")
#View(Alps_plot)

 
### Data Cleaning
 ##Data_sp
  #Rename column
data_sp %>%
  rename(
    Plot='code_gps'
  )->data_sp
#View(data_sp)

 #Create Site variable (with shorter name than lieudit)
data_sp<-data_sp%>%
  mutate(Site=case_when(
    lieudit=="Glacier Blanc" ~"Glacier Blanc",
    lieudit=="Glacier de Gébroulaz"~"Gebroulaz", ## Attention le e avec accent se transforme en ? parfois
    lieudit=="Glacier des Pélerins"~"Pelerins",  ## Attention le e avec accent se transforme en ? 
    lieudit=="Glacier de Saint-Sorlin"~"Saint Sorlin",
    lieudit=="Glacier du Tour"~"Tour",
    lieudit=="Glacier d'Orny"~"Orny"))

unique(data_sp$Site)

#select variables of interest
data_sp<-data_sp%>%select(Site,Plot,nom_reconnu,cd_ref, famille, alti_calc )


 #replace ZZ relev? sans v?g?tation by na
data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'ZZ relevé sans végétation','na'))->data_sp  ## Attention le e avec accent se transforme en ? a l'ouverure de R
data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'Zz Mousses','na'))->data_sp
data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'ZZ non rattachable','na'))->data_sp
data_sp %>% mutate(nom_reconnu=str_replace(nom_reconnu,'Zz Taxon à vérifier','na'))->data_sp
 
#Change plot name in data_sp file (export CBNA) for correspondence with other files (Plot - geomorpho file, NDVI data, ect)
data_sp %>% mutate(Plot=str_replace(Plot,'_','-'))->data_sp 
data_sp %>% mutate(Plot=str_replace(Plot,'T08a','T08A'))->data_sp
data_sp %>% mutate(Plot=str_replace(Plot,'T13a','T13A'))->data_sp
data_sp %>% mutate(Plot=str_replace(Plot,'S3-5','S3-05'))->data_sp
data_sp %>% mutate(Plot=str_replace(Plot,'T-65','T65'))->data_sp
data_sp %>% mutate(Plot=str_replace(Plot,'SLIA-9','SLIA-09'))->data_sp
data_sp %>% mutate(Plot=str_replace(Plot,'SLIA-099','SLIA-99'))->data_sp


#View(data_sp)

#count plot per glacier in data_sp and Alps_plot

Alps_plot%>%group_by(Site)%>%count()->plot1
plot1
# Site              n
# <chr>         <int>
# 1 Gebroulaz        69
# 2 Glacier Blanc    93
# 3 Pelerins         70
# 4 Saint Sorlin     55
# 5 Tour             66

data_sp%>%select(Site, Plot)%>%group_by(Site, Plot)%>%count()%>%group_by(Site)%>%count()->plot2
plot2
# Site              n
# <chr>         <int>
# 1 Gebroulaz        60 -> OK 9 plots that dont have cover (GB1-03; GB1-04, GB1-05, GB1-10, GBA-02, GBA-03, GBA-05, GBA-06, GBA-10)
# 2 Glacier Blanc    92 -> B3-03 dont have floristic data (not surveyed by Cedric's team??) - they only have geomorpho data
# 3 Pelerins         69 -> P100: missing in CBNA export (might correspond to New 1985-2003 plot) - I have the data in my hold excel file
# 4 Saint Sorlin     55 -> OK
# 5 Tour             65 -> T13B: missing in CBNA export - I have the data in my hold excel file

#Removing plot with problems from Alps_plot
Alps_plot<-Alps_plot%>%subset(Plot!="B3-03")%>%subset(Plot!="P100")%>%subset(Plot!="T13B")
#view(Alps_plot)

#******************************************************************************
# RICHNESS
#******************************************************************************
##Test calcul Richness index to compare with previous export files - RICHNESS INDEX - Gebroulaz - Saint Sorlin - Tour -Pelerins

data_sp %>%filter(nom_reconnu!="na")%>% group_by(Plot) %>% summarize(Plant_Richness=n()) %>% right_join(Alps_plot, by=c('Plot'='Plot'))->Alps_plot

 #verif data is ok : comparison between Plant_Richness and Richness (previous data calculated without CBNA export)
check_Richness_Cover<-Alps_plot%>%select(Plot, Richness, Plant_Richness, Plant_cover, Cover, BSC_cover)

check_Richness_Cover$Richness<-as.numeric(check_Richness_Cover$Richness)
check_Richness_Cover$Plant_Richness<-as.numeric(check_Richness_Cover$Plant_Richness)
check_Richness_Cover %>%mutate(dif=Richness-Plant_Richness)%>%filter(dif!="0")->check_Richness_Cover
#view(check_Richness_Cover)

#!!!!!!!!!!!!!!!!!!!!!
#-> P62 a du etre rentre deux fois

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

## c(0,7,13,20,30,120,175) = 7 years, 6 years, 7 years, 10years, 90 years, 55 years # to follow a balanced sampling between classes.
data_sp$ageChronoG2<-cut(data_sp$age, c(0,7,13,20,30,120,175))

levels(data_sp$ageChronoG2)=c("0-7 yrs","7-13 years","13-20 years", "20-30 yrs","30-120 yrs","LIA - Control")
data_sp
## Count data -> Balanced or unbalanced?
data_sp%>%group_by(Site, Plot, ageChronoG2)%>%count()%>%group_by(ageChronoG2)%>%count()->count2
count2

#******************************************************************************
# CREATING TRAIT DATA BASE FOR PROGLACIAL SPECIES
#******************************************************************************

####DISPERSAL MODE
### Import dispersal mode from (1) PFT_CBNA to Trait data frame

#Plant Functional Trait CBNA data base

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/PFT_CBNA.csv?token=GHSAT0AAAAAABTEILHDIESTCJEKV34X46UUYSXQGGQ"
PFT_CBNA <- read_csv(f, col_names = TRUE)
#View(PFT_CBNA)

PFT_CBNA %>%
  dplyr::select(CD_ref, mode_dispersion_CBNA_VALS)%>% right_join(filter(data_sp, nom_reconnu!="na"), by=c('CD_ref'='cd_ref'), na.rm=TRUE)->data_traitA

data_traitA %>%
  rename(
    dispersal_mode=mode_dispersion_CBNA_VALS ##rename column "mode_dispersion_CBNA_VALS" to friendlier name
  )->data_traitA


#dividing data traitA into two table: A1 with dispersal from previous CBNA data base, and A2 with dispersal missing

data_traitA$dispersal_mode[is.na(data_traitA$dispersal_mode)]<-"NA"

data_traitA%>%filter(dispersal_mode!="NA")->data_traitA


data_traitA%>%filter(dispersal_mode=="NA")->data_traitA2

## Import dispersal mode from (2) missing_CSR_dispersal to data_sp

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/missing_CSR_dispersal.csv?token=GHSAT0AAAAAABTEILHC7V74LX6J63K66K7QYSXQHEQ"
CSR_dispersal_2 <- read_csv(f, col_names = TRUE)

CSR_dispersal_2 %>%
  dplyr::select(CD_REF, dissemination)%>% right_join(data_traitA2, by=c('CD_REF'='CD_ref'), na.rm=TRUE)->data_traitA2

data_traitA2 %>%select(CD_REF, dissemination, Site, Plot, nom_reconnu, famille, alti_calc)%>%rename(dispersal_mode=dissemination)->data_traitA2


#merge data_traitA and data TraitA2

data_trait<-merge(data_traitA,data_traitA2, all=TRUE)

unique(data_trait$dispersal_mode)

# # Cleaning names in dispersal_mode 
# data_sp %>% mutate(dispersal_mode=str_replace(dispersal_mode,'épizoochore','epizoochore'))->data_sp #### Attention le e avec accent se transforme en ? a l'ouverure de R
# data_sp %>% mutate(dispersal_mode=str_replace(dispersal_mode,'epizochore','epizoochore'))->data_sp
# data_sp %>% mutate(dispersal_mode=str_replace(dispersal_mode,'anémochore','anemochore'))->data_sp

data_trait%>%mutate(cd_ref=coalesce(CD_ref, CD_REF))%>%select(cd_ref, dispersal_mode, Site, Plot, nom_reconnu, famille, alti_calc)->data_trait

TRAITS<-data_trait%>%select(cd_ref, dispersal_mode, nom_reconnu, famille)%>%unique

#view(TRAITS)

##TEST FOR CSR

#1_jOINING WITH CSR_CBNA_extra - column SA_CSR

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/CSR_CBNA_extra.csv?token=GHSAT0AAAAAABTEILHDHUL5UHGB4U2KIMEGYSXQIDA"
SA_CSR<- read_csv(f, col_names = TRUE)
#view(SA_CSR)

SA_CSR %>%
  dplyr::select(CD_REF7, SA_CSR)%>% right_join(TRAITS, by=c('CD_REF7'='cd_ref'), na.rm=TRUE)%>%unique->TRAITS
view(TRAITS)

#2_jOINING WITH CSR_lifeform_FI - Strategie_CSR

f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/files_PFT/CSR_lifeform_FI.csv?token=GHSAT0AAAAAABTEILHCIHK7MKYSKFV7EIAOYSXQITA"
CSR_lifeform<- read_csv(f, col_names = TRUE)

CSR_lifeform %>%
  dplyr::select(cdref7, Strategie_CSR)%>% right_join(TRAITS, by=c('cdref7'='CD_REF7'), na.rm=TRUE)%>%unique->TRAITS

# need to merge the data, but for some species SA_CSR est different de Strategie_CSR -> Le quel priorizer?


### Life Form

CSR_lifeform %>%
  dplyr::select(cdref7, Forme_vie)%>% right_join(TRAITS, by=c('cdref7'='cdref7'), na.rm=TRUE)%>%unique->TRAITS

#Rename column
PFT_CBNA %>%
  rename(
    Hauteur_moyenne_CBNA_VALS='_Hauteur_moyenne_CBNA_VALS'
  )->PFT_CBNA


## Other trait from CBNA file
PFT_CBNA %>%
  dplyr::select(CD_ref,
                Hauteur_moyenne_CBNA_VALS,
                Vittoz_SD_SeedMass,
                Vittoz_SD_SLAall,
                chorologie_classe_JULVE,
                phenologie_ordre_de_floraison_JULVE,
                grand_type_de_formation_JULVE,
                ListeLECA,
                Endemisme_CBNA_VALS,
                Feuillage_caducite_persistance_CBNA)%>% right_join(TRAITS, nom_reconnu, by=c('CD_ref'='cdref7'), na.rm=TRUE)%>%unique->TRAITS

view(TRAITS)
#write.csv(TRAITS,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\TRAITS.csv")


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
# test RQL ANALYSIS
#******************************************************************************

### R matrix - environment matrix

R<-Alps_plot%>%
  filter(Site!="Glacier Blanc")%>%   #remove glacier Blanc data because geomorpho highly incomplete
  select(Site,Plot,SlopeO, Landform_corr, GeoA, Rock,age)
#view(R)

 #Import alti calc from export CBNA (data_sp) to R
data_sp %>%
  dplyr::select(Plot, alti_calc)%>% right_join(R, by=c('Plot'='Plot'), na.rm=TRUE)->R
#View(R)

### Q matrix - species by trait matrix

TRAITS

### PREVIOUS CODE ### _ TO BE REMOVE (keep somewhere else in case)
  #identifying species with missing csr data
data_sp$CSR[is.na(data_sp$CSR)]<-"NA"
missing_CSR<-data_sp%>%subset(CSR=="NA")%>%select(CD_REF, nom_reconnu,famille,dispersal_mode, CSR)%>%unique
view(missing_CSR)
#write.csv(missing_CSR,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\missing_CSR.csv")

#identifying species with missing dispersal mode data
data_sp$dispersal_mode<-as.factor(data_sp$dispersal_mode)

data_sp$dispersal_mode[is.na(data_sp$dispersal_mode)]<-"NA"
#data_sp$dispersal_mode[is.na(data_sp$dispersal_mode)]<-"NULL"

missing_dispersal_mode<-data_sp%>%subset(dispersal_mode=="NA")%>%select(CD_REF, nom_reconnu,famille,dispersal_mode, CSR)%>%unique
view(missing_dispersal_mode)
#write.csv(missing_dispersal_mode,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\missing_dispersal_mode.csv")


missing_CSR_dispersal<-merge(missing_CSR, missing_dispersal_mode, all=TRUE)
#missing_CSR_dispersal%>%unique->missing_CSR_dispersal
view(missing_CSR_dispersal)
#write.csv(missing_CSR_dispersal,"C:/ECOLOGICAL CHANGES IN ALPINE ECOSYSTEMS/RESEARCH-DISSERTATION/ANALYSES_PS/CHRONOSEQUENCES/Plot_Sp\\missing_CSR_dispersal.csv")



  #creating a species-traits matrix
Q<-data_sp%>%select(CD_REF,libcdref7, nom_reconnu, dispersal_mode, CSR, famille)%>%unique()
view(Q)


### L matrix - species matrix
L<- dcast(data_sp, Plot~nom_reconnu,value.var= 'cover_sp')
Gebroulaz_spe[is.na(Gebroulaz_spe)]<-0
View(Gebroulaz_spe)

