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



#**********************************************************************************************************
### PLOT DISPERSAL 
#*********************************************************************************************************

#reloading the save csv on GitHub
f <- "https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/Data_sp_All.csv"
spe_all <- read_csv(f, col_names = TRUE)
#view(spe_all)



#completing age -> need to be verified and added in csv doc

spe_all$age[spe_all$Plot=="P90"]<-"7" #band old =2008-2018 -> median=2013->2020-2013=7
#Alps_plot$age[Alps_plot$Plot=="B3-03"]<-""
spe_all$age[spe_all$Plot=="S1-14"]<-"9.5" #~same age as S1-19 (9.5 years)
spe_all$age[spe_all$Plot=="T99"]<-"7" ##band old =2008-2018 -> median=2013->2020-2013=7
spe_all$age[spe_all$Plot=="OLIA-01"]<-"77.5" #same age than other LIA plots
spe_all$age[spe_all$Plot=="OLIA-09"]<-"77.5"
spe_all$age[spe_all$Plot=="OLIA-05"]<-"77.5"


spe_dispersal<-spe_all%>%filter(age!="NA")%>%select(-ageChronoG2)#deleting ageChronoG2 column because incomplete since new ages added
unique(spe_dispersal$Plot)#386 plot

view(spe_dispersal)
## Updating chronosequence groups

## c(0,7,13,20,30,120,175) = 7 years, 6 years, 7 years, 10years, 90 years, 55 years # to follow a balanced sampling between classes.
spe_dispersal$age<-as.numeric(spe_dispersal$age)
spe_dispersal$ageChronoG2<-cut(spe_dispersal$age, c(0,7,14,21,32,120,175))

levels(spe_dispersal$ageChronoG2)=c("0-7 yrs","7-13 years","13-20 years", "20-30 yrs","30-120 yrs","LIA - Control")
spe_dispersal
## Count data -> Balanced or unbalanced?
spe_dispersal%>%group_by(Site, Plot, ageChronoG2)%>%count()%>%group_by(ageChronoG2)%>%count()->count2
count2





TRAITS_modif2 %>%
  dplyr::select(cd_ref, dissemination_compil)%>% right_join(spe_dispersal, by=c('cd_ref'='cd_ref'), na.rm=TRUE)->spe_dispersal

spe_dispersal<-spe_dispersal%>%rename(dispersal_mode=dissemination_compil)

unique(spe_dispersal$dispersal_mode)

view(spe_dispersal)
spe_dispersal$dispersal_mode<-factor(spe_dispersal$dispersal_mode, levels = c("anemochore","barochore","autochore", "zoochore", "anemochore & zoochore"))

Dplot<-ggplot(filter(spe_dispersal, age!="NA"), aes(dispersal_mode, ageChronoG2), na.rm=TRUE)+
  geom_jitter(aes(color=dispersal_mode),size=1)+
  labs(title="Dispersal mode distribution - Sites: Orny, Tour, Pelerins, 
       Gebroulaz, Saint Sorlin, Glacier Blanc",
       subtitle = paste("n = ", nrow(spe_dispersal), "Sp"),
       x="", y="Age Groups")+
  theme(plot.title = element_text(size=11))
Dplot

Dplot<-ggplot(filter(spe_dispersal, age!="NA", dispersal_mode!="NA"), aes(dispersal_mode, ageChronoG2), na.rm=TRUE)+
  geom_jitter(aes(color=dispersal_mode),size=2)+
  labs(title="Dispersal mode distribution - Sites: Orny, Tour, Pelerins, Gebroulaz, Saint Sorlin, Glacier Blanc", 
       subtitle = paste("n = ", nrow(filter(spe_dispersal, dispersal_mode!="NA")), "Sp"),
       x="", y="Age Groups")+
  scale_color_brewer(palette="Dark2")
Dplot


DispAge <- spe_dispersal %>%
  count(ageChronoG2, dispersal_mode) %>%
  group_by(ageChronoG2) %>% #change to `group_by(Genotypes) %>%` for alternative approach
  mutate(prop = n / sum(n))
DispAge



ggplot(data = DispAge, aes(ageChronoG2, prop, fill = dispersal_mode)) + 
  geom_bar(stat = "identity", position = "dodge")+
  labs(title="Proportion Dispersal mode - All Sites")+
  coord_cartesian(ylim=c(0,1))



DispProportion <- spe_dispersal %>%
  count(ageChronoG2, dispersal_mode, Plot, Site) %>%
  group_by(ageChronoG2, Plot) %>% #change to `group_by(Genotypes) %>%` for alternative approach
  mutate(prop = n / sum(n))
DispProportion


##BOXPLOT

DispProportion$Site<-factor(DispProportion$Site,levels = c("Glacier Blanc","Saint Sorlin","Gebroulaz","Pelerins","Tour","Orny"))
 unique(DispProportion$Site)


anemo<-ggplot(filter(DispProportion,dispersal_mode=="anemochore"), aes(x =ageChronoG2, y =prop, color=Site)) + 
  #geom_point(aes(color=Site), size=0.4)+
  geom_boxplot(aes(fill=Site), alpha=0.4)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Anemochory")+
  coord_cartesian(ylim=c(0,1))
  #scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
anemo

anemoAll<-ggplot(filter(DispProportion,dispersal_mode=="anemochore"), aes(x =ageChronoG2, y =prop)) + 
  geom_point(size=0.8)+
  geom_boxplot(alpha=0.4, fill="blue4", alpha=0.4)+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Anemochory - All sites")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
anemoAll


baro<-ggplot(filter(DispProportion,dispersal_mode=="barochore"), aes(x =ageChronoG2, y =prop, color=Site)) + 
  #geom_point(aes(color=Site), size=0.4)+
  geom_boxplot(aes(fill=Site), alpha=0.4)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Barochory")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
baro

baroAll<-ggplot(filter(DispProportion,dispersal_mode=="barochore"), aes(x =ageChronoG2, y =prop)) + 
  geom_point(size=0.8)+
  geom_boxplot(alpha=0.4, fill="orange3", alpha=0.4)+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Barochory - All sites")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
baroAll

auto<-ggplot(filter(DispProportion,dispersal_mode=="autochore"), aes(x =ageChronoG2, y =prop, color=Site)) + 
  #geom_point(aes(color=Site), size=0.4)+
  geom_boxplot(aes(fill=Site), alpha=0.4)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual", title="Autochory")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
auto

autoAll<-ggplot(filter(DispProportion,dispersal_mode=="autochore"), aes(x =ageChronoG2, y =prop)) + 
  geom_point(size=0.8)+
  geom_boxplot(alpha=0.4, fill="palegreen4", alpha=0.4)+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Autochory - All sites")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
autoAll

zoo<-ggplot(filter(DispProportion,dispersal_mode=="zoochore"), aes(x =ageChronoG2, y =prop, color=Site)) + 
  #geom_point(aes(color=Site), size=0.4)+
  geom_boxplot(aes(fill=Site), alpha=0.4)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual", title = "zoochorie")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
zoo

zooAll<-ggplot(filter(DispProportion,dispersal_mode=="zoochore"), aes(x =ageChronoG2, y =prop)) + 
  geom_point(size=0.8)+
  geom_boxplot(alpha=0.4, fill="orchid4", alpha=0.4)+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Zoochory - All sites")+
  coord_cartesian(ylim=c(0,1))
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
zooAll


## Linear regression _Age proportion of dispersion mode
DispProportion2 <- spe_dispersal %>%
  count(age, dispersal_mode, Plot, Site) %>%
  group_by(Plot) %>% #change to `group_by(Genotypes) %>%` for alternative approach
  mutate(prop = n / sum(n))
view(DispProportion2)

anemoR<-ggplot(filter(DispProportion2,dispersal_mode=="anemochore"), aes(x =age, y =prop, color=Site)) + 
  geom_point(aes(color=Site), size=1)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Anemochory")+
  coord_cartesian(ylim=c(0,1))+
  geom_smooth(method='lm',se=FALSE, fullrange=TRUE,formula = y ~ x)+
  stat_cor(aes(color=Site),method ="pearson", label.x =35, size=3)
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
anemoR

baroR<-ggplot(filter(DispProportion2,dispersal_mode=="barochore"), aes(x =age, y =prop, color=Site)) + 
  geom_point(aes(color=Site), size=1)+
  scale_fill_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  scale_color_manual(values=c('blue2','deepskyblue','magenta3','darkgreen','green2','darkolivegreen2'))+
  #geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw(base_size = 10)+
  labs(x="",y="Proportion of individual",title="Barochory")+
  coord_cartesian(ylim=c(0,1))+
  geom_smooth(method='lm',se=FALSE, fullrange=TRUE,formula = y ~ x)+
  stat_cor(aes(color=Site),method ="pearson", label.x =140, size=3)
#scale_y_continuous(breaks=c(0,25,50,75,100,200),trans=squish_trans(c(100,Inf)))
#geom_hline(yintercept=5, linetype="dashed", color="black")
baroR




#******************************************************************************
# RLQ ANALYSIS
#******************************************************************************

# Preparation de la base de donnees

library(dplyr)
library(tidyr)
library(stringr)



##### RLQ analysis

Alps_plot2<-Alps_plot

### R matrix - environment matrix -env
##cleaning data from Alps_plot
#slope Orientation
Alps_plot2 %>% mutate(SlopeO=str_replace(SlopeO,'-','0'))->Alps_plot2 
# Alps_plot %>% mutate(SlopeO=str_replace(SlopeO,'110','ES'))->Alps_plot 
# Alps_plot %>% mutate(SlopeO=str_replace(SlopeO,'150','SE'))->Alps_plot 
# Alps_plot %>% mutate(SlopeO=str_replace(SlopeO,'180','S'))->Alps_plot 
Alps_plot2 %>% mutate(SlopeD=str_replace(SlopeD,'-','0'))->Alps_plot2 
Alps_plot2 %>% mutate(SlopeD=str_replace(SlopeD,'<5','3'))->Alps_plot2 
Alps_plot2 %>% mutate(SlopeD=str_replace(SlopeD,'0-50','30'))->Alps_plot2
#View(Alps_plot2)


#completing age -> need to be verified and added in csv doc

Alps_plot2$age[Alps_plot2$Plot=="P90"]<-"7" #band old =2008-2018 -> median=2013->2020-2013=7
#Alps_plot$age[Alps_plot$Plot=="B3-03"]<-""
Alps_plot2$age[Alps_plot2$Plot=="S1-14"]<-"9.5" #~same age as S1-19 (9.5 years)
Alps_plot2$age[Alps_plot2$Plot=="T99"]<-"7" ##band old =2008-2018 -> median=2013->2020-2013=7
Alps_plot2$age[Alps_plot2$Plot=="OLIA-01"]<-"77.5" #same age than other LIA plots
Alps_plot2$age[Alps_plot2$Plot=="OLIA-09"]<-"77.5"
Alps_plot2$age[Alps_plot2$Plot=="OLIA-05"]<-"77.5"

#data geoA missing or wrong for PA-03
Alps_plot2$GeoA[Alps_plot2$Plot=="PA-03"]<-"Moderate"
#data Sand missing for T45-> error na =0
Alps_plot2$Sand[Alps_plot2$Plot=="T45"]<-"0"

#data gravel to correct - and +
Alps_plot2$Gravel[Alps_plot2$Gravel=="-"]<-"0"
Alps_plot2$Gravel[Alps_plot2$Gravel=="+"]<-"0.5"


#transforming GeoA in numeric variable

Alps_plot2 %>% mutate(GeoA=str_replace(GeoA,'No','0'))->Alps_plot2
Alps_plot2 %>% mutate(GeoA=str_replace(GeoA,'Low','1'))->Alps_plot2 
Alps_plot2 %>% mutate(GeoA=str_replace(GeoA,'Moderate','2'))->Alps_plot2 
Alps_plot2 %>% mutate(GeoA=str_replace(GeoA,'High','3'))->Alps_plot2 

## Modifying slope orientation class to slope orientation in degree -> Quantitative variable
unique(Alps_plot2$SlopeO)%>%as.factor()
  #Error in data orientation for S1-01 (WSE ->NNE )
Alps_plot2$SlopeO[Alps_plot2$Plot=="S1-01"]<-"NNE"
  #right join with orientation class-Orientation degree csv.
f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/data_enviro/Classe_pente.csv"
classe_pente<-read_csv(f, col_names = TRUE)



classe_pente %>%select(Slope_class,SlopeO_class)%>%right_join(Alps_plot2, by=c('Slope_class'='SlopeO'))->Alps_plot2
#view(Alps_plot)
#unique(Alps_plot$SlopeO_class)%>%as.factor()

#view(Alps_plot2)

##selecting LIA data
#Alps_plot2$Band[is.na(Alps_plot2$Band)]<-"NA"

R_env<-Alps_plot2%>%
  filter(Site!="Glacier Blanc")%>%#remove glacier Blanc data because geomorpho highly incomplete
  filter(Plant_cover!=0)%>%   
  #filter(Band!="LIA")%>% #remove LIA data
  #select(Plot,age, SlopeD, Sand, GeoA,Position_corr,SlopeO_class, Landform_corr, Rock)%>%as.data.frame()#Landform_corr, removed
  select(Plot,age, slope, Sand,Gravel, GeoA,expo, Rock,TPI, alti)%>%as.data.frame() #Position_corr
#need to be a data frame to set row names (Setting row names on a tibble is deprecated.)
#view(R_env)
#str(R_env)

unique(R_env$Plot)

#R_env$SlopeD<-as.numeric(R_env$SlopeD)
#R_env$GeoA<-as.factor(R_env$GeoA)
R_env$GeoA<-as.numeric(R_env$GeoA)
R_env$Position_corr<-as.factor(R_env$Position_corr)
#R_env$SlopeO_class<-as.numeric(R_env$SlopeO_class)
R_env$Landform_corr<-as.factor(R_env$Landform_corr)
R_env$age<-as.numeric(R_env$age)
R_env$Sand<-as.numeric(R_env$Sand)
R_env$Gravel<-as.numeric(R_env$Gravel)

#sapply(R_env,class)
#unique(R_env$Position_corr)


### Q matrix - species by trait matrix - traits

#DOES NOT WORK BECAUSE OF ALL THE ACCENTS???
# f<-"https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/TRAITS_notcomplete.csv"
# traits<-read_csv(f, col_names = TRUE)

traits<-TRAITS_modif2
#problem des taxons qui se repetent
# -133087, Cerastium arvense subsp. strictum Gaudin, 1828 - CRS and CCS (Les deux strategies sont presente une fois pour exactement le meme taxon) ????
# -83528, Arctostaphylos uva-ursi (L.) Spreng., 1825 - same problem
# pour le moment 133087=CRS et 83528=CSS

#view(traits)

traits$SA_CSR[traits$cd_ref=="133087"]<-"crs"
traits$SA_CSR[traits$cd_ref=="83528"]<-"css"

#regroupement mode de dispersion -> zoochore
# traits %>% mutate(dissémination=str_replace(dissémination,"épizoochore",'zoochore'))->traits
# traits %>% mutate(dissémination=str_replace(dissémination,"endozoochore",'zoochore'))->traits
# traits %>% mutate(dissémination=str_replace(dissémination,"myrmécochore",'zoochore'))->traits

#unique(traits$dissémination)

###Donnees traits dispo
#famille =324 = all taxons
# dissemination compil = 322
#pollinisation_compil = 320
#sexualite = 308
#chorologie = 308
#formation vegetale= 308
#fruit =294
#inflorescence = 291
#SA_CR = 226

traits%>%filter(nom_reconnu_ss_auteur!="na")%>%
  select(cd_ref,
         #famille,
         nom_reconnu_ss_auteur, 
         dissemination_compil,
         SLA, 
         SEEDM,
        # LDMCp,
         #sexualite,
         #CHOROLOGIE,
         #SA_CSR,
         #FORMATION_VEGETALE,
         pollinisation_compil)%>%
           unique()->Q_traits
view(Q_traits)
Q_traits%>%drop_na()->Q_traits

#add new column noNA
Q_traits$noNA<-1

view(Q_traits) #row=224 - 306 -120 especes
unique(Q_traits$cd_ref)#306



### L matrix - species matrix -spe
#reloading the save csv on GitHub
f <- "https://raw.githubusercontent.com/anaiszimmer/Analyses_flore/main/data/Data_sp_All.csv"
spe_all <- read_csv(f, col_names = TRUE)
#view(spe_all)

## Selection of data without LIA
#import Band data from Alps_plot2
#Alps_plot2_noLIA<-Alps_plot2 %>%filter(Band!="LIA")
#view(Alps_plot2_noLIA)
#Alps_plot2%>%select(Plot, Band)%>% right_join(spe_all, by=c('Plot'='Plot'))->spe_all


#removing data from glacier Blanc and from LIA
spe<-spe_all%>%filter(Site!="Glacier Blanc")#%>%filter(Band!="LIA")

#selecting species with trait data complete
Q_traits %>%select(nom_reconnu_ss_auteur, noNA)%>% right_join(spe, by=c('nom_reconnu_ss_auteur'='nom_reconnu_ss_auteur'))->spe
spe<-spe%>%filter(noNA==1)
#view(spe)

list_spe<-spe$nom_reconnu_ss_auteur%>%as_tibble()%>%rename(nom_reconnu_ss_auteur='value')%>%unique()#194 taxons - 181 taxons sans LIA
list_spe$sp_ok<-1
#view(list_spe)



#selecting taxon present in list_spe in Q_traits to create Q_traits_ok

list_spe %>%select(nom_reconnu_ss_auteur, sp_ok)%>% 
      right_join(Q_traits, by=c('nom_reconnu_ss_auteur'='nom_reconnu_ss_auteur'))%>%
      as.data.frame()->Q_traits
#view(Q_traits)
Q_traits_ok<-Q_traits%>%filter(sp_ok==1)%>%select(-sp_ok,-cd_ref,-noNA)
Q_traits_ok$sexualite<-as.factor(Q_traits_ok$sexualite)
Q_traits_ok$pollinisation_compil<-as.factor(Q_traits_ok$pollinisation_compil)
Q_traits_ok$dissemination_compil<-as.factor(Q_traits_ok$dissemination_compil)
Q_traits_ok$CHOROLOGIE<-as.factor(Q_traits_ok$CHOROLOGIE)
Q_traits_ok$FORMATION_VEGETALE<-as.factor(Q_traits_ok$FORMATION_VEGETALE)
Q_traits_ok$SA_CSR<-as.factor(Q_traits_ok$SA_CSR)
Q_traits_ok$famille<-as.factor(Q_traits_ok$famille)

#sapply(Q_traits_ok,class)

row.names(Q_traits_ok)<-Q_traits_ok$nom_reconnu_ss_auteur
Q_traits_ok<-Q_traits_ok%>%select(-nom_reconnu_ss_auteur)

view(Q_traits_ok)


#creating matrix

unique(spe$nom_reconnu_ss_auteur)#194

?dcast()
str(spe)

#missing recouvrement data for P98 (3674761) and T56 (36747674) in CBNA data base
spe$recouvrement_sp[spe$id_observation=="36747613"]<-"x" 
spe$recouvrement_sp[spe$id_observation=="36747674"]<-"12"

spe$recouvrement_sp[spe$recouvrement_sp=="+"]<-"0.1"
spe$recouvrement_sp[spe$recouvrement_sp=="x"]<-"0.1"
spe$recouvrement_sp[spe$recouvrement_sp=="r"]<-"0.1"

spe$recouvrement_sp<-as.numeric(spe$recouvrement_sp)
view(spe)
#spe<-data.frame(spe)

spe<-spe%>%select(Plot, nom_reconnu_ss_auteur, recouvrement_sp)

unique(spe$Plot)#274

##check plot missing between R_env (282) and spe (174)
# plot_check<-spe%>%select(Plot, nom_reconnu_ss_auteur)%>%right_join(R_env, by=c("Plot"="Plot"))
# view(plot_check) #-> 8 plot that have no data for LDMC or SLA -> need to re-ajuste R_env
# view(R_env)

L_spe<-spe%>%data.table::dcast(Plot~nom_reconnu_ss_auteur,value.var= 'recouvrement_sp')
L_spe[is.na(L_spe)] <- 0

row.names(L_spe)<-L_spe$Plot
L_spe<-L_spe%>%select(-Plot)

#names(spe) <- make.names(names(spe), unique=TRUE)
#filter(nom_reconnu_ss_auteur !="na")
view(L_spe)
sapply(L_spe,class)




#*** RE- AJUSTING R_env

#list of plot present in spe
#selecting plot with species and trait data complete

spe$noNA<-1
spe %>%select(Plot, noNA)%>%unique()%>% right_join(R_env, by=c('Plot'='Plot'))->R_env
R_env<-R_env%>%filter(noNA==1)%>%as.data.frame()
#view(R_env)#274

#R_env matrix for RQL
row.names(R_env)<-R_env$Plot
R_env<-R_env%>%select(-Plot, -noNA)

view(R_env) #282 (without Blanc) or 235 (without Blanc and LIA) ; or 274 if using SLA and LDMC trait data



## Separate analysis of each matrix
library(ade4)

#?dudi.coa
#?dudi.hillsmith
sapply(R_env,class)

dim(L_spe)
dim(R_env)
dim(Q_traits_ok)


view(L_spe)

afc_L <- dudi.coa(L_spe, scannf = FALSE, nf=2) # correspondence analysis to the sites x species matrix
#sapply(afc_L,class)

acp_R <- dudi.hillsmith(R_env, row.w = afc_L$lw,
                             scannf = FALSE,nf=2) # sites x environment matrix

acp_Q <- dudi.hillsmith(Q_traits_ok, row.w = afc_L$cw,
                        scannf = FALSE,nf=2) # species x traits matrix principal component analyses

# acp_Q <- dudi.hillsmith(Q_traits_ok, row.w = rep(1,nrow(Q_traits_ok))/nrow(Q_traits_ok),
#                         scannf = FALSE,nf=2)

#acp_Q <- dudi.hillsmith(Q_traits_ok, scann = FALSE)

rlq <- rlq(acp_R, afc_L, acp_Q,
                 scannf = FALSE)

summary(rlq)

## plot the output

par(mfrow = c(1, 3))
par(mfrow = c(1, 1))


plot(rlq)
s.arrow(rlq$l1,boxes = FALSE,xax = 1, yax = 2,addaxes = TRUE, cgrid = 1)

s.arrow(rlq$c1,boxes = TRUE,clabel = 0.7)

s.label(rlq$lQ, boxes = TRUE,clabel = 0.7)


randtest(rlq)
fourthcorner.rlq(rlq,type="Q.axes")
fourthcorner.rlq(rlq,type="R.axes")






unique(R_env$SlopeO)


#***************************************************

data(aravo)

aravo
dim(aravo$spe) # L matrix
dim(aravo$env) # R matrix
dim(aravo$traits) # Q matrix

view(aravo$spe) # L matrix
view(aravo$env) # R matrix
view(aravo$traits) # Q matrix


afcL.aravo <- dudi.coa(aravo$spe, scannf = FALSE) # correspondence analysis to the sites x species matrix

acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw,
                             scannf = FALSE) # sites x environment matrix

acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw,
                       scannf = FALSE) # species x traits matrix principal component analyses

rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)
summary(rlq.aravo)

#plots
par(mfrow = c(1, 3))

s.arrow(rlq.aravo$l1)

s.arrow(rlq.aravo$c1)

s.label(rlq.aravo$lQ, boxes = FALSE)




#********************************************************************************************************************************
#* CLUSTER ANALYSIS ON vegetation releve : L_Spe
#*******************************************************************************************************************************
#*
library(factoextra)

df<-L_spe
#2_Estimating the optimal number of clusters

fviz_nbclust(df,kmeans,method="wss")# k=4


#3_Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 6, nstart = 25)


# Print the results
print(km.res)

# Cluster number for each of the observations
km.res$cluster
head(km.res$cluster, 4)

# Cluster size
km.res$size

# Cluster means
(Cluster_mean<-km.res$centers)


#4_PLotting and applying a dimensionality reduction algorithm_PCA with fviz_cluster()

?fviz_cluster()


fviz_cluster(km.res,df)
#set ggrepel globally
options(ggrepel.max.overlaps = Inf)


fviz_cluster(km.res,df,ellipse.type="norm",geom="text",labelsize = 9,repel=TRUE,
             main="Cluster vegetation data - ")




