

setwd("D:\\Bureau\\HEC\\CERT\\LOG SATISTIQUES_AUTOMNE_2020")
ISO_CODE=read.csv("ISO_CODE_TABLE.csv")
Data2019=read.csv("OWID_Covid_Data_2019.csv")
Data2020=read.csv("OWID_Covid_Data_2020.csv")


###############################################################################################################################################
# Probl?me 1:
###############################################################################################################################################
## ?tape 1. Joindre les donn?es 2019 et 2020:

data1920=rbind(Data2019,Data2020)

## ?tape 2. ajouter les noms des pays et des continents:
Covid_Data_sol=merge(data1920,ISO_CODE,by = "iso_code", all.x = TRUE)

## ?tape 3. selectionner les colonnes demand?es:
Covid_Data_sol=Covid_Data_sol[,-c(8,9,10)]

## ?tape 4. changer les noms des colonnes:

names(Covid_Data_sol)[3]="nouveaux_cas" 
names(Covid_Data_sol)[4]="nouveaux_deces" 
names(Covid_Data_sol)[5]="nouveaux_tests" 
names(Covid_Data_sol)[6]="nb_total_tests"
names(Covid_Data_sol)[8]="taux_ mortalite_cardiovasculaire"
names(Covid_Data_sol)[9]="prevalence_diabete"
names(Covid_Data_sol)[10]="femmes_fumeuses"
names(Covid_Data_sol)[11]="hommes_fumeurs"
names(Covid_Data_sol)[13]="pays" 

## ?tape 5. Exclure World:
Covid_Data_sol=Covid_Data_sol[Covid_Data_sol$pays!="World",]

###############################################################################################################################################
# Pronl?me 2:
###############################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------
## 2.1
#----------------------------------------------------------------------------------------------------------------------------------------------

Covid_Data_sol$date=as.Date(Covid_Data_sol$date)

Covid_Data_sol$mois=months(Covid_Data_sol$date)

Covid_Data_sol$trimstre=quarters(Covid_Data_sol$date)

#----------------------------------------------------------------------------------------------------------------------------------------------
## 2.2. 
#----------------------------------------------------------------------------------------------------------------------------------------------

Covid_Data_sol$test_par_population=Covid_Data_sol$nb_total_tests /Covid_Data_sol$population

#----------------------------------------------------------------------------------------------------------------------------------------------
## 2.3.
#----------------------------------------------------------------------------------------------------------------------------------------------


moyen_par_date=aggregate(Covid_Data_sol[,c(3)],by=list(Covid_Data_sol$date),FUN="mean",na.rm=T)
colnames(moyen_par_date)=c("date","Moyen_affecte_par_date")

Covid_Data_sol=merge(Covid_Data_sol,moyen_par_date,by="date",all.x = T)

Covid_Data_sol$comparaison_moyenne_globale=ifelse(Covid_Data_sol$nouveaux_cas >Covid_Data_sol$Moyen_affecte_par_date,1,0)


#----------------------------------------------------------------------------------------------------------------------------------------------
## 2.4.
#----------------------------------------------------------------------------------------------------------------------------------------------

### Etape 1: cr?er la nouvelle colonne:
#Note: Si hommes_fumeurs ou femmes_fumeuses est NA alors on ne peut pas faire la comparaison.
# Donc nous avons ajout? une quatri?me cat?gorie "Donn?es manquantes" 
Covid_Data_sol$comparaison_sex=ifelse(is.na(Covid_Data_sol$hommes_fumeurs) | is.na(Covid_Data_sol$femmes_fumeuses),"Donn?es manquantes",
                                      ifelse(Covid_Data_sol$hommes_fumeurs > Covid_Data_sol$femmes_fumeuses,
                                              "Hommes_plus_Femmes  ",
                                               ifelse(Covid_Data_sol$hommes_fumeurs < Covid_Data_sol$femmes_fumeuses,
                                                     "Hommes_Moins_Femmes  ",
                                                       "Hommes_Egale_Femmes")
                                             )
                                    )
###############################################################################################################################################
# Pronl?me 3.
###############################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------
## 3.1.
#----------------------------------------------------------------------------------------------------------------------------------------------
d31=aggregate(Covid_Data_sol[,c(3)],by=list(Covid_Data_sol$date),FUN="sum",na.rm=T)
names(d31)=c("date","nb_total_affecte")

d31[order(d31$nb_total_affecte,decreasing=T) , ][1,1]

# ***ou bien utiliser la foncion max pour r?pondre ? la question:
d31[d31$nb_total_affecte==max(d31$nb_total_affecte),]

#"2020-07-30"

#----------------------------------------------------------------------------------------------------------------------------------------------
## 3.2.
#----------------------------------------------------------------------------------------------------------------------------------------------
## calculer la population  et le nombre total de deces par continent et par pays
t_deces=aggregate(Covid_Data_sol[,c(4)],by=list(Covid_Data_sol$continent, Covid_Data_sol$pays ),FUN=c("sum"),na.rm=T)
t_pop=aggregate(Covid_Data_sol[,c(7)],by=list(Covid_Data_sol$continent, Covid_Data_sol$pays ),FUN=c("max"),na.rm=T)

t1=merge(t_pop,t_deces,by=c("Group.1","Group.2"))
names(t1)=c("continent","pays","population_T","nb_total_deces")


## calculer la population totale et le nombre total de deces par continent 
table32=aggregate(t1[,c(3,4)],by=list(t1$continent),FUN="sum",na.rm=T)
names(table32)=c("continent","population_T","nb_total_deces")

## calculer le taux de deces par continent
table32$taux_deces=table32$nb_total_deces/table32$population_T

#----------------------------------------------------------------------------------------------------------------------------------------------
## 3.3.
#----------------------------------------------------------------------------------------------------------------------------------------------
## calculer le nombre total de personnes affect?s et d?c?d?s par date et par continent
t1=aggregate(Covid_Data_sol[,c(3,4)],by=list(date=Covid_Data_sol$date ,continent=Covid_Data_sol$continent),FUN="sum",na.rm=T)

## calculer la minimum de personnes affect?s et d?c?d?s par date et par continent
t2=aggregate(Covid_Data_sol[,c(3,4)],by=list(date=Covid_Data_sol$date ,continent=Covid_Data_sol$continent),FUN="min",na.rm=T)

## calculer la maximum de personnes affect?s et d?c?d?s par date et par continent
t3=aggregate(Covid_Data_sol[,c(3,4)],by=list(date=Covid_Data_sol$date ,continent=Covid_Data_sol$continent),FUN="max",na.rm=T)

## mettre ensemble les 3 tables
t4=merge(t1,t2,by=c("date","continent"))
table33=merge(t4,t3,by=c("date","continent"))

names(table33)=c("date","continent",
            "nb_total_affecte","nb_total_deces",
            "nb_min_affecte","nb_min_deces",
            "nb_max_affecte","nb_max_deces")

## ordonner la sortie
table33=table33[with(table33, order(DATE, nb_total_deces,decreasing = T)), ] 



#----------------------------------------------------------------------------------------------------------------------------------------------
## 3.4.
#----------------------------------------------------------------------------------------------------------------------------------------------

#Total personnes affect?es et d?c?d?es
total.affectees.decedees<-aggregate(cbind(nouveaux_cas,nouveaux_deces)~pays,Covid_Data_sol,FUN="sum",na.rm=T)

#min date 100cas
min_date_100js<-aggregate(date~pays,data=Covid_Data_sol[Covid_Data_sol$nouveaux_cas>=100,],FUN="min",na.rm=T)

#nombre jours 1 d?c?s ou plus
nb_jours_deces1plus<-aggregate(date~pays,data=Covid_Data_sol[Covid_Data_sol$nouveaux_deces>=1,],FUN= "length")

#FUsion des donn?es calcul?es pr?c?demment

resultat.3.4<-merge(total.affectees.decedees,min_date_100js,by="pays")
resultat.3.4<-merge(resultat.3.4,nb_jours_deces1plus,by="pays")

names(resultat.3.4)<-c("pays","total_nouveaux_cas","total_nouveaux_deces","min_date_100cas","nb_jours_deces1plus")

resultat.3.4=resultat.3.4[order(resultat.3.4$total_nouveaux_cas,decreasing=T),]
head(resultat.3.4)


###############################################################################################################################################
# Pronl?me 4:
###############################################################################################################################################
#install.packages("ggplot2 ")
library(ggplot2)


#install.packages("gridExtra")
library(gridExtra)

#----------------------------------------------------------------------------------------------------------------------------------------------
## 4.1.
#----------------------------------------------------------------------------------------------------------------------------------------------

t1=Covid_Data_sol$femmes_fumeuses
t1=as.data.frame(t1)
names(t1)=c("Fumeur")
t1$Sexe="Femme"

t2=Covid_Data_sol$hommes_fumeurs
t2=as.data.frame(t2)
names(t2)=c("Fumeur")
t2$Sexe="Homme"

table41=rbind(t1,t2)


ggplot(table41,aes( y=Fumeur, x = Sexe)) + 
  geom_boxplot(na.rm = TRUE, colour="#00aaff") + 
  xlab("Sexe") + ylab("!!!!!")+
  ggtitle(paste0("Comparaison Homme/Femme"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#----------------------------------------------------------------------------------------------------------------------------------------------
## 4.2.
#----------------------------------------------------------------------------------------------------------------------------------------------

#ttest
Model0=t.test(t1$Fumeur,t2$Fumeur)
Model0

###############################################################################################################################################
# Pronl?me 5:
###############################################################################################################################################

#----------------------------------------------------------------------------------------------------------------------------------------------
## 5.1.
#----------------------------------------------------------------------------------------------------------------------------------------------

#Agr?ger les donn?es par continent et par date
agg_Covid_Data_sol=aggregate(nouveaux_cas~continent+date,data=Covid_Data_sol,FUN=sum)
agg_Covid_Data_sol$date = as.Date(agg_Covid_Data_sol$date)

#Cr?er les graphiques par continent
ASIA=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=='Asia',], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#C4961A")+
  ggtitle("Asia")

Europe=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=="Europe",], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#00AFBB")+
  ggtitle("Europe")

Africa=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=="Africa",], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#FC4E07")+
  ggtitle("Africa")

Oceania=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=="Oceania",], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#52854C")+
  ggtitle("Oceania")
  ggtitle("Oceania")

North_America=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=="North America",], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#009E73")+
  ggtitle("North America")
  ggtitle("North America" )

South_America=ggplot(agg_Covid_Data_sol[agg_Covid_Data_sol$continent=="South America",], aes(x=date, y=nouveaux_cas))+
  geom_line(na.rm = TRUE,color="#CC79A7")+
  ggtitle("South America")


grid.arrange(
  ASIA,
  Europe,
  Africa,
  Oceania,
  North_America,
  South_America,
  nrow = 3,
  top = "1ere vague de pandemie par continent"
)






#----------------------------------------------------------------------------------------------------------------------------------------------
## 5.2.
#----------------------------------------------------------------------------------------------------------------------------------------------

#Fonction pour calculer le cumulatif
cumul_par_pays<-function(data,var_to_sum,var_to_group){
  data[is.na(data[,var_to_sum]),var_to_sum] = 0
  liste_items = unique(data[,var_to_group])
  
  for (item in liste_items){
    sub_data=data[data[,var_to_group]==item,]
    cumul=cumsum(sub_data[,var_to_sum])
    data[data[,var_to_group]==item,paste0("cumul_",var_to_sum)]=cumul
    
  } 
  
  return(data)
  
}
Covid_Data_sol=Covid_Data_sol[order(Covid_Data_sol$date),]
data_cumulatif=cumul_par_pays(Covid_Data_sol,"nouveaux_cas",'pays')


#----------------------------------------------------------------------------------------------------------------------------------------------
## 5.3.
#----------------------------------------------------------------------------------------------------------------------------------------------


#Identifier les 5 pays ayant eu le plus de cas cumul?s 
top5_pays=aggregate(cumul_nouveaux_cas~pays,data=data_cumulatif,FUN=sum)
top5_pays=top5_pays[order(top5_pays$cumul_nouveaux_cas,decreasing=T),][1:5,]

#Changer le type de donn?es
data_cumulatif$pays=as.character(data_cumulatif$pays)
data_cumulatif$date=as.Date(data_cumulatif$date)

#Afficher graphiquement les 5 pays identifi?s
ggplot(data=data_cumulatif[data_cumulatif$pays %in% top5_pays$pays,],aes( x = date,
                   y=cumul_nouveaux_cas,
                   colour=factor(pays))) + 
  geom_line()



#----------------------------------------------------------------------------------------------------------------------------------------------
## 6.1.
#----------------------------------------------------------------------------------------------------------------------------------------------

## Relation entre nombre de deces et nouveau cas
ggplot(Covid_Data_sol,
       aes(x = nouveaux_cas, y = nouveaux_deces)) +
  geom_point(na.rm = TRUE,alpha = 0.3,  position = position_jitter()) + stat_smooth(method = "lm")


#----------------------------------------------------------------------------------------------------------------------------------------------
## 6.2.
#----------------------------------------------------------------------------------------------------------------------------------------------

### Construire un mod?le de r?gression lin?aire
model_rl  <- lm(nouveaux_deces~nouveaux_cas  ,
             data = Covid_Data_sol)
summary(model_rl)
  




