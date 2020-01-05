#Connexion et import dans res
#le champ [Year] n'?tant pas libre, il faudra remplir les ann?es correspondantes ? la requete souhait?e
#Taille limit?e ? 4 pour le champ sh4
#En cas de changement de machine, veiller a bien corriger l'adresse server, le nom du domaine et le sqlquery!! Ensuite veiller a installer RTools et les autres librairies nÃ©cessaires.

#setwd("~/R_Indice")

library(RODBC)
dbconnection <- odbcDriverConnect('Driver=SQL Server; Server=WIN-E1LOQVIE4GD\\EUROTRACE; Database=BENIN; trusted_connection=yes')
res <- sqlQuery(dbconnection, "SELECT [Year], [Period],[FLOW],[HS2012] as SH4, CONVERT(NUMERIC, sum([VALEURINSAE])) as VALEUR, CONVERT(NUMERIC, sum([POIDNET])) as POIDSNET, CONVERT(NUMERIC, sum([QUANTITE])) as QUANTITE FROM [dbo].[BENIN_DATA_INDICE3] WHERE [Year] in (2010,2011, 2012, 2013, 2014, 2016, 2017) and [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12') and [TRADETYPE] in('2', 'G') and [FLOW] in ('I', 'E', 'R') and LEN([HS2012])=4 group by [Year],[Period],[FLOW],[HS2012]", as.is=T)
close(dbconnection)

# Pr?paration des donn?es
res2<-data.frame(Year=res$Year, Period=res$Period, FLOW = gsub("R", "ET", gsub("E","ET", res$FLOW)), SH4=res$SH4, VALEUR=as.numeric(res$VALEUR), POIDSNET=as.numeric(res$POIDSNET), QUANTITE=as.numeric(res$QUANTITE))
res3<- aggregate(res2[,c("VALEUR","POIDSNET","QUANTITE")],by=list(Year = res2$Year, Period =  res2$Period, FLOW = res2$FLOW, SH4 = res2$SH4), sum, na.rm=TRUE)
dvu<-data.frame(Year = res3$Year,Period = res3$Period, FLOW = res3$FLOW, SH4 = res3$SH4, VALEURINSAE = res3$VALEUR,  VU=ifelse(res3$SH4!="2716", res3$VALEUR/res3$POIDSNET, res3$VALEUR/res3$QUANTITE))

#Importation depuis les fichiers
library(openxlsx)
#dvu<-read.xlsx("IndexPrime.xlsx", sheet = 1)
ech<-read.xlsx("Echantillon2015x.xlsx", sheet= 1)

#Renommage et Calcul de Laspeyres
my_data<-merge(dvu, ech, c("FLOW","SH4"))
El_Ind<-100*my_data$VU/my_data$PRIX_BASE

library(dplyr)

my_data1<-merge(my_data, summarize(group_by(my_data, FLOW, Year, Period), VTMENS = sum(VALEURINSAE)), c("Year", "Period", "FLOW"))

my_data2<-data.frame(Temps= as.Date(paste(my_data$Year, my_data$Period, 1, sep="-")), my_data, El_Ind, VTMENS = my_data1$VTMENS)

my_data2prime<- data.frame(my_data2, Ind_ps = my_data2$VALEURINSAE/my_data2$VTMENS/my_data2$El_Ind)

test0<-aggregate(my_data2prime$Ind_ps, list(my_data2prime$FLOW, my_data2prime$Year, my_data2prime$Period), sum, na.rm = TRUE)
test1<-data.frame(Flux = test0$Group.1, Annee = test0$Group.2, Mois = test0$Group.3, Indice_Paasche = 1/test0$x)
test2<-aggregate(my_data2$PONDERATION*my_data2$El_Ind/1000, list(my_data2$FLOW, my_data2$Year, my_data2$Period), sum, na.rm = TRUE)

#
 
test3SH<-my_data2 %>% 
  group_by(FLOW, Temps, SH2, Libellé_SH2) %>% 
  summarise(AgSH_Ind_Las = weighted.mean(El_Ind, PONDERATION)*sum(PONDERATION)/mean(PONDSH2))

test3STA<-my_data2 %>% 
  group_by(FLOW, Temps, Libellé_Statut) %>% 
  summarise(AGSTA_Ind_Las = weighted.mean(El_Ind, PONDERATION)*sum(PONDERATION)/mean(PONDSH2))

#Indices Elementaire Fichier attribution des Entêtes
names(my_data2)[names(my_data2)=="FLOW"]<-"Flux"
names(my_data2)[names(my_data2)=="Year"]<-"Annee"
names(my_data2)[names(my_data2)=="Period"]<-"Mois"
names(my_data2)[names(my_data2)=="El_Ind"]<-"Ind_Prod_Elt"

#Indices Mensuels Fichier attribution des Entêtes
names(test2)[names(test2)=="Group.1"]<-"Flux"
names(test2)[names(test2)=="Group.2"]<-"Annee"
names(test2)[names(test2)=="Group.3"]<-"Mois"
names(test2)[names(test2)=="x"]<-"Ind_Lasp"


#Compil my-data3 lasp paash fish
my_data3<-data.frame(temps=as.Date(paste(test2$Annee, test2$Mois, 1, sep="-"), "%Y-%m-%d"), test2, Ind_Paasch = test1$Indice_Paasche, Ind_Fish =sqrt(test2$Ind_Lasp*test1$Indice_Paasche))


#Exportation des Fichiers Excel a

Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
library(openxlsx) #load the package
write.xlsx(x = my_data3, file = paste("Indices_Globaux", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
write.xlsx(x = my_data2, file = paste("Indices_Produits", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = test3SH, file = paste("Indices_Chapitres", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
write.xlsx(x = test3STA, file = paste("Indices_Statut", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
# Graphique

library(ggplot2)
g1 = ggplot(my_data3, aes(x = temps, y=Ind_Lasp)) + aes(colour = Flux) + geom_line() + geom_smooth() + xlab("Période") + ylab("Indice Global(%)") + ggtitle("Indices des prix à l'Importation et à l'Exportation")
ggsave("Graphe_Global.pdf")
g2 = ggplot(my_data3, aes(x = Mois, y=Ind_Lasp, group=Annee)) + geom_line() + aes(colour=Annee) + facet_grid(.~Flux) + ylab("Indice Global(%)") + ggtitle("Evolution comparative des indices à l'Importation et à l'Exportation")
ggsave("Graphe_Comparatif.pdf")

#Effacer l'environnement