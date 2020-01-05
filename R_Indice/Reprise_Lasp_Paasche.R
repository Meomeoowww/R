#Importation depuis les fichiers
library(openxlsx)
dvu<-read.xlsx("IndexPrime.xlsx", sheet = 1)
ech<-read.xlsx("Echantillon2015.xlsx", sheet= 1)

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

names(my_data2)[names(my_data2)=="FLOW"]<-"Flux"
names(my_data2)[names(my_data2)=="Year"]<-"Annee"
names(my_data2)[names(my_data2)=="Period"]<-"Mois"
names(my_data2)[names(my_data2)=="El_Ind"]<-"Ind_Prod_Elt"
names(test2)[names(test2)=="Group.1"]<-"Flux"
names(test2)[names(test2)=="Group.2"]<-"Annee"
names(test2)[names(test2)=="Group.3"]<-"Mois"
names(test2)[names(test2)=="x"]<-"Ind_Lasp"
my_data3<-data.frame(temps=as.Date(paste(test2$Annee, test2$Mois, 1, sep="-"), "%Y-%m-%d"), test2, Ind_Paasch = test1$Indice_Paasche)

#Exportation des Fichiers Excel

Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
library(openxlsx) #load the package
write.xlsx(x = my_data3, file = paste("Indices_Globaux", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
write.xlsx(x = my_data2, file = paste("Indices_Produits", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

# Graphique

library(ggplot2)
g1 = ggplot(my_data3, aes(x = temps, y=Ind_Lasp)) + aes(colour = Flux) + geom_line() + geom_smooth() + xlab("Période") + ylab("Indice Global(%)") + ggtitle("Indices des prix à l'Importation et à l'Exportation")
ggsave("Graphe_1.pdf")
g2 = ggplot(my_data3, aes(x = Mois, y=Ind_Lasp, group=Annee)) + geom_line() + aes(colour=Annee) + facet_grid(.~Flux) + ylab("Indice Global(%)") + ggtitle("Evolution comparative des indices à l'Importation et à l'Exportation")
ggsave("Graphe_2.pdf")

#Effacer l'environnement