################### CREATION DES DATAFRAMES GLOBAUX IELP
IEP<-Imp_Gl_Paas%>%
  left_join(Exp_Gl_Paas,by = c("Year", "Period", "temps"))%>%
  ungroup(.)%>%
  select(temps,Import_Paas, Export_Paas)

IEL<-Imp_Gl_Lasp%>%
  left_join(Exp_Gl_Lasp,by = c("Year", "Period", "temps"))%>%
  ungroup(.)%>%
  select(temps,Import_Lasp, Export_Lasp)

####################################################
# Exportation des Fichiers Excel                   #
####################################################

#
setwd("~/R_Indice/Output")
Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
library(openxlsx) #load the package

#Globaux Laspeyres et Paasche
write.xlsx(x = IEL, file = paste("Indices_Globaux_Laspeyres", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
write.xlsx(x = IEP, file = paste("Indices_Globaux_Paasche", Sys.Date(), ".xlsx"),
           sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)

#Laspeyres- Paasche IMPORT
write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC1_Lasp, file = paste("Indice_Import_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC2_Lasp, file = paste("Indice_Import_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC1_Lasp, file = paste("Indice_Import_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC2_Lasp, file = paste("Indice_Import_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC1_Paas, file = paste("Indice_Import_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC2_Paas, file = paste("Indice_Import_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC1_Paas, file = paste("Indice_Import_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Imp_SITC2_Paas, file = paste("Indice_Import_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

#Export

write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC1_Lasp, file = paste("Indice_Export_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC2_Lasp, file = paste("Indice_Export_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC1_Lasp, file = paste("Indice_Export_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC2_Lasp, file = paste("Indice_Export_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC1_Paas, file = paste("Indice_Export_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC2_Paas, file = paste("Indice_Export_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC1_Paas, file = paste("Indice_Export_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
write.xlsx(x = Exp_SITC2_Paas, file = paste("Indice_Export_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)



###############################
# Graphique                   #
###############################
library(lubridate)
library(ggplot2)
#Paasche
pg1 = ggplot(IEP, aes(x = temps)) + geom_line(aes(y=Import_Paas, colour = "Import_Paas")) + geom_line(aes(y=Export_Paas, colour = "Export_Paas")) + geom_smooth(aes(y=Import_Paas))+geom_smooth(aes(y=Export_Paas))  + xlab("Période") + ylab("Indice Global(%)") + ggtitle("Indices des prix à l'Importation et à l'Exportation - Paasche")
ggsave("Graphe_Global_Paasche.pdf")
pg2 = ggplot(IEP, aes(x = month(IEP$temps), y=Import_Paas, group=year(IEP$temps))) + geom_line() + aes(colour=year(IEP$temps)) +  ylab("Indice Global(%)") + ggtitle("Evolution comparative de l'indices à l'Importation - Paasche")
ggsave("Graphe_Comparatif_Paasche.pdf")
#Lasp
lg1 = ggplot(IEL, aes(x = temps)) + geom_line(aes(y=Import_Lasp, colour = "Import_Lasp")) + geom_line(aes(y=Export_Lasp, colour = "Export_Lasp")) + geom_smooth(aes(y=Import_Lasp))+geom_smooth(aes(y=Export_Lasp))  + xlab("Période") + ylab("Indice Global(%)") + ggtitle("Indices des prix à l'Importation et à l'Exportation - Paasche")
ggsave("Graphe_Global_Laspeyres.pdf")
lg2 = ggplot(IEL, aes(x = month(IEL$temps), y=Import_Lasp, group=year(IEP$temps))) + geom_line() + aes(colour=year(IEP$temps)) +  ylab("Indice Global(%)") + ggtitle("Evolution comparative de l'indices à l'Importation - Laspeyres")
ggsave("Graphe_Comparatif_Laspeyres.pdf")

#R_Indice dossier
setwd("~/R_Indice")