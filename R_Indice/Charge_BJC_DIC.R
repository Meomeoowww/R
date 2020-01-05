###########################################Chargement BJCOMEXT#############################################


#Connexion et import dans res
#le champ [Year] n'étant pas libre, il faudra remplir les années correspondantes à la requete souhaitée
#
#En cas de changement de machine, veiller a bien corriger l'adresse server, 
#le nom du domaine et le sqlquery!! Ensuite veiller a installer RTools et les autres librairies néccessaires.

#setwd("~/R_Indice")

#BJC est le dataset principal en entier

library(RODBC)
dbconnection <- odbcDriverConnect('Driver=SQL Server; Server=WIN-E1LOQVIE4GD\\EUROTRACE; Database=BENIN; trusted_connection=yes')
BJC <- sqlQuery(dbconnection, "SELECT * 
                FROM [dbo].[BENIN_DATA_BJCOMEXT] 
                WHERE [Year] in (2015, 2016, 2017) and 
                [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')"
                , as.is=T)   

#formattage des variables d'intéret en numérique
BJC$VALEURINSAE<-as.numeric(BJC$VALEURINSAE)
BJC$POIDNET<-as.numeric(BJC$POIDNET)
BJC$QUANTITE<-as.numeric(BJC$QUANTITE)

  
#Importation des dictionnaires et relations utiles

##########REGIME (CPC7) et RELATIONS CPC2TYP CPC2FLOW
DIC_CPC <- sqlQuery(dbconnection, 
                "SELECT * FROM [dbo].[BENIN_DIC_CPC7]"
                , as.is=T)   
REL_CPC2TYP <- sqlQuery(dbconnection, 
                       "SELECT * FROM [dbo].[BENIN_DIC_CPC72TYPE]"
                       , as.is=T)
REL_CPC2FLOW <- sqlQuery(dbconnection, 
                        "SELECT * FROM [dbo].[BENIN_DIC_CPCFLOW]"
                        , as.is=T)

##########Dictionnaires (HS) (SITC) (BEC) et RELATIONS
DIC_HS <- sqlQuery(dbconnection, 
                    "SELECT * FROM [dbo].[BENIN_DIC_HS]"
                    , as.is=T)
DIC_SITC <- sqlQuery(dbconnection, 
                   "SELECT * FROM [dbo].[BENIN_DIC_SITC]"
                   , as.is=T) 
DIC_BEC <- sqlQuery(dbconnection, 
                   "SELECT * FROM [dbo].[BENIN_DIC_BEC]"
                   , as.is=T) 
REL_HS2SITC <- sqlQuery(dbconnection, 
                        "SELECT * FROM [dbo].[BENIN_DIC_HS2SITC]"
                        , as.is=T)
REL_HS2BEC <- sqlQuery(dbconnection, 
                         "SELECT * FROM [dbo].[BENIN_DIC_HS2BEC]"
                         , as.is=T)

##########Dictionnaires Mode de Transport (MODTRANSP), Pays (Country)
DIC_TRANSPORT <- sqlQuery(dbconnection, 
                   "SELECT * FROM [dbo].[BENIN_DIC_TRANSPORT]"
                   , as.is=T)   
DIC_COUNTRY <- sqlQuery(dbconnection, 
                        "SELECT * FROM [dbo].[BENIN_DIC_HS2SITC]"
                        , as.is=T)

##########Dictionnaires MOIS FLOW
DIC_MOIS <- sqlQuery(dbconnection, 
                          "SELECT * FROM [dbo].[BENIN_DIC_MOIS]"
                          , as.is=T)   
DIC_FLOW <- sqlQuery(dbconnection, 
                        "SELECT * FROM [dbo].[BENIN_DIC_FLOW]"
                        , as.is=T)
close(dbconnection)
