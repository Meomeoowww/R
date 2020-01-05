#' @title Finalisation des calculs et Exportation des Outputs
#'
#' @description Remplissage avec le prix de la période la plus proche pour Laspeyres, Aucune imputation pour Paasche,
#'
#' @param yearz
#'
#' @return les datasets contenant les indices et les outputs dans le dossier eiti
#'
#' @examples
#'
#' @export


C_db <- function(yearz) {

#filtre BJC1 Import

#mettre le champ Period ? OO pour l'ann?e de base '2015'
  BJC1<-BJC1 %>%
    mutate(Period=replace(Period, which(Year==yearz), '00'))%>%
    filter(POIDNET>0)


  #Donn?es annuelles subsett?es ? l'Import et sur le commerce g?n?ral
  Ech_Im<-subset(BJC1, (BJC1$FLOW=="I") & ((BJC1$TRADETYPE=="2")|(BJC1$TRADETYPE=="G")))
  Ech_Ex<-subset(BJC1, ((BJC1$FLOW=="E")|(BJC1$FLOW=="R")) & ((BJC1$TRADETYPE=="2")|(BJC1$TRADETYPE=="G")))

  #Ajouter le nombre (n) d'observations par sous-?chantillon du couple ann?e
  #p?riode bureau produit

  I31<-Ech_Im %>% add_count(Year, Period, HS6)
  E31<-Ech_Ex %>% add_count(Year, Period, HS6)

  #Donn?es subsett?es sur les transactions pour lesquelles le couple Ann?e - Produit contient plus de 30 observations
  #I31<-subset(I30, n>30)
  #E31<-subset(E30, n>30)

  #Compute MMI et RIQ:: OUTLTUK3 uniquement sur "YEAR" et "PERIOD", ATTENTION!!!

  library(dplyr)
  #Important NOTE, OUTLIERS ARE REPLACED BY NA IN LOG, So you need to Suppress them
  #Import
  I_OUTL<-I31%>%
    group_by(Year, Period, HS6) %>%
    mutate(log=log(Unit.Value))%>%
    #message("Summary of ", .$Year, " and ", $.Period, .$Lib_FR)
    do(OutlTuk3(.,log,max(.$n)))

  #Export
  E_OUTL<-E31%>%
    group_by(Year, Period, HS6) %>%
    mutate(log=log(Unit.Value))%>%
    #print(paste("Summary of",.$Year)%>%
    #message("Summary of ", .$Year)%>%
    do(OutlTuk3(.,log,max(.$n)))


  ####Suppression des NA (A utiliser lorsque OUtlTuk2 est ex?cut?)
  I_OUTL<-I_OUTL[complete.cases(I_OUTL$log), ]

  E_OUTL<-E_OUTL[complete.cases(E_OUTL$log), ]

  #Matrice des MMIRIQ

  IMatMMIRIQ<-I_OUTL%>%
    group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
    do(MMI=MMIRIQ2(., Unit.Value)[1], RIQ=MMIRIQ2(., Unit.Value)[2], Med=median(.$Unit.Value))

  EMatMMIRIQ<-E_OUTL%>%
    group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
    do(MMI=MMIRIQ2(., Unit.Value)[1], RIQ=MMIRIQ2(., Unit.Value)[2], Med=median(.$Unit.Value))

  #Filtre sur les MMI*RIQ<=1
  IMatMMIRIQ<-IMatMMIRIQ%>%
    mutate(.,ProdMR=round(MMI*RIQ, 1))%>%
    filter(ProdMR<=1)

  EMatMMIRIQ<-EMatMMIRIQ%>%
    mutate(.,ProdMR=round(MMI*RIQ, 1))%>%
    filter(ProdMR<=1)



  #fusion de MatMMIRIQ et des totaux valeur et quantit?
  ITL<- I_OUTL %>%
    select (Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6, VALEURINSAE, POIDNET)%>%
    group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6)%>%

    summarise(VT=sum(VALEURINSAE), PT=sum(POIDNET))%>%
    left_join(IMatMMIRIQ, .)%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))


  ETL<- E_OUTL %>%
    select (Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6, VALEURINSAE, POIDNET)%>%
    group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6)%>%

    summarise(VT=sum(VALEURINSAE), PT=sum(POIDNET))%>%
    left_join(EMatMMIRIQ, .)%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))

  #Drop Inutiles
  #Enlever les objets inutiles ()
  rm(BJC1, E_OUTL, I_OUTL, E31, I31, Ech_Ex, Ech_Im, EMatMMIRIQ, IMatMMIRIQ)


  ##########################################################Prep IETL
  #Pr?paration ETL2/ITL2

  ##################Export
  #Mod ETL
  ETL2<-ETL%>%
    mutate(NID=paste(BUREAU, MODTRANSPORT, PARTENAIRE, HS6, sep =""))%>%
    #mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
    select(Year, Period, temps, BUREAU, MODTRANSPORT, PARTENAIRE, NID, HS6, Med2, VT2, PT2)

  ##################Import
  #Mod ITL
  ITL2<-ITL%>%
    mutate(NID = paste(BUREAU,MODTRANSPORT,PARTENAIRE,HS6, sep =""))%>%
    #mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    mutate(VT2=as.numeric(VT), PT2=as.numeric(PT), Med2=as.numeric(Med))%>%
    select(Year, Period, temps, BUREAU,MODTRANSPORT,PARTENAIRE,NID, HS6, Med2, VT2, PT2)

  #Vider m?moire
  #rm(ETL,ITL,n, F_Index)

  ######################################################################

  #Calcul de L'indice ?l?mentaire2:Fonction Index
  ## Fonction Indice El
  F_Index4 <- function(df) {
    if (min(df$Year)==yearz) {
      Red<-vector(mode="double", length = nrow(df))
      for (i in 1:nrow(df))
        Red[i]<-as.numeric(df$Med2[i]) / as.numeric(df$Med2[which(df$Year==yearz)])
    }
    else {
      Red<-vector(mode="double", length = nrow(df))
      Red<-""}

    Mx<-data.frame(Year=df$Year, Period=df$Period, temps= df$temps, BUREAU=df$BUREAU, MODTRANSPORT=df$MODTRANSPORT, PARTENAIRE=df$PARTENAIRE, HS6=df$HS6, NID=df$NID, Red=as.factor(Red))
    return(Mx)
  }

  #Calcul de L'indice ?l?mentaire3
  #Export:

  ### NB: Si Carli=NA, alors c'est que le produit existe en 2016 mais n'a pas ?t? ?chang? ? la p?riode courante du NA

  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
#Moy Harmonique
  hm_mean = function(x, na.rm=TRUE){
    1/mean(1/x, na.rm=na.rm)
  }

  EDat<-ETL2%>%
    group_by(NID)%>%
    do(F_Index4(.))%>%
    mutate(Red=as.numeric(Red))%>%
    group_by(Year, Period, HS6)%>%
    summarize(Carli=100*gm_mean(Red, na.rm = T))%>%
    filter(!is.na(Carli)) #Pour enlever les cas ou des Red sont vides, ie le produit n'apparait pas ? l'ann?e de base 2015

  EPonderation<-ETL2%>%
    group_by(Year, Period, HS6)%>%
    summarize(Valeur=sum(VT2))%>%
    mutate(Pond10000=Valeur/sum(Valeur, na.rm = T)*10000)

  EDat<-EDat%>%
    left_join(.,EPonderation, by=c("Year"="Year", "Period"="Period", "HS6"="HS6"))


  # Import
  ### NB: Si Carli=NA, alors c'est que le produit existe en 2016 mais n'a pas ?t? ?chang? ? la p?riode 2015

  IDat<-ITL2%>%
    group_by(NID)%>%
    do(F_Index4(.))%>%
    mutate(Red=as.numeric(Red))%>%
    ungroup(.)%>%
    group_by(Year, Period, HS6)%>%
    summarize(Carli=100*gm_mean(Red, na.rm = T))%>%
    filter(!is.na(Carli)) #Pour enlever les cas ou des Red sont vides, ie le produit n'apparait pas ? l'ann?e de base 2015

  IPonderation<-ITL2%>%
    group_by(Year, Period, HS6)%>%
    summarize(Valeur=sum(VT2, na.rm = T))%>%
    mutate(Pond10000=Valeur/sum(Valeur, na.rm = T)*10000)

  IDat<-IDat%>%
    left_join(.,IPonderation, by=c("Year"="Year", "Period"="Period", "HS6"="HS6"))

  #library("dplyr")
  ##Nettoyage Dico
  DIC_HS<-DIC_HS%>%
    select(Code, Libelle=Lib_FR)
  DIC_SITC<-DIC_SITC%>%
    select(Code, Libelle=Lib_FR)

  REL_HS2SITC<-REL_HS2SITC%>%
    mutate(HS6=substr(Code,1,6))
  REL_HS2SITC<-REL_HS2SITC%>%
    select(HS6, SITC)%>%
    group_by(HS6, SITC)%>%
    summarise(.)


  #####################################################EXPORTATION
  #Expand sur la base des produits de l'ann?e de base
  E<-expand.grid(Year=unique(EDat$Year),Period=unique(EDat$Period),HS6=unique(EDat$HS6))
  E1<-E%>%
    rowwise %>%
    mutate(d1= ifelse((Year==yearz & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
    filter((d1==0)) %>%
    select(Year, Period, HS6)%>%
    left_join(.,EDat)%>%
    arrange(Year, Period, HS6)

  library("tidyverse")
  E2<-E1 %>%
    group_by(HS6)%>%
    fill(Carli)%>%
    mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    group_by(HS6)%>%
    mutate(Pond0=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(HS2)%>%
    mutate(PondHS2=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(HS4)%>%
    mutate(PondHS4=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
    mutate(SITC1=substr(SITC,1,1), SITC2=substr(SITC,1,2))%>%
    ungroup(.)%>%
    group_by(SITC1)%>%
    mutate(PondSITC1=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(SITC2)%>%
    mutate(PondSITC2=sum(Pond10000[which(Year==yearz)], na.rm = T))

  #HS
  Exp_HS2_Lasp<- E2%>%
    group_by(Year,Period,HS2)%>%
    summarise(Export_Lasp_HS2=sum(Carli*Pond0, na.rm = T)/mean(PondHS2, na.rm = T), Pond=mean(PondHS2, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS2"="Code"))

  Exp_HS4_Lasp<- E2%>%
    group_by(Year,Period,HS4)%>%
    summarise(Export_Lasp_HS4=sum(Carli*Pond0, na.rm = T)/mean(PondHS4, na.rm = T), Pond=mean(PondHS4, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS4"="Code"))

  #SITC
  Exp_SITC1_Lasp<- E2%>%
    group_by(Year,Period,SITC1)%>%
    summarise(Export_Lasp_SITC1=sum(Carli*Pond0, na.rm = T)/mean(PondSITC1, na.rm = T), Pond=mean(PondSITC1, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC1"="Code"))

  Exp_SITC2_Lasp<- E2%>%
    group_by(Year,Period,SITC2)%>%
    summarise(Export_Lasp_SITC2=sum(Carli*Pond0, na.rm = T)/mean(PondSITC2, na.rm = T), Pond=mean(PondSITC2, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC2"="Code"))

  #Global
  Exp_Gl_Lasp<-E2%>%
    group_by(Year, Period)%>%
    summarize(Export_Lasp=sum(Carli*Pond0, na.rm = T)/10000)%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
  #filter(!is.na(temps))


  #####################################################IMPORTATION
  #Expand sur la base des produits de l'ann?e de base
  I<-expand.grid(Year=unique(IDat$Year),Period=unique(IDat$Period),HS6=unique(IDat$HS6))
  I1<-I%>%
    rowwise %>%
    mutate(d1= ifelse((Year==yearz & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
    filter((d1==0)) %>%
    select(Year, Period, HS6)%>%
    left_join(.,IDat)%>%
    arrange(Year, Period, HS6)

  #library("tidyverse")
  I2<-I1 %>%
    group_by(HS6)%>%
    fill(Carli)%>%
    mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    group_by(HS6)%>%
    mutate(Pond0=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(HS2)%>%
    mutate(PondHS2=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(HS4)%>%
    mutate(PondHS4=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
    mutate(SITC1=substr(SITC,1,1), SITC2=substr(SITC,1,2))%>%
    ungroup(.)%>%
    group_by(SITC1)%>%
    mutate(PondSITC1=sum(Pond10000[which(Year==yearz)], na.rm = T))%>%
    ungroup(.)%>%
    group_by(SITC2)%>%
    mutate(PondSITC2=sum(Pond10000[which(Year==yearz)], na.rm = T))

  #HS
  Imp_HS2_Lasp<- I2%>%
    group_by(Year,Period,HS2)%>%
    summarise(Import_Lasp_HS2=sum(Carli*Pond0, na.rm = T)/mean(PondHS2, na.rm = T), Pond=mean(PondHS2, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS2"="Code"))

  Imp_HS4_Lasp<- E2%>%
    group_by(Year,Period,HS4)%>%
    summarise(Export_Lasp_HS4=sum(Carli*Pond0, na.rm = T)/mean(PondHS4, na.rm = T), Pond=mean(PondHS4, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS4"="Code"))
  #SITC
  Imp_SITC1_Lasp<- I2%>%
    group_by(Year,Period,SITC1)%>%
    summarise(Import_Lasp_SITC1=sum(Carli*Pond0, na.rm = T)/mean(PondSITC1, na.rm = T), Pond=mean(PondSITC1, na.rm = T))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC1"="Code"))

  Imp_SITC2_Lasp<- I2%>%
    group_by(Year,Period,SITC2)%>%
    summarise(Import_Lasp_SITC2=sum(Carli*Pond0)/mean(PondSITC2), Pond=mean(PondSITC2))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC2"="Code"))

  #Global
  Imp_Gl_Lasp<-I2%>%
    group_by(Year, Period)%>%
    summarize(Import_Lasp=sum(Carli*Pond0, na.rm = T)/10000)%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
  #filter(!is.na(temps))

  rm(E,E1,E2,EPonderation,ETL,ETL2,I,I1,I2,IPonderation,ITL,ITL2,dbconnection,F_Index4)


  ###########Paasche

  ##Nettoyage Dico
  #DIC_HS<-DIC_HS%>%
  #  select(Code, Libell?=Lib_FR)
  #DIC_SITC<-DIC_SITC%>%
  #  select(Code, Libell?=Lib_FR)
  #REL_HS2SITC<-REL_HS2SITC%>%
  #  mutate(HS6=substr(Code,1,6))

  #####################################################EXPORTATION
  #Expand sur la base des produits de l'ann?e de base
  E<-expand.grid(Year=unique(EDat$Year),Period=unique(EDat$Period),HS6=unique(EDat$HS6))
  E1<-E%>%
    rowwise %>%
    mutate(d1= ifelse((Year==yearz & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
    filter((d1==0)) %>%
    select(Year, Period, HS6)%>%
    left_join(.,EDat)%>%
    arrange(Year, Period, HS6)%>%
    mutate(Pond10000 = if_else(is.na(Pond10000), 0, Pond10000))

  library("tidyverse")
  E2<-E1 %>%
    group_by(HS6)%>%
    fill(Carli)%>%
    mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    ungroup(.)%>%
    group_by(Year, Period)%>%
    mutate(Pondt=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period, HS2)%>%
    mutate(PondHS2=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period,HS4)%>%
    mutate(PondHS4=sum(Pond10000, na.rm=TRUE))%>%
    left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
    mutate(SITC1=substr(SITC,1,1), SITC2=substr(SITC,1,2))%>%
    ungroup(.)%>%
    group_by(Year, Period,SITC1)%>%
    mutate(PondSITC1=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period,SITC2)%>%
    mutate(PondSITC2=sum(Pond10000, na.rm=TRUE))

  #HS
  Exp_HS2_Paas<- E2%>%
    group_by(Year,Period,HS2)%>%
    summarise(Export_Paas_HS2=1/sum(Pond10000/PondHS2/Carli, na.rm=TRUE), Pond=mean(PondHS2, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS2"="Code"))

  Exp_HS4_Paas<- E2%>%
    group_by(Year,Period,HS4)%>%
    summarise(Export_Paas_HS4=1/sum(Pond10000/PondHS4/Carli, na.rm=TRUE), Pond=mean(PondHS4, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS4"="Code"))

  #SITC
  Exp_SITC1_Paas<- E2%>%
    group_by(Year,Period,SITC1)%>%
    summarise(Export_Paas_SITC1=1/sum(Pond10000/PondSITC1/Carli, na.rm=TRUE), Pond=mean(PondSITC1, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC1"="Code"))

  Exp_SITC2_Paas<- E2%>%
    group_by(Year,Period,SITC2)%>%
    summarise(Export_Paas_SITC2=1/sum(Pond10000/PondSITC2/Carli, na.rm=TRUE), Pond=mean(PondSITC2, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC2"="Code"))

  #Global
  Exp_Gl_Paas<-E2%>%
    group_by(Year, Period)%>%
    summarise(Export_Paas=1/sum(Pond10000/Pondt/Carli, na.rm=TRUE), Pond=sum(Pond10000, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
  #filter(!is.na(temps))


  #####################################################IMPORTATION
  #Expand sur la base des produits de l'ann?e de base
  I<-expand.grid(Year=unique(IDat$Year),Period=unique(IDat$Period),HS6=unique(IDat$HS6))
  I1<-I%>%
    rowwise %>%
    mutate(d1= ifelse((Year==yearz & Period %in% c("01","02","03","04","05","06","07","08","09","10","11","12"))|!Year==2015 & Period=="00",1,0))%>%
    filter((d1==0)) %>%
    select(Year, Period, HS6)%>%
    left_join(.,IDat)%>%
    arrange(Year, Period, HS6)%>%
    mutate(Pond10000 = if_else(is.na(Pond10000), 0, Pond10000))

  library("tidyverse")
  I2<-I1 %>%
    group_by(HS6)%>%
    fill(Carli)%>%
    mutate(HS4=substr(HS6,1,4), HS2=substr(HS6,1,2))%>%
    ungroup(.)%>%
    group_by(Year, Period)%>%
    mutate(Pondt=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period, HS2)%>%
    mutate(PondHS2=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period,HS4)%>%
    mutate(PondHS4=sum(Pond10000, na.rm=TRUE))%>%
    left_join(.,REL_HS2SITC, by=c("HS6"="HS6"))%>%
    mutate(SITC1=substr(SITC,1,1), SITC2=substr(SITC,1,2))%>%
    ungroup(.)%>%
    group_by(Year, Period,SITC1)%>%
    mutate(PondSITC1=sum(Pond10000, na.rm=TRUE))%>%
    ungroup(.)%>%
    group_by(Year, Period,SITC2)%>%
    mutate(PondSITC2=sum(Pond10000, na.rm=TRUE))

  #HS
  Imp_HS2_Paas<- I2%>%
    group_by(Year,Period,HS2)%>%
    summarise(Import_Paas_HS2=1/sum(Pond10000/PondHS2/Carli, na.rm=TRUE), Pond=mean(PondHS2, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS2"="Code"))

  Imp_HS4_Paas<- I2%>%
    group_by(Year,Period,HS4)%>%
    summarise(Import_Paas_HS4=1/sum(Pond10000/PondHS4/Carli, na.rm=TRUE), Pond=mean(PondHS4, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_HS, by=c("HS4"="Code"))

  #SITC
  Imp_SITC1_Paas<- I2%>%
    group_by(Year,Period,SITC1)%>%
    summarise(Import_Paas_SITC1=1/sum(Pond10000/PondSITC1/Carli, na.rm=TRUE), Pond=mean(PondSITC1, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC1"="Code"))

  Imp_SITC2_Paas<- I2%>%
    group_by(Year,Period,SITC2)%>%
    summarise(Import_Paas_SITC2=1/sum(Pond10000/PondSITC2/Carli, na.rm=TRUE), Pond=mean(PondSITC2, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))%>%
    left_join(.,DIC_SITC, by=c("SITC2"="Code"))

  #Global
  Imp_Gl_Paas<-I2%>%
    group_by(Year, Period)%>%
    summarise(Import_Paas=1/sum(Pond10000/Pondt/Carli, na.rm=TRUE), Pond=sum(Pond10000, na.rm=TRUE))%>%
    mutate(temps=as.Date(paste(Year, Period, 1, sep="-"), "%Y-%m-%d"))#%>%
  #filter(!is.na(temps))

  rm(DIC_HS,DIC_SITC,E,E1,E2,EDat,EPonderation,ETL,ETL2,I,I1,I2,IDat,IPonderation,ITL,ITL2,REL_HS2SITC,dbconnection,F_Index4)

  ##################################################  OUTPUT

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
  #setwd("~/eiti")
  Sys.setenv(R_ZIPCMD= "C:/Rtools/bin/zip")
  library(openxlsx) #load the package

  #Globaux Laspeyres et Paasche
  write.xlsx(x = IEL, file = paste("Indices_Globaux_Laspeyres", Sys.Date(), ".xlsx"),
             sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)
  write.xlsx(x = IEP, file = paste("Indices_Globaux_Paasche", Sys.Date(), ".xlsx"),
             sheetName = "Indices", col.NAmes = TRUE, row.Names = FALSE)

  #Laspeyres- Paasche IMPORT
  write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_HS4_Lasp, file = paste("Indice_Import_HS4_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_SITC1_Lasp, file = paste("Indice_Import_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_SITC2_Lasp, file = paste("Indice_Import_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  #write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_HS2_Lasp, file = paste("Indice_Import_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_SITC1_Lasp, file = paste("Indice_Import_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_SITC2_Lasp, file = paste("Indice_Import_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_HS4_Paas, file = paste("Indice_Import_HS4_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_SITC1_Paas, file = paste("Indice_Import_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Imp_SITC2_Paas, file = paste("Indice_Import_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  #write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_HS2_Paas, file = paste("Indice_Import_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_SITC1_Paas, file = paste("Indice_Import_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Imp_SITC2_Paas, file = paste("Indice_Import_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  #Export

  write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_HS4_Lasp, file = paste("Indice_Export_HS4_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_SITC1_Lasp, file = paste("Indice_Export_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_SITC2_Lasp, file = paste("Indice_Export_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  #write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Exp_HS2_Lasp, file = paste("Indice_Export_HS2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Exp_SITC1_Lasp, file = paste("Indice_Export_SITC1_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Exp_SITC2_Lasp, file = paste("Indice_Export_SITC2_Lasp", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_HS4_Paas, file = paste("Indice_Export_HS4_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_SITC1_Paas, file = paste("Indice_Export_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  write.xlsx(x = Exp_SITC2_Paas, file = paste("Indice_Export_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)

  #write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Exp_HS2_Paas, file = paste("Indice_Export_HS2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  # write.xlsx(x = Exp_SITC1_Paas, file = paste("Indice_Export_SITC1_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)
  #write.xlsx(x = Exp_SITC2_Paas, file = paste("Indice_Export_SITC2_Paas", Sys.Date(), ".xlsx"), sheetName = "Indices", row.names = FALSE)



  ###############################
  # Graphique                   #
  ###############################
  library(lubridate)
  library(ggplot2)
  #Paasche
  pg1 = ggplot(IEP, aes(x = temps)) + geom_line(aes(y=Import_Paas, colour = "Import_Paas")) + geom_line(aes(y=Export_Paas, colour = "Export_Paas")) + geom_smooth(aes(y=Import_Paas))+geom_smooth(aes(y=Export_Paas))  + xlab("P?riode") + ylab("Indice Global(%)") + ggtitle("Indices des prix ? l'Importation et ? l'Exportation - Paasche")
  ggsave("Graphe_Global_Paasche.pdf")
  pg2 = ggplot(IEP, aes(x = month(IEP$temps), y=Import_Paas, group=year(IEP$temps))) + geom_line() + aes(colour=year(IEP$temps)) +  ylab("Indice Global(%)") + ggtitle("Evolution comparative de l'indices ? l'Importation - Paasche")
  ggsave("Graphe_Comparatif_Paasche.pdf")
  #Lasp
  lg1 = ggplot(IEL, aes(x = temps)) + geom_line(aes(y=Import_Lasp, colour = "Import_Lasp")) + geom_line(aes(y=Export_Lasp, colour = "Export_Lasp")) + geom_smooth(aes(y=Import_Lasp))+geom_smooth(aes(y=Export_Lasp))  + xlab("P?riode") + ylab("Indice Global(%)") + ggtitle("Indices des prix ? l'Importation et ? l'Exportation - Paasche")
  ggsave("Graphe_Global_Laspeyres.pdf")
  lg2 = ggplot(IEL, aes(x = month(IEL$temps), y=Import_Lasp, group=year(IEP$temps))) + geom_line() + aes(colour=year(IEP$temps)) +  ylab("Indice Global(%)") + ggtitle("Evolution comparative de l'indices ? l'Importation - Laspeyres")
  ggsave("Graphe_Comparatif_Laspeyres.pdf")

  #R_Indice dossier
  #setwd("~/eiti")

}
