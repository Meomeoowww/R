#filtre BJC1 Import

#mettre le champ Period � OO pour l'ann�e de base '2015'
BJC1<-BJC1 %>%
      mutate(Period=replace(Period, which(Year==2015), '00'))


#Donn�es annuelles subsett�es � l'Import et sur le commerce g�n�ral
Ech_Im<-subset(BJC1, (BJC1$FLOW=="I") & ((BJC1$TRADETYPE=="2")|(BJC1$TRADETYPE=="G")))
Ech_Ex<-subset(BJC1, ((BJC1$FLOW=="E")|(BJC1$FLOW=="R")) & ((BJC1$TRADETYPE=="2")|(BJC1$TRADETYPE=="G")))

#Ajouter le nombre (n) d'observations par sous-�chantillon du couple ann�e 
#p�riode bureau produit

I30<-Ech_Im %>% add_count(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6)
E30<-Ech_Ex %>% add_count(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6)

#Donn�es subsett�es sur les transactions pour lesquelles le couple Ann�e - Produit contient plus de 30 observations
I31<-subset(I30, n>30)
E31<-subset(E30, n>30)

#Compute MMI et RIQ

library(dplyr)
#Import
I_OUTL<-I31%>%
  group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
  #message("Summary of ", .$Year, " and ", $.Period, .$Lib_FR)
  do(OutlTuk2(.,log(Unit.Value)))

#Export
E_OUTL<-E31%>%
  group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
  #message("Summary of ", .$Year, " and ", $.Period, .$Lib_FR)
  do(OutlTuk2(.,log(Unit.Value)))

####Suppression des NA (A utiliser lorsque OUtlTuk2 est ex�cut�)
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



#fusion de MatMMIRIQ et des totaux valeur et quantit�
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
rm(BJC1, E_OUTL, I_OUTL, E30, I30, E31, I31, Ech_Ex, Ech_Im, EMatMMIRIQ, IMatMMIRIQ, MMIRIQ2, OutlTuk2)
