#filtre B21
#Donn�es annuelles subsett�es � l'Import et sur le commerce g�n�ral
Ech_Im<-subset(BJC1, (BJC1$FLOW=="I") & ((BJC1$TRADETYPE=="2")|(BJC1$TRADETYPE=="G")))


#Ajouter le nombre (n) d'observations par sous-�chantillon du couple ann�e 
#p�riode bureau produit

R30<-Ech_Im %>% add_count(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6)

#Donn�es subsett�es sur les transactions pour lesquelles le couple Ann�e - Produit contient plus de 30 observations
R31<-subset(R30, n>30)

#Compute MMI et RIQ

library(dplyr)

R_OUTL<-R31%>%
  group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
  #message("Summary of ", .$Year, " and ", $.Period, .$Lib_FR)
  do(OutlTuk2(.,log(Unit.Value)))

####Suppression des NA (A utiliser lorsque OUtlTuk2 est ex�cut�)
R_OUTL<-R_OUTL[complete.cases(R_OUTL$log), ]

MatMMIRIQ<-R_OUTL%>%
  group_by(Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6) %>%
  do(MMI=MMIRIQ2(., Unit.Value)[1], RIQ=MMIRIQ2(., Unit.Value)[2], Med=median(.$Unit.Value))


MatMMIRIQ<-MatMMIRIQ%>% mutate(.,ProdMR=round(MMI*RIQ, 1))



#fusion de MatMMIRIQ et des totaux valeur et quantit�
RTL<-merge(MatMMIRIQ, summarize(group_by(R_OUTL, Year, Period, BUREAU, MODTRANSPORT, PARTENAIRE, HS6), VAL = sum(VALEURINSAE), QUA = sum(POIDNET)), c("Year", "HS2012"))


