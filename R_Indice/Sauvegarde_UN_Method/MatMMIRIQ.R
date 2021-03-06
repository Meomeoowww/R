#filtre B21
#Donn�es annuelles subsett�es
echini<-subset(B21SQL, (B21SQL$Period=="00"))

#Donn�es � l'import subsett�
Ech_Im<-subset(echini, (echini$FLOW=='Import'))

#Ajouter le nombre (n) d'observations par sous-�chantillon du couple ann�e produit

R30<-Ech_Im %>% add_count(Year, HS2012)

#Donn�es subsett�es sur les transactions pour lesquelles le couple Ann�e - Produit contient plus de 30 observations
R31<-subset(R30, n>30)

#Compute MMI et RIQ

library(dplyr)

R_OUTL<-R31%>%
  group_by(Year, HS2012) %>%
  #message("Summary of ", .$Year, " and ", $.Period, .$Label_HS2012)
  do(OutlTuk(.,log(Unit.Value)))

MatMMIRIQ<-R_OUTL%>%
  group_by(Year, HS2012) %>%
  do(MMI=MMIRIQ(., Unit.Value)[1], RIQ=MMIRIQ(., Unit.Value)[2], Med=median(.$Unit.Value))


MatMMIRIQ<-MatMMIRIQ%>% mutate(.,ProdMR=round(MMI*RIQ, 1))

#MatMMIRIQ<-R31%>%
#  group_by(Partner) %>%
 # do(MMI=MMIRIQ(., Unit.Value)[1], RIQ=MMIRIQ(., Unit.Value)[2])

#fusion de MatMMIRIQ et des totaux valeur et quantit�
RTL<-merge(MatMMIRIQ, summarize(group_by(R_OUTL, Year, HS2012), VAL = sum(VALEURINSAE), QUA = sum(POIDNET)), c("Year", "HS2012"))


