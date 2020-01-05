#Calcul de L'indice élémentaire3
#Export: 
### NB: Si Carli=NA, alors c'est que le produit existe en 2016 mais n'a pas été échangé à la période courante du NA

EDat<-ETL2%>%
  group_by(NID)%>%
  do(F_Index4(.))%>%
  mutate(Red=as.numeric(Red))%>%
  group_by(Year, Period, HS6)%>%
  summarize(Carli=100*mean(Red, na.rm = T))%>%
  filter(!is.na(Carli)) #Pour enlever les cas ou des Red sont vides, ie le produit n'apparait pas à l'année de base 2015

EPonderation<-ETL2%>%
  group_by(Year, Period, HS6)%>%
  summarize(Valeur=sum(VT2))%>%
  mutate(Pond10000=Valeur/sum(Valeur, na.rm = T)*10000)

EDat<-EDat%>%
  left_join(.,EPonderation, by=c("Year"="Year", "Period"="Period", "HS6"="HS6"))


# Import
### NB: Si Carli=NA, alors c'est que le produit existe en 2016 mais n'a pas été échangé à la période 2015

IDat<-ITL2%>%
  group_by(NID)%>%
  do(F_Index4(.))%>%
  mutate(Red=as.numeric(Red))%>%
  ungroup(.)%>%
  group_by(Year, Period, HS6)%>%
  summarize(Carli=100*mean(Red, na.rm = T))%>%
  filter(!is.na(Carli)) #Pour enlever les cas ou des Red sont vides, ie le produit n'apparait pas à l'année de base 2015

IPonderation<-ITL2%>%
  group_by(Year, Period, HS6)%>%
  summarize(Valeur=sum(VT2, na.rm = T))%>%
  mutate(Pond10000=Valeur/sum(Valeur, na.rm = T)*10000)

IDat<-IDat%>%
  left_join(.,IPonderation, by=c("Year"="Year", "Period"="Period", "HS6"="HS6"))
