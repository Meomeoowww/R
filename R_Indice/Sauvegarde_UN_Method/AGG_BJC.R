# Préparation des données
###Calcul de la Valeur Unitaire (mm pour l'électricité)
library(dplyr)
BJC<-BJC%>% 
  mutate(., Unit.Value=ifelse(.$HS8BENIN!="27160000",
        as.numeric(.$VALEURINSAE)/as.numeric(.$POIDNET), 
        as.numeric(.$VALEURINSAE)/as.numeric(.$QUANTITE))) %>% 
        
  mutate(., HS6=substr(BJC$HS,1,6))

###Jointure de la Table BJC avec le Les codes Flux et TYPCOM pour pouvoir Filtrer
BJC <- left_join(BJC, REL_CPC2FLOW, by= c("REGIME"="Code")) %>%
       left_join(., REL_CPC2TYP, by= c("REGIME"="Code")) 

#Libellé HS6 à activer si besoin
#BJC <- left_join(BJC, DIC_HS, by= c("HS6"="Code"))