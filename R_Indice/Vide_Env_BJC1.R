#Subsetter sur la clé et quelques variables additionnelles
BJC1<-subset(BJC, select=c(Year, Period, JOUR, BUREAU, DECTYPE, NUMDEC, NUMART, 
             AGENT, ENTREPRISE, MODTRANSPORT, NATTRANSPORT, 
             REGIME, HS, PORIGINE, PDESTINATION, PINTERM, PARTENAIRE, 
             POIDNET, QUANTITE, VALEURINSAE, VU, SU, HS6, Unit.Value, 
             TRS_FLOW, TRS_TRADETYPE))

#Nommer colonnes
colnames(BJC1)[25]<- "FLOW"
colnames(BJC1)[26]<- "TRADETYPE"
#colnames(BJC1)[27]<- "LabelHS6"

#Enlever les objets inutiles (dicos et relations - Exécuter si nécessaire)
#rm(BJC, DIC_BEC, DIC_COUNTRY, DIC_CPC, DIC_FLOW, DIC_HS, DIC_MOIS, DIC_SITC, DIC_TRANSPORT, REL_CPC2FLOW, REL_CPC2TYP, REL_HS2BEC, REL_HS2SITC)
rm(BJC, DIC_BEC, DIC_COUNTRY, DIC_CPC, DIC_FLOW, DIC_MOIS , DIC_TRANSPORT, REL_CPC2FLOW, REL_CPC2TYP, REL_HS2BEC)

