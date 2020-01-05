#' @title Renomme champs et Calcul de la Valeur Unitaire
#'
#' @description Renomme les champs Bureau de douane, mode de transport, régime, partenaire, produit HS, valeur, poids net et quantité et crée le champ valeur unitaire. Il est nécessaire que les dictionnaires et relations Flux et Type de Commerce soient au préalable chargés.
#'
#' @param input_df,customsoffice,modeoftransport,cpc,partner,product,value,netweight,quantity
#'
#' @return Le dataset BJC
#'
#' @examples
#'
#' @export

prep_db <- function(input_df, customsoffice,modeoftransport, cpc, partner, product, value, netweight, quantity) {

  library(dplyr)

  cols<-c(customsoffice,modeoftransport,cpc,partner,product,value,netweight,quantity)
  new_cols<-c("BUREAU", "MODTRANSPORT","REGIME","PARTENAIRE","HS","VALEURINSAE","POIDNET","QUANTITE")

  BJC<-input_df %>%
            rename_(.,.dots = setNames(cols, new_cols))

  #formattage des variables d'intéret en numérique
  BJC$VALEURINSAE<-as.numeric(BJC$VALEURINSAE)
  BJC$POIDNET<-as.numeric(BJC$POIDNET)
  BJC$QUANTITE<-as.numeric(BJC$QUANTITE)

  BJC<-BJC%>%
    mutate(., Unit.Value=ifelse(.$HS!="2716000000",
                                as.numeric(.$VALEURINSAE)/as.numeric(.$POIDNET),
                                as.numeric(.$VALEURINSAE)/as.numeric(.$QUANTITE))) %>%

    mutate(., HS6=substr(BJC$HS,1,6))


   ###Jointure de la Table BJC avec le Les codes Flux et TYPCOM pour pouvoir Filtrer
  BJC1 <<- left_join(BJC, REL_CPC2FLOW, by= c("REGIME"="Code")) %>%
    left_join(., REL_CPC2TYP, by= c("REGIME"="Code"))

  #Subsetter sur la cl? et quelques variables additionnelles
  #BJC1<-subset(BJC, select=c(Year, Period, BUREAU, MODTRANSPORT, REGIME, HS, PARTENAIRE,
   #                          POIDNET, QUANTITE, VALEURINSAE, HS6, Unit.Value,
    #                         TRS_FLOW, TRS_TRADETYPE))



  #Nommer colonnes
  #colnames(BJC1)[13]<- "FLOW"
  #colnames(BJC1)[14]<- "TRADETYPE"
  #colnames(BJC1)[27]<- "LabelHS6"

  #Enlever les objets inutiles (dicos et relations - Ex?cuter si n?cessaire)
  #rm(BJC, DIC_BEC, DIC_COUNTRY, DIC_CPC, DIC_FLOW, DIC_HS, DIC_MOIS, DIC_SITC, DIC_TRANSPORT, REL_CPC2FLOW, REL_CPC2TYP, REL_HS2BEC, REL_HS2SITC)

  ls()

  rm(list=setdiff(ls(), "BJC1"))



    BJC<<-BJC
}
