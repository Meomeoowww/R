#' @title Chargement de la base et des dictionnaires
#'
#' @description Charge de dataset principal, les dictionnaires et les relations
#'
#' @param dbc,dp,syear,eyear
#'
#' @return Dataframes contenant une sélection du dataset principal et des dictionnaires/relations
#'
#' @examples
#'
#' @export

charge_dp <- function(dbc,dom,dp,syear,eyear) {
#lyear<-(syear:eyear)

library("RODBC")
#print(a)
dbconnection <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",dbc,"; Database=",dom,"; trusted_connection=yes)"))

#print(dbconnection)
BJC <- sqlQuery(dbconnection, paste0("SELECT * FROM [dbo].[",dom,"_DATA_",dp,"] WHERE ([Year] BETWEEN ",syear," and ",eyear,") and [Period] in ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')"), as.is=T)
close(dbconnection)
#return(as.data.frame(BJC))
BJC<<-as.data.frame(BJC)
}

######################################################

charge_dic <- function(dbc,dom,dp,dic) {

  #A affecter à un Dataframe: ex:HS<-charge_dico()...
  #Dicos essentiels REGIME,CPC-FLUX,CPC-TYPECOM, HS,SITC, HS2SITC

  library("RODBC")

  dbconnection <- odbcDriverConnect(paste0("Driver=SQL Server; Server=",dbc,"; Database=",dom,"; trusted_connection=yes)"))

  dico <- sqlQuery(dbconnection,
                      paste0("SELECT * FROM [dbo].[",dom,"_DIC_",dic,"]")
                      , as.is=T)
  close(dbconnection)

  as.data.frame(dico)
  #dico<<-dico

  }

format_relSITCTYPFLOW <- function() {

  library(dplyr)
  REL_CPC2TYP<<-REL_CPC2TYP%>%
    rename_at(vars(starts_with("TRS_")), funs(paste0("TRADETYPE")))

  REL_CPC2FLOW<<-REL_CPC2FLOW%>%
    rename_at(vars(starts_with("TRS_")), funs(paste0("FLOW")))

  REL_HS2SITC<<-REL_HS2SITC%>%
    rename_at(vars(starts_with("TRS_")), funs(paste0("SITC")))

  }
