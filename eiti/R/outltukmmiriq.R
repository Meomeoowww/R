#' @title Fonctions usuelles de calcul des Indicateurs MMI RIQ et détection d'outliers
#'
#' @description OutlTuk et MMIRIQ
#'
#' @param input_df,customsoffice,modeoftransport,cpc,partner,product,value,netweight,quantity
#'
#' @return Le dataset BJC
#'
#' @examples
#'
#' @export



#OUTLTUK

#Fonction Tukey Outlier D?tection avec suppression des outliers lorsque leur trade share est inf. ? 5%

OutlTuk3 <- function(dt, var,n) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #boxplot(var_name, main="With outliers")
  #hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  #boxplot(var_name, main="Without outliers")
  #hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  #title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)

  if((((na2 - na1) / tot*100)<=25)& (n>30)){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(dt))
  }

}

#MMIRIQ

MMIRIQ2 <- function(df, v_input) {

  #Evaluation et substitution de la variable

  dfvar<-eval(substitute(v_input), eval(df))


  if (max(dfvar)-min(dfvar)<1) {MMI<-1}
  else {


    #---------
    # Library
    library(classInt)

    Etendue<-max(dfvar)-min(dfvar)
    nclass=classIntervals(dfvar, n=10, style='equal', intervalClosure='right')

    library(modes)

    mode(x = nclass$brks)
    df$nclass=cut(dfvar, 10)

    # Computation du MMI
    ##Creation de zz zee1 vee
    zee<-table(df$nclass)
    zee1<-as.matrix(zee)
    vee<-vector(mode="integer", length=10)

    ##version  zee avec nivo suivant
    for(i in 1:10) {

      if (i >= 1 & i<=9) {vee[i]<-zee1[i+1]-zee1[i] }

      else  {vee[i]<- 0-zee1[i]}

    }

    qee<-as.vector(sign(vee))
    qee3<-vector(mode="integer", length=10)

    for (i in 1:10) {
      if (i==1 & qee[i]==-1) {qee3[i]<-1}
      else if (i==1 & qee[i]==1) {qee3[i]<-0}
      else if (i >1 & i<=9 & qee[i]==1 & qee[i+1]==-1) {qee3[i+1]<-1}
      #else qee3[i]<-0
    }

    MMIDATA<-data.frame(classe=zee, Multimode=qee3)

    MMI<-sum(MMIDATA[which(MMIDATA[,3]==1),2])^2/sum(MMIDATA[which(MMIDATA[,3]==1),2]^2)

    #---------
  }

  ##Computation RIQ
  RIQ<-IQR(dfvar)/median(dfvar)

  Result<- c(round(RIQ, 1), round(MMI,1))
  return(Result)
}
