MMIRIQ <- function(df, v_input) {
  
  #Evaluation et substitution de la variable
  
  dfvar<-eval(substitute(v_input), eval(df)) 

  # Library
  library(classInt)
  
  nclass=classIntervals(dfvar, n=10, style='equal', intervalClosure='right')
  Etendue<-max(dfvar)-min(dfvar)
  
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
  
    ##Computation RIQ
  RIQ<-IQR(dfvar)/median(dfvar)
  
  Result<- c(RIQ,MMI)
  return(Result)
}

MMIRIQ(ech, Unit.Value)
