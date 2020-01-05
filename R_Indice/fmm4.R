TRIAL <- function(dt, Var) {
  
  
  #Importation des données
  
  # Library
  library(classInt)
  
  nclass=classIntervals(dt[Var], n=10, style='equal', intervalClosure='right')
  
  print(nclass)
 }

TRIAL(nech, Var="unit.Value")
