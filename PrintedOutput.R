

printCurrentEstimates = function(estimatedModel){
  
  end_t = dim(estimatedModel$a)[1]
  
  national2pp = estimatedModel$popweights %*% estimatedModel$a[end_t,]
  
  stateNames = c('NSW','VIC', 'QLD', 'SA', 'WA', 'TAS', 'NT', 'ACT')
  
  cat('National\t')
  for(i in 1:8){cat(stateNames[i]);cat('\t')}
  cat('\n')
  cat(round(national2pp,1))
  cat('\t')
  for(i in 1:8){cat(round(estimatedModel$a[end_t,i],1));cat('\t')}
  cat('\n')
}

logCurrentEstimates = function(estimatedModel, logFileName){
  end_t = dim(estimatedModel$a)[1]
  national2pp = estimatedModel$popweights %*% estimatedModel$a[end_t,]
  result = data.frame(Timestamp = Sys.time(),
                      National = national2pp,
                      NSW = estimatedModel$a[end_t,1],
                      VIC = estimatedModel$a[end_t,2],
                      QLD = estimatedModel$a[end_t,3],
                      SA = estimatedModel$a[end_t,4],
                      WA = estimatedModel$a[end_t,5],
                      TAS = estimatedModel$a[end_t,6],
                      NT = estimatedModel$a[end_t,7],
                      ACT = estimatedModel$a[end_t,8])
  if(file.exists(logFileName)){
    write.table(result, file = logFileName, sep = ",", row.names=FALSE, col.names = FALSE, append=TRUE)
  }else{
    write.table(result, file = logFileName, sep = ",", row.names=FALSE, col.names = TRUE, append=FALSE)
  }
}

