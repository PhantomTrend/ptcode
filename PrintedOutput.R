

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

