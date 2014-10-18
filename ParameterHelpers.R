
# Helper functions to convert between numeric vectors (used in optimisation)
# and labelled lists


getDefaultParamList <- function(v){
  out <- list()
  for(party in partyNames){
    out[[party]][['AUS']] <- 0.4
    for(s in stateNames){
      out[[party]][[s]] <- 0.05
    }
  }
  for(pollster in pollsters){
    out[[pollster]][['NoiseVariance']] <- 4
    for(party in observedPartyNames){
      out[[pollster]][[party]] <- 0
    }
  }
  return(out)
}

# For debugging etc
getRandomParamList <- function(){
  nParams <- length(paramListToVector(getDefaultParamList()))
  return(paramVectorToList(rnorm(nParams)))
}

paramVectorToList <- function(v){
  out <- list()
  vIndex <- 1
  for(party in partyNames){
    out[[party]][['AUS']] <- exp(v[vIndex])
    vIndex <- vIndex + 1
    for(s in stateNames){
      out[[party]][[s]] <- exp(v[vIndex])
      vIndex <- vIndex + 1
    }
  }
  for(pollster in pollsters){
    out[[pollster]][['NoiseVariance']] <- exp(v[vIndex])
    vIndex <- vIndex + 1
    for(party in observedPartyNames){
      out[[pollster]][[party]] <- v[vIndex]
      vIndex <- vIndex + 1
    }
  }
  return(out)
}

paramListToVector <- function(p){
  out <- as.numeric(unlist(p))
  vIndex <- 1
  for(party in partyNames){
    out[vIndex] <- log(out[vIndex])
    vIndex <- vIndex + 1
    for(s in stateNames){
      out[vIndex] <- log(out[vIndex])
      vIndex <- vIndex + 1
    }
  }
  for(pollster in pollsters){
    out[vIndex] <- log(out[vIndex])
    vIndex <- vIndex + 1
    for(party in observedPartyNames){
      vIndex <- vIndex + 1
    }
  }
  return(out)
}

