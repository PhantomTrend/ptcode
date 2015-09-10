library('assertthat')

# Helper functions to convert between numeric vectors (used in optimisation)
# and labelled lists


covarianceTerms <- list()
covarianceTerms[['LNP']] <- list('ALP')
covarianceTerms[['GRN']] <- list('ALP','LNP')
covarianceTerms[['PUP']] <- list('ALP','LNP')
covarianceTerms[['OTH']] <- list('ALP','LNP')
invisible(assert_that(all(names(covarianceTerms) %in% partyNames)))
for(party in names(covarianceTerms)){
  invisible(assert_that(all(names(covarianceTerms[[party]]) %in% partyNames)))
}

getDefaultParamList <- function(v){
  out <- list()
  for(party in partyNames){
    out[[party]][['AUS']] <- 0.5
    for(s in stateNames){
      out[[party]][[s]] <- 0.1
    }
  }
  
  for(party in names(covarianceTerms)){
    thisCovarianceList <- list()
    for(otherParty in covarianceTerms[[party]]){
      thisCovarianceList[[otherParty]] <- 0
    }
    out[['Covariance']][[party]] <- thisCovarianceList
  }
  
  for(pollster in setdiff(pollsters, 'Election')){
    out[[pollster]][['NoiseVariance']] <- 6
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
  
  for(party in names(covarianceTerms)){
    thisCovarianceList <- list()
    for(otherParty in covarianceTerms[[party]]){
      thisCovarianceList[[otherParty]] <- v[vIndex]
      vIndex <- vIndex + 1
    }
    out[['Covariance']][[party]] <- thisCovarianceList
  }
  
  for(pollster in setdiff(pollsters, 'Election')){
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
  
  for(party in names(covarianceTerms)){
    for(otherParty in covarianceTerms[[party]]){
      vIndex <- vIndex + 1
    }
  }
  
  for(pollster in setdiff(pollsters, 'Election')){
    out[vIndex] <- log(out[vIndex])
    vIndex <- vIndex + 1
    for(party in observedPartyNames){
      vIndex <- vIndex + 1
    }
  }
  return(out)
}

