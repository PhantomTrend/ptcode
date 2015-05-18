

# The notation for KFAS's state space models follows Harvey's book,
# so it's    x(t) = T*x(t-1) + R*e(t),   e ~ N(0,Q)

makeQmatrix <- function(params){
  R1 <- matrix(0, nrow= nLatentComponentsBase, ncol= nLatentComponentsBase + nParties  )
  for(componentI in seq_along(latentComponentNamesBase)){
    thisParty <- latentPartyNames[componentI]
    thisElectorate <- latentStateNames[componentI]
    
    # Individual state-party shock
    R1[componentI, componentI] <- params[[thisParty]][[thisElectorate]]
    
    # Nationwide party shock
    partyIndex <- which(partyNames == thisParty)
    R1[componentI, nLatentComponentsBase + partyIndex] <- params[[thisParty]][['AUS']]
    
    # Covariance between nationwide shocks
    thisPartyParams <- params[['Covariance']][[thisParty]]
    for(otherParty in names(thisPartyParams)){
      partyIndex <- which(partyNames == otherParty)
      R1[componentI, nLatentComponentsBase + partyIndex] <- thisPartyParams[[otherParty]]
    }
  }
  Qsmall <- bigR %*% (R1 %*% t(R1)) %*% t(bigR)  + 1e-9*diag(nLatentComponents)
  
  Q <- array(Qsmall, c(nLatentComponents, nLatentComponents, 1))
  return(Q)
}




