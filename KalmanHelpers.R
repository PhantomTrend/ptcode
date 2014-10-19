

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
  }
  Q <- R1 %*% t(R1)
  
  # KFAS needs a 3d array
  Q <- array(Q, c(nLatentComponentsBase, nLatentComponentsBase, 1))
  return(Q)
}




