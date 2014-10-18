

# The notation for KFAS's state space models follows Harvey's book,
# so it's    x(t) = T*x(t-1) + R*e(t),   e ~ N(0,Q)
# In principle R can have more rows than Q, but KFAS assumes that
# the dimension of Q is the same as the dimension of x, so this
# function circumvents that a bit by calculating R*R'.

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
  
  # Q now holds the covariance for the latent shocks, but we embed it in a larger
  # dimension because the state vector also includes lagged values
  Q <- rbind(cbind(Q, matrix(0, nrow=nLatentComponentsBase, ncol=nLaggedComponents)),
             matrix(0, nrow=nLaggedComponents, ncol=nLatentComponents))
  
  # KFAS needs a 3d array
  Q <- array(Q, c(nLatentComponents, nLatentComponents, 1))
  return(Q)
}




