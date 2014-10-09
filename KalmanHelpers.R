

# The notation for KFAS's state space models follows Harvey's book,
# so it's    x(t) = T*x(t-1) + R*e(t),   e ~ N(0,Q)
# In principle R can have more rows than Q, but KFAS assumes that
# the dimension of Q is the same as the dimension of x, so this
# function circumvents that a bit by calculating R*R'.

makeQmatrix <- function(params){
  R1 <- matrix(0, nrow= nLatentComponents, ncol= nLatentComponents + nParties -1 )
  for(componentI in seq_along(latentComponentNames)){
    thisParty <- latentPartyNames[componentI]
    thisElectorate <- latentStateNames[componentI]
    if(thisParty == 'OTH'){
      # Constrained to keep the total at 100% for each electorate
      for(otherParty in setdiff(partyNames, c('OTH'))){
        otherLocalColumn <- which(latentPartyNames == otherParty & latentStateNames == thisElectorate)
        R1[componentI, otherLocalColumn] <- -params[[otherParty]][[thisElectorate]]
        otherNationwideColumn <- nLatentComponents + which(partyNames == otherParty)
        R1[componentI, otherNationwideColumn] <- -params[[otherParty]][['AUS']]
      }
    }else{
      # Individual state-party shock
      R1[componentI, componentI] <- params[[thisParty]][[thisElectorate]]
      # Nationwide party shock
      partyIndex <- which(partyNames == thisParty)
      R1[componentI, nLatentComponents + partyIndex] <- params[[thisParty]][['AUS']]
    }
  }
  Q <- R1 %*% t(R1)
  # A sort of hacky way to ensure that Q is p.d.
  # An alternative would be to rewrite the statespace model without the
  # the redundant OTH component.
  Q <- Q + diag(rep(1e-12, nLatentComponents))
  # KFAS needs a 3d array
  Q <- array(Q, c(nLatentComponents, nLatentComponents, 1))
  return(Q)
}




