
reciprocalLogPrior = function(pars,model){
  
  paramList = paramVectorToList(pars)
  logprior = 0
  
  # I use half-t priors on the variances for the latent innovations because
  # they're weakly informative.
  #  See Gelman, "Prior distributions for variance parameters in hierarchical
  #    models", Bayesian Analysis 1:3(2006):515--33.
  # The scale is ideally set to values that are high but not off the scale.
  largeWeeklyMovementInNationalPrimaryVote <- 4
  largeWeeklyMovementInOneStatesPrimaryVote <- 1
  for(party in partyNames){
    relativeVariance <- paramList[[party]][['AUS']] / (largeWeeklyMovementInNationalPrimaryVote**2)
    logprior <- logprior + log(dt(relativeVariance, df=3))
    for(s in stateNames){
      relativeVariance <- paramList[[party]][[s]] / (largeWeeklyMovementInOneStatesPrimaryVote**2)
      logprior <- logprior + log(dt(relativeVariance, df=3))
    }
  }
  
  # Pollster mean error terms are N(0,1) because a persistent plus or minus
  # of more than 3 doesn't seem plausible.
  # Error variances are set to half-t, scaled as above to a large but not impossible
  # value (4ppt std.dev.).
  for(pollster in pollsters){
    relativeVariance <- paramList[[pollster]][['NoiseVariance']] / (4**2)
    logprior <- logprior + log(dt(relativeVariance, df=3))
    for(party in observedPartyNames){
      logprior <- logprior + log(dnorm(paramList[[pollster]][[party]], sd=1))
    }
  }

  return(-logprior)  
}

