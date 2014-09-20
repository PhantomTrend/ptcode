
library(KFAS)


# A convenience function to convert between a numeric vector
# (needed for calling optim()) and a list
unpackParams = function(pars,model){
  list(
    qfactorvol=pars[1],
    diagq=pars[2:9],
    newspollNationalBias = pars[10],
    newspollWVol = pars[11],
    newspollQVol = pars[12:16],
    essentialBias = pars[17],
    essentialVol = pars[18],
    galaxyBias = pars[19],
    galaxyVol = pars[20],
    nielsenBias = pars[21],
    nielsenVol = pars[22],
    morganBias = pars[23],
    morganVol = pars[24],
    reachTelBias = pars[25],
    reachTelVol = pars[26])
}

# Log-prior function, given parameter vector "pars"
reciprocalLogPrior = function(pars,model){
  parlist = unpackParams(pars)
  logprior = 0
  # I use half-t priors on the variances because they're weakly informative.
  #  See Gelman, "Prior distributions for variance parameters in hierarchical
  #    models", Bayesian Analysis 1:3(2006):515--33.
  # The scale is ideally set to values that are high but not off the scale.
  # The State-specific idiosyncratic variances are on the tight side for
  # unobserved series, because otherwise the model is tempted to use Tassie
  # as a swing variable rather than attribute rapid movements to pollster idiosyncracies.
  # The state-variable innovations are scaled to units of percentage change per week.
  qfactorVariance = (52/365 * exp(parlist$qfactorvol))^2
  largeQfactorVariance = 0.5^2       
  stateQvariances = (52/365 * exp(parlist$diagq))^2
  largeStateVariance = 0.5^2
  largeStateVarianceForUnobservedSeries = 0.1^2
  logprior = logprior + log(dt(qfactorVariance/largeQfactorVariance, df=3)) +
    sum(log(dt(c(stateQvariances[1:5]/largeStateVariance,
                 stateQvariances[6:8]/largeStateVarianceForUnobservedSeries), df=3)))
  
  # Pollster mean error terms are N(0,1) because a persistent plus or minus
  # of more than 3 doesn't seem plausible.
  # Error variances are set to half-t, scaled as above to a large but not impossible
  # value (4ppt std.dev.).
  pollsterMeans = as.numeric(parlist[c('newspollNationalBias',
                                       'essentialBias','galaxyBias', 'nielsenBias',
                                       'morganBias', 'reachTelBias' )])
  logprior = logprior + sum(log(dnorm(pollsterMeans, sd=1)))
  pollsterVariances = exp( c(as.numeric(parlist[c('newspollWVol',
                                             'essentialVol', 'galaxyVol',
                                             'nielsenVol', 'morganVol', 'reachTelVol' )]),
                        as.numeric(parlist$newspollQVol) ) )
  logprior = logprior + sum(log(dt(pollsterVariances/(4^2), df=3)))
  return(-logprior)
  
}


# Likelihood function, given parameter vector "pars"
reciprocalLogLikelihood = function(pars,model,pollsterTs,estimate=TRUE){
  m = attr(model, 'm')
  p = attr(model, 'p')
  n = attr(model, 'n')
  parlist = unpackParams(pars)
  R1 = cbind(rep(52/365 * exp(parlist$qfactorvol),m), diag( 52/365 * exp(parlist$diagq), nrow=m, ncol=m ))
  Q = R1 %*% t(R1)
  model$Q = array( Q, c(m, m, 1) )
  model$H = array( diag( 0, nrow=p, ncol=p  ), 
                   c(p, p, n) )
  
  model$H[,,which(pollsterTs=='Election')] = diag(0.01^2, nrow=p, ncol=p)
  
  model$H[,,which(pollsterTs=='NewspollW')] = diag(exp(parlist$newspollWVol), nrow=p, ncol=p)
  model$H[,,which(pollsterTs=='NewspollQ')] = diag(c(0,exp(parlist$newspollQVol)), nrow=p, ncol=p)
  model$y[which(pollsterTs=='NewspollW'),] = Y[which(pollsterTs=='NewspollW'),] - parlist$newspollNationalBias
  model$y[which(pollsterTs=='NewspollQ'),] = sweep(Y[which(pollsterTs=='NewspollQ'),],MARGIN=2,c(rep(parlist$newspollNationalBias,6),NA,NA,NA),FUN="-")
  model$H[,,which(pollsterTs=='Essential')] = diag(exp(parlist$essentialVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='Essential'),] = Y[which(pollsterTs=='Essential'),] - parlist$essentialBias
  model$H[,,which(pollsterTs=='Galaxy')] = diag(exp(parlist$galaxyVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='Galaxy'),] = Y[which(pollsterTs=='Galaxy'),] - parlist$galaxyBias
  model$H[,,which(pollsterTs=='ACNielsen')] = diag(exp(parlist$nielsenVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='ACNielsen'),] = Y[which(pollsterTs=='ACNielsen'),] - parlist$nielsenBias
  model$H[,,which(pollsterTs=='RoyMorgan')] = diag(exp(parlist$morganVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='RoyMorgan'),] = Y[which(pollsterTs=='RoyMorgan'),] - parlist$morganBias
  model$H[,,which(pollsterTs=='ReachTEL')] = diag(exp(parlist$reachTelVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='ReachTEL'),] = Y[which(pollsterTs=='ReachTEL'),] - parlist$reachTelBias
  
  if(estimate){
    return(-logLik(model))
  }else{  
    return(model)
  }
}



# Inputs: - Y: a time-series numeric matrix of polls and elections
#            Column order: National, NSW, VIC, QLD, SA, WA, TAS, NT, ACT
#         - pollsterTs: a time-series of factors naming which pollster
#                   is responsible for which observation
#         - verbose: controls printing during estimation
#         - reestimateFromScratch: if true, use default parameter starting values; otherwise
#                 use the previously-estimated full sample mode
# Output: an SSModel with estimated parameters, containing smoothed&filtered
#            estimates of latent state 2PPs.

fitNationalPollingModel = function(Y, pollsterTs, verbose=TRUE, reestimateFromScratch=FALSE){

n_obs = dim(Y)[1]

# Load data on state populations
#  Source: http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/
# TODO: time-varying population weights via interpolation
electoralRoll2010 = c(4552976,3506844, 2684538, 1099031, 1341005, 356203, 118401, 242842)
electoralRollAug2013 = c(4816991, 3715925, 2840091, 1130388, 1452272, 362892, 128971, 265269)
popweights = apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean) /
                  sum(apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean))

# Ordering of the latent state vector: NSW, VIC, QLD, SA, WA, TAS, NT, ACT


# Observation matrix for the state-space model.
# See KFAS help pages for more detail. The notation
# follows Harvey's Kalman Filter book.
Z = matrix(c(  popweights,
               c(1,0,0,0,0,0,0,0),
               c(0,1,0,0,0,0,0,0),
               c(0,0,1,0,0,0,0,0),
               c(0,0,0,1,0,0,0,0),
               c(0,0,0,0,1,0,0,0),
               c(0,0,0,0,0,1,0,0),
               c(0,0,0,0,0,0,1,0),
               c(0,0,0,0,0,0,0,1)
), ncol=8, byrow=TRUE)

# Transition matrix = identity
bigT = diag(nrow=8,ncol=8)
# Impact matrix
R = diag(nrow=8,ncol=8)

# Set up state space model; covariance matrices Q and H to be filled
# during estimation.
# Initialise the latent 2PPs at 50% plus or minus 20.
mod1 = SSModel( Y ~ 0+ SSMcustom(Z, bigT, R, Q=diag(NA, nrow=8, ncol=8),
                                 a1=rep(50,8), P1=diag(100,nrow=8,ncol=8),
                                 index=NULL, n=n_obs)  )

defaultParams = c(
  0,        # log-volatility of Q factor
  rep(-2,8),    # log-volatilities in diag(Q)
  1,        # Newspoll national bias
  1,        # Newspoll weekly national log-vol
  rep(1,5),  # Newspoll quarterly  state log-vols
  0,        # Essential bias
  1,         # Essential log-vol
  0,        # Galaxy bias
  1,        # Galaxy log-vol
  0,       # Nielsen bias
  1,         # Nielsen log-vol
  0,        # Morgan bias
  1  ,        # Morgan log-vol
  0,        # ReachTEL bias
  1         # ReachTEL log-vol
)

# One I prepared earlier
fullSampleMode = c(0.578871007628583, -1.21738908236711, -0.771152353521847, -0.758377328784425, 
                   -0.750302500333523, -0.487139344129751, -0.157841490112537, -1.06388716919061, 
                   -0.972587833269029, 0.27122375549047, 1.1185980695119, 0.883913961288849, 
                   0.984801834975024, 1.46067979465315, 1.38111052806411, 1.36962715687059, 
                   0.933064390628468, 0.356860239924654, 0.219693249866502, 0.574683461794892, 
                   0.346828250385154, 1.34752456636377, 2.16377158818504, 1.29530271208014, 
                   0.532923095698009, 0.196784776954039)





 if(verbose){
  optimControl = list(trace=1,REPORT=1)
  }else{
  optimControl= list(trace=0)
  }
 if(reestimateFromScratch){
   startingValues = defaultParams
 }else{
   startingValues = fullSampleMode
 }

posteriorfn = function(x,model,pollster){ return(reciprocalLogLikelihood(x,model,pollster) + reciprocalLogPrior(x)) }

fit = optim(fn=posteriorfn, par=startingValues, method='BFGS', model=mod1,pollster=pollsterTs,
            control=optimControl)
dput(fit$par)


mod1a = reciprocalLogLikelihood(fit$par, mod1, pollsterTs, estimate=FALSE)

out = KFS(mod1a, filtering='state',smoothing='state')

out$paramVector = fit$p
out$paramList = unpackParams(fit$p)
out$popweights = popweights
out$origModel = mod1

return(out)
}


























