
library(KFAS)


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
  1        # Morgan log-vol
)

# One I prepared earlier
fullSampleMode = c(0.447135091273355, -1.18932666931997, -0.743271413198109, -0.602404840074456, 
                   -0.700372264885641, -0.585907354937972, 0.321890809013179, -0.951614053245056, 
                   -0.894401756186261, 1.37791208287335, 2.01889613577442, 1.5583777492386, 
                   1.64017228248424, 2.77926075447892, 2.80470669941256, 2.77513095619234, 
                   1.81435418059188, 0.948906886757349, 0.997898426623023, 0.629084821770763, 
                   0.599086711126461, 2.25833508413846, 2.83849882749308, 2.86536444280255

# A convenience function to convert between a numeric vector
# (needed for calling optim()) and a list
unpackParams = function(pars){
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
    morganVol = pars[24])
}

# Likelihood function, given parameter vector "pars"
likfn = function(pars,model,estimate=TRUE){
  m = attr(model, 'm')
  p = attr(model, 'p')
  n = attr(model, 'n')
  parlist = unpackParams(pars)
  R1 = cbind(rep(52/365 * exp(parlist$qfactorvol),m), diag( 52/365 * exp(parlist$diagq), nrow=m, ncol=m ))
  Q = R1 %*% t(R1)
  model$Q = array( Q, c(m, m, 1) )
  model$H = array( diag( 0, nrow=p, ncol=p  ), 
                   c(p, p, n) )
  
#   model$H[,,which(pollsterTs=='Election')] = diag(1e-4, nrow=p, ncol=p)
  model$H[,,which(pollsterTs=='Election')] = diag(0, nrow=p, ncol=p)
  
  model$H[,,which(pollsterTs=='NewspollW')] = diag(exp(0.5*parlist$newspollWVol), nrow=p, ncol=p)
  model$H[,,which(pollsterTs=='NewspollQ')] = diag(c(0,exp(0.5*parlist$newspollQVol)), nrow=p, ncol=p)
  model$y[which(pollsterTs=='NewspollW'),] = Y[which(pollsterTs=='NewspollW'),] - parlist$newspollNationalBias
  model$y[which(pollsterTs=='NewspollQ'),] = sweep(Y[which(pollsterTs=='NewspollQ'),],MARGIN=2,c(rep(parlist$newspollNationalBias,6),NA,NA,NA),FUN="-")
  model$H[,,which(pollsterTs=='Essential')] = diag(exp(0.5*parlist$essentialVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='Essential'),] = Y[which(pollsterTs=='Essential'),] - parlist$essentialBias
  model$H[,,which(pollsterTs=='Galaxy')] = diag(exp(0.5*parlist$galaxyVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='Galaxy'),] = Y[which(pollsterTs=='Galaxy'),] - parlist$galaxyBias
  model$H[,,which(pollsterTs=='ACNielsen')] = diag(exp(0.5*parlist$nielsenVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='ACNielsen'),] = Y[which(pollsterTs=='ACNielsen'),] - parlist$nielsenBias
  model$H[,,which(pollsterTs=='RoyMorgan')] = diag(exp(0.5*parlist$morganVol), nrow=p, ncol=p)
  model$y[which(pollsterTs=='RoyMorgan'),] = Y[which(pollsterTs=='RoyMorgan'),] - parlist$morganBias
  
  if(estimate){
    return(-logLik(model))
       }else{  
    return(model)
  }
}


 if(verbose){
  optimControl = list(trace=5,REPORT=1)
  }else{
  optimControl= list(trace=0)
  }
 if(reestimateFromScratch){
   startingValues = defaultParams
 }else{
   startingValues = fullSampleMode
 }
fit = optim(f=likfn, p=startingValues, method='BFGS', model=mod1,
            control=optimControl)

mod1 = likfn(fit$p, mod1, estimate=FALSE)

out = KFS(mod1, filtering='state',smoothing='state')

out$paramVector = fit$p
out$paramList = unpackParams(fit$p)
out$popweights = popweights

return(out)
}


























