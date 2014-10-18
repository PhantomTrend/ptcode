
library(dplyr)
library(assertive)
library(KFAS)

longData <- tbl_df(read.csv('PollingData/MergedData.csv'))
longData$PollEndDate <- as.Date(longData$PollEndDate)



pollsters <- unique(longData$Pollster)
nPollsters <- length(pollsters)

# Prepare for some pollsters reporting quarterly averages etc.
pollDurations <- list()
for(pollster in pollsters){
  # Every poll assumed to take 1 week, by default
  pollDurations[[pollster]] <- 1
}
# pollDurations[['Newspoll Quarterly']] <- 13
pollDurations[['Newspoll Quarterly']] <- 4
pollDurations[['Essential']] <- 2
lagLength <- max(as.numeric(pollDurations))


# Load data on state populations
#  Source: http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/
# TODO: time-varying population weights via interpolation
electoralRoll2010 = c(4552976,3506844, 2684538, 1099031, 1341005, 356203, 118401, 242842)
electoralRollAug2013 = c(4816991, 3715925, 2840091, 1130388, 1452272, 362892, 128971, 265269)
popweights = apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean) /
  sum(apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean))
stateNames <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
names(popweights) <- stateNames



partyNames <- c("ALP", "LNP", "GRN", "PUP", "OTH")
observedPartyNames <- partyNames
nParties <- length(partyNames)


latentComponentNamesBase <- as.vector(outer(stateNames, partyNames, FUN = 'paste'))
latentPartyNames <- rep(partyNames, each=length(stateNames))
latentStateNames <- rep(stateNames, nParties)
latentComponentNames <- latentComponentNamesBase
if(lagLength > 1){
  for(thisLag in 2:lagLength){
    componentNamesWithLag <- unlist(lapply(latentComponentNamesBase, function(x){paste0(x,' Lag', thisLag)}))
    latentComponentNames <- c( latentComponentNames, componentNamesWithLag )
  }
}
latentLags <- rep(1:lagLength, each=length(latentComponentNamesBase))

nLatentComponents <- length(latentComponentNames)
nLatentComponentsBase <- length(latentComponentNamesBase)
nLaggedComponents <- nLatentComponents - nLatentComponentsBase

invisible(assert_that(all(as.vector(mapply(paste, latentStateNames, latentPartyNames)) == latentComponentNamesBase)))


getYear <- function(d){ return(as.integer(format(d, '%Y'))) }
getWeek <- function(d){ return(as.integer(format(d, '%W'))) }
firstMondayOfYear <- function(y){
  assert_is_a_string(y)
  assert_is_a_number(as.integer(y))
  firstWeek <- seq( as.Date(paste0(y,'-01-01')), as.Date(paste0(y,'-01-07')), by='1 day' )
  return(firstWeek[which(weekdays(firstWeek)=='Monday')])
}
getDateFromYearAndWeek <- function(y,w){ return( firstMondayOfYear(as.character(y)) + 7*w )}


modelData <- longData %>% filter(PollEndDate >= as.Date("2000-01-01")) %>%
  arrange(PollEndDate) %>% mutate(Year = getYear(PollEndDate),
                                  Week = getWeek(PollEndDate),
                                  RowNumber = (Year-2000)*52 + Week ) %>%
  filter(Party %in% observedPartyNames)   %>%  
  select(RowNumber, Pollster, Party, Vote, Electorate, Year, Week, PollEndDate)

modelData$Lag <- 1
for(thisPollster in pollsters){
  modelData$Lag[which(modelData$Pollster == thisPollster)] <- pollDurations[[thisPollster]]
}

pollstersReportingWeeklyData <- names(pollDurations)[which(pollDurations == 1)]
observationTypes <- unique(modelData  %>% filter(Pollster %in% pollstersReportingWeeklyData)
                           %>% arrange(Electorate) %>% select(Party, Electorate) )
observationTypes$Lag <- 1
lagLengthsInDataset <- unlist(unique(pollDurations))
for(lagLength in lagLengthsInDataset){
  if(lagLength == 1){
    next
  }
  pollstersWithThisLagLength <- names(pollDurations)[which(pollDurations == lagLength)]
  obsTypesFromThesePollsters <- unique(modelData %>% filter(Pollster %in% pollstersWithThisLagLength)
                                       %>% arrange(Electorate) %>% select(Party, Electorate) )
  obsTypesFromThesePollsters$Lag <- lagLength
  observationTypes <- rbind(observationTypes, obsTypesFromThesePollsters)
}


Z <- matrix(0, nrow=nrow(observationTypes), ncol = nLatentComponents)
for(zi in 1:nrow(observationTypes)){
  obsType <- observationTypes[zi,]
  if(obsType$Electorate == 'AUS'){
    if(obsType$Party == 'PUPOTH'){
      for(state in stateNames){
        Z[zi,which(latentStateNames == state &
                     (latentPartyNames == 'PUP' | latentPartyNames == 'OTH'))] <- popweights[[state]]
      }
    }else{
      for(state in stateNames){
        Z[zi,which(latentStateNames == state & latentPartyNames == obsType$Party)] <- popweights[[state]]
      }
    }
  }else{
    if(obsType$Party == 'PUPOTH'){
      Z[zi,which(latentStateNames == obsType$Electorate &
                   (latentPartyNames == 'PUP' | latentPartyNames == 'OTH'))] <- 1
    }else{
      Z[zi,which(latentStateNames == obsType$Electorate & latentPartyNames == obsType$Party)] <- 1
    }
  }
  if(obsType$Lag > 1){
    zComponentsToBeRepeated <- Z[zi,1:length(latentComponentNamesBase)]
    zRowWithLags <- rep(zComponentsToBeRepeated, obsType$Lag) / obsType$Lag
    Z[zi,1:length(zRowWithLags)] <- zRowWithLags
  }
}


modelData$ObservationColumn <- NA
for(zi in 1:nrow(observationTypes)){
  modelData$ObservationColumn[which(modelData$Party == observationTypes$Party[zi] &
                                      modelData$Electorate == observationTypes$Electorate[zi] &
                                      modelData$Lag == observationTypes$Lag[zi])] <- zi
}


nObservations <- max(modelData$RowNumber)
nObservationTypes <- nrow(observationTypes)
Y <- matrix(NA, nrow=nObservations, ncol=nObservationTypes)

# Transition matrix = identity, plus identity matrices to retain the lagged values
# T  =  [ I, 0 ....   0 
#         I, 0, 0, .. 0
#         0, I, 0, .. 0  etc ]


smallIdentityMatrix <- diag(nLatentComponentsBase)
largeIdentityMatrix <- diag(nLaggedComponents)
bigT <- rbind(  cbind(smallIdentityMatrix, matrix(0, nrow=nLatentComponentsBase, ncol=nLaggedComponents)),
                cbind(largeIdentityMatrix, matrix(0, nrow=nLaggedComponents, ncol=nLatentComponentsBase))  )
# Impact matrix
R <- diag(nrow=nLatentComponents,ncol=nLatentComponents)

# Set up state space model; covariance matrices Q and H to be filled
# during estimation.
# Initialise the latent values at 50% plus or minus 20.
mod1 = SSModel( Y ~ 0+ SSMcustom(Z, bigT, R, Q=diag(NA, nrow=nLatentComponents, ncol=nLatentComponents),
                                 a1=rep(50,nLatentComponents), P1=diag(100,nrow=nLatentComponents,ncol=nLatentComponents),
                                 index=NULL, n=nObservations)  )


source('ParameterHelpers.R')
source('KalmanHelpers.R')
source('DataHelpers.R')


reciprocalLogLikelihood = function(paramVector,model,estimate=TRUE){
  paramList = paramVectorToList(paramVector)
  
  model$Q <- makeQmatrix(paramList)
  yAndH <- makeDataMatrix(modelData, paramList)
  model$y <- yAndH$Y
  model$H <- yAndH$H
  
  weeklyLogLikelihood <- logLik(model, check.model=TRUE)
  
  # Add likelihood for election weeks
  electionWeeks <- (filter(modelData, Pollster=='Election'))$RowNumber
  electionEvePolls <- filter(modelData, Pollster != 'Election', RowNumber %in% electionWeeks)
  electionEveLogLikelihood <- 0
  for(electionRowNumber in unique(electionEvePolls$RowNumber)){
    thisSetOfPolls <- filter(modelData, RowNumber %in% electionRowNumber)
    actualResult <- filter(thisSetOfPolls, Pollster == 'Election')
    pollResults <- filter(thisSetOfPolls, Pollster != 'Election')
    for(rowI in 1:nrow(pollResults)){
      thisParty <- pollResults[rowI, 'Party']
      thisNumber <- pollResults[rowI,'Vote'] - paramList[[thisPollster]][[thisParty]]
      thisPollster <- pollResults[rowI, 'Pollster']
      thisElectorate <- pollResults[rowI, 'Electorate']
      
      actualNumber <- (actualResult %>% filter(Party == thisParty, Electorate==thisElectorate))$Vote
      thisLogLikelihood <- dnorm( actualNumber, mean=thisNumber, 
                                  sd = sqrt(paramList[[thisPollster]][['NoiseVariance']]), log=TRUE)
      electionEveLogLikelihood <- electionEveLogLikelihood + thisLogLikelihood
    }
  }
  
  totalLogLikelihood <- weeklyLogLikelihood + electionEveLogLikelihood
  
  if(estimate){
    return(-totalLogLikelihood)
  }else{  
    return(model)
  }
}

theta0 <- getDefaultParamList()
startTime <- proc.time()
print(reciprocalLogLikelihood(paramListToVector(theta0), mod1))
print(proc.time() - startTime)

fittedMod <- reciprocalLogLikelihood(paramListToVector(theta0), mod1, estimate=FALSE)
soothed <- KFS(fittedMod, filtering='state', smoothing='state')
# print(soothed$alphahat[,1])

plot(soothed$alphahat[,1])





