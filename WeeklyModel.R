
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
  # Every poll assumed to take 1 week, by default.
  # (In the model, 1 week = the smallest unit of time.)
  pollDurations[[pollster]] <- "Week"
}
pollDurations[['Newspoll Quarterly']] <- "Quarter"
pollDurations[['Essential']] <- "Fortnight"
pollDurations[['Morgan Multi']] <- "Fortnight"
lagLength <- 2   # We'll keep track of x(t-1), x(t-2), and approximate the quarterlies with an ARIMA.
quarterlyPDLcoefficient <- 1/8

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
observedPartyNames <- c(partyNames, "PUPOTH", "GRNOTH")
nParties <- length(partyNames)



latentComponentNamesBase <- as.vector(outer(stateNames, partyNames, FUN = 'paste'))
latentPartyNames <- rep(partyNames, each=length(stateNames))
latentStateNames <- rep(stateNames, nParties)
latentComponentNames <- latentComponentNamesBase
componentNamesWithLag <- unlist(lapply(latentComponentNamesBase, function(x){paste0(x,' Lag 1')}))
componentNamesMA <- unlist(lapply(latentComponentNamesBase, function(x){paste0(x,' MovingAvg')}))
latentComponentNames <- c( latentComponentNames, componentNamesWithLag, componentNamesMA )

nLatentComponents <- length(latentComponentNames)
nLatentComponentsBase <- length(latentComponentNamesBase)

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


unobservedElectorates <- setdiff(stateNames, unique( (modelData %>% filter(Pollster != 'Election'))$Electorate ))



modelData$Lag <- NA
for(thisPollster in pollsters){
  modelData$Lag[which(modelData$Pollster == thisPollster)] <- pollDurations[[thisPollster]]
}

pollstersReportingWeeklyData <- names(pollDurations)[which(pollDurations == "Week")]
observationTypes <- unique(modelData  %>% filter(Pollster %in% pollstersReportingWeeklyData)
                           %>% arrange(Electorate) %>% select(Party, Electorate) )
observationTypes$Lag <- "Week"
lagLengthsInDataset <- unlist(unique(pollDurations))
for(lagLength in lagLengthsInDataset){
  if(lagLength == "Week"){
    next
  }
  pollstersWithThisLagLength <- names(pollDurations)[which(pollDurations == lagLength)]
  obsTypesFromThesePollsters <- unique(modelData %>% filter(Pollster %in% pollstersWithThisLagLength)
                                       %>% arrange(Electorate) %>% select(Party, Electorate) )
  if(nrow(obsTypesFromThesePollsters) == 0){
    next
  }
  obsTypesFromThesePollsters$Lag <- lagLength
  observationTypes <- rbind(observationTypes, obsTypesFromThesePollsters)
}

Z <- matrix(0, nrow=nrow(observationTypes), ncol = nLatentComponentsBase*3)
for(zi in 1:nrow(observationTypes)){
  obsType <- observationTypes[zi,]
  if(obsType$Electorate == 'AUS'){
    if(obsType$Party == 'PUPOTH'){
      for(state in stateNames){
        Z[zi,which(latentStateNames == state &
                     (latentPartyNames == 'PUP' | latentPartyNames == 'OTH'))] <- popweights[[state]]
      }
    }else if(obsType$Party == 'GRNOTH'){
      for(state in stateNames){
        Z[zi,which(latentStateNames == state &
                     (latentPartyNames == 'GRN' | latentPartyNames == 'OTH'))] <- popweights[[state]]
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
    }else if(obsType$Party == 'GRNOTH'){
      Z[zi,which(latentStateNames == obsType$Electorate &
                   (latentPartyNames == 'GRN' | latentPartyNames == 'OTH'))] <- 1
    }else{
      Z[zi,which(latentStateNames == obsType$Electorate & latentPartyNames == obsType$Party)] <- 1
    }
  }
  if(obsType$Lag == 'Week'){
    next
  }
  if(obsType$Lag == 'Fortnight'){
    zComponentsToBeRepeated <- Z[zi,1:nLatentComponentsBase]
    zRowWithLags <- rep(zComponentsToBeRepeated, 2) / 2
    Z[zi,1:length(zRowWithLags)] <- zRowWithLags
  }else{
    if(obsType$Lag == 'Quarter'){
      zComponentsToBeReused <- Z[zi,1:nLatentComponentsBase]
      zRow <- c(rep(0, nLatentComponentsBase *2), zComponentsToBeReused)
      Z[zi,] <- zRow
    }else{
      stop(sprintf('Unknown lag type %s', obsType$Lag))
    }
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

# Transition matrix
# T  =  [ I, 0, 0       (random walk for weekly components)
#         I, 0, 0       (keeping track of lagged weekly components for fortnightly avg)
#       c*I, 0, (1-c)*I ]   (Approximation to quarterly average)
# Impact matrix
# R  =  [ I,
#         0,
#         c*I ]

smallIdentityMatrix <- diag(nLatentComponentsBase)
smallZeroMatrix <- matrix(0, nrow=nLatentComponentsBase, ncol=nLatentComponentsBase)
bigT <- rbind( cbind(smallIdentityMatrix, smallZeroMatrix, smallZeroMatrix),
               cbind(smallIdentityMatrix, smallZeroMatrix, smallZeroMatrix),
               cbind( quarterlyPDLcoefficient*smallIdentityMatrix, smallZeroMatrix, (1-quarterlyPDLcoefficient) * smallIdentityMatrix)  )

R <- rbind(smallIdentityMatrix,
           smallZeroMatrix,
           quarterlyPDLcoefficient * smallIdentityMatrix)

# Initialise distribution of primary votes at t = 0
startingValues <- list(ALP = 50, LNP = 50, GRN = 2, PUP = 0, OTH = 10)
startingPlusOrMinus <- list(ALP = 25, LNP = 25, GRN = 2, PUP = 1, OTH = 8)
a1 <- rep(NA, nLatentComponentsBase)
diagP1 <- rep(NA, nLatentComponentsBase)
for(party in partyNames){
  a1[which(latentPartyNames == party)] <- startingValues[[party]]
  diagP1[which(latentPartyNames == party)] <- (startingPlusOrMinus[[party]]/2)**2
}
a1 <- rep(a1, (nLatentComponents/nLatentComponentsBase))
P1 <- diag(rep(diagP1, (nLatentComponents/nLatentComponentsBase)))

mod1 = SSModel( Y ~ 0+ SSMcustom(Z, bigT, R, Q=diag(NA, nrow=nLatentComponentsBase, ncol=nLatentComponentsBase),
                                 a1=a1, P1=P1, index=NULL, n=nObservations)  )


source('ParameterHelpers.R')
source('KalmanHelpers.R')
source('DataHelpers.R')
source('PriorDistributions.R')

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
      thisParty <- pollResults$Party[rowI]
      thisNumber <- pollResults$Vote[rowI] - paramList[[thisPollster]][[thisParty]]
      thisPollster <- pollResults$Pollster[rowI]
      thisElectorate <- pollResults$Electorate[rowI]
      if(thisElectorate == 'AUS' | !(thisParty %in% partyNames)){
        next
      }
      actualNumber <- (actualResult %>% filter(Party == thisParty, Electorate==thisElectorate))$Vote
      if(is.na(actualNumber)){
        next
      }
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

posteriorfn = function(x,model){ result <- reciprocalLogLikelihood(x,model) + reciprocalLogPrior(x)
                                 return(result) }


theta0 <- dget('EstimatedMode.R')

# optimControl = list(trace=6,REPORT=1, maxit=10)
# fit = optim(fn=posteriorfn, par=paramListToVector(theta0), method='CG',
#             model=mod1, control=optimControl)


thetaNow <- paramListToVector(theta0)
momentum <- rep(0, length(thetaNow))
fNow <- posteriorfn(thetaNow, mod1)
proposalScale <- 0.01
nMHiterations <- 10000
for(iterI in 1:nMHiterations){
  thetaOld <- thetaNow
  blockSize <- 50
  blockIndices <- sample.int(n=length(thetaNow), size=blockSize, replace=FALSE)
  randomStep <- rep(0, length(thetaNow))
  randomStep[blockIndices] <- proposalScale * rnorm(blockSize)
  thetaProposed <- thetaNow + momentum + randomStep
  fProposed <- posteriorfn(thetaProposed, mod1)
  print(c('Current ', -fNow, ' Proposed ', -fProposed))
  if(runif(1) < ((-fProposed) - (-fNow))){
    print('Accepted.')
    thetaNow <- thetaProposed
    fNow <- fProposed
  }
  momentum <- 0.7*momentum + (thetaNow - thetaOld)
}


estimatedMode <- paramVectorToList(thetaNow)

dput(estimatedMode, file='EstimatedMode.R')     # Save to file, in sorta-human-readable form

fittedModel <- reciprocalLogLikelihood(thetaNow, mod1, estimate=FALSE)
smoothedModel <- KFS(fittedModel, filtering='state', smoothing='state')


modelOutput <- modelData[0,]
for(componentI in 1:nLatentComponentsBase){
  modelOutput <- rbind(modelOutput,
                       data.frame(RowNumber = 1:nObservations,
                           Pollster = 'Smoothed',
                           Party = latentPartyNames[componentI],
                           Vote = as.numeric(smoothedModel$alphahat[,componentI]),
                           Electorate = latentStateNames[componentI],
                           Year = NA, Week = NA, PollEndDate = NA, Lag = 0, ObservationColumn = NA),
                       data.frame(RowNumber = 1:nObservations,
                                  Pollster = 'Filtered',
                                  Party = latentPartyNames[componentI],
                                  Vote = as.numeric(smoothedModel$a[1:nObservations,componentI]),
                                  Electorate = latentStateNames[componentI],
                                  Year = NA, Week = NA, PollEndDate = NA, Lag = 0, ObservationColumn = NA)
  )
}
for(party in partyNames){
  componentCols <- which(latentPartyNames == party)
  ausVectorSmoothed <- as.vector(popweights %*% t(smoothedModel$alphahat[,componentCols]))
  ausVectorFiltered <- as.vector(popweights %*% t(smoothedModel$a[1:nObservations,componentCols]))
  modelOutput <- rbind(modelOutput,
                       data.frame(RowNumber = 1:nObservations,
                                  Pollster = 'Smoothed',
                                  Party = party,
                                  Vote = ausVectorSmoothed,
                                  Electorate = 'AUS',
                                  Year = NA, Week = NA, PollEndDate = NA, Lag = 0, ObservationColumn = NA),
                       data.frame(RowNumber = 1:nObservations,
                                  Pollster = 'Filtered',
                                  Party = party,
                                  Vote = ausVectorFiltered,
                                  Electorate = 'AUS',
                                  Year = NA, Week = NA, PollEndDate = NA, Lag = 0, ObservationColumn = NA)
                       
  )
}


pupStartRow <- (modelData %>% filter(Party=='PUP', !is.na(Vote)) %>% arrange(RowNumber))$RowNumber[1]
modelOutput <- mutate(modelOutput, Vote=ifelse(Party=='PUP' & RowNumber < pupStartRow, NA, Vote))



library(ggplot2)

for(thisState in stateNames){
print(ggplot() + aes(x=RowNumber, y=Vote) +
  geom_point(data = modelData %>% filter(Electorate==thisState), mapping=aes(shape=Pollster, colour=Party)) +
  geom_line(data=modelOutput %>% filter(Electorate==thisState, Pollster=='Smoothed'), mapping=aes(colour=Party)) +
  scale_shape_manual(values=1:nPollsters) + ggtitle(thisState))
}
print( ggplot() + aes(x=RowNumber, y=Vote) +
         geom_point(data = modelData %>% filter(Electorate=='AUS'), mapping=aes(shape=Pollster, colour=Party)) +
         geom_line(data=modelOutput %>% filter(Electorate=='AUS', Pollster=='Smoothed'), mapping=aes(colour=Party)) +
         scale_shape_manual(values=1:nPollsters) + ggtitle('AUS')  )



