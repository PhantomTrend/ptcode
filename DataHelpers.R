

# Utility functions
weightedMeanOfPolls <- function(fullDataEntries, paramList){
  x <- fullDataEntries$Vote
  sigma2 <- rep(0, nrow(fullDataEntries))
  for(i in 1:nrow(fullDataEntries)){
    pollsterName <- fullDataEntries$Pollster[i]
    partyName <- fullDataEntries$Party[i]
    electorateName <- fullDataEntries$Electorate[i]
    x[i] <- x[i] - as.numeric(paramList[[pollsterName]][[partyName]][[electorateName]])
    sigma2[i] <- pollsterVarianceInElectorate(paramList[[pollsterName]][['NoiseVariance']], electorateName)
  }
  return( sum(x/sigma2)/sum(1/sigma2) )
}

varianceOfWeightedMean <- function(fullDataEntries, paramList){
  sigma2 <- rep(0, nrow(fullDataEntries))
  for(i in 1:nrow(fullDataEntries)){
    sigma2[i] <- pollsterVarianceInElectorate(paramList[[fullDataEntries$Pollster[i]]][['NoiseVariance']],
                                              fullDataEntries$Electorate[i])
  }
  return( prod(sigma2)/sum(sigma2) )
}

# Rescale pollster's noise variance for each state such that taking an average
# of their state-by-state estimates will have the same variance as their reported
# national estimate
pollsterVarianceInElectorate <- function(australiaWideVariance, thisElectorate){
  if(thisElectorate == "AUS"){
    return(australiaWideVariance)
  }
  return(australiaWideVariance / popweights[[thisElectorate]])
}

# Low-level data operations

makeDataMatrixEntry <- function(fullDataEntries, paramList){
  if(any(fullDataEntries$Pollster == 'Election')){
    return(fullDataEntries$Vote[which(fullDataEntries$Pollster == 'Election')])
  }
  if(nrow(fullDataEntries)==1){
    pollsterName <- fullDataEntries$Pollster[1]
    partyName <- fullDataEntries$Party[1]
    electorateName <- fullDataEntries$Electorate[1]
    return(fullDataEntries$Vote - paramList[[pollsterName]][[partyName]][[electorateName]])
  }else{
    return( weightedMeanOfPolls(fullDataEntries, paramList) )
  }
}

makeH <- function(fullDataEntries, paramList){
  if(any(fullDataEntries$Pollster == 'Election')){
    return(0.045**2)    # Elections only have rounding error
  }
  if(nrow(fullDataEntries)==1){
    return(pollsterVarianceInElectorate(paramList[[fullDataEntries$Pollster[1]]][['NoiseVariance']],
                                        fullDataEntries$Electorate[1]))
  }else{
    return( varianceOfWeightedMean(fullDataEntries, paramList) )
  }
}


makeDataMatrixRowAndH <- function(fullDataRow, paramList){
  columns <- unique(fullDataRow$ObservationColumn)
  rowOutput <- matrix(NA, nrow=1, ncol=nrow(observationTypes))
  diagH <- rep(NA, nrow(observationTypes))
  for(column in columns){
    rowOutput[1,column] <- makeDataMatrixEntry(fullDataRow %>% filter(ObservationColumn == column), paramList)
    diagH[column] <- makeH(fullDataRow %>% filter(ObservationColumn == column), paramList)
  }
  return(list(Row = rowOutput, H = diag(diagH, nrow=nrow(observationTypes))))
}


# Given a data frame with multiple observations for particular weeks,
# this assembles them into single observations with time-varying uncertainty,
# conditional on the accuracy of individual pollsters.
makeDataMatrix <- function(fullData, paramList){
  rowNumbers <- unique(fullData$RowNumber)
  nObservationTypes <- nrow(observationTypes)
  nObservations <- max(rowNumbers)
  Y <- matrix(NA, nrow=nObservations, ncol=nObservationTypes)
  H <-  array( diag( 0, nrow=nObservationTypes, ncol=nObservationTypes  ), 
               c(nObservationTypes, nObservationTypes, nObservations) )
  for(rowI in seq_along(rowNumbers)){
    thisRow <- rowNumbers[rowI]
    dataChunk <- fullData %>% filter(RowNumber == thisRow)
    yAndH <- makeDataMatrixRowAndH(dataChunk, paramList)
    Y[thisRow,] <- yAndH[['Row']]
    H[,,thisRow] <- yAndH[['H']]
  }
  return(list(Y=Y, H=H))
}


