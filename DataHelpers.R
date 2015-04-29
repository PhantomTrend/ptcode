

# Utility functions
weightedMeanOfPolls <- function(fullDataEntries, paramList){
  x <- fullDataEntries$Vote
  sigma2 <- rep(0, nrow(fullDataEntries))
  for(i in 1:nrow(fullDataEntries)){
    pollsterName <- as.character(fullDataEntries$Pollster[i])
    x[i] <- x[i] - as.numeric(paramList[[pollsterName]][[as.character(fullDataEntries$Party[i])]])
    sigma2[i] <- paramList[[pollsterName]][['NoiseVariance']]
  }
  return( sum(x/sigma2)/sum(1/sigma2) )
}

varianceOfWeightedMean <- function(fullDataEntries, paramList){
  sigma2 <- rep(0, nrow(fullDataEntries))
  for(i in 1:nrow(fullDataEntries)){
    sigma2[i] <- paramList[[as.character(fullDataEntries$Pollster[i])]][['NoiseVariance']]
  }
  return( prod(sigma2)/sum(sigma2) )
}

# Low-level data operations

makeDataMatrixEntry <- function(fullDataEntries, paramList){
  if(any(fullDataEntries$Pollster == 'Election')){
    return(fullDataEntries$Vote[which(fullDataEntries$Pollster == 'Election')])
  }
  if(nrow(fullDataEntries)==1){
    return(fullDataEntries$Vote - paramList[[as.character(fullDataEntries$Pollster[1])]][[fullDataEntries$Party[1]]])
  }else{
    return( weightedMeanOfPolls(fullDataEntries, paramList) )
  }
}

makeH <- function(fullDataEntries, paramList){
  if(any(fullDataEntries$Pollster == 'Election')){
    return(0.045**2)    # Elections only have rounding error
  }
  if(nrow(fullDataEntries)==1){
    return(paramList[[as.character(fullDataEntries$Pollster[1])]][['NoiseVariance']])
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


