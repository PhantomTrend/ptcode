

getPreviousState2pp = function(dataDir, previousElectionYear){
  tppByState = tbl_df(read.csv(paste(dataDir,'tpp_by_state_at_elections.csv',sep='/')))
  tppByState$date = as.Date(tppByState$date, format="%d/%m/%Y")
  
  yearRowHashHack = list('2013'=1,'2010'=2,'2007'=3,'2004'=4,'2001'=5,'1998'=6)
  election_t = as.numeric(yearRowHashHack[as.character(previousElectionYear)])
  
  previousResultVector = c(tppByState$Labor2ppNSW[election_t],
          tppByState$Labor2ppVIC[election_t],
          tppByState$Labor2ppQLD[election_t],
          tppByState$Labor2ppSA[election_t],
          tppByState$Labor2ppWA[election_t],
          tppByState$Labor2ppTAS[election_t],
          tppByState$Labor2ppNT[election_t],
          tppByState$Labor2ppACT[election_t])
  
  return(previousResultVector)
}


loadHistoricalErrors = function(dataDir, maxYear=2013){
  tppByState = tbl_df(read.csv(paste(dataDir,'tpp_by_state_at_elections.csv',sep='/')))
  tppByState$date = as.Date(tppByState$date, format="%d/%m/%Y")
  
  tppdata2013 = tbl_df(read.csv(paste(dataDir,'tpp_by_division_2013.csv',sep='/'),skip = 1))
  tppdata2010 = tbl_df(read.csv(paste(dataDir,'tpp_by_division_2010.csv',sep='/'),skip = 1))
  tppdata2007 = tbl_df(read.csv(paste(dataDir,'tpp_by_division_2007.csv',sep='/'),skip = 1))
  tppdata2004 = tbl_df(read.csv(paste(dataDir,'tpp_by_division_2004.csv',sep='/'),skip = 1))
  
  prepareData = function(modelData, election_t, year){
    if(year == 2013 || year == 2010){alpWasInGovt = TRUE}else{alpWasInGovt=FALSE}
    modelData = mutate(modelData, year=as.character(year),
                       State = StateAb) %>% select(-StateAb)
    if(alpWasInGovt){
      modelData = mutate(modelData, alp2pp = Australian.Labor.Party.Percentage,
                         alp2ppLastTime = alp2pp - Swing)
    }else{
      modelData = mutate(modelData, alp2pp = Australian.Labor.Party.Percentage,
                         alp2ppLastTime = alp2pp + Swing) 
    }
    
    modelData = mutate(modelData, alpIncumbent = 0)
    modelData[modelData$alp2ppLastTime > 50, "alpIncumbent"] = 1

    modelData = mutate(modelData, StateSwing = NA )
    modelData[modelData$State=='NSW','StateSwing'] = (tppByState$Labor2ppNSW[election_t] - tppByState$Labor2ppNSW[election_t+1])
    modelData[modelData$State=='VIC','StateSwing'] =  (tppByState$Labor2ppVIC[election_t] - tppByState$Labor2ppVIC[election_t+1])
    modelData[modelData$State=='QLD','StateSwing'] =  (tppByState$Labor2ppQLD[election_t] - tppByState$Labor2ppQLD[election_t+1])
    modelData[modelData$State=='WA','StateSwing'] = (tppByState$Labor2ppWA[election_t] - tppByState$Labor2ppWA[election_t+1])
    modelData[modelData$State=='SA','StateSwing'] =  (tppByState$Labor2ppSA[election_t] - tppByState$Labor2ppSA[election_t+1])
    modelData[modelData$State=='TAS','StateSwing'] =  (tppByState$Labor2ppTAS[election_t] - tppByState$Labor2ppTAS[election_t+1])
    modelData[modelData$State=='ACT','StateSwing'] = (tppByState$Labor2ppACT[election_t] - tppByState$Labor2ppACT[election_t+1])
    modelData[modelData$State=='NT','StateSwing'] = (tppByState$Labor2ppNT[election_t] - tppByState$Labor2ppNT[election_t+1])
    modelData = modelData[which(modelData$alp2ppLastTime > 0),]    # Drop booths with independents etc
    modelData = modelData[which(modelData$alp2ppLastTime < 100),]
    return(modelData)
  }
  
  tppdata2013 = prepareData(tppdata2013, 1, 2013)
  tppdata2010 = prepareData(tppdata2010, 2, 2010)
  tppdata2007 = prepareData(tppdata2007, 3, 2007)
  tppdata2004 = prepareData(tppdata2004, 4, 2004)
  
  errors2013 = tppdata2013$alp2pp - (tppdata2013$alp2ppLastTime+tppdata2013$StateSwing)
  errors2010 = tppdata2010$alp2pp - (tppdata2010$alp2ppLastTime+tppdata2010$StateSwing)
  errors2007 = tppdata2007$alp2pp - (tppdata2007$alp2ppLastTime+tppdata2007$StateSwing)
  errors2004 = tppdata2004$alp2pp - (tppdata2004$alp2ppLastTime+tppdata2004$StateSwing)
  
  result = errors2004
  if(maxYear > 2004){result = c(result,errors2007)}
  if(maxYear > 2007){result = c(result,errors2010)}
  if(maxYear > 2010){result = c(result,errors2013)}
  return(result)
}





getSeatOutcomes = function(divisions,assumptions,stateSwingVector,historicalSeatErrors){
  nSeats = nrow(divisions)
  
  # Start with the 2pp from last time
  seat2pp = divisions$alp2pp
  
  # Add the statewide swings
  seat2pp[which(divisions$State=='NSW')] = seat2pp[which(divisions$State=='NSW')] + stateSwingVector[1]
  seat2pp[which(divisions$State=='VIC')] = seat2pp[which(divisions$State=='VIC')] + stateSwingVector[2]
  seat2pp[which(divisions$State=='QLD')] = seat2pp[which(divisions$State=='QLD')] + stateSwingVector[3]
  seat2pp[which(divisions$State=='SA')] = seat2pp[which(divisions$State=='SA')] + stateSwingVector[4]
  seat2pp[which(divisions$State=='WA')] = seat2pp[which(divisions$State=='WA')] + stateSwingVector[5]
  seat2pp[which(divisions$State=='TAS')] = seat2pp[which(divisions$State=='TAS')] + stateSwingVector[6]
  seat2pp[which(divisions$State=='NT')] = seat2pp[which(divisions$State=='NT')] + stateSwingVector[7]
  seat2pp[which(divisions$State=='ACT')] = seat2pp[which(divisions$State=='ACT')] + stateSwingVector[8]
  
  # Add a bootstrapped error
  seat2pp = seat2pp + sample(historicalSeatErrors, nSeats)
  
  outcome = rep(NA,nSeats)
  outcome[which(seat2pp > 50)] = 'ALP'
  outcome[which(seat2pp < 50)] = 'LNP'
  
  # Overwrite certain special seats
  for(i in 1:length(assumptions)){
    outcome[which(divisions$DivisionName == names(assumptions)[i])] = as.character(assumptions[i])
  }
  
  return(outcome)
}


simulateElection = function( state2pp, state2ppCovariance, divisions, assumptions, stateReps,
                             seatReps, dataDir, previousElection ){
  outcomeMatrix = matrix(NA, nrow = nrow(divisions), ncol=(stateReps*seatReps))
  
  historicalSeatErrors = loadHistoricalErrors(dataDir, previousElection)
  currentState2pp = getPreviousState2pp(dataDir, previousElection)
  
  cholCov = t(state2ppCovariance)
  
  for(stateRepI in 1:stateReps){
    stateSwingVector = (state2pp + (cholCov %*% rnorm(8))) - currentState2pp
    
    for(seatRepI in 1:seatReps){
      outcomeMatrix[,(stateRepI-1)*seatReps + seatRepI] = getSeatOutcomes(divisions,assumptions,
                                                      stateSwingVector,historicalSeatErrors) 
    }
    
  }
  
  return(outcomeMatrix)
}


