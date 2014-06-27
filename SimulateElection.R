
library(dplyr)
library(gstat)
library(rgdal)


source('BarryFilter.R')

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


getKrigedGrid = function( spatials, booths, longitudeLimits, latitudeLimits ){
  
  gridStepSizeX = diff(longitudeLimits)/100
  gridStepSizeY = diff(latitudeLimits)/100
  
  gridCoordsX = seq(from = longitudeLimits[1], to = longitudeLimits[2], by = gridStepSizeX)
  gridCoordsY = seq(from = latitudeLimits[1], to = latitudeLimits[2], by=gridStepSizeY) 
  cityGrid = expand.grid(Long = gridCoordsX, Lat = gridCoordsY)
  coordinates(cityGrid) = ~Long+Lat
  gridded(cityGrid) = TRUE
  proj4string(cityGrid) = CRS('+proj=longlat +ellps=GRS80 +no_defs')
  
  # Plots for various years in Melbourne and Sydney suggest that this 
  # variogram model is fairly ok.
  alpvg = variogram(ALP_2PP ~ 1, spatials, cressie=TRUE)
  modelVg = fit.variogram(alpvg, vgm(300, 'Sph', 19, 5))
  if(attr(modelVg, 'singular')){
    return(list(singularVariogram=TRUE))
  }
  # Use kriging to estimate the ambient 2PP vote across the electorate and its
  # surrounding area.
  voteModel = krige(ALP_2PP ~ 1, locations = spatials, newdata = cityGrid, model = modelVg, nmax=6)
  
  # Now use a kernel density estimate for the population density.
  nBooths = nrow(booths)
  # Kernel bandwidth: a couple of grid squares
  hX = gridStepSizeX
  hY = gridStepSizeY
  # Calculate scaled distance between each grid point and each booth
  ax = outer(gridCoordsX, booths$Long, "-")/hX
  ay = outer(gridCoordsY, booths$Lat, "-")/hY
  # Scale the horizontal normal distributions by the booth sizes
  # (Horizontal only to avoid double-counting)
  xContribs = sweep( matrix(dnorm(ax), , nBooths), MARGIN=2, booths$Formal_votes, `*` )
  yContribs = matrix(dnorm(ay), , nBooths)
  populationKernelDensity = tcrossprod(xContribs, yContribs)/(nBooths * hX * hY)
  
  populationModel = expand.grid(Long = gridCoordsX, Lat = gridCoordsY)
  populationModel$population = as.vector(populationKernelDensity)
  coordinates(populationModel) = ~Long+Lat
  gridded(populationModel) = TRUE
  proj4string(populationModel) = CRS('+proj=longlat +ellps=GRS80 +no_defs')
  
  return(list(singularVariogram=FALSE, voteModel=voteModel, populationModel=populationModel, grid=cityGrid))
}


getSeatOutcomesWithoutRedistributions = function(stateSwings,seatName, stateName, assumptions, alp2ppLastTime,
                                                 historicalSeatErrors, seatReps){
  
  stateReps = ncol(stateSwings)
  nOutcomes = stateReps*seatReps
  
  if(seatName %in% names(assumptions)){
    return(list(winner=rep(assumptions[[seatName]], nOutcomes),
                alp2pp=rep(0,nOutcomes)))
  }
  
  # NSW, VIC, QLD, SA, WA, TAS, NT, ACT
  stateRow = switch(stateName,
                    'NSW'=1,'VIC'=2,'QLD'=3,'SA'=4,'WA'=5,'TAS'=6,'NT'=7,'ACT'=8)
  thisStateSwings = rep(as.numeric(stateSwings[stateRow,]),seatReps)
  
  seatErrors = sample(historicalSeatErrors, nOutcomes, replace=TRUE)
  
  simulatedVotes = rep(alp2ppLastTime, nOutcomes) + thisStateSwings + seatErrors
  
  outcomes = rep('ALP', nOutcomes)
  outcomes[which(simulatedVotes < 50)] = 'LNP'
  return(list(winner=outcomes,alp2pp=simulatedVotes))
}


getSeatOutcomesWithRedistributions = function(stateSwings,seatName, stateName, assumptions,
                           boothsData, boundariesData, boundariesDivisionNameCol,
                           alp2ppLastTime, historicalSeatErrors, seatReps){
  
  print(seatName)
  
  stateReps = ncol(stateSwings)
  nOutcomes = stateReps * seatReps
  
  if(seatName %in% names(assumptions)){
    print(paste0('Assumption: ', assumptions[[seatName]]))
    return(getSeatOutcomesWithoutRedistributions(stateSwings,seatName, stateName, assumptions, alp2ppLastTime,
                                                 historicalSeatErrors, seatReps))
  }

  # NSW, VIC, QLD, SA, WA, TAS, NT, ACT
  stateRow = switch(stateName,
                    'NSW'=1,'VIC'=2,'QLD'=3,'SA'=4,'WA'=5,'TAS'=6,'NT'=7,'ACT'=8)
  thisStateSwings = as.numeric(stateSwings[stateRow,])
  
  # Match the booths data with the seat polygons using the seat name. The toupper() handles
  # cases like "McEwen"[booths]/"Mcewen"[boundaries].
  seatBoundariesRow = which(toupper(as.character(as.data.frame(boundariesData)[,boundariesDivisionNameCol]))
                                  == toupper(seatName))
  if(length(seatBoundariesRow)==0){
    print('Seat name not in boundaries data. Using last election 2pp.')
    return(getSeatOutcomesWithoutRedistributions(stateSwings,seatName, stateName, assumptions, alp2ppLastTime,
                                                 historicalSeatErrors, seatReps))
  }
  thisSeatPolygons = as(boundariesData[seatBoundariesRow ,,drop=FALSE],'SpatialPolygons')
  
  numberOfPolygons = length(slot(slot(thisSeatPolygons,'polygons')[[1]],'Polygons'))
  if(numberOfPolygons > 1){
    # This seat has multiple land areas (islands, etc.)
    # Choose the largest polygon and throw the rest away.
    print('Using largest polygon only...')
    polygonList = lapply( thisSeatPolygons@polygons , slot , "Polygons" )[[1]]
    polygonAreas = unlist(lapply( polygonList , slot , "area" ))
    largestPolygon = which(polygonAreas==max(polygonAreas))
    thisSeatPolygons = SpatialPolygons(list(Polygons(list(polygonList[[largestPolygon]]), ID="1")),
                                       proj4string = CRS(proj4string(thisSeatPolygons)))
  }
  
  
  
  # Get the outer limits of the seat
  longitudeLimits = as.numeric(bbox(thisSeatPolygons)['x',])
  latitudeLimits = as.numeric(bbox(thisSeatPolygons)['y',])
  # Expand them a bit, to make sure we capture effects near the border
  longitudeLimits = longitudeLimits + c(-0.1,0.1)
  latitudeLimits = latitudeLimits + c(-0.1,0.1)
  
  nearbyBoothsLastElection = filter(boothsData, Lat > latitudeLimits[1] & Lat < latitudeLimits[2] &
                                      Long > longitudeLimits[1] & Long < longitudeLimits[2] )

  nearbyBoothsSpatials = remove.duplicates( SpatialPointsDataFrame( as.data.frame(select(nearbyBoothsLastElection, Long, Lat)),
                                                                    as.data.frame(select(nearbyBoothsLastElection, -(Long:Lat))),
                                                                    proj4string = CRS('+proj=longlat +ellps=GRS80 +no_defs')) )
  
  k1 = getKrigedGrid(nearbyBoothsSpatials, nearbyBoothsLastElection, longitudeLimits, latitudeLimits)
  if(k1$singularVariogram){
    print('Can\'t fit a variogram. Using last election 2pp.')
    return(getSeatOutcomesWithoutRedistributions(stateSwings,seatName, stateName, assumptions, alp2ppLastTime,
                                                 historicalSeatErrors, seatReps))
  }
  squaresInSeat = k1$voteModel %over% as(thisSeatPolygons, 'SpatialPolygons')
  
  votesPerSquare = as.data.frame(k1$voteModel)[which(squaresInSeat==1),'var1.pred']
  peoplePerSquare = as.data.frame(k1$populationModel)[which(squaresInSeat==1),'population']
  estimatedVote = weighted.mean(x = votesPerSquare, w = peoplePerSquare)
  
  seatErrors = sample(historicalSeatErrors, nOutcomes, replace=TRUE)
  
  simulatedVotes = rep(estimatedVote, nOutcomes) + rep(thisStateSwings, seatReps)
  outcomes = rep('ALP', nOutcomes)
  outcomes[which(simulatedVotes < 50)] = 'LNP'
  return(list(winner=outcomes,alp2pp=simulatedVotes))
}


simulateElection = function( state2pp, state2ppCovariance, divisions, assumptions, stateReps,
                             seatReps, dataDir, previousElection, useRedistributions ){
  outcomeMatrix = matrix(NA, nrow = nrow(divisions), ncol=(stateReps*seatReps))
  voteMatrix = matrix(NA, nrow = nrow(divisions), ncol=(stateReps*seatReps))
  
  historicalSeatErrors = loadHistoricalErrors(dataDir, previousElection)
  currentState2pp = getPreviousState2pp(dataDir, previousElection)
  
  cholCov = t(state2ppCovariance)
  
  stateSwings = matrix(NA, nrow=8, ncol=stateReps)
  for(stateRepI in 1:stateReps){
    stateSwings[,stateRepI] = (state2pp + (cholCov %*% rnorm(8))) - currentState2pp
    }

  if(useRedistributions){
      boothsData = switch(as.character(previousElection),
                    '2004' = tbl_df(read.csv('electionmaps/booths_data/2004.csv')),
                    '2007' = tbl_df(read.csv('electionmaps/booths_data/2007.csv')),
                    '2010' = tbl_df(read.csv('electionmaps/booths_data/2010.csv')),
                    '2013' = tbl_df(read.csv('electionmaps/booths_data/2013.csv')))  %>% barryFilter()

      boundariesData = switch(as.character(previousElection),
                        '2004' = readOGR(dsn='electionmaps/newshapes', layer='CED07aAUST_region'),
                        # Note (DW Barry): the ABS's filename for the 2010 election says 2011, but the boundaries are from 2010
                        '2007' = readOGR(dsn='electionmaps/newshapes', layer='CED_2011_AUST'),
                        '2010' = readOGR(dsn='electionmaps/newshapes', layer='COM20111216_ELB_region'),
                        '2013' = readOGR(dsn='electionmaps/newshapes', layer='COM20111216_ELB_region'))
      boundariesDivisionNameCol = switch(as.character(previousElection),
                      '2004' = 3, '2007'=2, '2010'=1, '2013'=1)

      for(seatI in 1:nrow(divisions)){
          seatOutcomes = getSeatOutcomesWithRedistributions(stateSwings,
                                            as.character(divisions[seatI,'DivisionName']),
                                  as.character(divisions[seatI,'State']), assumptions, boothsData,
                                            boundariesData, boundariesDivisionNameCol,
                                            divisions[seatI,'alp2pp'], historicalSeatErrors, seatReps)
          outcomeMatrix[seatI,] = seatOutcomes$winner
          voteMatrix[seatI,] = seatOutcomes$alp2pp
           }
      }else{
        for(seatI in 1:nrow(divisions)){
          seatOutcomes = getSeatOutcomesWithoutRedistributions(stateSwings,
                                                  as.character(divisions[seatI,'DivisionName']),
                                                  as.character(divisions[seatI,'State']), assumptions,
                                                  divisions[seatI,'alp2pp'], historicalSeatErrors, seatReps)
          outcomeMatrix[seatI,] = seatOutcomes$winner
          voteMatrix[seatI,] = seatOutcomes$alp2pp
        }
       }
  return(list(winner=outcomeMatrix,alp2pp=voteMatrix))
}


