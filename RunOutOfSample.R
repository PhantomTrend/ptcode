

source('LoadPollsAndElections.R')

dataDir = 'Data'
startDate = as.Date('2003-01-01')

dataList = loadPollsAndElections(dataDir, startDate)
endDate = as.Date(end(dataList$data))



source('SimulateElection.R')

stateReps = 500
seatReps = 10
set.seed(31337)


# Prepare a time series matrix for input to the state-space model,
# with one day per row
Y = as.ts(dataList$data)
# Remove national 2pp readings when state-based ones are available to avoid
# stochastic singularity, given that we're using approximate population weights.
Y[which(!is.na(Y[,2])),1] = NA


electionRows = which(as.ts(dataList$pollster)=='Election')

electionYearToTest = 2010
electionRowToTest = electionRows[2] -1

previousElectionToTest = (electionYearToTest-3)

source('NationalPollingModel.R')
estimatedModel = fitNationalPollingModel(Y[1:electionRowToTest,],
                                         as.ts(dataList$pollster)[1:electionRowToTest],
                                         verbose=FALSE, reestimateFromScratch=TRUE)



source('PrintedOutput.R')
printCurrentEstimates(estimatedModel)

oldPendulumFilename = paste0('Data/tpp_by_division_',previousElectionToTest,'.csv')
newPendulumFilename = paste0('Data/tpp_by_division_',electionYearToTest,'.csv')

currentPendulum = tbl_df(read.csv(oldPendulumFilename, skip=1)) %>%
  mutate(DivisionName=DivisionNm, State=StateAb, CurrentHolder = PartyAb,
         alp2pp = Australian.Labor.Party.Percentage) %>%
  select(DivisionName, State, CurrentHolder, alp2pp)
levels(currentPendulum$CurrentHolder) = c(levels(currentPendulum$CurrentHolder), 'Greens', 'Indep', 'PUP', 'KAP')

assumptions = list()

state2pp = estimatedModel$a[nrow(estimatedModel$a),]
state2ppCovariance = estimatedModel$P[,,nrow(estimatedModel$a)]
electionOutcomes = simulateElection(state2pp, state2ppCovariance, currentPendulum, assumptions, stateReps,
                                    seatReps, dataDir, previousElection=previousElectionToTest,
                                    useRedistributions=TRUE)


alpTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='ALP')})
lnpTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='LNP')})
indepTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='Indep')})
greensTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='Greens')})
pupTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='PUP')})
kapTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='KAP')})

otherTotals = indepTotals + greensTotals + pupTotals + kapTotals


mean2pp = apply(electionOutcomes$alp2pp, 1, mean)
min2pp = apply(electionOutcomes$alp2pp, 1, function(x){as.numeric(quantile(x, 0.025))})
max2pp = apply(electionOutcomes$alp2pp, 1, function(x){as.numeric(quantile(x, 0.975))})


actualPendulum = tbl_df(read.csv(newPendulumFilename, skip=1)) %>%
  mutate(DivisionName=DivisionNm, State=StateAb, CurrentHolder = PartyAb,
         alp2pp = Australian.Labor.Party.Percentage) %>%
  select(DivisionName, State, CurrentHolder, alp2pp)
levels(currentPendulum$CurrentHolder) = c(levels(currentPendulum$CurrentHolder), 'Greens', 'Indep', 'PUP', 'KAP')


predictions = cbind(currentPendulum, mean2pp, min2pp, max2pp)


plotData = inner_join(actualPendulum, predictions, by='DivisionName')



library(ggplot2)

print(  ggplot(plotData) +
#    geom_errorbarh(aes(x=mean2pp,y=alp2pp.x,xmax=max2pp,xmin=min2pp), colour='grey55') +
  geom_point(aes(x=mean2pp, y=alp2pp.x), colour='mediumvioletred') +
  scale_y_continuous(limits=c(25,75)) + scale_x_continuous(limits=c(25,75)) +
  labs(x='Predicted', y='Actual') + ggtitle(paste0('ALP Two-party Preferred, ', electionYearToTest))  )


surpriseAlpWins = plotData %>% filter(CurrentHolder.x == 'ALP') %>% filter(mean2pp < 50) %>% arrange(max2pp)
surpriseCoalitionWins = plotData %>% filter(CurrentHolder.x != 'ALP') %>% filter(mean2pp > 50) %>% arrange(min2pp)

# Share of electorates that fell within their 95% confidence interval
coverageRatio = sum((plotData$alp2pp.x < plotData$max2pp) * (plotData$alp2pp.x > plotData$min2pp) ) /
                  length( (plotData$alp2pp.x < plotData$max2pp) * (plotData$alp2pp.x > plotData$min2pp) )




