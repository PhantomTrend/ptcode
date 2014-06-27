

source('LoadPollsAndElections.R')

dataDir = 'Data'
startDate = as.Date('2003-01-01')

dataList = loadPollsAndElections(dataDir, startDate)
endDate = as.Date(end(dataList$data))

# Prepare a time series matrix for input to the state-space model,
# with one day per row
Y = as.ts(dataList$data)
# Remove national 2pp readings when state-based ones are available to avoid
# stochastic singularity, given that we're using approximate population weights.
Y[which(!is.na(Y[,2])),1] = NA


source('NationalPollingModel.R')
estimatedModel = fitNationalPollingModel(Y, as.ts(dataList$pollster), verbose=TRUE, reestimateFromScratch=FALSE)

source('PrintedOutput.R')
printCurrentEstimates(estimatedModel)


source('GeneratePlots.R')
longTermPlot = plotNationalTrend(estimatedModel, dataList$data, dataList$pollster,
                  plotStartDate=as.Date('2003-01-01'), plotEndDate=as.Date('2014-12-31'),
                  outputFileName='historical2pp.png', showPollsters=TRUE, alpPovPlot=(runif(1) < 0.5))
currentPlot = plotNationalTrend(estimatedModel, dataList$data, dataList$pollster,
                  plotStartDate=as.Date('2013-01-01'), plotEndDate=as.Date('2014-08-01'),
                  outputFileName='national2pp.png', showPollsters=FALSE, alpPovPlot=(runif(1) < 0.5))
print(longTermPlot)
print(currentPlot)



currentPendulum = tbl_df(read.csv('Data/tpp_by_division_2013.csv', skip=1)) %>%
                mutate(DivisionName=DivisionNm, State=StateAb, CurrentHolder = PartyAb,
                       alp2pp = Australian.Labor.Party.Percentage) %>%
                select(DivisionName, State, CurrentHolder, alp2pp)
levels(currentPendulum$CurrentHolder) = c(levels(currentPendulum$CurrentHolder), 'Greens', 'Indep', 'PUP', 'KAP')

currentPendulum[which(currentPendulum$DivisionName=='Melbourne'),'CurrentHolder'] = 'Greens'
currentPendulum[which(currentPendulum$DivisionName=='Fairfax'),'CurrentHolder'] = 'PUP'
currentPendulum[which(currentPendulum$DivisionName=='Kennedy'),'CurrentHolder'] = 'KAP'
currentPendulum[which(currentPendulum$DivisionName=='Indi'),'CurrentHolder'] = 'Indep'
currentPendulum[which(currentPendulum$DivisionName=='Denison'),'CurrentHolder'] = 'Indep'

assumptions = list()
assumptions['Melbourne'] = 'Greens'
assumptions['Fairfax'] = 'PUP'
assumptions['Kennedy'] = 'KAP'
assumptions['Indi'] = 'Indep'
assumptions['Denison'] = 'Indep'


source('SimulateElection.R')

stateReps = 800
seatReps = 100
set.seed(31337)
state2pp = estimatedModel$a[nrow(estimatedModel$a),]
state2ppCovariance = estimatedModel$P[,,nrow(estimatedModel$a)]
electionOutcomes = simulateElection(state2pp, state2ppCovariance, currentPendulum, assumptions, stateReps,
                        seatReps, dataDir, previousElection=2013, useRedistributions=FALSE)

alpTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='ALP')})
lnpTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='LNP')})
indepTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='Indep')})
greensTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='Greens')})
pupTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='PUP')})
kapTotals = apply(electionOutcomes$winner, 2, function(x){sum(x=='KAP')})

otherTotals = indepTotals + greensTotals + pupTotals + kapTotals


results = tbl_df(data.frame(govtTotals = lnpTotals, otherTotals = otherTotals))

resultData = group_by(results,govtTotals) %>% summarise(n()) %>% arrange(govtTotals)
resultData$prob = resultData[,2]/sum(resultData[,2]) *100

resultData$outcome = 'Hung'
resultData[which(resultData$govtTotals > 76),'outcome'] = 'LNP'
resultData[which(resultData$govtTotals < 71),'outcome'] = 'ALP'
print(summarise(resultData %>% group_by(outcome), sum(prob)))
xMin = 40
xMax = 90
histogramData = filter(resultData, govtTotals >= xMin ) %>% filter(govtTotals < xMax)
histogramData$outcomeColour = 'grey'
histogramData[which(histogramData$outcome=='LNP'),'outcomeColour'] = 'blue'
histogramData[which(histogramData$outcome=='ALP'),'outcomeColour'] = 'red'

electionSummaryPlot = ggplot(histogramData) +
  geom_bar(aes(x=govtTotals,y=prob),fill=histogramData$outcomeColour,stat='Identity') +
  labs(x='Government Seats', y='Probability (%)') +
  ggtitle('Simulated Election Outcomes')
print(electionSummaryPlot)

ggsave('electionSummary.png')



alpProb = apply(electionOutcomes$winner, 1, function(x){sum(x=='ALP')/(stateReps*seatReps)*100})
meanAlpVote = apply(electionOutcomes$alp2pp, 1, mean)
newPendulum = cbind(currentPendulum, alpProb, meanAlpVote)

alpLosses = filter(newPendulum, CurrentHolder=='ALP') %>% filter(alpProb < 50)
alpGains = filter(newPendulum, CurrentHolder!='ALP') %>% filter(alpProb > 50) %>% arrange(alpProb)














