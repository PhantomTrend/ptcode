library(dplyr)

if(interactive()){
  args <- c('PlotData/.sentinel', 'FittedModel.RData', '2013-01-01')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputSentinel <- args[1]
outputDirectory <- dirname(outputSentinel)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
inputModelFile <- args[2]

startDateForInitialPlot <- as.Date(args[3])

load(inputModelFile)

electorateNames <- unique(modelOutput$Electorate)
readableElectorateNames <- list(AUS = 'Australia',
                                NSW = 'New South Wales',
                                VIC = 'Victoria',
                                SA = 'South Australia',
                                WA = 'Western Australia',
                                TAS = 'Tasmania',
                                QLD = 'Queensland',
                                NT = 'Northern Territory',
                                ACT = 'ACT')
readablePartyNames <- list(ALP = 'ALP',
                           LNP = 'Coalition',
                           GRN = 'Greens',
                           PUP = 'Palmer United',
                           OTH = 'Others',
                           PUPOTH = 'Palmer + Others',
                           GRNOTH = 'Greens + Others')

for(shortPartyName in names(readablePartyNames)){
  modelOutput$Party[modelOutput$Party == shortPartyName] <- readablePartyNames[[shortPartyName]]
  modelData$Party[modelData$Party == shortPartyName] <- readablePartyNames[[shortPartyName]]
}

for(shortElectorateName in names(readableElectorateNames)){
  modelOutput$Electorate[modelOutput$Electorate == shortElectorateName] <- readableElectorateNames[[shortElectorateName]]
  modelData$Electorate[modelData$Electorate == shortElectorateName] <- readableElectorateNames[[shortElectorateName]]
}

initialModelPlotData <- modelOutput %>% filter(PollEndDate >= startDateForInitialPlot)
initialPollPlotData <- modelData %>% filter(PollEndDate >= startDateForInitialPlot)

write.csv(initialModelPlotData, file=paste(outputDirectory, 'ShortModelOutput.csv', sep='/'), row.names=FALSE)
write.csv(initialPollPlotData, file=paste(outputDirectory, 'ShortPollData.csv', sep='/'), row.names=FALSE)
write.csv(modelData, file=paste(outputDirectory, 'PollData.csv', sep='/'), row.names=FALSE)
write.csv(modelOutput, file=paste(outputDirectory, 'ModelOutput.csv', sep='/'), row.names=FALSE)


emptyOutput <- data.frame()
write.table(emptyOutput, file=outputSentinel, col.names=FALSE)



