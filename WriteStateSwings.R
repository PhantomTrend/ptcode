library(dplyr)
library(assertthat)
set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/StateSwings.csv', 'FittedModel.RData', 'ElectionData/FirstPrefs.csv',
            '10', '2013-09-07')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
outputDirectory <- dirname(outputFile)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
load(args[2])
lastElectionDate <- as.Date(args[5])
firstPrefs <- tbl_df(read.csv(args[3])) %>% mutate(PollEndDate == as.Date(PollEndDate)) %>% filter(PollEndDate == lastElectionDate)
nRepetitions <- as.numeric(args[4])

choleskyOfLatentSwings <- chol(finalPeriodCovariance)

nObservations <- dim(smoothedModel$alphahat)[1]
nLatentStates <- length(latentComponentNamesBase)
finalPeriodState <- smoothedModel$alphahat[nObservations,1:nLatentStates]

nParties <- length(unique(latentPartyNames))
nStates <- length(unique(latentStateNames))

votesLastTime <- c()
for(i in 1:nLatentStates){
  votesLastTime <- c(votesLastTime,
                     firstPrefs$Vote[which(firstPrefs$Electorate == latentStateNames[i] &
                                             firstPrefs$Party == latentPartyNames[i])])
}
invisible(assert_that(length(votesLastTime) == nLatentStates))

output <- data.frame( Electorate = character(), Party = character(), Vote = numeric(),
                      Swing = numeric(), Repetition = integer() )
for(repI in 1:nRepetitions){
  theseVotes <- as.vector( finalPeriodState + t(choleskyOfLatentSwings) %*% rnorm(nLatentStates) )
  output <- rbind(output,
                  data.frame(Electorate = latentStateNames,
                             Party = latentPartyNames,
                             Vote = theseVotes,
                             Swing = theseVotes - votesLastTime,
                             Repetition = repI))
}

write.csv(output, file=outputFile, row.names=FALSE)


