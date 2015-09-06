
# Input: a CSV with polling data, political parties in rows, states in columns
# Output: a Hadley-style dataset, one number per row

library(tidyr)
library(dplyr)

if(interactive()){
  args <- c('PollingData/StateDataLong.csv', 'PollingData/StateData.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFileName <- args[1]
outputDirectory <- dirname(outputFileName)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
inputFileName  <- args[2]

stateData <- tbl_df(read.csv(inputFileName, header=TRUE, stringsAsFactors=TRUE,
                                na.strings='#N/A'))
stateData$PollEndDate <- as.Date(stateData$PollEndDate, format='%d/%m/%y')

stateData <- stateData %>% gather(PollEndDate, Vote, NSW:WA)
colnames(stateData)[4] <- 'Electorate'
stateData$Electorate <- as.character(stateData$Electorate)

write.csv(stateData, outputFileName, row.names=FALSE)

