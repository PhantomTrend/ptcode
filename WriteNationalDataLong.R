
# Input: a CSV with polling data, political parties in columns
# Output: a Hadley-style dataset, one number per row

library(tidyr)
library(dplyr)

if(interactive()){
  args <- c('PollingData/NationalDataLong.csv', 'PollingData/NationalData.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFileName <- args[1]
inputFileName  <- args[2]

nationalData <- tbl_df(read.csv(inputFileName, header=TRUE, stringsAsFactors=TRUE,
                         na.strings='#N/A'))

nationalData$PollEndDate <- as.Date(nationalData$PollEndDate, format='%d/%m/%Y')

nationalData <- nationalData %>% gather(PollEndDate, Vote, -(PollEndDate:Pollster))
colnames(nationalData)[3] <- 'Party'

nationalData <- mutate(nationalData, Electorate = 'AUS')

write.csv(nationalData, outputFileName, row.names=FALSE)

