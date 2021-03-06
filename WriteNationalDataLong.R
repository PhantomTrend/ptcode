
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
outputDirectory <- dirname(outputFileName)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
inputFileName  <- args[2]

nationalData <- tbl_df(read.csv(inputFileName, header=TRUE, stringsAsFactors=TRUE,
                         na.strings='#N/A'))

nationalData$PollEndDate <- as.Date(nationalData$PollEndDate, format='%d/%m/%y')

nationalDataTidy <- nationalData %>% gather(PollEndDate, Vote, -(PollEndDate:Pollster))
colnames(nationalDataTidy)[3] <- 'Party'

nationalDataWithElectorate <- mutate(nationalDataTidy, Electorate = 'AUS')

write.csv(nationalDataWithElectorate, outputFileName, row.names=FALSE)

