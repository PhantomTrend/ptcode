library(dplyr)

if(interactive()){
  args <- c('PollsForDb.csv', 'PollingData/MergedData.csv', 'PollingData/National2ppData.csv', 'PollingData/PollingURLs.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
mergedDataPath <- args[2]
twoPPpath <- args[3]
urlPath <- args[4]

longData <- tbl_df(read.csv(mergedDataPath, stringsAsFactors=FALSE))
longData$PollEndDate <- as.Date(longData$PollEndDate)

urlList <- tbl_df(read.csv(urlPath, stringsAsFactors=FALSE))
urlList$PollEndDate <- as.Date(urlList$PollEndDate, format="%d/%m/%y")
urlList$URL <- as.character(urlList$URL)

twoPPdata <- read.csv(twoPPpath, stringsAsFactors=FALSE)
twoPPdata$PollEndDate <- as.Date(twoPPdata$date, format="%d/%m/%y")

allData <- longData %>% rbind(twoPPdata %>% mutate(Party="ALP2pp", Vote=Labor2ppAUS, Electorate="AUS")
                              %>% select(PollEndDate, Pollster, Party, Vote, Electorate))

pollData <- allData %>% left_join(urlList, by=c('PollEndDate', 'Pollster'))

for(pollster in unique(pollData$Pollster)){
  pollData$Pollster[pollData$Pollster == pollster] <- gsub(" ", "", pollster)
}

write.csv(pollData, file=outputFile, row.names=FALSE)
