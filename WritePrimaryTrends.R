library(dplyr)

if(interactive()){
  args <- c('ElectionResults/PrimaryVotes.csv', 'FittedModel.RData')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
inputModelFile <- args[2]


load(inputModelFile)

stdDevs <- modelOutput %>% filter(Pollster == "SmoothedOneStdDevWidth") %>%
  mutate(OneSd = Vote) %>%
  select(RowNumber, Party, Electorate, OneSd)

inputForDb <- modelOutput %>% filter(Pollster == "Smoothed") %>%
  select(RowNumber, Party, Electorate, PollEndDate, Vote) %>%
  inner_join(stdDevs, by=c("RowNumber","Party","Electorate")) %>%
  select(-RowNumber)


write.csv(inputForDb, file=outputFile, row.names=FALSE)





