
library(tidyr)
library(dplyr)

if(interactive()){
  args <- c('PollingData/MergedData.csv', 'PollingData/NationalDataLong.csv', 'PollingData/StateDataLong.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

nationalData <- tbl_df(read.csv(args[2]))
nationalData$PollEndDate <- as.Date(nationalData$PollEndDate)

stateData <- tbl_df(read.csv(args[3]))
stateData$PollEndDate <- as.Date(stateData$PollEndDate)
stateData$Vote[which(stateData$Vote == '<0.5')] <- "0.2"
stateData$Vote <- as.numeric(stateData$Vote)

fixMinorParties <- function(x){
  if(any(x$Party == 'FFP')){
    # We don't track Family First; merge it with Other
    x[which(x$Party=='OTH'),'Vote'] = x[which(x$Party=='FFP'),'Vote'] + x[which(x$Party=='OTH'),'Vote']
    x <- x[-which(x$Party == 'FFP'),]
  }
  if(any(x$Party == 'IND')){
    # We don't track Independent; merge it with Other
    x[which(x$Party=='OTH'),'Vote'] = x[which(x$Party=='IND'),'Vote'] + x[which(x$Party=='OTH'),'Vote']
    x <- x[-which(x$Party == 'IND'),]
  }
  if(!any(x$Party == 'PUP')){
    if(x$PollEndDate[1] >= as.Date('2013-05-01')){
      # This poll is after PUP's foundation, but doesn't report it separately
      x$Vote[which(x$Party=='PUPOTH')] = x$Vote[which(x$Party=='OTH')]
      x <- x[-which(x$Party=='OTH')]
    }else{
      # PUP didn't exist; mark it zero to ensure the model doesn't try to
      # infer a vote for it
      x <- rbind(x, data.frame(PollEndDate = x$PollEndDate[1],
                               Pollster = x$Pollster[1],
                               Party = 'PUP',
                               Electorate = unique(x$Electorate),
                               Vote = 0))
    }
  }
  voteTotal <- sum(na.omit(x$Vote))
  if(abs(voteTotal-100) > 2){
    badness <- data.frame(VoteTotal = voteTotal,
                          Pollster = x$Pollster[1],
                          Electorate = x$Electorate[1],
                          PollEndDate = x$PollEndDate[1])
  }else{
    badness <- data.frame(VoteTotal = 0, Pollster="x", Electorate="x",
                          PollEndDate = as.Date("1900-01-01"))
    badness <- badness[-1,]
  }
  return(badness)
}

stateDataNew <- stateData %>% group_by(PollEndDate, Electorate, Pollster) %>% do(fixMinorParties(.)) %>% ungroup()
stateDataNew %>% arrange(desc(PollEndDate))
