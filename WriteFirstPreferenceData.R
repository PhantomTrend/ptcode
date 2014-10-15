
library('dplyr')
library('assertthat')

if(interactive()){
  args <- c('ElectionData/FirstPrefs.csv', 'ElectionData/HouseFirstPrefsByStateByParty2013.csv',
            'ElectionData/HouseFirstPrefsByStateByParty2010.csv',
            'ElectionData/HouseFirstPrefsByStateByParty2007.csv',
            'ElectionData/HouseFirstPrefsByStateByParty2004.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFileName <- args[1]
input2013  <- args[2]
input2010  <- args[3]
input2007  <- args[4]
input2004  <- args[5]


summariseFirstPrefsByState <- function(df){
  alpVotes <- sum(df %>% filter(PartyAb=='ALP') %>% select(TotalVotes))
  lnpVotes <- sum(df %>% filter(PartyAb=='LP' | PartyAb=='NP' | PartyAb == 'LNQ' |
                                  PartyAb=='LNP' | PartyAb=='CLP') %>% select(TotalVotes))
  grnVotes <- sum(df %>% filter(PartyAb=='GRN') %>% select(TotalVotes))
  if(any(df$PartyAb=='PUP')){
    pupVotes <- sum(df %>% filter(PartyAb=='PUP') %>% select(TotalVotes))
  }else{
    pupVotes <- 0
  }
  othVotes <- sum(df %>% filter(PartyAb != 'ALP', PartyAb != 'LP', PartyAb != 'LNP', PartyAb != 'CLP', PartyAb != 'LNQ',
                                PartyAb != 'NP', PartyAb != 'GRN', PartyAb != 'PUP') %>% select(TotalVotes))
  totalVotes <- sum(df %>% select(TotalVotes))
  invisible(assert_that( alpVotes + lnpVotes + grnVotes + pupVotes + othVotes == totalVotes ))
  
  output <- data.frame(Electorate = df$StateAb[1],
                       Party = c('ALP', 'LNP', 'GRN', 'PUP', 'OTH'),
                       Vote = c(alpVotes/totalVotes*100, lnpVotes/totalVotes*100,
                                grnVotes/totalVotes*100, pupVotes/totalVotes*100, othVotes/totalVotes*100 ))
  return(output)
}




housePrefs2013 <- tbl_df(read.csv(input2013, skip=1))
housePrefs2010 <- tbl_df(read.csv(input2010, skip=1))
housePrefs2007 <- tbl_df(read.csv(input2007, skip=1))
housePrefs2004 <- tbl_df(read.csv(input2004, skip=1))

summary2013 <- (housePrefs2013 %>% group_by(StateAb) %>%
                  do(summariseFirstPrefsByState(.)) %>% ungroup() %>% select(-StateAb))
summary2013$PollEndDate <- '2013-09-07'
summary2010 <- (housePrefs2010 %>% group_by(StateAb) %>%
                  do(summariseFirstPrefsByState(.)) %>% ungroup() %>% select(-StateAb))
summary2010$PollEndDate <- '2010-08-21'
summary2007 <- (housePrefs2007 %>% group_by(StateAb) %>%
                  do(summariseFirstPrefsByState(.)) %>% ungroup() %>% select(-StateAb))
summary2007$PollEndDate <- '2007-11-24'
summary2004 <- (housePrefs2004 %>% group_by(StateAb) %>%
                  do(summariseFirstPrefsByState(.)) %>% ungroup() %>% select(-StateAb))
summary2004$PollEndDate <- '2004-10-09'


completeSummary <- rbind(summary2013, summary2010, summary2007, summary2004)
completeSummary$Pollster <- 'Election'


write.csv(completeSummary, outputFileName, row.names=FALSE)

