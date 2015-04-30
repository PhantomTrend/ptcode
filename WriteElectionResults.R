
library(dplyr)

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/SeatResults.csv',
            'ElectionResults/StateSwings.csv', 'ElectionData/HouseTcpFlowByStateByParty2013.csv',
            'ElectionData/HouseFirstPrefsByCandidateByVoteType2013.csv',
            'ElectionData/HouseFirstPrefsByStateByParty2013.csv',
            'ElectionData/Incumbents.csv',
            '3')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
inputSwingsFile <- args[2]
tcpFlowFile <- args[3]
firstPrefsFile <- args[4]
stateSwingsLastTimeFile <- args[5]
incumbentFile <- args[6]
nSeatSimulations <- as.numeric(args[7])


incumbentData <- read.csv(incumbentFile, stringsAsFactors=FALSE)
lnpPartyNames <- c('LP','NP','LNQ','LNP','CLP')

tcpData <- (tbl_df(read.csv(tcpFlowFile, skip=1, stringsAsFactors=FALSE)) %>%
              filter(FromPartyGroupAb != ""))   # Remove first preferences

# Summarise all Liberal-National parties under the 'LNP' label
toLNProws <- which(tcpData$ToPartyDisplayAb %in% lnpPartyNames)
tcpData$ToPartyDisplayAb[toLNProws] <- 'LNP'
fromLNProws <- which(tcpData$FromPartyGroupAb %in% lnpPartyNames)
tcpData$FromPartyGroupAb[fromLNProws] <- 'LNP'

tcpData$ToPartyDisplayAb[which(!(tcpData$ToPartyDisplayAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'
tcpData$FromPartyGroupAb[which(!(tcpData$FromPartyGroupAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'


firstPrefs <- tbl_df(read.csv(firstPrefsFile, skip=1, stringsAsFactors=FALSE))

stateSwings <- tbl_df(read.csv(inputSwingsFile, stringsAsFactors=FALSE))

stateSwingsLastTime <- tbl_df(read.csv(stateSwingsLastTimeFile, skip=1, stringsAsFactors=FALSE))
stateSwingsLastTime$PartyAb[stateSwingsLastTime$PartyAb %in% lnpPartyNames] <- 'LNP'
stateSwingsLastTime$PartyAb[!(stateSwingsLastTime$PartyAb %in% c('LNP','ALP','GRN','PUP'))] <- 'OTH'
summaryStateSwingsLastTime <- stateSwingsLastTime %>%
  mutate(WeightedSwing = TotalSwing*TotalVotes) %>%
  group_by(StateAb, PartyAb) %>%
  summarise(Swing = sum(WeightedSwing)/sum(TotalVotes)) %>%
  ungroup()

seatSwingsLastTime <- firstPrefs %>% filter(CandidateID != 999)
seatSwingsLastTime$PartyAb[seatSwingsLastTime$PartyAb %in% lnpPartyNames] <- 'LNP'
seatSwingsLastTime$PartyAb[!(seatSwingsLastTime$PartyAb %in% c('ALP','LNP','GRN','PUP'))] <- 'OTH'
seatSwingsLastTime <- seatSwingsLastTime %>% mutate(WeightedSwing = TotalVotes*Swing) %>%
  group_by(StateAb, DivisionNm, PartyAb) %>% summarise(TotalSwing = sum(WeightedSwing)/sum(TotalVotes))  %>%
  ungroup() %>% left_join(summaryStateSwingsLastTime, by=c('StateAb', 'PartyAb')) %>%
  mutate(RelativeSwing = TotalSwing - Swing)


output <- data.frame(Electorate = character(),
                     ALPWins = integer(), LNPWins = integer(), GRNWins = integer(), PUPWins = integer(),
                     OTHWins = integer(), Repetition = integer())

simulateOneElection <- function(swing, votesLastTime, preferenceFlow){
  
  votes <- inner_join(votesLastTime, swing, by='Party') %>% mutate(PctLastTime = Vote/sum(Vote)*100,
                                                                   Pct = PctLastTime + Swing) %>%
    filter(Pct > 0)
  while(!any(votes$Pct > 50)){
    if(nrow(votes) == 2){
      break
    }
    smallestParty <- votes$Party[which(votes$Pct == min(votes$Pct))]
    prefs <- filter(preferenceFlow, ToPartyDisplayAb %in% votes$Party,
                    FromPartyGroupAb == smallestParty) %>% mutate(PctFlow = TotalVotes/sum(TotalVotes))
    for(toParty in prefs$ToPartyDisplayAb){
      votes$Pct[which(votes$Party == toParty)] <- votes$Pct[which(votes$Party == toParty)] +
        prefs$PctFlow[which(prefs$ToPartyDisplayAb == toParty)] * votes$Pct[which(votes$Party == smallestParty)]
    }
    votes <- votes[-which(votes$Party == smallestParty),]
  }
  return(votes$Party[which(votes$Pct == max(votes$Pct))])
}



lastStateName <- ''

for(electorateName in unique(firstPrefs$DivisionNm)){
  # Informals are recorded with BallotPosition 999.
  theseFirstPrefs <- filter(firstPrefs, DivisionNm == electorateName, BallotPosition != 999)
  thisState <- theseFirstPrefs$StateAb[1]
  
  if(thisState != lastStateName){
    print(thisState)
    lastStateName <- thisState
  }
  
  print(electorateName)
  
  getVotes <- function(partycode){
    partyRows <- which(theseFirstPrefs$PartyAb %in% partycode)
    if(length(partyRows) > 0){
      votes <- sum(theseFirstPrefs$TotalVotes[partyRows])
    }else{
      votes <- 0
    }
    return(votes)
  }
  alpVotes <- getVotes('ALP')
  lnpVotes <- getVotes(lnpPartyNames)
  grnVotes <- getVotes('GRN')
  pupVotes <- getVotes('PUP')
  othVotes <- sum(theseFirstPrefs$TotalVotes) - (alpVotes + lnpVotes + grnVotes + pupVotes)
  
  theseVotes <- tbl_df(data.frame(Party = c('ALP', 'LNP', 'GRN', 'PUP', 'OTH'),
                           Vote = c(alpVotes, lnpVotes, grnVotes, pupVotes, othVotes)))
  
  theseSwings <- stateSwings %>% filter( Electorate == thisState )
  
  summaryFlow <- tcpData %>%  filter(State == thisState) %>%
    group_by(ToPartyDisplayAb, FromPartyGroupAb) %>%
    summarise(TotalVotes = sum(TransferVotes)) %>% ungroup() %>%
    filter(ToPartyDisplayAb != FromPartyGroupAb)

  
  thisSeatOutput <- data.frame(Electorate = character(),
             ALPWins = integer(), LNPWins = integer(), GRNWins = integer(), PUPWins = integer(),
             OTHWins = integer(), Repetition = integer())
  for(stateRep in unique(theseSwings$Repetition)){
    thisStateSwing <- filter(theseSwings, Repetition == stateRep) %>% select(Party, Swing)
    
    luckyWinners <- c()
    for(seatRep in 1:nSeatSimulations){
      resampledSeat <- sample(unique(seatSwingsLastTime$DivisionNm),size=1)
      resampledSwing <- filter(seatSwingsLastTime, DivisionNm == resampledSeat) %>%
        select(PartyAb, RelativeSwing) %>% rename(Party = PartyAb)
      thisSwing <- inner_join(thisStateSwing, resampledSwing, by='Party') %>%
        mutate(Swing = Swing + RelativeSwing)
      luckyWinners <- c(luckyWinners, simulateOneElection(thisSwing, theseVotes, summaryFlow))
    }
    thisSeatOutput <- rbind(thisSeatOutput,
                            data.frame(Electorate = electorateName,
                                       ALPWins = sum(luckyWinners == 'ALP'),
                                       LNPWins = sum(luckyWinners == 'LNP'),
                                       GRNWins = sum(luckyWinners == 'GRN'),
                                       PUPWins = sum(luckyWinners == 'PUP'),
                                       OTHWins = sum(luckyWinners == 'OTH'), Repetition = stateRep))
  }
  output = rbind(output, thisSeatOutput)
}














