
library(dplyr)

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/SeatResults.csv',
            'ElectionResults/StateSwings.csv', 'ElectionData/HouseTcpFlowByStateByParty2013.csv',
            'ElectionData/HouseFirstPrefsByCandidateByVoteType2013.csv',
            '10')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
inputSwingsFile <- args[2]
tcpFlowFile <- args[3]
firstPrefsFile <- args[4]
nSeatSimulations <- as.numeric(args[5])


lnpPartyNames <- c('LP','NP','LNQ','LNP','CLP')

tcpData <- (tbl_df(read.csv(tcpFlowFile, skip=1)) %>%
              filter(FromPartyGroupAb != ""))   # Remove first preferences

# Summarise all Liberal-National parties under the 'LNP' label
toLNProws <- which(tcpData$ToPartyDisplayAb %in% lnpPartyNames)
tcpData$ToPartyDisplayAb[toLNProws] <- 'LNP'
fromLNProws <- which(tcpData$FromPartyGroupAb %in% lnpPartyNames)
tcpData$FromPartyGroupAb[fromLNProws] <- 'LNP'

tcpData$ToPartyDisplayAb[which(!(tcpData$ToPartyDisplayAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'
tcpData$FromPartyGroupAb[which(!(tcpData$FromPartyGroupAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'


firstPrefs <- tbl_df(read.csv(firstPrefsFile, skip=1))

stateSwings <- tbl_df(read.csv(inputSwingsFile))

output <- data.frame(Electorate = character(),
                     ALPWins = integer(), LNPWins = integer(), GRNWins = integer(), PUPWins = integer(),
                     OTHWins = integer(), Repetition = integer())



simulateOneElection <- function(swing, votesLastTime, preferenceFlow){
  # TODO: use residuals from previous election
  swing$Swing <- swing$Swing + 1.5*rnorm(nrow(swing))
  
  votes <- inner_join(votesLastTime, swing, by='Party') %>% mutate(PctLastTime = Vote/sum(Vote)*100,
                                                                   Pct = PctLastTime + Swing) %>%
    rowwise() %>% mutate(Pct = max(0, Pct)) %>% ungroup()
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
    thisSwing <- filter(theseSwings, Repetition == stateRep) %>% select(Party, Swing)
    luckyWinners <- c()
    for(seatRep in 1:nSeatSimulations){
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

print('Mean ALP seats won:')
print(sum(output$ALPWins)/(max(output$Repetition) * nSeatSimulations))

write.csv(output, file=outputFile, row.names=FALSE)












