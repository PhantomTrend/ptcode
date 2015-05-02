
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
                     OTHWins = integer(),
                     ALPPrimary = numeric(), LNPPrimary = numeric(), GRNPrimary = numeric(),
                     PUPPrimary = numeric(), OTHPrimary = numeric(),
                     ALP2PP = numeric(), LNP2PP = numeric(),
                     Repetition = integer())

simulateOneElection <- function(swing, votesLastTime, preferenceFlow){
  primaryVotes <- inner_join(votesLastTime, swing, by='Party') %>% mutate(PctLastTime = Vote/sum(Vote)*100,
                                                                   Pct = PctLastTime + Swing) %>%
    filter(Pct > 0)
  primaryVotes$Party <- as.character(primaryVotes$Party)
  votes <- primaryVotes
  while(nrow(votes) > 2){
    smallestParty <- votes$Party[which(votes$Pct == min(votes$Pct))[1]]
    prefs <- filter(preferenceFlow, ToPartyDisplayAb %in% votes$Party,
                    FromPartyGroupAb == smallestParty) %>% mutate(PctFlow = TotalVotes/sum(TotalVotes))
    for(toParty in prefs$ToPartyDisplayAb){
      votes$Pct[which(votes$Party == toParty)] <- votes$Pct[which(votes$Party == toParty)] +
        prefs$PctFlow[which(prefs$ToPartyDisplayAb == toParty)] * votes$Pct[which(votes$Party == smallestParty)]
    }
    votes <- votes[-which(votes$Party == smallestParty),]
  }
  winner <- votes$Party[which(votes$Pct == max(votes$Pct))]
  getPrimaryVote <- function(partyName){
    v = (primaryVotes %>% filter(Party == partyName))$Pct
    if(length(v) == 0){
      v = 0
    }
    return(v)
  }
  alpPrimary <- getPrimaryVote("ALP")
  lnpPrimary <- getPrimaryVote("LNP")
  grnPrimary <- getPrimaryVote("GRN")
  pupPrimary <- getPrimaryVote("PUP")
  othPrimary <- getPrimaryVote("OTH")
  alp2ppRow <- which(votes$Party == "ALP")
  lnp2ppRow <- which(votes$Party == "LNP")
  if(length(alp2ppRow) == 0 || length(lnp2ppRow) == 0){
    alp2pp <- NA
    lnp2pp <- NA
  }else{
    alp2pp <- votes[alp2ppRow,]$Pct / sum(votes$Pct) *100
    lnp2pp <- votes[lnp2ppRow,]$Pct / sum(votes$Pct) *100
  }
  return(data.frame(Winner = winner, ALP = alpPrimary, LNP = lnpPrimary, GRN = grnPrimary,
                    PUP = pupPrimary, OTH = othPrimary, ALP2PP = alp2pp, LNP2PP = lnp2pp))
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
  theseVotes$Pary <- as.character(theseVotes$Party)
  
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
    
    seatSims <- data.frame(ALPPrimary = numeric(), LNPPrimary = numeric(), GRNPrimary = numeric(),
                           PUPPrimary = numeric(), OTHPrimary = numeric(),
                           ALP2PP = numeric(), LNP2PP = numeric())
    luckyWinners <- c()
    for(seatRep in 1:nSeatSimulations){
      resampledSeat <- sample(unique(seatSwingsLastTime$DivisionNm),size=1)
      resampledSwing <- filter(seatSwingsLastTime, DivisionNm == resampledSeat) %>%
        select(PartyAb, RelativeSwing) %>% rename(Party = PartyAb)
      thisSwing <- inner_join(thisStateSwing, resampledSwing, by='Party') %>%
        mutate(Swing = Swing + RelativeSwing)
      thisSim <- simulateOneElection(thisSwing, theseVotes, summaryFlow)
      seatSims <- rbind(seatSims, thisSim[names(thisSim) != "Winner"])
      luckyWinners <- c(luckyWinners, as.character(thisSim$Winner))
    }
    thisSeatOutput <- rbind(thisSeatOutput,
                            data.frame(Electorate = electorateName,
                                       ALPWins = sum(luckyWinners == 'ALP'),
                                       LNPWins = sum(luckyWinners == 'LNP'),
                                       GRNWins = sum(luckyWinners == 'GRN'),
                                       PUPWins = sum(luckyWinners == 'PUP'),
                                       OTHWins = sum(luckyWinners == 'OTH'),
                                       ALPPrimary = mean(seatSims$ALP),
                                       LNPPrimary = mean(seatSims$LNP),
                                       GRNPrimary = mean(seatSims$GRN),
                                       PUPPrimary = mean(seatSims$PUP),
                                       OTHPrimary = mean(seatSims$OTH),
                                       ALP2PP = mean(seatSims$ALP2PP),
                                       LNP2PP = mean(seatSims$LNP2PP),
                                       Repetition = stateRep))
  }
  output = rbind(output, thisSeatOutput)
}


nRepetitions <- max(stateSwings$Repetition)

getWinPct <- function(wins){ sum(wins)/(nSeatSimulations*nRepetitions)*100  }
outcomeSummary <- output %>% group_by(Electorate) %>%
  summarise(ALPWinPct = getWinPct(ALPWins),
            LNPWinPct = getWinPct(LNPWins),
            GRNWinPct = getWinPct(GRNWins),
            PUPWinPct = getWinPct(PUPWins),
            OTHWinPct = getWinPct(OTHWins),
            ALPPrimary = mean(na.omit(ALPPrimary)),
            LNPPrimary = mean(na.omit(LNPPrimary)),
            GRNPrimary = mean(na.omit(GRNPrimary)),
            PUPPrimary = mean(na.omit(PUPPrimary)),
            OTHPrimary = mean(na.omit(OTHPrimary)),
            ALP2PP = mean(na.omit(ALP2PP)),
            LNP2PP = mean(na.omit(LNP2PP))) %>%
  mutate(Electorate = as.character(Electorate))
            
incumbentData$Party[incumbentData$Party=="Liberal"] <- "LNP"
incumbentData$Party[incumbentData$Party=="National"] <- "LNP"
incumbentData$Party[incumbentData$Party=="Independent"] <- "OTH"
incumbentData$Party[incumbentData$Party=="Palmer United"] <- "PUP"
incumbentData$Party[incumbentData$Party=="Katter's Australian Party"] <- "OTH"
incumbentData$Party[incumbentData$Party=="Greens"] <- "GRN"
incumbentData$Party[incumbentData$Party=="CLP"] <- "LNP"
incumbentData$Party[incumbentData$Party=="Labor"] <- "ALP"
incumbentData <- incumbentData %>% rename(IncumbentParty = Party)
summariseResult <- function(summaryRow){
  if(summaryRow$IncumbentParty == "OTH"){
    return (data.frame(Electorate = summaryRow$Electorate,
                      Winner = "OTH",
                      Description = "Assumed OTH win"))
  }
  winningPcts <- c(summaryRow$ALPWinPct, summaryRow$LNPWinPct, summaryRow$GRNWinPct,
                   summaryRow$PUPWinPct, summaryRow$OTHWinPct)
  maxWinPct <- max(winningPcts)
  parties <- c("ALP","LNP","GRN","PUP","OTH")
  winner <- parties[winningPcts == maxWinPct][1]
  type <- ifelse(winner == summaryRow$IncumbentParty, "retain", "win")
  margin <- "Marginal"
  if(maxWinPct > 60){
    margin <- "Likely"
  }
  if(maxWinPct > 80){
    margin <- "Easy"
  }
  return(data.frame(Electorate = summaryRow$Electorate,
                    Winner = winner,
                    Description = paste(margin, winner, type)))
}

lowerHouseWinners <- outcomeSummary %>% inner_join(incumbentData, by="Electorate") %>%
  rowwise() %>% do(summariseResult(.)) %>% ungroup()

finalOutput <- outcomeSummary %>% inner_join(incumbentData, by="Electorate") %>%
  inner_join(lowerHouseWinners, by="Electorate")

write.csv(finalOutput, file=outputFile, row.names=FALSE)








