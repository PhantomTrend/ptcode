
# Use suppressMessages() to turn off garbage that clutters the console
suppressMessages(library(dplyr))
suppressMessages(library(readr))

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/Seats/Whitlam.csv',
            'ElectionResults/StateSwings.csv',
            'ElectionResults/TwoPartyPreferred.csv',
            'ElectionResults/PrimaryVotes.csv',
            'ElectionData/HouseTcpFlowByStateByParty2013.csv',
            'ElectionData/HouseFirstPrefsByCandidateByVoteType2013.csv',
            'ElectionData/HouseFirstPrefsByStateByParty2013.csv',
            'ElectionData/WA_Redistributed.csv',
            'ElectionData/NSW_Redistributed.csv',
            'Whitlam',
            '25')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
outputDirectory <- dirname(outputFile)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}

inputSwingsFile <- args[2]
tppTrendFile <- args[3]
primaryTrendFile <- args[4]
tcpFlowFile <- args[5]
firstPrefsFile <- args[6]
stateSwingsLastTimeFile <- args[7]
waRedistributionFile <- args[8]
nswRedistributionFile <- args[9]
electorateNameEscaped <- args[10]
nSeatSimulations <- as.numeric(args[11])


waElectorates <- c("Brand", "Burt", "Canning", "Cowan", "Curtin", "Durack", "Forrest", "Fremantle", "Hasluck", "Moore", "O_Connor", "Pearce", "Perth", "Stirling", "Swan", "Tangney")
nswElectorates <- c("Banks", "Barton", "Bennelong", "Berowra", "Blaxland", "Bradfield", "Calare", "Chifley", "Cook", "Cowper", "Cunningham", "Dobell", "Eden-Monaro", "Farrer", "Fowler", "Gilmore", "Grayndler", "Greenway", "Hughes", "Hume", "Hunter", "Kingsford_Smith", "Lindsay", "Lyne", "Macarthur", "Mackellar", "Macquarie", "McMahon", "Mitchell", "New_England", "Newcastle", "North_Sydney", "Page", "Parkes", "Parramatta", "Paterson", "Reid", "Richmond", "Riverina", "Robertson", "Shortland", "Sydney", "Warringah", "Watson", "Wentworth", "Werriwa", "Whitlam")

firstPrefs <- tbl_df(read.csv(firstPrefsFile, skip=1, stringsAsFactors=FALSE))

lnpPartyNames <- c('LP','NP','LNQ','LNP','CLP')

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

tcpData <- (tbl_df(read.csv(tcpFlowFile, skip=1, stringsAsFactors=FALSE)) %>%
              filter(FromPartyGroupAb != ""))   # Remove first preferences

# Summarise all Liberal-National parties under the 'LNP' label
toLNProws <- which(tcpData$ToPartyDisplayAb %in% lnpPartyNames)
tcpData$ToPartyDisplayAb[toLNProws] <- 'LNP'
fromLNProws <- which(tcpData$FromPartyGroupAb %in% lnpPartyNames)
tcpData$FromPartyGroupAb[fromLNProws] <- 'LNP'

tcpData$ToPartyDisplayAb[which(!(tcpData$ToPartyDisplayAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'
tcpData$FromPartyGroupAb[which(!(tcpData$FromPartyGroupAb %in% c('ALP', 'LNP', 'GRN', 'PUP')))] <- 'OTH'


electorateNames <- unique(firstPrefs$DivisionNm)
electorateNamesEscaped <- gsub("'","_",gsub(" ","_",electorateNames))
electorateName <- electorateNames[which(electorateNamesEscaped == electorateNameEscaped)]
if(length(electorateName)==0){
  # Renamed electorate
  electorateName <- electorateNameEscaped
}

stateSwings <- tbl_df(read.csv(inputSwingsFile, stringsAsFactors=FALSE))

if(electorateNameEscaped %in% nswElectorates){
  nswData <- read_csv(nswRedistributionFile, skip=1)
  alpVotes <- nswData %>% filter(Electorate == electorateNameEscaped) %>% .[["ALP"]]
  lnpVotes <- nswData %>% filter(Electorate == electorateNameEscaped) %>% .[["LNP"]]
  grnVotes <- nswData %>% filter(Electorate == electorateNameEscaped) %>% .[["GRN"]]
  
  # A quick approximation to PUP first preferences from 2013, using a statewide
  # average
  nswPupShareOfPupPlusOther <- 4.26/10.17
  pupOthVotes <- nswData %>% filter(Electorate == electorateNameEscaped) %>% .[["PUPOTH"]]
  pupVotes <- pupOthVotes * nswPupShareOfPupPlusOther
  othVotes <- pupOthVotes * (1-nswPupShareOfPupPlusOther)
  thisState <- "NSW"
}else if(electorateNameEscaped %in% waElectorates){
  waData <- read_csv(waRedistributionFile, skip=1)
  alpVotes <- waData %>% filter(Electorate==electorateNameEscaped) %>% .[["ALP"]]
  lnpVotes <- waData %>% filter(Electorate==electorateNameEscaped) %>% .[["LNP"]]
  grnVotes <- waData %>% filter(Electorate==electorateNameEscaped) %>% .[["GRN"]]
  pupVotes <- waData %>% filter(Electorate==electorateNameEscaped) %>% .[["PUP"]]
  othVotes <- waData %>% filter(Electorate==electorateNameEscaped) %>% .[["OTH"]]
  thisState <- "WA"
}else{
  # Informals are recorded with BallotPosition 999.
  theseFirstPrefs <- filter(firstPrefs, DivisionNm == electorateName, BallotPosition != 999)
  thisState <- theseFirstPrefs$StateAb[1]
  if(thisState == "NSW" || thisState == "WA"){
    stop("Must use redistributed boundaries for seat " + electorateNameEscaped)
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
}

# We can't track Independent/Other polls very well, so we can reduce variance in the resampled
# residuals by dropping seats where independents/others were a big factor.
unusualSeats <- c("Lyne","New England","Fairfax","Indi","Denison","Kennedy")
seatSwingsLastTime <- seatSwingsLastTime %>% filter(!DivisionNm %in% unusualSeats)

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
  grn2ppRow <- which(votes$Party == "GRN")
  if(length(alp2ppRow) == 0){
    if(length(lnp2ppRow) == 0 || length(grn2ppRow)==0){
      alp2pp <- NA
      lnp2pp <- NA
      grn2pp <- NA
    }else{
      grn2pp <- votes[grn2ppRow,]$Pct / sum(votes$Pct) * 100
      lnp2pp <- votes[lnp2ppRow,]$Pct / sum(votes$Pct) * 100
      alp2pp <- NA
    }
  }else if(length(lnp2ppRow) == 0) {
    if(length(grn2ppRow)==0){
      alp2pp <- NA
      lnp2pp <- NA
      grn2pp <- NA
    }else{
      grn2pp <- votes[grn2ppRow,]$Pct / sum(votes$Pct) * 100
      alp2pp <- votes[alp2ppRow,]$Pct / sum(votes$Pct) * 100
      lnp2pp <- NA
    }
  }else{
    alp2pp <- votes[alp2ppRow,]$Pct / sum(votes$Pct) *100
    lnp2pp <- votes[lnp2ppRow,]$Pct / sum(votes$Pct) *100
    grn2pp <- NA
  }
  return(data.frame(Winner = winner, ALP = alpPrimary, LNP = lnpPrimary, GRN = grnPrimary,
                    PUP = pupPrimary, OTH = othPrimary, ALP2PP = alp2pp, LNP2PP = lnp2pp, GRN2PP = grn2pp))
}


theseVotes <- tbl_df(data.frame(Party = c('ALP', 'LNP', 'GRN', 'PUP', 'OTH'),
                                Vote = c(alpVotes, lnpVotes, grnVotes, pupVotes, othVotes)))
theseVotes$Party <- as.character(theseVotes$Party)

theseSwings <- stateSwings %>% filter( Electorate == thisState )

summaryFlow <- tcpData %>%  filter(State == thisState) %>%
  group_by(ToPartyDisplayAb, FromPartyGroupAb) %>%
  summarise(TotalVotes = sum(TransferVotes)) %>% ungroup() %>%
  filter(ToPartyDisplayAb != FromPartyGroupAb)

# Temporary patch for GRN preference flows, based on:
# Melbourne: http://results.aec.gov.au/17496/Website/HouseDivisionDop-17496-228.htm
# Batman: http://results.aec.gov.au/17496/Website/HouseDivisionDop-17496-199.htm
# Sydney: http://results.aec.gov.au/17496/Website/HouseDivisionDop-17496-149.htm
# Grayndler: http://results.aec.gov.au/17496/Website/HouseDivisionDop-17496-121.htm
if(!any(summaryFlow$ToPartyDisplayAb=="GRN" & summaryFlow$FromPartyGroupAb=="ALP")){
  alpToLnpFlow <-  summaryFlow[which(summaryFlow$ToPartyDisplayAb=="LNP" & summaryFlow$FromPartyGroupAb=="ALP"),'TotalVotes']
  if(nrow(alpToLnpFlow)> 0){
    summaryFlow <- rbind(summaryFlow, data.frame(ToPartyDisplayAb="GRN",FromPartyGroupAb="ALP",
                                               TotalVotes=6 *alpToLnpFlow))
  }
}
if(!any(summaryFlow$ToPartyDisplayAb=="GRN" & summaryFlow$FromPartyGroupAb=="LNP")){
  lnpToAlpFlow <- summaryFlow[which(summaryFlow$ToPartyDisplayAb=="ALP" & summaryFlow$FromPartyGroupAb=="LNP"),'TotalVotes']
  if(nrow(lnpToAlpFlow)>0){
    summaryFlow <- rbind(summaryFlow, data.frame(ToPartyDisplayAb="GRN",FromPartyGroupAb="LNP",
      TotalVotes=1/2 * lnpToAlpFlow))
  }
}
if(!any(summaryFlow$ToPartyDisplayAb=="GRN" & summaryFlow$FromPartyGroupAb=="OTH")){
  summaryFlow <- rbind(summaryFlow, data.frame(ToPartyDisplayAb="GRN",FromPartyGroupAb="OTH",
   TotalVotes= summaryFlow[which(summaryFlow$ToPartyDisplayAb=="ALP" & summaryFlow$FromPartyGroupAb=="OTH"),'TotalVotes']))
}

thisSeatOutput <- data.frame(Electorate = character(),
                             ALPWins = integer(), LNPWins = integer(), GRNWins = integer(), PUPWins = integer(),
                             OTHWins = integer(), Repetition = integer())
for(stateRep in unique(theseSwings$Repetition)){
  thisStateSwing <- filter(theseSwings, Repetition == stateRep) %>% select(Party, Swing)
  
  seatSims <- data.frame(ALPPrimary = numeric(), LNPPrimary = numeric(), GRNPrimary = numeric(),
                         PUPPrimary = numeric(), OTHPrimary = numeric(),
                         ALP2PP = numeric(), LNP2PP = numeric(), GRN2PP = numeric(), ResampledSeat = character())
  luckyWinners <- c()
  for(seatRep in 1:nSeatSimulations){
    resampledSeat <- sample(unique(seatSwingsLastTime$DivisionNm),size=1)
    resampledSwing <- filter(seatSwingsLastTime, DivisionNm == resampledSeat) %>%
      select(PartyAb, RelativeSwing) %>% rename(Party = PartyAb)
    thisSwing <- inner_join(thisStateSwing, resampledSwing, by='Party') %>%
      mutate(Swing = Swing + RelativeSwing)
    thisSim <- simulateOneElection(thisSwing, theseVotes, summaryFlow)
    seatSims <- rbind(seatSims, thisSim[names(thisSim) != "Winner"] %>% mutate(ResampledSeat=resampledSeat))
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
                                     ALP2PP = mean(seatSims$ALP2PP, na.rm = TRUE),
                                     LNP2PP = mean(seatSims$LNP2PP, na.rm = TRUE),
                                     GRN2PP = mean(seatSims$GRN2PP, na.rm = TRUE),
                                     Repetition = stateRep))
}

write_csv(thisSeatOutput, outputFile)
