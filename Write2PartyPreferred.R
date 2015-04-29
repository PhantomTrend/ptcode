
library(dplyr)
library(assertthat)

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/TwoPartyPreferred.csv',
            'FittedModel.RData', 'ElectionData/HouseTppFlowByStateByParty2013.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
inputModel <- args[2]
tppFlowFile <- args[3]



tppData <- (tbl_df(read.csv(tppFlowFile, skip=1, stringsAsFactors=FALSE)) %>%
              filter(PartyAb != ""))   # Remove first preferences

lnpPartyNames <- c('LP','NP','LNQ','LNP','CLP')
tppData$PartyAb[which(!(tppData$PartyAb %in% c('ALP', 'GRN', 'PUP', lnpPartyNames)))] <- 'OTH'

tppData <- filter(tppData, PartyAb %in% c('GRN','PUP','OTH')) %>% group_by(StateAb, PartyAb) %>%
  summarise(ALPTotal = sum(Australian.Labor.Party.Transfer.Votes),
            TotalVotes = sum(Australian.Labor.Party.Transfer.Votes) + 
              sum(Liberal.National.Coalition.Transfer.Votes)) %>%
  mutate(ALP2ppShare = ALPTotal/TotalVotes)


# Load data on state populations
# Copy-pasted from WriteFittedModel.R, ugh
electoralRoll2010 = c(4552976,3506844, 2684538, 1099031, 1341005, 356203, 118401, 242842)
electoralRollAug2013 = c(4816991, 3715925, 2840091, 1130388, 1452272, 362892, 128971, 265269)
popweights = apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean) /
  sum(apply(cbind(electoralRoll2010, electoralRollAug2013), 1, mean))
stateNames <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
names(popweights) <- stateNames


load(inputModel)
obsDates <- sort(unique(modelOutput$PollEndDate))
nObservations <- length(obsDates)


pupStartRow <- (modelData %>% filter(Party=='PUP', !is.na(Vote)) %>% arrange(RowNumber))$RowNumber[1]


output <- data.frame(PollEndDate = character(),
                     Electorate = character(),
                     ALP2pp = numeric(),
                     OneSd = numeric())

state2ppcalc <- matrix(0, nrow=nObservations, ncol=length(stateNames))
colnames(state2ppcalc) <- stateNames
state2ppVar <- matrix(0, nrow=nObservations, ncol=length(stateNames))
colnames(state2ppVar) <- stateNames

for(thisState in stateNames){
  stateCols <- which(latentStateNames == thisState)
  thisStateParties <- latentPartyNames[stateCols]
  
  grnWeight <- (filter(tppData, StateAb == thisState, PartyAb == 'GRN'))$ALP2ppShare
  pupWeight <- (filter(tppData, StateAb == thisState, PartyAb == 'PUP'))$ALP2ppShare
  othWeight <- (filter(tppData, StateAb == thisState, PartyAb == 'OTH'))$ALP2ppShare
  
  alpPrimary <- modelOutput %>% filter(Pollster=="Smoothed", Electorate==thisState, Party=="ALP") %>% .[["Vote"]]
  grnPrimary <- modelOutput %>% filter(Pollster=="Smoothed", Electorate==thisState, Party=="GRN") %>% .[["Vote"]]
  pupPrimary <- modelOutput %>% filter(Pollster=="Smoothed", Electorate==thisState, Party=="PUP") %>% .[["Vote"]]
  othPrimary <- modelOutput %>% filter(Pollster=="Smoothed", Electorate==thisState, Party=="OTH") %>% .[["Vote"]]
  
  alp2pp <- alpPrimary + grnWeight*grnPrimary + pupWeight*ifelse(is.na(pupPrimary),0,pupPrimary) + othWeight*othPrimary
  
  primaryColumns <- c(which(latentComponentNamesBase == paste(thisState, "ALP")),
                      which(latentComponentNamesBase == paste(thisState, "GRN")),
              #        which(latentComponentNamesBase == paste(thisState, "PUP")),
                      which(latentComponentNamesBase == paste(thisState, "OTH")))
  weights <- c(1, grnWeight, othWeight)
  alp2ppOneSd <- sqrt(apply(smoothedModel$Pstar, 3, function(v){
    return(t(weights) %*% v[primaryColumns, primaryColumns] %*% weights)
  }))
  
  output <- rbind(output,
                  data.frame(PollEndDate = obsDates,
                             Electorate = thisState,
                             ALP2pp = alp2pp,
                             OneSd = alp2ppOneSd
                             ))
  state2ppcalc[,thisState] <- alp2pp
  state2ppVar[,thisState] <- alp2ppOneSd**2
}

output <- rbind(output,
                data.frame(PollEndDate = obsDates,
                           Electorate = 'AUS',
                           ALP2pp = c(state2ppcalc %*% popweights),
                           OneSd = c(sqrt(state2ppVar %*% (popweights**2))) ))


write.csv(output, file=outputFile, row.names=FALSE)












