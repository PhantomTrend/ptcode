
library(dplyr)
library(assertthat)
library(KFAS)

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/TwoPartyPreferred.csv',
            'FittedModel.RData', 'ElectionData/HouseTppFlowByStateByParty2013.csv',
            "10")
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFile <- args[1]
inputModel <- args[2]
tppFlowFile <- args[3]
nSimulations <- as.numeric(args[4])



tppData <- (tbl_df(read.csv(tppFlowFile, skip=1)) %>%
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
simulations <- simulateSSM(fittedModel, type='state', nsim = nSimulations)[,1:length(latentStateNames),]
obsDates <- sort(unique(modelOutput$PollEndDate))
nObservations <- length(obsDates)


pupStartRow <- (modelData %>% filter(Party=='PUP', !is.na(Vote)) %>% arrange(RowNumber))$RowNumber[1]


output <- data.frame(PollEndDate = character(),
                     Electorate = character(),
                     ALP2pp = integer(),
                     Repetition = integer())

state2ppcalc <- matrix(0, nrow=nObservations, ncol=length(stateNames))
colnames(state2ppcalc) <- stateNames

for(repI in 1:nSimulations){
  for(thisState in stateNames){
    stateCols <- which(latentStateNames == thisState)
    thisStateParties <- latentPartyNames[stateCols]
    thisStateALP2pp <- simulations[,stateCols[which(thisStateParties=='ALP')],repI]
    thisStateALP2pp <- thisStateALP2pp +
      (c(rep(0,(pupStartRow-1)),
         simulations[pupStartRow:nObservations,stateCols[which(thisStateParties=='PUP')],repI]) * 
      (filter(tppData, StateAb == thisState, PartyAb == 'PUP'))$ALP2ppShare )
    thisStateALP2pp <- thisStateALP2pp +
      (simulations[,stateCols[which(thisStateParties=='GRN')],repI] * 
         (filter(tppData, StateAb == thisState, PartyAb == 'GRN'))$ALP2ppShare )
    thisStateALP2pp <- thisStateALP2pp +
      (simulations[,stateCols[which(thisStateParties=='OTH')],repI] * 
         (filter(tppData, StateAb == thisState, PartyAb == 'OTH'))$ALP2ppShare )
    state2ppcalc[,thisState] <- thisStateALP2pp
    output <- rbind(output,
                    data.frame(PollEndDate = obsDates,
                               Electorate = thisState,
                               ALP2pp = thisStateALP2pp,
                               Repetition = repI))
  }
  output <- rbind(output,
                  data.frame(PollEndDate = obsDates,
                             Electorate = 'AUS',
                             ALP2pp = c(state2ppcalc %*% popweights),
                             Repetition = repI))
}

write.csv(output, file=outputFile, row.names=FALSE)












