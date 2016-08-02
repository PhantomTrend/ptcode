library(dplyr)
library(readr)
library(ggplot2)


savePlotAsSvg <- function(plotToSave, baseFileName, widthInPx=300, heightInPx=250,
                          jpgFallback=TRUE, fallbackWidth=1200, fallbackHeight=1000){
  plotDpi <- 72
  # Subtract a fudge factor to make sure we're just _inside_ the bounding box
  plotWidthInches <- widthInPx / plotDpi * 0.99
  plotHeightInches <- heightInPx / plotDpi * 0.99
  
  fallbackDpi <- 300
  plotWidthFallbackInches <- fallbackWidth / fallbackDpi
  plotHeightFallbackInches <- fallbackHeight / fallbackDpi
  
  svgDevice <- function(file, width, height) {
    library(RSvgDevice)
    devSVG(file = file, width = width, height = height, bg = "white", fg = "black",
           onefile = TRUE, xmlHeader = TRUE)
  }
  
  svgFilename <- paste0(baseFileName, ".svg")
  ggsave(plot=plotToSave, filename=svgFilename, device=svgDevice,
         width=plotWidthInches, height=plotHeightInches, units="in")
  
  # Fix the default font in SVG charts, which looks terrible
  svgText <- readLines(svgFilename)
  formattedSvg <- gsub("style=\"font-size:11\"", "font-family=\"'Helvetica Neue',Helvetica,Arial,sans-serif\" style=\"fill: #666\" font-size=\"10\"",
                       gsub("style=\"font-size:13\"", "font-family=\"'Helvetica Neue',Helvetica,Arial,sans-serif\" style=\"fill: #666\" font-size=\"12\"",
                       gsub( "style=\"font-size:9\"", "font-family=\"'Helvetica Neue',Helvetica,Arial,sans-serif\" style=\"fill: #666\" font-size=\"10\"", svgText )))
  cat(formattedSvg, file=svgFilename, sep="\n")
  
  if(jpgFallback){
    ggsave(plot=plotToSave, filename=paste0(baseFileName, ".jpg"),
           width=plotWidthFallbackInches, height=plotHeightFallbackInches, units="in")
  }
}

predictedPrimary <- read_csv("ElectionResults/StateSwings.csv") %>%
  group_by(Electorate,Party) %>% summarise(PredictedVote = mean(Vote), OneSd = sqrt(var(Vote))) %>%
  ungroup() %>% filter(Party != "PUP")

predicted2pp <- read_csv("ElectionResults/TwoPartyPreferred.csv") %>%
  filter(PollEndDate == max(.$PollEndDate)) %>% select(-PollEndDate)

partyNames <- list('LP'='LNP','NP'='LNP','LNQ'='LNP','LNP'='LNP','CLP'='LNP','ALP'='ALP','GRN'='GRN','PUP'='PUP')
actualNationalPrimary <- read_csv("ElectionData/HouseFirstPrefsByPartyDownload-20499.csv", skip=1) %>%
  mutate(Party = ifelse(PartyAb %in% names(partyNames), as.character(partyNames[PartyAb]), 'OTH')) %>%
  group_by(Party) %>%
  summarise(ActualVote = sum(TotalPercentage)) %>%
  ungroup() %>%
  mutate(Electorate = "Australia")
actualPrimary <- read_csv("ElectionData/HouseFirstPrefsByStateByPartyDownload-20499.csv", skip=1) %>%
  mutate(Party = ifelse(PartyAb %in% names(partyNames), as.character(partyNames[PartyAb]), 'OTH')) %>%
  group_by(Party,StateAb) %>%
  summarise(ActualVote = sum(TotalPercentage)) %>%
  ungroup() %>%
  rename(Electorate = StateAb) %>%
  rbind(actualNationalPrimary)

primaryAccuracyByState <- ggplot(actualPrimary %>% inner_join(predictedPrimary, by=c("Party","Electorate"))) +
  aes(x=ActualVote, y=PredictedVote,colour=Electorate,shape=Party) +
  geom_errorbar(aes(ymin=PredictedVote-2*OneSd,ymax=PredictedVote+2*OneSd)) +
  geom_point() +
  ylab("Predicted Primary Vote (%)") + xlab("Actual Primary Vote (%)") +
  geom_abline(colour="grey") + coord_fixed() +
  ggtitle("Kalman filter: Statewide prediction accuracy")
savePlotAsSvg(primaryAccuracyByState, "phantomTrendStateAccuracy")

actual2pp <- read_csv("ElectionData/HouseTppByStateDownload-20499.csv", skip=1) %>%
  rename(Electorate = StateAb,
         ALP2ppActual = `Australian Labor Party Percentage`,
         ALPVotes = `Australian Labor Party Votes`) %>%
  select(Electorate, ALP2ppActual, ALPVotes)

ggplot(predicted2pp %>% inner_join(actual2pp, by="Electorate")) +
  aes(x=ALP2ppActual,y=ALP2pp,colour=Electorate) + geom_errorbar(aes(ymin=ALP2pp-2*OneSd,ymax=ALP2pp+2*OneSd)) +
  geom_point() +
  geom_abline(colour="grey") + coord_fixed() + ylim(c(35,70)) + xlim(c(35,70))

actualSeat2pp <- read_csv("ElectionData/HouseTppByDivisionDownload-20499.csv", skip=1) %>%
  rename(ALP2PPActual = `Australian Labor Party Percentage`,
         Electorate = DivisionNm)
predictedSeat2pp <- read_csv("ElectionResults/SeatResults.csv") %>%
  mutate(Electorate = ifelse(Electorate == "Fraser", "Fenner", Electorate))

seatSummary <- actualSeat2pp %>% inner_join(predictedSeat2pp, by="Electorate") %>%
  # Two-candidate-preferred results haven't been published yet
  filter(ALP2PPActual > 0) %>%
  mutate(Wrong = (ALP2PP > 50 & ALP2PPActual < 50) | (ALP2PP < 50 & ALP2PPActual > 50))

ggplot(seatSummary) +
  aes(x=ALP2PPActual, y=ALP2PP, colour=StateAb) +
  geom_point() + geom_abline(colour="grey") + coord_fixed()


seatAccuracyPlot <- ggplot(seatSummary) +
  aes(x=ALP2PPActual, y=ALP2PP, colour=Wrong) +
  scale_colour_manual(values=c("black","red"), guide="none")+
  geom_point() + geom_abline(colour="grey") + coord_fixed() +
  xlab("Actual ALP 2PP (%)") + ylab("Predicted ALP 2PP (%)") +
  ggtitle("Reps electorates: Prediction accuracy")
savePlotAsSvg(seatAccuracyPlot, "phantomTrendSeatAccuracy")

lastElection2pp <- read_csv("ElectionData/HouseTppByState2013.csv", skip=1) %>%
  rename(Electorate = StateAb,
        ALP2ppActual = `Australian Labor Party Percentage`,
        ALPVotes = `Australian Labor Party Votes`) %>%
  select(Electorate, ALP2ppActual, ALPVotes, TotalVotes)

national2ppLastTime <- sum(lastElection2pp$ALPVotes)/sum(lastElection2pp$TotalVotes) * 100
predictedNational2ppThisTime <- 51
uniformSwing <- predictedNational2ppThisTime - national2ppLastTime
uniformSwingPredictions <- lastElection2pp %>%
  mutate(UniformSwing = ALP2ppActual + uniformSwing) %>%
  select(Electorate, UniformSwing)

uniformSwingComparison <- actual2pp %>% inner_join(uniformSwingPredictions, by='Electorate') %>%
  mutate(AbsError = abs(UniformSwing - ALP2ppActual), Type='Unif. Swing') %>%
  select(Electorate, AbsError, Type) %>%
  rbind(actual2pp %>% inner_join(predicted2pp, by='Electorate') %>%
          mutate(AbsError = abs(ALP2pp - ALP2ppActual), Type='Model') %>%
          select(Electorate, AbsError, Type))

uniformComparisonPlot <- ggplot(uniformSwingComparison) + aes(x=Electorate, y=AbsError, fill=Type) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Two-party preferred prediction errors") +
  ylab("Abs. % error") + xlab("")
savePlotAsSvg(uniformComparisonPlot, "phantomTrendUniformSwingComparison")




