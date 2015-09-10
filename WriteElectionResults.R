
library(dplyr)
library(rjson)
library(readr)

set.seed(31337)

if(interactive()){
  args <- c('ElectionResults/.sentinel',
            'ElectionResults/Seats/.sentinel',
            'ElectionData/HouseFirstPrefsByCandidateByVoteType2013.csv',
            'ElectionData/Incumbents.csv',
            'ElectionResults/TwoPartyPreferred.csv',
            'ElectionResults/PrimaryVotes.csv'
            )
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputSentinel <- args[1]
outputDirectory <- dirname(outputSentinel)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}

seatsSentinel <- args[2]
seatsDirectory <- dirname(seatsSentinel)

firstPrefsFile <- args[3]
incumbentFile <- args[4]
tppTrendFile <- args[5]
primaryTrendFile <- args[6]

firstPrefs <- tbl_df(read.csv(firstPrefsFile, skip=1, stringsAsFactors=FALSE))
electorateNames <- unique(firstPrefs$DivisionNm)
electorateFileNames <- gsub("$",".csv",gsub("'","_",gsub(" ","_",electorateNames)))

individualResultsSchema <- data.frame(Electorate = character(),
                     ALPWins = integer(), LNPWins = integer(), GRNWins = integer(), PUPWins = integer(),
                     OTHWins = integer(),
                     ALPPrimary = numeric(), LNPPrimary = numeric(), GRNPrimary = numeric(),
                     PUPPrimary = numeric(), OTHPrimary = numeric(),
                     ALP2PP = numeric(), LNP2PP = numeric(),
                     Repetition = integer())

individualResults <- Reduce(rbind,
                            lapply(paste(seatsDirectory, electorateFileNames, sep="/"), read_csv),
                            individualResultsSchema)

nRepetitions <- max(individualResults$Repetition)
nSeatSimulations <- max(individualResults$ALPWins)

getWinPct <- function(wins){ sum(wins)/(nSeatSimulations*nRepetitions)*100  }
outcomeSummary <- individualResults %>% group_by(Electorate) %>%
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
            ALP2PP = mean(na.omit(as.numeric(ALP2PP))),
            LNP2PP = mean(na.omit(as.numeric(LNP2PP)))) %>%
  mutate(Electorate = as.character(Electorate))
            
incumbentData <- read_csv(incumbentFile)
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

seatOutputFile <- paste0(outputDirectory, "/SeatResults.csv")
write.csv(finalOutput, file=seatOutputFile, row.names=FALSE)

pickWinner <- function(seatOutcomes){
  if(seatOutcomes$IncumbentParty == "OTH"){
    return(data.frame(Winner="OTH"))
  }
  winners <- c(seatOutcomes$ALPWins, seatOutcomes$LNPWins, seatOutcomes$GRNWins, seatOutcomes$PUPWins, seatOutcomes$OTHWins)
  names(winners) <- c("ALP","LNP","GRN","PUP","OTH")
  return(data.frame(Winner=names(winners)[which(winners == max(winners))[1]]))
}
summariseElectionOutcome <- function(d){
  outcomes <- d %>% inner_join(incumbentData, by="Electorate") %>% rowwise() %>% do(pickWinner(.))
  return(data.frame(ALP = sum(outcomes$Winner=="ALP"),
                    LNP = sum(outcomes$Winner=="LNP"),
                    GRN = sum(outcomes$Winner=="GRN"),
                    PUP = sum(outcomes$Winner=="PUP"),
                    OTH = sum(outcomes$Winner=="OTH")))
}

seatsWon <- individualResults %>% group_by(Repetition) %>% do(summariseElectionOutcome(.)) %>% ungroup() %>%
  mutate(Outcome = ifelse(ALP > 75, "ALP Majority", ifelse(LNP > 75, "LNP Majority", "Hung Parliament")))


tppTrend <- read_csv(tppTrendFile) %>% filter(Electorate=="AUS") %>% tail(1)

primaryTrend <- read_csv(primaryTrendFile) %>% filter(Electorate=="AUS") %>%
                arrange(PollEndDate) %>% tail(5)
primarySummary <- data.frame(ALP = primaryTrend %>% filter(Party=="ALP") %>% .[['Vote']] %>% round(.,1),
                             LNP = primaryTrend %>% filter(Party=="LNP") %>% .[['Vote']] %>% round(.,1),
                             GRN = primaryTrend %>% filter(Party=="GRN") %>% .[['Vote']] %>% round(.,1),
                             PUP = primaryTrend %>% filter(Party=="PUP") %>% .[['Vote']] %>% round(.,1),
                             OTH = primaryTrend %>% filter(Party=="OTH") %>% .[['Vote']] %>% round(.,1) )
summaryOutputFile <- paste0(outputDirectory, "/ElectionSummary.json")
outcomeProbabilities <- summary(as.factor(seatsWon$Outcome))/(nRepetitions)*100

if(!("ALP Majority" %in% names(outcomeProbabilities))) {
  extraOutcome <-  1
  names(extraOutcome) <- "ALP Majority"
  outcomeProbabilities <- c(outcomeProbabilities, extraOutcome)
}
if(!("LNP Majority" %in% names(outcomeProbabilities))) {
  extraOutcome <-  1
  names(extraOutcome) <- "LNP Majority"
  outcomeProbabilities <- c(outcomeProbabilities, extraOutcome)
}
if(!("Hung Parliament" %in% names(outcomeProbabilities))) {
  extraOutcome <-  1
  names(extraOutcome) <- "Hung Parliament"
  outcomeProbabilities <- c(outcomeProbabilities, extraOutcome)
}
sink(file=summaryOutputFile)
cat(toJSON(list(
  twoPartyPreferred = round(tppTrend$ALP2pp,1),
  primary = primarySummary,
  outcomeProbabilities = outcomeProbabilities,
  mostLikelyOutcome = names(which(unlist(outcomeProbabilities) == max(unlist(outcomeProbabilities))))
  )
  ))
sink()


pubDateStr <- strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %z")

rssHeader <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\" ?>
  <rss version=\"2.0\">
  <channel>
  <title>The Phantom Trend</title>
  <description>Quantitative forecasts of the Australian election</description>
  <link>http://www.phantomtrend.com</link>
  <lastBuildDate>",pubDateStr,"</lastBuildDate>
  <pubDate>",pubDateStr,"</pubDate>
  <ttl>720</ttl>")
rssFooter <- "</channel>
  </rss>"

alp2pp <- tppTrend$ALP2pp
rssTitle <- ifelse(alp2pp > 50, paste(round(alp2pp,1), "to ALP national 2PP"),
                   paste(round(100-alp2pp,1), "to LNP national 2PP"))

outcomeDescription <- paste0("Outcome probabilities: ",
                             "ALP majority ", outcomeProbabilities[["ALP Majority"]], "%, ",
                             "Hung parliament ", outcomeProbabilities[["Hung Parliament"]], "%, ",
                             "LNP majority ", outcomeProbabilities[["LNP Majority"]], "%")
rssDescription <- paste0("Primary votes: ",
                        paste(primaryTrend %>% mutate(Vote=round(Vote,1), Result=paste(Party, Vote, sep=": ")) %>% .[["Result"]],
                              collapse=", "), "; ", outcomeDescription)

rssItem <- paste0("  <item>
  <title>", rssTitle, "</title>
  <description>", rssDescription, "</description>
  <link>http://www.phantomtrend.com</link>
  <pubDate>", pubDateStr, "</pubDate>
  </item>\n")
  
rss <- paste0(rssHeader, rssItem, rssFooter)

rssFile <- paste0(outputDirectory, "/rss.xml")
sink(file=rssFile)
cat(rss)
sink()

emptyOutput <- data.frame()
write.table(emptyOutput, file=outputSentinel, col.names=FALSE)

