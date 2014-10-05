
library(tidyr)
library(dplyr)
library(assertthat)
library(assertive)


if(interactive()){
  args <- c('PollingData/MergedData.csv', 'PollingData/NationalDataLong.csv', 'PollingData/StateDataLong.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFileName <- args[1]

nationalData <- tbl_df(read.csv(args[2]))
nationalData$PollEndDate <- as.Date(nationalData$PollEndDate)

stateData <- tbl_df(read.csv(args[3]))
stateData$PollEndDate <- as.Date(stateData$PollEndDate)
stateData$Vote[which(stateData$Vote == '<0.5')] <- "0.2"
stateData$Vote <- as.numeric(stateData$Vote)

fixMinorParties <- function(x){
  # Take minor parties that we don't track, and add them to Other
  collateWithOther <- function(df, partyCode){
    if(any(df$Party == partyCode)){
      if(!is.na(df[which(x$Party==partyCode),'Vote'])){
        df[which(x$Party=='OTH'),'Vote'] = df[which(x$Party==partyCode),'Vote'] + df[which(x$Party=='OTH'),'Vote']
      }
      df <- df[-which(x$Party == partyCode),]
    }
    return(df)
  }
  x <- collateWithOther(x, 'FFP')  # Family First
  x <- collateWithOther(x, 'IND')    # Misc. independents
  x <- collateWithOther(x, 'DEM')    # Democrats
  x <- collateWithOther(x, 'PHON')   # Pauline Hanson
  
  # Allow PUP numbers to be either missing altogether or NA.
  if(!any(x$Party == 'PUP')){
    rewritePup <- TRUE
  }else{
    if(is.na(x[x$Party=='PUP','Vote'])){
      rewritePup <- TRUE
    }else{
      rewritePup <- FALSE
    }
  }
  if(rewritePup){
    if(x$PollEndDate[1] >= as.Date('2013-05-01')){
      # This poll is after PUP's foundation, but doesn't report it separately
      pupOtherRow <- data.frame(PollEndDate = x$PollEndDate[1],
                                Pollster = x$Pollster[1],
                                Party = 'PUPOTH',
                                Electorate = x$Electorate[1],
                                Vote = x$Vote[which(x$Party=='OTH')])
      x <- rbind(x, pupOtherRow)
      x <- x[-which(x$Party=='OTH'),]
      x <- x[-which(x$Party=='PUP'),]
    }else{
      # PUP didn't exist; mark it zero to ensure the model doesn't try to
      # infer a vote for it
      if(any(x$Party=='PUP')){
        x[which(x$Party=='PUP'),'Vote'] <- 0
      }else{
        x <- rbind(x, data.frame(PollEndDate = x$PollEndDate[1],
                                 Pollster = x$Pollster[1],
                                 Party = 'PUP',
                                 Electorate = unique(x$Electorate),
                                 Vote = 0))
      }
    }
  }
  return(x)
}

validateData <- function(x){
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
badStateData <- stateData %>% group_by(PollEndDate, Electorate, Pollster) %>% do(validateData(.))
assert_that(nrow(badStateData)==1)     # One missing set of WA numbers

# Morgan reports Coalition numbers as "XX (YY)" where XX is the Liberal primary, YY is the National, and
#  ZZ = (XX+YY) is the implicit Coalition primary. The APH Library have transcribed only XX+YY for all Morgan
#  polls. But prior to 2005, Morgan were apparently reporting "ZZ (YY)", so the LNP numbers in the
#  APH Library spreadsheet are overstated by ~3-5% in that period.
# TODO: re-transcribe the numbers...
nationalData <- filter(nationalData, !(Pollster == 'Morgan' & PollEndDate < as.Date("2004-12-01")))

nationalDataNew <- nationalData %>% group_by(PollEndDate, Electorate, Pollster) %>% do(fixMinorParties(.)) %>% ungroup()
badNationalData <- nationalDataNew %>% group_by(PollEndDate, Electorate, Pollster) %>% do(validateData(.))
assert_that(nrow(badNationalData)==5)
# TODO: check/replace these five polls intead of dropping them
for(badI in 1:nrow(badNationalData)){
  nationalDataNew <- filter(nationalDataNew, !(Pollster == badNationalData[badI,'Pollster'] & 
                                                 PollEndDate == badNationalData[badI, 'PollEndDate']))
}


completeData <- (rbind(nationalDataNew, stateDataNew) %>%
                   arrange(PollEndDate,Electorate,Party) %>%
                   mutate(Pollster = factor(Pollster),      # Making these guys into factors will mean
                          Party = factor(Party),            # that summary() works better
                          Electorate = factor(Electorate)))
print(summary(completeData))

# Keep note of Newspoll Quarterly reports, so we can model their averaging properly when fitting the model
newspollQuarterlyDates <- unique((stateDataNew %>% filter(Pollster == 'Newspoll'))$PollEndDate)
levels(completeData$Pollster) <- c(levels(completeData$Pollster), 'Newspoll Quarterly')
completeData[which(completeData$PollEndDate %in% newspollQuarterlyDates &
                     completeData$Pollster == 'Newspoll'), 'Pollster'] <- 'Newspoll Quarterly'

# Now some health checks on the finalised data
assert_all_are_in_past(as.POSIXct(completeData$PollEndDate))
assert_is_numeric(completeData$Vote)
assert_that(length(which(is.na(completeData$Vote))) == 4)   # One Nielsen WA state poll
assert_all_are_non_negative(na.omit(completeData$Vote))
nonZeroVotes <- na.omit(completeData$Vote[completeData$Vote>0])
assert_all_are_in_closed_range(nonZeroVotes, 1, 60)
assert_that(is_in_closed_range(mean(nonZeroVotes), 20, 30))
pollstersWeKnowAbout <- c("Essential", "Essential Online", "Galaxy", "Morgan", "Morgan Multi", 
                          "Morgan SMS", "Newspoll", "Newspoll Quarterly", "Nielsen", "ReachTEL")
assert_that(all(levels(completeData$Pollster) %in% pollstersWeKnowAbout))
assert_all_are_not_na(completeData$Pollster)
partiesWeKnowAbout <- c("ALP", "GRN", "LNP", "OTH", "PUP", "PUPOTH")
assert_that(all(levels(completeData$Party) %in% partiesWeKnowAbout))
assert_all_are_not_na(completeData$Party)
electoratesWeKnowAbout <- c("AUS", "NSW", "QLD", "SA", "VIC", "WA")
assert_that(all(levels(completeData$Electorate) %in% electoratesWeKnowAbout))
assert_all_are_not_na(completeData$Electorate)

write.csv(completeData, outputFileName, row.names=FALSE)


