
library(tidyr)
library(dplyr)
library(assertive)
library(assertthat)

if(interactive()){
  args <- c('PollingData/MergedData.csv', 'PollingData/NationalDataLong.csv',
            'PollingData/StateDataLong.csv', 'ElectionData/FirstPrefs.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputFileName <- args[1]

nationalData <- tbl_df(read.csv(args[2], stringsAsFactors=FALSE))
nationalData$PollEndDate <- as.Date(nationalData$PollEndDate)

stateData <- tbl_df(read.csv(args[3], stringsAsFactors=FALSE))
stateData$PollEndDate <- as.Date(stateData$PollEndDate)
stateData$Vote[which(stateData$Vote == '<0.5')] <- "0.2"
stateData$Vote <- as.numeric(stateData$Vote)

# This is approximately the date when Clive Palmer announced he was forming
# a new party. The party itself was registered as the UAP in April 2013, then
# re-registered as PUP a month later.
pupFoundationDate <- as.Date('2012-11-15')

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
  x <- collateWithOther(x, 'FFP')    # Family First
  x <- collateWithOther(x, 'IND')    # Misc. independents
  x <- collateWithOther(x, 'DEM')    # Democrats
  x <- collateWithOther(x, 'PHON')   # Pauline Hanson
  
  # Allow PUP and GRN numbers to be either missing altogether or NA.
  missingOrNA <- function(partyName){
    if(!any(x[["Party"]] == partyName)){
      dorewrite <- TRUE
    }else{
      if(is.na(x[x[["Party"]]==partyName,'Vote']) & !is.na(x[x[["Party"]]=='OTH','Vote'])){
        dorewrite <- TRUE
      }else{
        dorewrite <- FALSE
      }
    }
    return(dorewrite)
  }
  rewritePup <- missingOrNA('PUP')
  rewriteGrn <- missingOrNA('GRN')
  
  if(rewritePup){
    if(x$PollEndDate[1] >= pupFoundationDate){
      # This poll is after PUP's foundation, but doesn't report it separately
      assert_is_non_empty(which(x$Party == 'OTH'))
      if(rewriteGrn){
        print(c(x$PollEndDate[1], x$Pollster[1]))
      }
      pupOtherRow <- data.frame(PollEndDate = x$PollEndDate[1],
                                Pollster = x$Pollster[1],
                                Party = 'PUPOTH',
                                Electorate = x$Electorate[1],
                                Vote = x$Vote[which(x$Party=='OTH')])
      x <- rbind(x, pupOtherRow)
      x <- x[-which(x$Party=='OTH'),]
      if(any(x$Party=='PUP')){
        x <- x[-which(x$Party=='PUP'),]
      }
    }else{
      # PUP didn't exist
      if(any(x$Party=='PUP')){
        x[which(x$Party=='PUP'),'Vote'] <- NA
      }else{
        x <- rbind(x, data.frame(PollEndDate = x$PollEndDate[1],
                                 Pollster = x$Pollster[1],
                                 Party = 'PUP',
                                 Electorate = unique(x$Electorate),
                                 Vote = NA))
      }
    }
  }
  if(rewriteGrn){
    # Some early polls in the sample include GRN with OTH
    assert_is_non_empty(which(x$Party == 'OTH'))
    grnOtherRow <- data.frame(PollEndDate = x$PollEndDate[1],
                              Pollster = x$Pollster[1],
                              Party = 'GRNOTH',
                              Electorate = x$Electorate[1],
                              Vote = x$Vote[which(x$Party=='OTH')])
    x <- rbind(x, grnOtherRow)
    x <- x[-which(x$Party=='OTH'),]
    if(any(x$Party=='GRN')){
      x <- x[-which(x$Party=='GRN'),]
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
assert_that(nrow(badStateData)==10)     # One missing set of WA numbers, plus Galaxy's QLD-only polls

# Morgan reports Coalition numbers as "XX (YY)" where XX is the Liberal primary, YY is the National, and
#  ZZ = (XX+YY) is the implicit Coalition primary. The APH Library have transcribed only XX+YY for all Morgan
#  polls. But prior to 2005, Morgan were apparently reporting "ZZ (YY)", so the LNP numbers in the
#  APH Library spreadsheet are overstated by ~3-5% in that period.
# TODO: re-transcribe the numbers...
nationalData <- filter(nationalData, !(Pollster == 'Morgan' & PollEndDate < as.Date("2004-12-01")))

nationalDataNew <- nationalData %>% group_by(PollEndDate, Electorate, Pollster) %>% do(fixMinorParties(.)) %>% ungroup()
badNationalData <- nationalDataNew %>% group_by(PollEndDate, Electorate, Pollster) %>% do(validateData(.))
invisible(assert_that(nrow(badNationalData)==5))
# TODO: check/replace these five polls intead of dropping them
for(badI in 1:nrow(badNationalData)){
  nationalDataNew <- filter(nationalDataNew, !(Pollster == badNationalData$Pollster[badI] & 
                                                 PollEndDate == badNationalData$PollEndDate[badI]))
}


electoralData <- tbl_df(read.csv(args[4], stringsAsFactors=FALSE))
badElectoralData <- electoralData %>% group_by(PollEndDate, Electorate, Pollster) %>% do(validateData(.))
invisible(assert_that(nrow(badElectoralData)==0))


completeData <- (rbind(nationalDataNew, stateDataNew, electoralData) %>%
                   arrange(PollEndDate,Electorate,Party) %>%
                   mutate(Pollster = factor(Pollster),      # Making these guys into factors will mean
                          Party = factor(Party),            # that summary() works better
                          Electorate = factor(Electorate)))

completeData$Vote[which(completeData$Party == 'PUP' &
                          completeData$PollEndDate < pupFoundationDate)] <- NA


print(summary(completeData))

# Keep note of Newspoll Quarterly reports, so we can model their averaging properly when fitting the model
isEndOfMonth <- function(d){
  monthDay <- format(d, "%m-%d")
  return(monthDay == "03-31" || monthDay == "06-30" || monthDay == "09-30" || monthDay == "12-31")
}
newspollStateDates <- unique((stateDataNew %>% filter(Pollster == 'Newspoll'))$PollEndDate)
newspollQuarterlyDates <- newspollStateDates[(Vectorize(isEndOfMonth))(newspollStateDates)]
levels(completeData$Pollster) <- c(levels(completeData$Pollster), 'Newspoll Quarterly')
completeData[which(completeData$PollEndDate %in% newspollQuarterlyDates &
                     completeData$Pollster == 'Newspoll'), 'Pollster'] <- 'Newspoll Quarterly'

# Now some health checks on the finalised data
# TODO: why does Rscript throw an error on this line, when R.app doesn't?
# assert_all_are_in_past(as.POSIXct(completeData$PollEndDate))

assert_is_numeric(completeData$Vote)
# One Nielsen WA state poll + Galaxy QLD + the pre-2013 PUP votes should be NA
invisible(assert_that(length(which(is.na(completeData$Vote))) == 1553))   
assert_all_are_non_negative(na.omit(completeData$Vote))
nonZeroVotes <- na.omit(completeData$Vote[completeData$Vote>0])
assert_all_are_in_closed_range(nonZeroVotes, 0.9, 60)
invisible(assert_that(is_in_closed_range(mean(nonZeroVotes), 20, 30)))
pollstersWeKnowAbout <- c('Election', "Essential", "Essential Online", "Galaxy", "Morgan", "Morgan Multi", 
                          "Morgan SMS", "Newspoll", "Newspoll Quarterly", "Nielsen", "ReachTEL", "Ipsos")
invisible(assert_that(all(levels(completeData$Pollster) %in% pollstersWeKnowAbout)))
assert_all_are_not_na(completeData$Pollster)
partiesWeKnowAbout <- c("ALP", "GRN", "LNP", "OTH", "PUP", "PUPOTH", "GRNOTH")
invisible(assert_that(all(levels(completeData$Party) %in% partiesWeKnowAbout)))
assert_all_are_not_na(completeData$Party)
electoratesWeKnowAbout <- c("AUS", "NSW", "QLD", "SA", "VIC", "WA", "ACT", "NT", "TAS")
invisible(assert_that(all(levels(completeData$Electorate) %in% electoratesWeKnowAbout)))
assert_all_are_not_na(completeData$Electorate)

write.csv(completeData, outputFileName, row.names=FALSE)


