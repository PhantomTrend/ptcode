
library(tidyr)
library(dplyr)
library(assertthat)

if(interactive()){
  args <- c('PollingData/MergedData.csv', 'PollingData/NationalDataLong.csv', 'PollingData/StateDataLong.csv')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

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
  
  if(!any(x$Party == 'PUP')){
    if(x$PollEndDate[1] >= as.Date('2013-05-01')){
      # This poll is after PUP's foundation, but doesn't report it separately
      x$Vote[which(x$Party=='PUPOTH')] = x$Vote[which(x$Party=='OTH')]
      x <- x[-which(x$Party=='OTH')]
    }else{
      # PUP didn't exist; mark it zero to ensure the model doesn't try to
      # infer a vote for it
      x <- rbind(x, data.frame(PollEndDate = x$PollEndDate[1],
                               Pollster = x$Pollster[1],
                               Party = 'PUP',
                               Electorate = unique(x$Electorate),
                               Vote = 0))
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
print(badNationalData %>% arrange(PollEndDate))






