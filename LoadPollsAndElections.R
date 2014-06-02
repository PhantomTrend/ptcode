
library(dplyr)
library(zoo)

# Inputs:  dataDir  - path to directory containing CSV files
#        startDate  - a Date to begin the time series at
# Outputs: a list containing
#              data  - a zoo numeric matrix with polls and election numbers
#           pollster - a zoo vector of factors containing the pollster name 

loadPollsAndElections = function(dataDir,startDate){
  
  # Read CSV files containing poll and election data
  newspoll = tbl_df(read.csv(paste(dataDir,'newspoll.csv',sep='/'), na.strings="#N/A"))
  newspoll$date = as.Date(newspoll$date, format="%d/%m/%Y")
  
  essential = tbl_df(read.csv(paste(dataDir,'essential.csv',sep='/'), na.strings='#N/A'))
  essential$date = as.Date(essential$date, format='%d/%m/%Y')
  
  galaxy = tbl_df(read.csv(paste(dataDir,'galaxy.csv',sep='/'), na.strings='#N/A'))
  galaxy$date = as.Date(galaxy$date, format='%d/%m/%Y')
  
  nielsen = tbl_df(read.csv(paste(dataDir,'nielsen.csv',sep='/'), na.strings='#N/A'))
  nielsen$date = as.Date(nielsen$date, format='%d/%m/%Y')
  
  morgan = tbl_df(read.csv(paste(dataDir,'morgan.csv',sep='/'), na.strings='#N/A'))
  morgan$date = as.Date(morgan$date, format='%d/%m/%Y')
  
  
  elections = tbl_df(read.csv(paste(dataDir,'tpp_by_state_at_elections.csv',sep='/')))
  elections$date = as.Date(elections$date, format="%d/%m/%Y")
  
  
  # Put polling data into conformable setup for merging.
  # The state-space model isn't defined when we have multiple observations
  # in a single time period (as happens occasionally with polls) so these helper
  # functions will take any clashing dates and move them around a little bit.
  adjustDate = function(origDate,dateIsBad ){
    deltad = c(+1,-1,+2,-2,+3,-3)
    for(dd in deltad){
      if( !dateIsBad(origDate + dd) ) { return(origDate + dd) }
    }
    stop('Bad date')
  }
  adjustAllDates = function(df,dateIsBad){
    badDates = which(sapply(df$date,dateIsBad))
    for(dd in badDates){ df$date[dd] = adjustDate(df$date[dd], dateIsBad )}
    df
  }
  
  # newspoll.csv has two columns for the national 2pp values, Q and W depending
  # on whether it's their quarterly or high-frequency release.
  # For this model, we record the high-frequency one and ignore the quarterly one.
  newspoll = mutate(newspoll,
                    Labor2ppAUS=ifelse(is.na(Labor2ppQ),Labor2ppW,Labor2ppQ),       
                    pollster=ifelse(is.na(Labor2ppW),'NewspollQ','NewspollW'),
                    Labor2ppTAS=NA, Labor2ppACT=NA, Labor2ppNT=NA) %>%
    select(-(Labor2ppW:Labor2ppQ))
  
  essential = mutate(essential,   
                     pollster='Essential',
                     Labor2ppNSW=NA, Labor2ppVIC=NA, Labor2ppQLD=NA, Labor2ppSA=NA,
                     Labor2ppWA=NA, Labor2ppTAS=NA, Labor2ppNT=NA, Labor2ppACT=NA)
  essential = adjustAllDates(essential, function(d){ (d %in% newspoll$date) || (d %in% elections$date) } )
  
  
  galaxy = mutate(galaxy,   
                  pollster='Galaxy',
                  Labor2ppNSW=NA, Labor2ppVIC=NA,  Labor2ppSA=NA,
                  Labor2ppWA=NA, Labor2ppTAS=NA, Labor2ppNT=NA, Labor2ppACT=NA)
  galaxy = adjustAllDates(galaxy, function(d){
    (d %in% newspoll$date) || (d %in% elections$date) || (d %in% essential$date) } )
  
  nielsen = mutate(nielsen,   
                   pollster='ACNielsen',
                   Labor2ppNSW=NA, Labor2ppVIC=NA, Labor2ppQLD=NA, Labor2ppSA=NA,
                   Labor2ppWA=NA, Labor2ppTAS=NA, Labor2ppNT=NA, Labor2ppACT=NA)
  nielsen = adjustAllDates(nielsen, function(d){
    ((d %in% newspoll$date) || (d %in% elections$date) || (d %in% galaxy$date)
     || (d %in% essential$date)) } )
  
  morgan = mutate(morgan,   
                  pollster='RoyMorgan',
                  Labor2ppNSW=NA, Labor2ppVIC=NA, Labor2ppQLD=NA, Labor2ppSA=NA,
                  Labor2ppWA=NA, Labor2ppTAS=NA, Labor2ppNT=NA, Labor2ppACT=NA)
  morgan = adjustAllDates(morgan, function(d){
    ((d %in% newspoll$date) || (d %in% elections$date) || (d %in% galaxy$date)
     || (d %in% nielsen$date) || (d %in% essential$date)) } )
  
  elections = mutate(elections, pollster='Election')
  
  bigData = rbind(newspoll,
                  essential,
                  galaxy,
                  nielsen,
                  morgan,
                  elections
                  )  %>%
              mutate(pollster=factor(pollster)) %>%
              arrange(date)  %>%
              filter(date > startDate)

  dataZoo = zoo(select(bigData, Labor2ppAUS,
                       Labor2ppNSW, Labor2ppVIC, Labor2ppQLD, Labor2ppSA,
                       Labor2ppWA, Labor2ppTAS, Labor2ppNT, Labor2ppACT), bigData$date)
  pollsterZoo = zoo(select(bigData, pollster), bigData$date)
  
  
  return(list(data=dataZoo, pollster=pollsterZoo))
}




