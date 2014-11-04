
library(ggplot2)
library(scales)
library(dplyr)

if(interactive()){
  args <- c('TppPlots/.sentinel',
          'ElectionResults/TwoPartyPreferred.csv', 'PollingData/National2ppData.csv',
          '2013-01-01', '2014-12-31')
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputSentinel <- args[1]
outputDirectory <- dirname(outputSentinel)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}

tppSimulations <- tbl_df(read.csv(args[2]))
tppObservations <- tbl_df(read.csv(args[3]))
tppSimulations$PollEndDate <- as.Date(tppSimulations$PollEndDate)
tppObservations$PollEndDate <- as.Date(tppObservations$date, format='%d/%m/%y')

plotStartDate <- as.Date(args[4])
plotEndDate <- as.Date(args[5])


electorateNames <- unique(tppSimulations$Electorate)
readableElectorateNames <- list(AUS = 'Australia',
                                NSW = 'New South Wales',
                                VIC = 'Victoria',
                                SA = 'South Australia',
                                WA = 'Western Australia',
                                TAS = 'Tasmania',
                                QLD = 'Queensland',
                                NT = 'Northern Territory',
                                ACT = 'ACT')

plotColour <- 'red'
xscale <- scale_x_date(breaks=date_breaks('3 months'),
                      minor_breaks=date_breaks('months'), labels=date_format("%b %Y"))


for(thisState in unique(tppSimulations$Electorate)){
  summaryData <- tppSimulations %>% filter(Electorate == thisState, PollEndDate >= plotStartDate) %>%
    group_by(PollEndDate) %>% summarise(AlpMean = mean(ALP2pp),
                                        AlpLo = quantile(ALP2pp, probs=c(.05)),
                                        AlpHi = quantile(ALP2pp, probs=c(.95))) %>% ungroup()
  summaryData$AlpMeanNext <- c(summaryData$AlpMean[-1], tail(summaryData$AlpMean,1))
  summaryData$AlpLoNext <- c(summaryData$AlpLo[-1], tail(summaryData$AlpLo,1))
  summaryData$AlpHiNext <- c(summaryData$AlpHi[-1], tail(summaryData$AlpHi,1))
  
  repeatedLo <- c(summaryData$AlpLo, summaryData$AlpLo)
  repeatedHi <- c(summaryData$AlpHi, summaryData$AlpHi)
  repeatedLo <- repeatedLo[order(c(1:length(summaryData$AlpLo),1:length(summaryData$AlpLo)))]
  repeatedHi <- repeatedHi[order(c(1:length(summaryData$AlpLo),1:length(summaryData$AlpLo)))]
  plotDatesStart <- summaryData$PollEndDate
  plotDatesEnd <- plotDatesStart+7
  plotDates <- c(plotDatesStart,plotDatesEnd)[order(c(1:length(plotDatesStart),1:length(plotDatesStart)))]
  confidenceInterval <- data.frame(date = c(plotDates, rev(plotDates)),
                                   value = c(repeatedLo, rev(repeatedHi)))
  twoPPplot <- ggplot(summaryData) +
    geom_segment(aes(x=PollEndDate, xend=PollEndDate+7,
                     y=AlpMean, yend=AlpMean), colour='darkred') +
    geom_segment(aes(x=PollEndDate+7, xend=PollEndDate+7,
                     y=AlpMean, yend=AlpMeanNext), colour='darkred') +
    geom_polygon(data=confidenceInterval, aes(x=date, y=value),
                 fill=plotColour, alpha=0.2) + xlab('') + ylab('') +
    ggtitle(readableElectorateNames[[thisState]]) + xscale
  if(thisState == 'AUS'){
    twoPPplot <- twoPPplot +
      geom_point(data=tppObservations %>% filter(PollEndDate >= plotStartDate),
                 aes(x=PollEndDate, y=Labor2ppAUS))
  }
  fileName <- sprintf('%s/%s.png', outputDirectory, thisState)
  ggsave(filename = fileName,
         plot = twoPPplot, 
         width=25, height=20, units='cm', dpi=150)
}

lastDate <- max(tppSimulations$PollEndDate)
lastRow <- tppSimulations %>% filter(Electorate == 'AUS', PollEndDate == lastDate)

print(sprintf('Mean ALP vote in week beginning %s: %.1f', format(lastDate, '%d-%b'),
              mean(lastRow$ALP2pp)))
alpRange <- quantile(lastRow$ALP2pp, probs=c(.05,.95))
print(sprintf('90pct range: %.1f to %.1f', alpRange[1], alpRange[2]))

emptyOutput <- data.frame()
write.table(emptyOutput, file=outputSentinel, col.names=FALSE)





