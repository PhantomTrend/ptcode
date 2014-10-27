
library(ggplot2)
library(scales)
library(dplyr)

if(interactive()){
  args <- c('PlotOutputLongrun/.sentinel', 'FittedModel.RData', "2000-01-01", "2014-12-01",
            "HidePollsters")
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputSentinel <- args[1]
outputDirectory <- dirname(outputSentinel)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
inputModelFile <- args[2]
plotStartDate <- as.Date(args[3])
plotEndDate <- as.Date(args[4])
if(args[5] == 'ShowPollsters'){
  showPollsters <- TRUE
}else if(args[5] == 'HidePollsters'){
  showPollsters <- FALSE
}else{
  stop(sprintf('Invalid argument %s', args[5]))
}

load(inputModelFile)

electorateNames <- unique(modelOutput$Electorate)
readableElectorateNames <- list(AUS = 'Australia',
                                NSW = 'New South Wales',
                                VIC = 'Victoria',
                                SA = 'South Australia',
                                WA = 'Western Australia',
                                TAS = 'Tasmania',
                                QLD = 'Queensland',
                                NT = 'Northern Territory',
                                ACT = 'ACT')
readablePartyNames <- list(ALP = 'ALP',
                           LNP = 'Coalition',
                           GRN = 'Greens',
                           PUP = 'Palmer United',
                           OTH = 'Others',
                           PUPOTH = 'Palmer + Others',
                           GRNOTH = 'Greens + Others')

for(shortPartyName in names(readablePartyNames)){
  modelOutput$Party[modelOutput$Party == shortPartyName] <- readablePartyNames[[shortPartyName]]
  modelData$Party[modelData$Party == shortPartyName] <- readablePartyNames[[shortPartyName]]
}

for(shortElectorateName in names(readableElectorateNames)){
  modelOutput$Electorate[modelOutput$Electorate == shortElectorateName] <- readableElectorateNames[[shortElectorateName]]
  modelData$Electorate[modelData$Electorate == shortElectorateName] <- readableElectorateNames[[shortElectorateName]]
}

modelOutput <- filter(modelOutput, PollEndDate >= plotStartDate,
                      PollEndDate <= plotEndDate)
modelData <- filter(modelData, PollEndDate >= plotStartDate,
                    PollEndDate <= plotEndDate)

nPollsters <- length(unique(modelData$Pollster))

partyColours <- c( 'darkred',
                   'darkblue',
                   'darkgreen',
                   'purple',
                   'lightgreen',
                   'turquoise',
                   'yellow')
names(partyColours) <- c('ALP', 'Coalition', 'Greens',
                         'Palmer United' ,
                         'Greens + Others',
                         'Palmer + Others',
                         'Others')

colScale <- scale_colour_manual(name = "Party", values = partyColours, guide='none')


plotLengthInWeeks <- as.numeric(difftime(plotEndDate, plotStartDate, units='weeks'))
if(plotLengthInWeeks > 500){
  lineSize = 1
  pointSize = 1
  xscale = scale_x_date(breaks=date_breaks('3 years'),
                        minor_breaks=date_breaks('years'), labels=date_format("%Y"))
}else if(plotLengthInWeeks > 200){
  lineSize = 1
  pointSize = 0.7
  xscale = scale_x_date(breaks=date_breaks('1 year'),
                        minor_breaks=date_breaks('years'), labels=date_format("%Y"))
}else{
  lineSize = 1
  pointSize = 1
  xscale = scale_x_date(breaks=date_breaks('3 months'),
                        minor_breaks=date_breaks('months'), labels=date_format("%b %Y"))
}

if(showPollsters){
  shapeScale <- scale_shape_manual(values=1:nPollsters)
  pointAes <- aes(shape=Pollster, colour=Party)
}else{
  shapeScale <- NULL
  pointAes <- aes(colour=Party)
}

for(thisState in unlist(readableElectorateNames)){
  primaryPlot <- ggplot() + aes(x=PollEndDate, y=Vote) +
    geom_point(data = modelData %>% filter(Electorate==thisState),
               mapping=pointAes, size=pointSize) +
    geom_line(data=modelOutput %>% filter(Electorate==thisState, Pollster=='Smoothed'),
              mapping=aes(colour=Party), size=lineSize) + colScale + xscale +
    shapeScale + ggtitle(thisState) + xlab('') + ylab('')
  fileName <- sprintf('%s/%s.png', outputDirectory, thisState)
  ggsave(filename = fileName,
         plot = primaryPlot, 
         width=25, height=20, units='cm', dpi=150)
}


emptyOutput <- data.frame()
write.table(emptyOutput, file=outputSentinel, col.names=FALSE)




