
library(ggplot2)
library(scales)
library(dplyr)

if(interactive()){
  args <- c('PlotOutputLongrun/.sentinel', 'PlotData/.sentinel',
            "2013-01-01", "2014-12-31",
            "HidePollsters")
}else{
  args <- commandArgs(trailingOnly = TRUE)
}

outputSentinel <- args[1]
outputDirectory <- dirname(outputSentinel)
if(!file.exists(outputDirectory)){
  dir.create(outputDirectory, recursive=TRUE)
}
inputDirectory <- dirname(args[2])
stopifnot(file.exists(inputDirectory))
plotStartDate <- as.Date(args[3])
plotEndDate <- as.Date(args[4])
if(args[5] == 'ShowPollsters'){
  showPollsters <- TRUE
}else if(args[5] == 'HidePollsters'){
  showPollsters <- FALSE
}else{
  stop(sprintf('Invalid argument %s', args[5]))
}

modelOutput <- read.csv(paste0(inputDirectory, '/ModelOutput.csv'))
modelData <- read.csv(paste0(inputDirectory, '/PollData.csv'))

modelOutput$PollEndDate <- as.Date(modelOutput$PollEndDate)
modelData$PollEndDate <- as.Date(modelData$PollEndDate)


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

modelOutput <- modelOutput %>% group_by(Party,Electorate,Pollster) %>% 
  mutate(VoteNext = c(Vote[-1], tail(Vote,1))) %>% ungroup()

electorateNames <- unique(modelOutput$Electorate)

for(thisState in unlist(electorateNames)){
  primaryPlot <- ggplot() + aes(x=PollEndDate, y=Vote) +
    geom_point(data = modelData %>% filter(Electorate==thisState),
               mapping=pointAes, size=pointSize) +
    geom_segment(data=modelOutput %>% filter(Electorate==thisState, Pollster=='Smoothed'),
                 mapping=aes(colour=Party, x=PollEndDate+7, xend = PollEndDate+7, yend=VoteNext), size=lineSize) +
    geom_segment(data=modelOutput %>% filter(Electorate==thisState, Pollster=='Smoothed'),
              mapping=aes(colour=Party,x = PollEndDate, xend = PollEndDate+7, yend=Vote), size=lineSize) +
    colScale + xscale +
    shapeScale + ggtitle(sprintf('%s: Primary Votes',thisState)) + xlab('') + ylab('')
  fileName <- sprintf('%s/%s.png', outputDirectory, thisState)
  ggsave(filename = fileName,
         plot = primaryPlot, 
         width=25, height=20, units='cm', dpi=150)
}


emptyOutput <- data.frame()
write.table(emptyOutput, file=outputSentinel, col.names=FALSE)




