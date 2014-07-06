

library(ggplot2)

plotNationalTrend = function(estimatedModel, pollingDataZoo, pollsterZoo, plotStartDate,
                             plotEndDate, outputFileName, showPollsters, alpPovPlot){
  
  
  dataStartDate = start(pollingDataZoo)
  dataEndDate = end(pollingDataZoo)
  
  fittedNational2pp = zoo(
    as.vector(estimatedModel$popweights %*% t(estimatedModel$alphahat)),
       seq(dataStartDate, dataEndDate, by='day') )
  
  # Calculate a confidence interval using var(x) = p'*V(t)*p, where V is the covariance
  # matrix of the smoothed estimate from KFAS and p is the vector of population weights
  oneSdWidth = sqrt(apply(estimatedModel$V, 3, function(V){t(estimatedModel$popweights) %*% V %*% estimatedModel$popweights}))
  oneSdWidth = zoo(oneSdWidth, seq(dataStartDate, dataEndDate, by='day'))
  topOfConfidenceInterval = fittedNational2pp + 2*oneSdWidth
  bottomOfConfidenceInterval = fittedNational2pp - 2*oneSdWidth
  
  plotData = cbind( pollingDataZoo, fittedNational2pp,
                    topOfConfidenceInterval, bottomOfConfidenceInterval )
  # Pad out the data in case plotEndDate > dataEndDate, etc
  padding = zoo(NA, seq(min(plotStartDate,dataStartDate), max(plotEndDate,dataEndDate), by='day') )
  plotData = cbind(plotData,padding)
  plotData = window(plotData, start=plotStartDate, end=plotEndDate)
  # Convert to data.frame with a date column, as ggplot2 likes
  plotData = cbind(data.frame(date=index(plotData)), as.data.frame(plotData))
  # Now add in the 'pollster' factor (cbinding it with a Zoo matrix causes everything else
  #   to be converted to factors)
  pollsterDf = window(cbind(pollsterZoo, padding), start=plotStartDate, end=plotEndDate)
  plotData$pollster = as.factor(pollsterDf$pollster)
  
  
  if(alpPovPlot){
    lineColour = 'red'
    plotTitle = 'ALP National Two-Party Preferred'
    plotData$polled2ppAUS = plotData$Labor2ppAUS
  }else{
    lineColour = 'blue'
    plotTitle = 'LNP National Two-Party Preferred'
    plotData$polled2ppAUS = 100 - plotData$Labor2ppAUS
    plotData$fittedNational2pp = 100 - plotData$fittedNational2pp
    plotData$topOfConfidenceInterval = 100 - plotData$topOfConfidenceInterval
    plotData$bottomOfConfidenceInterval = 100 - plotData$bottomOfConfidenceInterval
  }
  
  # Thick lines look better on short-range plots, but they go a bit weird
  # on long-run plots.
  if(plotEndDate - plotStartDate > 900){
    lineSize = 1
  }else{
    lineSize = 2
  }
  
  outputPlot = ggplot(plotData) + aes(x=date) +
    (if(showPollsters){
      geom_point(aes(y=polled2ppAUS, colour=pollster), size=2)
    }else{geom_point(aes(y=polled2ppAUS), colour='black', alpha=0.7, size=2) }) +
          geom_line(aes(y=fittedNational2pp), size=lineSize, colour=lineColour) +
          geom_ribbon(aes(ymax=topOfConfidenceInterval, ymin=bottomOfConfidenceInterval),
                      fill=lineColour, alpha=0.2) +
          labs(x='',y='') + ggtitle(plotTitle) +
            theme(title=element_text(size=28, vjust=2),
                  axis.text=element_text(colour='black', size=18))
  if(!is.null(outputFileName)){ggsave(outputFileName, width=13, height=8)}
  return(outputPlot)
}




plotStateTrend = function(estimatedModel, pollingDataZoo, pollsterZoo, plotStartDate,
                             plotEndDate, outputFileName, showPollsters, alpPovPlot, stateName){
  
  
  dataStartDate = start(pollingDataZoo)
  dataEndDate = end(pollingDataZoo)
  
  stateCol = switch(stateName,
                    'NSW'=1,'VIC'=2,'QLD'=3,'SA'=4,'WA'=5,'TAS'=6,'NT'=7,'ACT'=8)
  
  fittedState2pp = zoo(
    as.vector(estimatedModel$alphahat[,stateCol]),
    seq(dataStartDate, dataEndDate, by='day') )
  
  oneSdWidth = sqrt(as.vector(estimatedModel$V[stateCol,stateCol,]))
  oneSdWidth = zoo(oneSdWidth, seq(dataStartDate, dataEndDate, by='day'))
  topOfConfidenceInterval = fittedState2pp + 2*oneSdWidth
  bottomOfConfidenceInterval = fittedState2pp - 2*oneSdWidth
  
  plotData = cbind( pollingDataZoo, fittedState2pp,
                    topOfConfidenceInterval, bottomOfConfidenceInterval )
  # Pad out the data in case plotEndDate > dataEndDate, etc
  padding = zoo(NA, seq(min(plotStartDate,dataStartDate), max(plotEndDate,dataEndDate), by='day') )
  plotData = cbind(plotData,padding)
  plotData = window(plotData, start=plotStartDate, end=plotEndDate)
  # Convert to data.frame with a date column, as ggplot2 likes
  plotData = cbind(data.frame(date=index(plotData)), as.data.frame(plotData))
  # Now add in the 'pollster' factor (cbinding it with a Zoo matrix causes everything else
  #   to be converted to factors)
  pollsterDf = window(cbind(pollsterZoo, padding), start=plotStartDate, end=plotEndDate)
  plotData$pollster = as.factor(pollsterDf$pollster)
  
  
  if(alpPovPlot){
    lineColour = 'red'
    plotTitle = paste('ALP Two-Party Preferred in', stateName)
    plotData$polled2ppState = switch(stateName,
                                   'NSW'=plotData$Labor2ppNSW,'VIC'=plotData$Labor2ppVIC,
                                   'QLD'=plotData$Labor2ppQLD,'SA'=plotData$Labor2ppSA,
                                   'WA'=plotData$Labor2ppWA,'TAS'=plotData$Labor2ppTAS,
                                   'NT'=plotData$Labor2ppNT,'ACT'=plotData$Labor2ppACT)
  }else{
    lineColour = 'blue'
    plotTitle = paste('LNP Two-Party Preferred in', stateName)
    plotData$polled2ppState = 100 - switch(stateName,
                                         'NSW'=plotData$Labor2ppNSW,'VIC'=plotData$Labor2ppVIC,
                                         'QLD'=plotData$Labor2ppQLD,'SA'=plotData$Labor2ppSA,
                                         'WA'=plotData$Labor2ppWA,'TAS'=plotData$Labor2ppTAS,
                                         'NT'=plotData$Labor2ppNT,'ACT'=plotData$Labor2ppACT)
    plotData$fittedState2pp = 100 - plotData$fittedState2pp
    plotData$topOfConfidenceInterval = 100 - plotData$topOfConfidenceInterval
    plotData$bottomOfConfidenceInterval = 100 - plotData$bottomOfConfidenceInterval
  }
  
  # Thick lines look better on short-range plots, but they go a bit weird
  # on long-run plots.
  if(plotEndDate - plotStartDate > 900){
    lineSize = 1
  }else{
    lineSize = 2
  }
  
  outputPlot = ggplot(plotData) + aes(x=date) +
    geom_line(aes(y=fittedState2pp), size=lineSize, colour=lineColour) +
    geom_ribbon(aes(ymax=topOfConfidenceInterval, ymin=bottomOfConfidenceInterval),
                fill=lineColour, alpha=0.2) +
    (if(showPollsters){geom_point(aes(y=polled2ppState, colour=pollster)  )
    }else{geom_point(aes(y=polled2ppState), colour='black', alpha=0.7) }) +
    labs(x='',y='') + ggtitle(plotTitle)  +
    theme(title=element_text(size=28, vjust=2),
          axis.text=element_text(colour='black', size=18))
  if(!is.null(outputFileName)){ggsave(outputFileName, width=13, height=8)}
  return(outputPlot)
}



