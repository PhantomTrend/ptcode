

library(ggplot2)

plotNationalTrend = function(estimatedModel, pollingDataZoo, pollsterZoo, plotStartDate,
                             plotEndDate, outputFileName, showPollsters, alpPovPlot){
  
  
  dataStartDate = start(pollingDataZoo)
  dataEndDate = end(pollingDataZoo)
  
  fittedNational2pp = zoo(
    as.vector(estimatedModel$popweights %*% t(estimatedModel$alphahat)),
       seq(dataStartDate, dataEndDate, by='day') )
  
  # Calculate a confidence interval as p'*V(t)*p, where V is the covariance
  # matrix of the smoothed estimate from KFAS
  oneSdWidth = apply(estimatedModel$V, 3, function(V){t(estimatedModel$popweights) %*% V %*% estimatedModel$popweights})
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
  
  outputPlot = ggplot(plotData) + aes(x=date) +
          geom_line(aes(y=fittedNational2pp), size=2, colour=lineColour) +
          geom_ribbon(aes(ymax=topOfConfidenceInterval, ymin=bottomOfConfidenceInterval),
                      fill=lineColour, alpha=0.2) +
           (if(showPollsters){geom_point(aes(y=polled2ppAUS, colour=pollster)  )
                 }else{geom_point(aes(y=polled2ppAUS), colour='black', alpha=0.7) }) +
          labs(x='',y='') + ggtitle(plotTitle) 
  if(!is.null(outputFileName)){ggsave(outputFileName)}
  return(outputPlot)
}


