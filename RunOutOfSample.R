

source('LoadPollsAndElections.R')

dataDir = 'Data'
startDate = as.Date('2003-01-01')

dataList = loadPollsAndElections(dataDir, startDate)
endDate = as.Date(end(dataList$data))

# Prepare a time series matrix for input to the state-space model,
# with one day per row
Y = as.ts(dataList$data)
# Remove national 2pp readings when state-based ones are available to avoid
# stochastic singularity, given that we're using approximate population weights.
Y[which(!is.na(Y[,2])),1] = NA

source('NationalPollingModel.R')
source('PrintedOutput.R')

pollsterTs = as.ts(dataList$pollster)
electionRows = which(pollsterTs=='Election')

for( i in 2:length(electionRows)  ){
    thisY = Y[1:(electionRows[i]-1),]
    thisPollster = pollsterTs[1:(electionRows[i]-1)] 
   print(paste('Estimating forecast for election on', index(dataList$pollster[which(dataList$pollster=='Election')[i]])))
   estimatedModel = fitNationalPollingModel(thisY, thisPollster, verbose=FALSE, reestimateFromScratch=TRUE)
   printCurrentEstimates(estimatedModel)
}



