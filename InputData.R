library(dplyr)

longData <- read.csv("Working/MergedData.csv", stringsAsFactors = FALSE)
national2ppData <- read.csv("PollingData/National2ppData.csv", stringsAsFactors = FALSE, na.strings="#N/A")
nationalData <- read.csv("PollingData/NationalData.csv", stringsAsFactors = FALSE, na.strings="#N/A")
stateData <- read.csv("PollingData/StateData.csv", stringsAsFactors = FALSE, na.strings="#N/A")
pollingUrls <- read.csv("PollingData/PollingURLs.csv", stringsAsFactors=FALSE, na.strings="#N/A")


parseDate <- function(s){
  shortDateWithYear <- function(q) as.Date(toupper(q), format="%d %b %Y")
  shortDate <- function(q) as.Date(toupper(q), format="%d %b")
  default <- as.Date
  
  parsingFunctions <- c(shortDateWithYear, shortDate, default)
  for(f in parsingFunctions){
    result <- try(f(s), silent=TRUE)
    if(!is.na(result) && class(result) == "Date"){
      return(f(s))
    }
  }
  return(NA)
}

pollsterCombos <- longData %>% filter(Pollster != "Election") %>%
  filter(PollEndDate > as.Date("2014-01-01")) %>%
  select(Pollster, Party, Electorate) %>% unique()

pollster <- readline(prompt="Pollster > ")
knownPollsters <- pollsterCombos %>% select(Pollster) %>% unique() %>% .[[1]]
if(! pollster %in% knownPollsters) {
  print(knownPollsters)
  stop("Unknown pollster")
}

expectedElectorates <- pollsterCombos %>% filter(Pollster == pollster) %>% select(Electorate) %>% unique() %>% .[[1]]
if(length(expectedElectorates) > 1) {
  warning("Multi-electorates not implemented yet")
}
electorate <- "AUS"

url <- readline(prompt="URL > ")

pollEndDate <- parseDate(readline(prompt="End Date > "))

if(is.na(pollEndDate)) {
  stop("Invalid date")
}
if(pollEndDate > as.Date(Sys.time())){
  stop("Future date")
}
if(pollEndDate < (as.Date(Sys.time()) - 30)) {
  stop("Old date")
}

alp2pp <- readline(prompt = "2PP ALP > ")
twoPPdataRow <- data.frame(date = format(pollEndDate, "%d/%m/%y"),
                        Labor2ppAUS = as.numeric(alp2pp), Pollster=pollster)

nationalDataRow <- nationalData[1,]
for(i in names(nationalDataRow)){
  nationalDataRow[[i]] <- NA
}
nationalDataRow[["PollEndDate"]] <- format(pollEndDate, "%Y-%m-%d")
nationalDataRow$Pollster <- pollster
for(party in names(nationalDataRow)[3:ncol(nationalDataRow)]){
  if(party %in% c("DEM","FFP","PHON")){
    next
  }
  partyResult <- readline(prompt = paste0(party, " > "))
  if(partyResult == ""){
    partyResult <- NA
  }
  nationalDataRow[[party]] <- as.numeric(partyResult)
}
if(abs(sum(nationalDataRow[3:ncol(nationalDataRow)], na.rm=TRUE) - 100) > 2) {
  stop("Doesn't sum to 100.")
}

urlDataRow <- data.frame(PollEndDate = as.character(format(pollEndDate, "%d/%m/%y")),
                         URL = url,
                         Pollster = pollster)

print("Data to be written: ")
print(urlDataRow)
print(nationalDataRow)
print(twoPPdataRow)

write.csv(rbind(urlDataRow, pollingUrls), "PollingData/PollingURLs.csv", row.names=FALSE, quote=FALSE, na="#N/A")
write.csv(rbind(nationalDataRow, nationalData), "PollingData/NationalData.csv", row.names=FALSE, quote=FALSE, na="#N/A")
write.csv(rbind(national2ppData, twoPPdataRow), "PollingData/National2ppData.csv", row.names=FALSE, quote=FALSE, na="#N/A")

