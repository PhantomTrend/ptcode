

# D.W. Barry's hand-trimming of outliers etc., rewritten to use dplyr SQL-like logic.
# Input is a dplyr data frame read from one of Barry's csv files of booth data.

barryFilter = function(df){
  # Booths to get rid of because they're a long way from the rest of the electorate:
  outlier_booths = c("Jervis Bay", "Wreck Bay", "Lord Howe Island", "Home Island", "West Island", "Cocos Islands PPVC", "Christmas Island")
  df = filter(df, !(Booth %in% outlier_booths))
  
  # Remove booths with less than a threshold number of formal votes unless the booth is sufficiently isolated:
  # [JH: I've cut the isolation criterion because I only care about tightly-bunched urban booths.]
  threshold_votes = 100
  df = filter(df, Formal_votes >= threshold_votes)
  
  # Remove points where longitude = 0 (= latitude):
  df = filter(df, Long != 0)
  
  # Remove PPVCs (prepolling)
  # This syntax is a bit ugh. Hopefully later versions of dplyr will include a match() type
  # option filter() as well as in select()
  df = filter(df, !((1:nrow(df)) %in% grep('PPVC', Booth)))
  
  # Seat naming issues
  levels(df$Seat) = c(levels(df$Seat), 'McMahon', 'Kingsford Smith')
  # Remove hyphens from "Kingsford-Smith":
  df[which(df$Seat == "Kingsford-Smith"),'Seat'] = "Kingsford Smith"
  # Rename Prospect to McMahon:
  df[which(df$Seat == "Prospect"),'Seat'] = "McMahon"
  
  
  #   # The following are booths that are used by many electorates.  For some reason WA was full of them.
  #   capcity_booths <- c("Brisbane City", "Sydney", "Melbourne", "Adelaide", "Perth")
  #   capcity_wa1 <- c("Bunbury Central", "Busselton", "Busselton West", "Dunsborough", "Falcon", "Lancelin")
  #   capcity_wa2 <- c("Mandurah Central", "Margaret River", "Nannup", "Rottnest", "Yallingup", "Broome")
  #   capcity_wa3 <- c("Carnarvon", "Exmouth", "Kalbarri", "Divisional Office (PREPOLL)")
  #   capcity_booths <- c(capcity_booths, capcity_wa1, capcity_wa2, capcity_wa3)
  # [ JH: DWBarry trims these booths by throwing them away if they're outside the bounding box of the
  # rest of each electorate's booths. I use the more simple-minded approach of allocating the row with
  # the max formal votes to each booth.
  df = df %>% group_by(Booth) %>% mutate(VoteDiff = Formal_votes - max(Formal_votes)) %>%
    filter(VoteDiff == 0) %>% select(-VoteDiff) %>% ungroup()
  
  
  # JH: some years have the same Lat and Long for different booths (due to patchy address data).
  # I just take a weighted average of their 2PP data, then delete them prior to kriging 
  # via remove.duplicates().
  df = df %>% group_by(Lat,Long) %>% mutate(ALP_2PP = weighted.mean(ALP_2PP, Formal_votes)) %>% ungroup()
  
  return(df)
}


