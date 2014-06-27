library(dplyr)


pollingplace2013data = tbl_df(read.csv('polling_places_2013.csv')) %>%
  mutate(PollingPlaceID = PPId)

pollingplace2010data = tbl_df(read.csv('polling_places_2010.csv', skip=1))
pollingplace2007data = tbl_df(read.csv('polling_places_2007.csv', skip=1))
pollingplace2004data = tbl_df(read.csv('polling_places_2004.csv', skip=1))


tppdata2013 = tbl_df(read.csv('tpp_by_polling_place_2013.csv',skip = 1))
tppdata2010 = tbl_df(read.csv('tpp_by_polling_place_2010.csv',skip = 1))
tppdata2007 = tbl_df(read.csv('tpp_by_polling_place_2007.csv',skip = 1))
tppdata2004 = tbl_df(read.csv('tpp_by_polling_place_2004.csv',skip = 1))


library(rgdal)
library(sp)

numbers2013 = tppdata2013 %>% mutate(State = StateAb, Division = DivisionNm,
                                     alp2pp = Australian.Labor.Party.Percentage, alpSwing = Swing) %>%
  select(PollingPlaceID, State, DivisionID, Division, alp2pp, alpSwing, TotalVotes)
places2013 = pollingplace2013data  %>% select(PollingPlaceID, Long, Lat)
data2013 = na.omit(inner_join(numbers2013, places2013, by='PollingPlaceID'))

loc2013 = SpatialPointsDataFrame( as.data.frame(select(data2013, Long, Lat)),
                                  as.data.frame(select(data2013, -(Long:Lat))),
                                  proj4string = CRS('+proj=longlat'))

numbers2010 = tppdata2010 %>% mutate(State = StateAb, Division = DivisionNm,
                                     alp2pp = Australian.Labor.Party.Percentage, alpSwing = Swing) %>%
  select(PollingPlaceID, State, DivisionID, Division, alp2pp, alpSwing, TotalVotes)
places2010 = pollingplace2010data %>% mutate(Long=Longitude,Lat=Latitude) %>% select(PollingPlaceID, Long, Lat)
data2010 = na.omit(inner_join(numbers2010, places2010, by='PollingPlaceID'))
loc2010 = SpatialPointsDataFrame( as.data.frame(select(data2010, Long, Lat)),
                                  as.data.frame(select(data2010, -(Long:Lat))),
                                  proj4string = CRS('+proj=longlat'))


numbers2007 = tppdata2007 %>% mutate(State = StateAb, Division = DivisionNm,
                                     alp2pp = Australian.Labor.Party.Percentage, alpSwing = -Swing) %>%
  select(PollingPlaceID, State, DivisionID, Division, alp2pp, alpSwing, TotalVotes)
places2007 = pollingplace2007data %>% mutate(Long=Longitude,Lat=Latitude) %>% select(PollingPlaceID, Long, Lat)
data2007 = na.omit(inner_join(numbers2007, places2007, by='PollingPlaceID'))
loc2007 = SpatialPointsDataFrame( as.data.frame(select(data2007, Long, Lat)),
                                  as.data.frame(select(data2007, -(Long:Lat))),
                                  proj4string = CRS('+proj=longlat'))


numbers2004 = tppdata2004 %>% mutate(State = StateAb, Division = DivisionNm,
                                     alp2pp = Australian.Labor.Party.Percentage, alpSwing = -Swing) %>%
  select(PollingPlaceID, State, DivisionID, Division, alp2pp, alpSwing, TotalVotes)
# The 2004 data on polling places is missing longitudes and latitudes, so paste them in from the 2007 one.
places2004 = pollingplace2004data %>% left_join(pollingplace2007data, by='PollingPlaceID') %>%
  mutate(Long=Longitude,Lat=Latitude) %>% select(PollingPlaceID, Long, Lat)
data2004 = na.omit(inner_join(numbers2004, places2004, by='PollingPlaceID'))
loc2004 = SpatialPointsDataFrame( as.data.frame(select(data2004, Long, Lat)),
                                  as.data.frame(select(data2004, -(Long:Lat))),
                                  proj4string = CRS('+proj=longlat'))

library(ggplot2)
library(ggmap)
library(RgoogleMaps)


plotCityBooths = function(spdf, locationString, cityName, year, maxSqrtSize){
  gc = geocode(locationString)
  center = as.numeric(gc)
  G = ggmap(get_googlemap(center=center, color='bw'), extent='device')
  # Use a sqrt() on TotalVotes per the box-cox estimate mentioned above.
  # Use a gradientn() for the colours to make sure that safe-ish seats aren't blended into beige 
  plotresult = G + geom_point(aes(x=Long, y=Lat,colour=alp2pp/100, size=sqrt(TotalVotes)), data=as.data.frame(spdf)) +
    scale_colour_gradientn(colours=c('Blue4','Blue1','Darkgoldenrod1','red1', 'red4'), values=c(0,0.4,0.5,0.6,1), guide=FALSE) +
    scale_size_area(max_size=4, limits=c(0,maxSqrtSize), guide=FALSE) +
    ggtitle(paste(cityName, year)) #+  theme(plot.title = element_text(size = rel(2)))
  return(plotresult)
}


generatePlots = function(locationString, cityName, showPlots=FALSE, savePlots=TRUE){
  plot2013 = plotCityBooths(loc2013, locationString, cityName, 2013, 100)
  plot2010 = plotCityBooths(loc2010, locationString, cityName, 2010, 100)
  plot2007 = plotCityBooths(loc2007, locationString, cityName,2007, 100)
  plot2004 = plotCityBooths(loc2004, locationString, cityName, 2004, 100)
  if(showPlots){
    print(plot2004)
    print(plot2007)
    print(plot2010)
    print(plot2013)
  }
  if(savePlots){
    ggsave(plot=plot2004, filename=paste0('CityPlots/',cityName,2004,'.png'), dpi=150)
    ggsave(plot=plot2007, filename=paste0('CityPlots/',cityName,2007,'.png'), dpi=150)
    ggsave(plot=plot2010, filename=paste0('CityPlots/',cityName,2010,'.png'), dpi=150)
    ggsave(plot=plot2013, filename=paste0('CityPlots/',cityName,2013,'.png'), dpi=150)
  }
}


generatePlots('Parramatta,NSW,Australia', 'Sydney')
generatePlots('Melbourne,VIC,Australia', 'Melbourne')
generatePlots('Brisbane,QLD,Australia', 'Brisbane')
generatePlots('Perth,WA,Australia', 'Perth')
generatePlots('Adelaide, SA, Australia', 'Adelaide')
generatePlots('Canberra, ACT,Australia', 'Canberra')
generatePlots('Hobart,TAS,Australia', 'Hobart')
generatePlots('Darwin,NT,Australia', 'Darwin')




















