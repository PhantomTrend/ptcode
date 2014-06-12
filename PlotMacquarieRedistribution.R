

library(dplyr)
library(ggplot2)
library(gstat)



source('BarryFilter.R')




barryBooths2004 = tbl_df(read.csv('electionmaps/booths_data/2004.csv')) %>% barryFilter()
barryBooths2007 = tbl_df(read.csv('electionmaps/booths_data/2007.csv')) %>% barryFilter()


library(MASS)   # for kde2d



# Function to generate kriged estimates of 2pp voting and a KDE of population density
getKrigedGrid = function( spatials, booths, longitudeLimits, latitudeLimits ){
  
  gridStepSizeX = diff(longitudeLimits)/100
  gridStepSizeY = diff(latitudeLimits)/100
  
  gridCoordsX = seq(from = longitudeLimits[1], to = longitudeLimits[2], by = gridStepSizeX)
  gridCoordsY = seq(from = latitudeLimits[1], to = latitudeLimits[2], by=gridStepSizeY) 
  cityGrid = expand.grid(Long = gridCoordsX, Lat = gridCoordsY)
  coordinates(cityGrid) = ~Long+Lat
  gridded(cityGrid) = TRUE
  proj4string(cityGrid) = CRS('+proj=longlat +ellps=GRS80 +no_defs')
  
  # Plots for various years in Melbourne and Sydney suggest that this 
  # variogram model is fairly ok.
  alpvg = variogram(ALP_2PP ~ 1, spatials, cressie=TRUE)
  modelVg = fit.variogram(alpvg, vgm(300, 'Sph', 19, 5))
  
  # Use kriging to estimate the ambient 2PP vote across the electorate and its
  # surrounding area.
  voteModel = krige(ALP_2PP ~ 1, locations = spatials, newdata = cityGrid, model = modelVg, nmax=6)
  
  # Now use a kernel density estimate for the population density.
  # I resample the population in order to use the built-in kde routine, which
  # expects a vector of samples.
  # TODO: code up a kde function that accepts weights instead.
  boothPopulationWeights = booths$Formal_votes / sum(booths$Formal_votes)
  nBooths = nrow(booths)
  nPopulationSamples = 5000
  populationSamples = base::sample.int(nBooths, nPopulationSamples, replace=TRUE, prob=boothPopulationWeights)
  xDraws = booths$Long[populationSamples]
  yDraws = booths$Lat[populationSamples]
  populationDensity = kde2d(xDraws, yDraws, n=c(length(gridCoordsX),length(gridCoordsY)),
                            lims=c(longitudeLimits, latitudeLimits))
  
  populationModel = expand.grid(Long = gridCoordsX, Lat = gridCoordsY)
  populationModel$population = as.vector(populationDensity$z)
  coordinates(populationModel) = ~Long+Lat
  gridded(populationModel) = TRUE
  proj4string(populationModel) = CRS('+proj=longlat +ellps=GRS80 +no_defs')
  
  return(list(voteModel=voteModel, populationModel=populationModel, grid=cityGrid))
}


library(rgdal)


boundaries2007 = readOGR(dsn='electionmaps/newshapes', layer='CED07aAUST_region')
boundaries2004 = readOGR(dsn='electionmaps/newshapes', layer='CED04aAUST_region')


seatName = 'Macquarie'

thisSeatPolygons04 = as(boundaries2004[which(boundaries2004$NAME_2004== seatName),,drop=FALSE],
                      'SpatialPolygons')
thisSeatPolygons07 = as(boundaries2007[which(boundaries2007$NAME_2007== seatName),,drop=FALSE],
                        'SpatialPolygons')

# Get the bounding box of the seat in its two incarnations
longitudeLimits04 = as.numeric(bbox(thisSeatPolygons04)['x',])
latitudeLimits04 = as.numeric(bbox(thisSeatPolygons04)['y',])
longitudeLimits07 = as.numeric(bbox(thisSeatPolygons07)['x',])
latitudeLimits07 = as.numeric(bbox(thisSeatPolygons07)['y',])
longitudeLimits = c(min(longitudeLimits04[1],longitudeLimits07[1]),
                    max(longitudeLimits04[2],longitudeLimits07[2]))
latitudeLimits = c(min(latitudeLimits04[1],latitudeLimits07[1]),
                    max(latitudeLimits04[2],latitudeLimits07[2]))

# Expand them a bit, to make sure we capture effects near the border
longitudeLimits = longitudeLimits + c(-0.1,0.1)
latitudeLimits = latitudeLimits + c(-0.1,0.1)


nearbyBoothsLastElection = filter(barryBooths2004, Lat > latitudeLimits[1] & Lat < latitudeLimits[2] &
                                    Long > longitudeLimits[1] & Long < longitudeLimits[2] )


nearbyBoothsSpatials = remove.duplicates( SpatialPointsDataFrame( as.data.frame(select(nearbyBoothsLastElection, Long, Lat)),
                                                                  as.data.frame(select(nearbyBoothsLastElection, -(Long:Lat))),
                                                                  proj4string = CRS('+proj=longlat +ellps=GRS80 +no_defs')) )



# Convert the seat boundaries into a dataframe ready for plotting, using
# the ggplot2::fortify() utility
seatLines04Df = fortify(thisSeatPolygons04)
seatLines07Df = fortify(thisSeatPolygons07)
boothDf = as.data.frame(nearbyBoothsLastElection)
names(seatLines04Df) = c('Long', 'Lat')     # Capitalise x,y coordinates the same as the Booths data
names(seatLines07Df) = c('Long', 'Lat')


k1 = getKrigedGrid(nearbyBoothsSpatials, nearbyBoothsLastElection, longitudeLimits, latitudeLimits)



k1df = as.data.frame(k1$voteModel)
p1df = as.data.frame(k1$populationModel)


heatMap = ggplot() + aes(x=Long,y=Lat) + 
          geom_point(data=boothDf) +
        geom_raster(aes(fill=var1.pred), data=k1df, alpha=0.5) +
        coord_cartesian() + 
      scale_fill_gradientn(colours=c('Blue4','Blue1','Darkgoldenrod1','red1', 'red4'),
                       values=c(0,0.4,0.5,0.6,1), guide=FALSE) +
  geom_path(data=seatLines04Df, colour='darkmagenta', size=2) + 
  geom_path(data=seatLines07Df, colour='limegreen', size=2) + 
  annotate('text', x=150.8, y=-33.2, label='2004', colour='darkmagenta', size=8) +
  annotate('text', x=149.6, y=-33.9, label='2007', colour='limegreen', size=8) +
  xlab('') + ylab('') 
ggsave('macquarie_heatmap.png')

squaresInSeat04 = k1$voteModel %over% as(thisSeatPolygons04, 'SpatialPolygons')
squaresInSeat07 = k1$voteModel %over% as(thisSeatPolygons07, 'SpatialPolygons')


p1df$population[which(is.na(squaresInSeat04) | is.na(squaresInSeat07))] = 0
ggplot() + aes(x=Long,y=Lat) + 
  geom_point(data=boothDf) +
  geom_raster(aes(fill=population), data=p1df, alpha=1) +
  coord_cartesian() + 
  scale_fill_gradient(low='black', high='white', guide=FALSE) +
  geom_path(data=seatLines04Df, colour='darkmagenta', size=2) + 
  geom_path(data=seatLines07Df, colour='limegreen', size=2) + 
  annotate('text', x=151, y=-33.2, label='2004', colour='darkmagenta', size=12) +
  annotate('text', x=149.6, y=-33.9, label='2007', colour='limegreen', size=12)











