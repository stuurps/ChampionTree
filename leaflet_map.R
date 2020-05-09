library(leaflet)  # for generating interactive Javascript maps
library(leaflet.extras)
library(rgdal)    # GDAL bindings for loading GPX-data
library(sp)       # spatial operations library
library(lubridate)# datetime-operatings, here to convert from strings
library(ggplot2)  # general plotting library

wp1 <- readOGR('1_walk.gpx', layer = "tracks")
wp1$ID <- 1
wp2 <- readOGR('2_walk.gpx', layer = "tracks")
wp2$ID <- 2
wp3 <- readOGR('3_walk.gpx', layer = "tracks")
wp3$ID <- 3
wp4 <- readOGR('4_walk.gpx', layer = "tracks")
wp4$ID <- 4
wp5 <- readOGR('5_walk.gpx', layer = "tracks")
wp5$ID <- 5

#Get coords
wp1c <- as.data.frame(coordinates(wp1))
wp2c <- as.data.frame(coordinates(wp2))
wp3c <- as.data.frame(coordinates(wp3))
wp4c <- as.data.frame(coordinates(wp4))
wp5c <- as.data.frame(coordinates(wp5))

icon.glyphicon <- makeAwesomeIcon(icon = "flag", 
                                  iconColor = "black", markerColor = "blue", library = "glyphicon")

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data=wp1, color = "darkcyan") %>%
  addPolylines(data=wp2, color = "navy") %>%
  addPolylines(data=wp3, color = "black") %>%
  addPolylines(data=wp4, color = "darkgreen") %>%
  addPolylines(data=wp5, color = "firebrick") %>%
  addAwesomeMarkers(
    lng = wp1c$X1[1], lat = wp1c$X2[1],
    popup = 'North Bristol (15 trees)<br> <a href="https://ridewithgps.com/routes/32517065" target="_blank">GPX Route</a>',
    icon = icon.glyphicon) %>%
  addAwesomeMarkers(
    lng = wp2c$X1[1], lat = wp2c$X2[1],
    popup = 'East Bristol (16 trees)<br> <a href="https://ridewithgps.com/routes/32517068" target="_blank">GPX Route</a>',
    icon = icon.glyphicon) %>%
  addAwesomeMarkers(
    lng = wp3c$X1[1], lat = wp3c$X2[1],
    popup = 'Central Bristol (57 trees)<br> <a href="https://ridewithgps.com/routes/32517064" target="_blank">GPX Route</a>',
    icon = icon.glyphicon) %>%
  addAwesomeMarkers(
    lng = wp4c$X1[1], lat = wp4c$X2[1],
    popup = 'South Bristol (18 trees)<br> <a href="https://ridewithgps.com/routes/32517067" target="_blank">GPX Route</a>',
    icon = icon.glyphicon) %>%
  addAwesomeMarkers(
    lng = wp5c$X1[1], lat = wp5c$X2[1],
    popup = 'West Bristol (21 trees)<br> <a href="https://ridewithgps.com/routes/32517066" target="_blank">GPX Route</a>',
    icon = icon.glyphicon)

library(htmlwidgets)
saveWidget(m, file="bristol-walks.html")

