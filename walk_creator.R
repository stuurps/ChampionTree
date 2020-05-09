library(data.table)
library(geosphere)
library(TSP)
library(tspmeta)
library(googleway)
library(rgdal)
library(cluster)

rm(list = ls())

writeGPX_wptrk <- function(dataframe, file) {
  trk <- subset(dataframe, is.na(dataframe$Name))
  wp <- subset(dataframe,!is.na(dataframe$Name))
  wp <- wp[!duplicated(wp),]
  #Set Up
  o <- c('<gpx version="1.1" creator="R">')
  
  #Add Way Points
  o <-
    c(
      o,
      paste(
        '<wpt lat="',
        wp$lat,
        '" lon="',
        wp$lon,
        '"> <name>',
        wp$Name,
        "</name> <sym>Park</sym></wpt>",
        sep = ''
      )
    )
  
  #Add Route Name
  o <- c(o, '<trk> <name>Route</name> <trkseg>')
  
  #Add Track points
  o <-
    c(o,
      paste('<trkpt lat="', trk$lat, '" lon="', trk$lon, '"></trkpt>', sep =
              ''))
  o <- c(o, '</trkseg>', '</trk>')
  o <- c(o, '</gpx>')
  if (is.character(file) || inherits(file, "connection"))
    cat(o, file = file, sep = '\n')
}

key <- "AIzaSyDrSBPZsMbhDmnGAU0l-l--80skNl5aue0"
set_key(key = key)
google_keys()

md <- fread("champion_tree_20200331.csv")

md <- md[!is.na(md$Lat),]
head(md)

# Build out clusters
mydata <- subset(md, select = c(Lat, Long))

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var))
for (i in 2:15)
  wss[i] <- sum(kmeans(mydata,
                       centers = i)$withinss)
plot(1:15,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution

#Plot cluster
clusplot(
  mydata,
  fit$cluster,
  color = TRUE,
  shade = TRUE,
  labels = 2,
  lines = 0
)

# get cluster means
aggregate(mydata, by = list(fit$cluster), FUN = mean)
# append cluster assignment
mydata <- data.frame(md, fit$cluster)

head(mydata)

#Update info
mydata[is.na(mydata)] <- "Unknown"
mydata$Name <-
  paste0(mydata$Common.Name, ". Girth (cm): ", mydata$Girth..cm.)

for (i in unique(mydata$fit.cluster)) {
  tmp <- subset(mydata, fit.cluster == i, select = c(Lat, Long, Name))
  if (nrow(tmp) >= 5) {
    print(paste0("Building Walk - ",i))
    colnames(tmp)[1] <- "lat"
    colnames(tmp)[2] <- "lon"
    tmp$ID <- 1:nrow(tmp)
    
    coords.mx <- as.matrix(tmp[, 1:2])
    
    # Compute distance matrix
    dist.mx <- dist(coords.mx)
    
    # Construct a TSP object
    tsp.ins <- tsp_instance(coords.mx, dist.mx)
    tour <- as.data.table(run_solver(tsp.ins, method = "2-opt"))
    colnames(tour)[1] <- "V1"
    tour$RANK <- 1:nrow(tour)
    
    tmp <- merge(tmp, tour, by.x = "ID", by.y = "V1")
    setorder(tmp, RANK)
    tmp <- subset(tmp, select = c(lat, lon, Name))
    
    #Add end point as initial start point
    tmp <- rbind(tmp, tmp[1, ])
    tmp$Name[nrow(tmp)] <- NA
    
    tmp_dir <- NULL
    for (x in 1:(nrow(tmp) - 1)) {
      o <- tmp[x, ]
      d <- tmp[x + 1, ]
      df <- google_directions(
        origin = o[, c(1, 2)],
        destination = d[, c(1, 2)],
        key = key,
        mode = "walking",
        simplify = TRUE
      )
      
      direction <- direction_steps(df)
      tmp_dir_sin <- tmp[x, ]
      
      for (z in 1:nrow(direction)) {
        dir <- NULL
        dir$lat[1] <- direction$start_location$lat[z]
        dir$lon[1] <- direction$start_location$lng[z]
        dir$lat[2] <- direction$end_location$lat[z]
        dir$lon[2] <- direction$end_location$lng[z]
        dir <- as.data.table(dir)
        tmp_dir_sin <- rbind(tmp_dir_sin, dir, fill = T)
        
      }
      
      tmp_dir <- rbind(tmp_dir, tmp_dir_sin)
      
      #Change NA to NULL
      
    }
    
    #Add end directions to beginning
    write.csv(tmp_dir,
              paste0(i, "_simple_championtree_cls_output.csv"),
              row.names = F)
    #writeGPX(tmp_dir, filename = "",type="w")
    writeGPX_wptrk(tmp_dir, paste0(i, "_walk.gpx"))
  }
}
