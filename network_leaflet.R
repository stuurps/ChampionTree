#Overlay on leaflet 
library(igraph)

df <- lrgst_lnks
meta <- node_lrgst
meta <- meta[,c(1,5,4,3,2)]
meta$name <- 0:(nrow(meta)-1)

g <- graph.data.frame(df, directed=FALSE, vertices = meta)
lo <- layout.norm(as.matrix(meta[,2:3]))

library(sp)
gg <- get.data.frame(g, "both")
vert <- gg$vertices
coordinates(vert) <- ~ lng + lat

edges <- gg$edges

edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ],
           vert[vert$name == edges[i, "to"], ]),
     "SpatialLines")
})


for (i in seq_along(edges)) {
  edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
}

edges <- do.call(rbind, edges)

location_label <- function(loc){
  paste0(
    loc
  )
}

edge_label <- function(weight){
  paste0(
    "Weighting: ",weight
  )
}

head(vert)

library(leaflet)
leaflet(vert) %>% addTiles() %>% addMarkers(data = vert, popup = ~location_label(Name_Common)) %>%
  addPolylines(data = edges, weight = 1, popup = edge_label(df$width))

