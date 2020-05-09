library(data.table)
library(igraph)
library(networkD3)
library(geosphere)
library(magrittr)
library(htmlwidgets)
library(scales)

rm(list = ls())
gc()

md <- fread("/Users/stuartbarker/Downloads/trees.csv", sep = ";")
nrow(md)
head(md)

setDT(md)[, paste0("loc", 1:2) := tstrsplit(geo_point_2d, ",")]

#Clean
colnames(md)[23] <- "Name_Common"
colnames(md)[24] <- "Name_Full"
md$lat <- as.numeric(md$loc1)
md$lng <- as.numeric(md$loc2)

#Find avg crown width 
#cw <- subset(md, md$`Crown width` != "No", select = c(`Crown width`))
#cw$`Crown width` <- as.numeric(cw$`Crown width`)
#summary(cw) #Mean   : 7.964  

#Subset
md_sub <- subset(md, select = c(Name_Full, Name_Common, lat, lng,`Crown width`))
colnames(md_sub)[5] <- "cw"

#Input cw mean (could do this by Name but i'll just take overall average)
md_sub$cw_new <- md_sub$cw
#md_sub$cw_new[which(md_sub$cw == "No")] = "7.964"
#md_sub$cw_new[which(md_sub$cw == "")] = "7.964"
md_sub <- subset(md_sub, cw_new != "No")
md_sub <- subset(md_sub, cw_new != "")
md_sub$cw_new <- as.numeric(md_sub$cw_new)
summary(md_sub)

#Remove trees with no names
md_sub <- subset(md_sub, Name_Common != "")

#Create unique tree id
md_sub$id <- 0:(nrow(md_sub)-1)
md_sub$ID_NAME <- paste0(md_sub$Name_Common, " ID:", md_sub$id)

#Find close distances
lnks <- NULL
x <- 0
for(i in 1:nrow(md_sub)) {
  input <- md_sub[i, ]
  compare <- md_sub[-i, ]
  compare$dist <-
    distHaversine(c(input$lng, input$lat), compare[, 4:3])
  compare$dist_max <- (input$cw_new/2) + (compare$cw_new / 2)
  tmp <- subset(compare, dist <= dist_max)
  if (nrow(tmp) > 0) {
    print(paste0(nrow(tmp), " links"))
    for (z in 1:nrow(tmp)) {
      x <- x + 1
      
      lnks$source[x] <- input$Name_Common
      lnks$target[x] <- tmp$Name_Common[z]
      lnks$value[x] <- 1
      lnks$source_id[x] <- input$ID_NAME
      lnks$target_id[x] <- tmp$ID_NAME[z]
    }
    
  }
}

#save.image(file = "lnks.RData")

lnks <- as.data.frame(lnks)
lnks$value <- 1
sum(lnks$value)

agg <- aggregate(value ~ source + target, lnks, sum)
agg$same <- as.character(agg$source) == as.character(agg$target)
agg <- subset(agg, same != TRUE)
agg$same <- NULL
agg <- subset(agg, value > 9)

nodes <- as.data.frame(unique(agg$source)) 
colnames(nodes)[1] <- "name"
nodes1 <- as.data.frame(unique(agg$target)) 
colnames(nodes1)[1] <- "name"
nodes <- as.data.table(rbind(nodes,nodes1))
nodes <- nodes[!duplicated(nodes), ]

nodes$node <- 0:(nrow(nodes)-1)
nodes <- nodes[,2:1]

#Sum links 
lnks_agg <- aggregate(value ~ source + target, lnks, sum)
lnks_agg$same <- as.character(lnks_agg$source) == as.character(lnks_agg$target)
lnks_agg <- subset(lnks_agg, same != TRUE)
lnks_agg$same <- NULL
lnks_agg <- subset(lnks_agg, value > 9)
lnks_agg <- merge(lnks_agg, nodes, by.x = "source", by.y = "name")
lnks_agg <- merge(lnks_agg, nodes, by.x = "target", by.y = "name")
lnks_agg$target <- NULL
lnks_agg$source <- NULL
colnames(lnks_agg)[2] <- "source"
colnames(lnks_agg)[3] <- "target"
lnks_agg <- lnks_agg[,c(2,3,1)]

#Remove opposite links (swap the table round and rbind)
lnks_agg_opp <- lnks_agg[c(2,1,3)]
colnames(lnks_agg_opp)[1] <- "source"
colnames(lnks_agg_opp)[2] <- "target"
lnks_agg <- rbind(lnks_agg,lnks_agg_opp)
nrow(lnks_agg_opp)
nrow(lnks_agg)
lnks_agg <- lnks_agg[!duplicated(lnks_agg), ]
nrow(lnks_agg)

#Reduce Links size to make it easier to view
#lnks_agg$value <- lnks_agg$value * 0.1
lnks_agg$scale_value <- scales::rescale(lnks_agg$value, c(5,100))


# Plot
forceNetwork(Links = lnks_agg, Nodes = nodes,
             Source = "source", Target = "target",
             Value = "scale_value", NodeID = "name",
             Group = "name", opacity = 1, charge = -1000, zoom = T) #%>%
  saveNetwork(file = 'bristol_network.html')


#Overall
#Use lnks prior to agg 
head(lnks)

node_all <- subset(md_sub, select = c(ID_NAME,Name_Common,lat,lng))
colnames(node_all)[1] <- "node"
node_all <- node_all[!duplicated(node_all), ]

lnks_all <- subset(lnks, select = c(source_id,target_id,value))

node_all <- node_all[node_all$node %in% lnks_all$source_id | node_all$node %in% lnks_all$target_id]

#Tidy up node id
node_all$id_new <- 0:(nrow(node_all)-1)
node_all_sub <- subset(node_all, select = c(node, id_new))
lnks_all <- merge(lnks_all, node_all_sub, by.x = "source_id", by.y = "node")
lnks_all$source_id <- NULL
colnames(lnks_all)[3] <- "source"

lnks_all <- merge(lnks_all, node_all_sub, by.x = "target_id", by.y = "node")
lnks_all$target_id <- NULL
colnames(lnks_all)[3] <- "target"

#node_all$node <- NULL
colnames(node_all)[5] <- "node_id"

forceNetwork(Links = lnks_all, Nodes = node_all,
             Source = "source", Target = "target",
             Value = "value", NodeID = "node", charge = 0, bounded = T,
             Group = "Name_Common", opacity = 0.8, zoom = F) %>%
  saveNetwork(file = 'bristol_network_indiv.html')

grph <- graph_from_data_frame(lnks_all[,2:3], directed = F)
grph <- simplify(grph)
cfg <- cluster_fast_greedy(grph)
cm <- communities(cfg)
length(cfg)
membership(cfg)
sizes(cfg)

x <- which.max(sizes(cfg))

subg <- induced.subgraph(grph, which(membership(cfg) == x))

lrgst <- networkD3::igraph_to_networkD3(subg)
lrgst_lnks <- lrgst$links
lrgst_lnks$value <- 1

node_lrgst <- lrgst$nodes
node_lrgst <- merge(node_lrgst, node_all, by.x = "name", by.y = "node_id")

#Re Index
forceNetwork(Links = lrgst_lnks, Nodes = node_lrgst,
             Source = "source", Target = "target",
             NodeID = "node", 
             charge = -10,
             Group = "Name_Common", opacity = 1, legend = T, zoom = T)  %>%
  saveNetwork(file = 'bristol_network_largest.html')

n <- length(sizes(cfg))
x <- sort(sizes(cfg),partial=n-1)[n-2]
x 
x <- 6


subg <- induced.subgraph(grph, which(membership(cfg) == x))

lrgst <- networkD3::igraph_to_networkD3(subg)
lrgst_lnks <- lrgst$links
lrgst_lnks$value <- 1

node_lrgst <- lrgst$nodes
node_lrgst <- merge(node_lrgst, node_all, by.x = "name", by.y = "node_id")

forceNetwork(Links = lrgst_lnks, Nodes = node_lrgst,
             Source = "source", Target = "target",
             NodeID = "node",  
             charge = -10,
             Group = "Name_Common", opacity = 1, legend = T, zoom = T)  %>%
  saveNetwork(file = 'bristol_network_2largest.html')

x <- sort(sizes(cfg),partial=n-1)[n-3]
x
x <- 3


subg <- induced.subgraph(grph, which(membership(cfg) == x))

lrgst <- networkD3::igraph_to_networkD3(subg)
lrgst_lnks <- lrgst$links
lrgst_lnks$value <- 1

node_lrgst <- lrgst$nodes
node_lrgst <- merge(node_lrgst, node_all, by.x = "name", by.y = "node_id")

forceNetwork(Links = lrgst_lnks, Nodes = node_lrgst,
             Source = "source", Target = "target",
             NodeID = "node",    
             charge = -10,
             Group = "Name_Common", opacity = 1, legend = T, zoom = T)  %>%
  saveNetwork(file = 'bristol_network_3largest.html')

#Try Sankey
networkD3::sankeyNetwork(Links = lnks_agg, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name', sinksRight = T)

