library(data.table)
library(igraph)
library(networkD3)
library(geosphere)
library(magrittr)
library(htmlwidgets)
library(scales)
library(igraph)
library(dplyr)

# install and load 'RBioFabric' from GitHub
library(RBioFabric)

set.seed(123)

head(lnks)

node_all <- subset(md_sub, select = c(ID_NAME,Name_Common,lat,lng))

colnames(node_all)[1] <- "node"
node_all <- node_all[!duplicated(node_all), ]

lnks_all <- subset(lnks, select = c(source_id,target_id,value))

#Remove subset
#cnt <- as.data.frame(table(lnks_all$source_id))
#cnt <- subset(cnt, Freq > 0)

#lnks_all <- lnks_all[lnks_all$source_id %in% cnt$Var1,]

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

colnames(node_all)[5] <- "node_id"

head(lnks_all)
head(node_all)

#Prep for software
lcyt <- lnks_all
ncyt <- node_all

lcyt$value <- NULL

lcyt <- merge(lcyt,ncyt[,c("node", "Name_Common", "node_id")], by.x = "source", by.y = "node_id")
head(lcyt)
colnames(lcyt)[3] <- "source_label_detail"
colnames(lcyt)[4] <- "source_label_overview"

lcyt <- merge(lcyt,ncyt[,c("node", "Name_Common", "node_id")], by.x = "target", by.y = "node_id")
head(lcyt)
colnames(lcyt)[5] <- "target_label_detail"
colnames(lcyt)[6] <- "target_label_overview"

lcyt$ID <- 1:nrow(lcyt)

head(lcyt)

#Remove just one occurence 
cnt <- as.data.frame(table(lcyt$source_label_overview))
cnt <- subset(cnt, Freq > 3)
lcyt <- lcyt[lcyt$source_label_overview %in% cnt$Var1,]
cnt <- as.data.frame(table(lcyt$target_label_overview))
cnt <- subset(cnt, Freq > 3)
lcyt <- lcyt[lcyt$target_label_overview %in% cnt$Var1,]
cnt <- as.data.frame(table(lcyt$source_label_detail))
cnt <- subset(cnt, Freq > 3)
lcyt <- lcyt[lcyt$source_label_detail %in% cnt$Var1,]
cnt <- as.data.frame(table(lcyt$target_label_detail))
cnt <- subset(cnt, Freq > 3)
lcyt <- lcyt[lcyt$target_label_detail %in% cnt$Var1,]

#Add colour names 
color <- as.data.frame(colors(distinct = T))
sceunique <- as.data.frame(unique(lcyt$source_label_overview))
colnames(sceunique)[1] <- "label_overview"
tarunique <- as.data.frame(unique(lcyt$target_label_overview))
colnames(tarunique)[1] <- "label_overview"
sceunique <- rbind(sceunique, tarunique)
sceunique <- as.data.frame(sceunique[!duplicated(sceunique), ])

#color <- cbind(sceunique, sample_n(color, nrow(sceunique)))
color <- cbind(sceunique, 1:nrow(sceunique))
colnames(color)[1] <- "name"
colnames(color)[2] <- "source_color"

lcyt <- merge(lcyt,color, by.x = "source_label_overview", by.y = "name")
colnames(color)[2] <- "target_color"
lcyt <- merge(lcyt,color, by.x = "target_label_overview", by.y = "name")
head(lcyt)

lcyt$source_color <- NULL
lcyt$target_color <- NULL

write.table(lcyt,"lcyt_text.txt", sep="\t", row.names = FALSE)
write.csv(lcyt, "lcyt.csv", row.names = F)

write.csv(lnks_all, "lnks.csv", row.names = F)
write.csv(node_all, "node.csv", row.names = F)

#node_all$node <- NULL
####New Names and Size
grph <- graph_from_data_frame(lnks_all[,2:3], directed = F)
grph <- simplify(grph)
cfg <- cluster_fast_greedy(grph)
cm <- communities(cfg)
length(cfg)
membership(cfg)
sz <- sizes(cfg)

x <- sizes(cfg)[sizes(cfg) > 0]

subg <- induced.subgraph(grph, which(membership(cfg) %in% x))

lrgst <- networkD3::igraph_to_networkD3(subg)
lrgst_lnks <- lrgst$links

head(lnks_all)
head(node_all)



write.csv(lrgst_lnks, "lnks_sub.csv", row.names = F)

library(sigmajs)
g2 <- layout_(g,as_star())

plot(layout_with_lgl(g))

# generate data
noda <- sg_make_nodes(20) # 20 nodes
edga <- sg_make_edges(noda, 50) # 50 edges

sig <- sigmaFromIgraph(g)
sig

node_dup <- merge(lnks_all[,c("source", "target")], node_all[,c("node", "node_id")], by.x = "source", by.y = "node_id")
node_dup$ID <- 1:(nrow(node_dup))

lnks_all$ID <-  1:(nrow(lnks_all))
lnks_all_sub$source <- lnks_all_sub$source + 1
lnks_all_sub$target <- lnks_all_sub$target + 1

lnks_all_sub <- lnks_all[ lnks_all$source %in% node_all$node_id ,]
lnks_all_sub <- lnks_all_sub[ lnks_all_sub$target %in% node_all$node_id ,]

node_all$ID <- 1:nrow(node_all)

sigmajs() %>% # initialise
  sg_nodes(node_all, id = ID) %>% # add nodes
  sg_edges(lnks_all_sub, id = ID, source = source, target = target) %>% # add edges
  sg_layout() %>%  # layout
  sg_cluster() %>% # cluster
  sg_drag_nodes() %>% # allows user to drag nodes
  sg_neighbours() # show node neighbours on node click

sigmajs() %>% # initialise
  sg_nodes(noda, id = id, label = label) %>% # add nodes
  sg_edges(edga, id = id, source = source, target = target) %>% # add edges
  sg_layout() %>%  # layout
  sg_cluster() %>% # cluster
  sg_drag_nodes() %>% # allows user to drag nodes
  sg_neighbours() # show node neighbours on node click


g <- graph_from_data_frame(lnks_all[,2:3], directed = T)
g1 <- simplify(g)
cfg <- cluster_fast_greedy(g1)


plot(g1)
png("ig_ex.png")
plot(g3)
dev.off()

gparm<- mst.plot.mod(g1, #nodes.color=color.list$citynight, 
                            #lab.cex=1, lab.color="white", v.sf=0, 
                            layout.function=layout.fruchterman.reingold)


xx<-plot.modules(g1, v.size=1,
                 layout.function=layout.graphopt)

hc <- rgb(t(col2rgb(heat.colors(20)))/255,alpha=.2)
cl <- rgb(r=0, b=.7, g=1, alpha=.05)
xx <- mst.plot.mod(g1, vertex.color=cl, v.size=3, sf=-20,
                      colors=hc, e.size=.5, mst.e.size=.75,
                      layout.function=layout.fruchterman.reingold)


# This example has 1000 nodes, just like the provided example, but it 
# adds 6 edges in each step, making for an interesting shape; play
# around with different values.

# bfGraph = barabasi.game(1000, m=6, directed=FALSE)

# Plot it up! For best results, make the PDF in the same
# aspect ratio as the network, though a little extra height
# covers the top labels. Given the size of the network,
# a PDF width of 100 gives us good resolution.

height <- vcount(bfGraph)
width <- ecount(bfGraph)
aspect <- height / width;
plotWidth <- 100.0
plotHeight <- plotWidth * (aspect * 1.2)
pdf("myBioFabricOutput.pdf", width=plotWidth, height=plotHeight)
bioFabric(bfGraph)
dev.off()

simpleNetwork(lnks_all[,2:3])%>%
  saveNetwork(file = 'bristol_simple.html')

library(webshot)
webshot("bristol_simple.html", zoom = 1, delay = (60), file = "ex2simp.png")

id <- plot.modules(g1, layout.function = layout.graphopt, modules.color = cl,
                   mod.edge.col=c("green","darkgreen") , tkplot=FALSE, ed.color = c("blue"),sf=-25)
