rm(list=ls())

library(igraph)
#library(rgexf)
#library(qgraph)
#library(network)
#library(sna)
#library(ndtv)
#library(visNetwork)
#library(GGally)
#library(igraphdata)

########################
## edge.list function ##
########################
edge.weights <- function(community, network, weight.within = 100, weight.between = 1) {
bridges <- crossing(communities = community, graph = network)
weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
return(weights) 
}
###########################

setwd("E:/2_my_project/3_metacoupling/IGUIDE/sample_code/community_detection")

meat.node <- read.csv("Meat_2015_node.csv", header=T, stringsAsFactors=FALSE, check.names=FALSE)
meat.link <- read.csv("Meat_2015_link.csv", header=T, stringsAsFactors=FALSE, check.names=FALSE)

#for reproducible purposes
set.seed(23548723)

meat.link <- meat.link[order(meat.link$sender,meat.link$receiver),]

#### Delete LUX
## Node
meat.node <- meat.node[which(meat.node$ISO3 != "LUX"),]
meat.node <- meat.node[which(meat.node$ISO3 != "BEL"),]

## Link
meat.link <- meat.link[which(meat.link$sender != "LUX"),]
meat.link <- meat.link[which(meat.link$receiver != "LUX"),]

meat.link <- meat.link[which(meat.link$sender != "BEL"),]
meat.link <- meat.link[which(meat.link$receiver != "BEL"),]

#### Select colum and log transformation ####
## Total Export + Import with log transformation
# Node
meat.node$Total.trade <- meat.node$PM_exp + meat.node$PM_imp + meat.node$RM_exp + meat.node$RM_imp
meat.node$lTotal.trade <- meat.node$Total.trade
# 8- Total.trade, 9 - lTotal.trade; 7 - Cluster results
meat.node <- meat.node[,c(1,10,7)]

# Link
meat.link$lTotal.tonne <- meat.link$Total.tonne
# 9 - Total.tonne, 10 - lTotal.tonne
meat.link <- meat.link[,c(5,6,10)]


##### Create Graph
meat.net <- graph_from_data_frame(d=meat.link, vertices=meat.node, directed=T)
meat.net

#http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html
V(meat.net) # nodes
vertex_attr(meat.net) # all attributes of the nodes
E(meat.net) # edges
E(meat.net)$weight # weights for each edge
edge_attr(meat.net) # all attributes of the edges
meat.net[] # adjacency matrix

##!!!!!!!!!!###
E(meat.net)$weight <- E(meat.net)$lTotal.tonne

meat.net <- simplify(meat.net, remove.multiple=F, remove.loops=T)
#Kliquefinder.community <- make_clusters(unwto.net, membership=V(unwto.net)$K00_13)


#### Community Detection Algorithms
# http://francescopochetti.com/community-detection-social-networks/
# https://stackoverflow.com/questions/9471906/what-are-the-differences-between-community-detection-algorithms-in-igraph
# https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/

# edge.betweenness.community; fastgreedy.community; label.propagation.community; leading.eigenvector.community; multilevel.community; optimal.community; spinglass.community; infomap.community; walktrap.community
#meat.community <- walktrap.community(meat.net)
#meat.community
#modularity(meat.community)
meat.community <- make_clusters(meat.net, membership=V(meat.net)$C2015_op)

#### Export community results to csv
#https://stackoverflow.com/questions/40246762/igraph-write-communities
#com <- membership(meat.community)
com <- cbind(V(meat.net)$name, meat.community$membership)
com <- as.data.frame(com)
com
names(com) <- c("ISO3", "Group")
#write.csv(com, "community_results_2015.csv")


####################
### Draw Figures ###
#http://stackoverflow.com/questions/30376904/changing-node-vertice-opacity-in-igraph-in-r
#prettyColors <- palette(rainbow(7))
prettyColors <- c("olivedrab1","khaki1","indianred1","cornflowerblue")
##2344EA blue; #FF0000 red
meat.col <- prettyColors[membership(meat.community)] 

### Coreness ####
#http://pablobarbera.com/big-data-upf/html/02b-networks-descriptive-analysis.html
V(meat.net)$coreness <- coreness(meat.net)
E(meat.net)$coreness <- coreness(meat.net)

# layout functions : http://igraph.org/r/doc/
#unwto.Layout.kk <- layout.kamada.kawai(unwto.net, weights = E(unwto.net)$weight, niter=1000)
#unwto.Layout.auto <- layout.auto(unwto.net, weights = E(unwto.net)$weight, niter=1000)

#E(meat.net)$weight <- edge.weights(meat.community, meat.net, weight.within=5)

#meat.Layout.drl <- layout_with_drl(meat.net, weights = E(meat.net)$weight)

#meat.Layout <- layout_with_fr(meat.net, coords= meat.Layout.drl, weights = E(meat.net)$coreness, start.temp=0.1, niter=1000)

#meat.Layout2 <- as.data.frame(meat.Layout)
#meat.Layout2$ISO3 <- meat.node$ISO3
#meat.Layout2$lTotal.trade <- meat.node$lTotal.trade
#meat.Layout2$C2015_op <- meat.node$C2015_op
#write.csv(meat.Layout2, "meat_layout_2015_5_1000.csv", row.names=F)

#!!!!!!!!
meat.Layout1 <- read.csv("meat_layout_2015_5_1000_revised.csv", header=T, stringsAsFactors=FALSE, check.names=FALSE)
meat.Layout <- as.matrix(meat.Layout1)

## Change coordinates 9 - BEL; 97 - NPL
# RWA, compare with UGA 125
#meat.Layout[107,1] <- 0.55
#meat.Layout[107,2] <- 0.77

# STP
#meat.Layout[114,1] <- -0.4
#meat.Layout[114,2] <- -0.95

# start.temp=5, grid="nogrid"
#layout.fruchterman.reingold
#layout_with_fr
 xmin <- min(meat.Layout[,1])
 xmax <- max(meat.Layout[,1])
 ymin <- min(meat.Layout[,2])
 ymax <- max(meat.Layout[,2])

############ characteristics
# Compute node degrees (#links) and use that to set node size:
trade.sum <- (V(meat.net)$lTotal.trade)
#V(meat.net)$lTotal.sum <- V(meat.net)$lTotal.trade
V(meat.net)$size <- log1p((V(meat.net)$lTotal.trade))*0.35

V(meat.net)$color <- meat.col

# Set edge width based on weight:
#E(meat.net)$width <- V(meat.net)$lTotal.trade*0.001

# Setting them to NA will render no labels:
V(meat.net)$label.color <- "black"
V(meat.net)$label <- V(meat.net)$name
V(meat.net)$label.cex <- log1p(V(meat.net)$lTotal.trade)*0.065
#V(meat.net)$label.cex <- 0.5
V(meat.net)$label.family <- "ArialMT"
V(meat.net)$label.font <- 2

### Plot
# options: http://igraph.org/r/doc/plot.common.html
# temporal networks in R: http://estebanmoro.org/2015/12/temporal-networks-with-r-and-igraph-updated/
g <- plot(x=meat.community, y=meat.net,
 			layout=meat.Layout, 
 			mark.groups=NULL, 
 			edge.color = c("tomato2", "darkgrey")[crossing(meat.community, meat.net)+1], 
 			edge.arrow.size=0, 
 			rescale=F, 
 			xlim=c(xmin,xmax),ylim=c(ymin,ymax), 
 			asp=0, 
 			col = meat.col,
 			vertex.frame.color = meat.col)
# vertex.frame.width=0)#,vertex.shape=meat.shp) #,  area=vcount(unwto.net)^2)
#vertex.label.font=c(1,2,3,4), # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol

#plot(meat.net, vertex.label=NA, vertex.size=5, vertex.color=membership(meat.community), layout=layout.fruchterman.reingold)

dev.copy2pdf(file = "Meat_network_2015_5_1000.pdf", height=10, width=10)

#mark.groups=Kliquefinder.community; color=Kliquefinder.col, tomato2

## Export unwto.net to Gephi
unwto.net.G <- igraph.to.gexf(unwto.net)
mygexf <- write.gexf(nodes=V(unwto.net), edges=E(unwto.net))