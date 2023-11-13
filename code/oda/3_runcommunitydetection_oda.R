##### Goal: use edge list to run leiden and louvain community detection #####

#### [0] Prep ####
library(igraph)
library(dplyr)
library(tidyr)


#### [1] Load data ####
edge_df <- read.csv("output/oda/edgelist_annual_oda.csv")

#### [2] Process data ####
# choose time period?
edge_t <- edge_df #%>% filter(TIME==2021)

# edge list --> network (sanity check)
net <- graph_from_edgelist(as.matrix(edge_t[,2:3]),directed = FALSE)
net <- set.edge.attribute(net,"weight",value=edge_t$Weight)
# plot(net)

# network --> df (for input to functions)
df_t <- as_long_data_frame(net)


#### [3] Run community detection ####
#### [3.1] Leiden ####
# pre-made function --> import from .py file?



source = "from_name"
target = "to_name"
weight = "weight"
leidenPartitions(df_t, source, target,weight)

# igraph built-in algorithms
g <- graph.data.frame(df_t)
get.adjacency(g,sparse=FALSE)



#### [3.2] Louvain ####
# pre-made function --> import from .py file?





# igraph built-in algorithms (input = network object)
cluster_louvain(net)



