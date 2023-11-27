## Network Community Detection
# making network

rm(list=ls())

library(dplyr)
library(igraph)

setwd("/Users/maryamtorkashvand/Library/CloudStorage/OneDrive-UniversityofIowa/Metacoupling_project/project/output")

# List all files with 'flows' in their name and ending with .csv
files <- list.files(pattern = "Flows_national.*\\.csv$")

# Directory to save the output files
output_dir <- "/Users/maryamtorkashvand/Library/CloudStorage/OneDrive-UniversityofIowa/Metacoupling_project/project/output" 

# Define and create a subfolder for processed networks
processed_networks_folder <- "Networks"
networks_folder_path <- file.path(output_dir, processed_networks_folder)
if (!dir.exists(networks_folder_path)) {
  dir.create(networks_folder_path)
}

edge.weights <- function(community, network, weight.within = 100, weight.between = 1) {
  bridges <- crossing(communities = community, graph = network)
  weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
  return(weights)
}

# Loop over each file
for (file in files) {
  # Read the CSV file
  un.sub.ag.m2.dist <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  # Filter the data
  un.sub.ag.m2.dist.yr <- un.sub.ag.m2.dist %>% 
    filter(Year == 2019) %>%
    filter(Exporter.M49 %in% c("Caribbean", "Central America", "Northern America", "South America")) %>%
    filter(Importer.M49 %in% c("Caribbean", "Central America", "Northern America", "South America"))

  #links
  un.link <- un.sub.ag.m2.dist.yr [,c(1,2,6)]
  
  ## count & aggregate
  un.sub.ag.m2.dist.ex <- un.sub.ag.m2.dist.yr %>%
    group_by(Exporter.ISO3, Exporter.M49, Year) %>% #,Weight
    dplyr::summarize(#n.exp=n(),
      sum.export.value = sum(Trade.Value.US)/10^6
      #avg.export.value = sum.export.value/n.exp
    )

  un.sub.ag.m2.dist.im <- un.sub.ag.m2.dist.yr %>%
    group_by(Importer.ISO3, Importer.M49, Year) %>% #.Weight
    dplyr::summarize(#n.imp=n(),
      # $ dollars to million dollars
      sum.import.value = sum(Trade.Value.US)/10^6
      #avg.import.value = sum.import.value/n.imp
    )

  #merge
  un.sub.ag.m2.dist.sum <- merge(un.sub.ag.m2.dist.ex, un.sub.ag.m2.dist.im,
                                 by.x=c("Exporter.ISO3", "Exporter.M49", "Year"),
                                 by.y=c("Importer.ISO3", "Importer.M49", "Year"), all=T)
  
  names(un.sub.ag.m2.dist.sum)[1:2] <- c("ISO3", "subregion")

  #subset and calculation
  un.sub.ag.m2.dist.sum$total.value <- un.sub.ag.m2.dist.sum$sum.export.value+un.sub.ag.m2.dist.sum$sum.import.value
  un.node <- un.sub.ag.m2.dist.sum %>% select(ISO3, total.value)
  
  #for reproducible purposes
  set.seed(23548723)

  un.link2 <- un.link[order(un.link$Exporter.ISO3,un.link$Importer.ISO3),]
  un.link2$lTrade.value.US <- un.link2$Trade.Value.US
  # 9 - Total.tonne, 10 - lTotal.tonne
  un.link2 <- un.link2[,c(-3)]
  
  # Create Graph
  un.net <- graph_from_data_frame(d=un.link2, vertices=un.node, directed=T)
  head (un.net)

  # Select only the required columns
  #network_data <- un.link2[, c("Exporter.ISO3", "Importer.ISO3", "ITrade.value.US")]

  # Rename columns to Exporter, Importer, and Weight
  #colnames(network_data) <- c("Exporter", "Importer", "Weight")

  # Write to CSV
  output_filename <- paste0(output_dir, "/", processed_networks_folder, "/", 
                            tools::file_path_sans_ext(basename(file)), "_network.csv")
  write.csv(un.link2, file = output_filename, row.names = FALSE)
  }

