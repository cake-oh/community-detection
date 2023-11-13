##### Goal: create edge lists for ODA data; both aggregated and annual #####

#### [0] Prep ####
library(igraph)
library(dplyr)
library(tidyr)

#### [1] Load data ####
df <- read.csv("output/ODA_Flows_national.csv")

#### [2] Create Edge + Node List ####
# Edge data target columns: ID, i (sender #), j (receiver #), time (year), sender (country), receiver (country), RM.tonne

nrow(unique(df[c("Sender.ISO3","Receiver.ISO3")])) # check how many unique combos (edges) = 5256 over all time periods

# edge dataframe across all time periods
edge_df <- df %>% select(TIME,Sender.ISO3,Receiver.ISO3,DONOR,RECIPIENT,Weight,Value) %>% 
  group_by(Sender.ISO3,Receiver.ISO3) %>% 
  mutate(ID = cur_group_id(), # ID of edge 
         transactions=n(), # number of transactions per edge
         Value=sum(Value)) %>% # sum of value per edge
  distinct(ID,.keep_all=TRUE) %>% # only keep one row per edge ID
  arrange(ID,TIME) %>% rename(Distance=Weight,Weight=Value) # rename columns

# edge dataframe with years - basically same as input dataset
edge_df_annual <- df %>% select(TIME,Sender.ISO3,Receiver.ISO3,DONOR,RECIPIENT,Weight,Value) %>% 
  group_by(Sender.ISO3,Receiver.ISO3,TIME) %>% 
  mutate(ID = cur_group_id(), # ID of edge 
         transactions=n(), # number of transactions per edge
         Value=sum(Value)) %>% # sum of value per edge
  distinct(ID,.keep_all=TRUE) %>% # only keep one row per edge ID
  arrange(ID,TIME) %>% rename(Distance=Weight,Weight=Value) # rename columns



#### [3] Save edge lists ####
# write.csv(edge_df,"output/edgelist_oda.csv",row.names = FALSE) # aggregated
# write.csv(edge_df_annual,"output/edgelist_annual_oda.csv",row.names = FALSE) # annual
