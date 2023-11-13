
#### [0] Prep ####
getwd()

library(reshape2)
library(dplyr)
library(stringr)

#### [1] Load data ####
# Distance & Countries
dist <- read.csv("data/World_countries_eez_matrix_ISO3__1980_2010s.csv", header=T, stringsAsFactors=FALSE)
country <- read.csv("data/UNCTAD_country_code_hchl_dac.csv", header=T, stringsAsFactors=FALSE)
country.iso3 <- country$iso3

country.sub <- subset(country, !is.na(dac.code))

# Official Development Assistance
input <-read.csv("data/OECD_ODA_select_new_cnst21.csv")

# Foreign Direct Investment
# cdis <- read.csv("data/IMF_CDIS_select_new_cnst10.csv")

#### [2] Clean data ####
## subset
input.sub <- subset(input, DATATYPE=="A")

## add HCHL column
input.hchl <- merge(input.sub, country.sub[,c(5,6,1)], by.x="DONOR", by.y="dac.code")
input.hchl2 <- merge(input.hchl, country.sub[,c(5,6,1)], by.x="RECIPIENT", by.y="dac.code")

input.hchl3 <- unique(input.hchl2)

# names
names(input.hchl3)[23:26] <- c("Sender.HCHL", "Sender.ISO3", "Receiver.HCHL", "Receiver.ISO3")

## Delete same sender = receiver
input.hchl3$Value[which(input.hchl3$Sender.ISO3 == input.hchl3$Receiver.ISO3)] <- 0

## select only >0 columns
input.hchl3.sub <- subset(input.hchl3, Value > 0)

## add distant
input.hchl3.dist <- merge(input.hchl3.sub, dist, by.x=c("Sender.ISO3","Receiver.ISO3"), by.y=c("Sender.ISO3","Receiver.ISO3"), all.x=T)

# distant <- NA; adjacent <- 1
input.hchl3.dist$Weight[is.na(input.hchl3.dist$Weight)] <- "distant" # Change NA to "distant"

input.hchl3.dist$Weight[input.hchl3.dist$Weight == 1] <- "adjacent" # Change 1 to "adjacent"


## Add income & HC column separately
input.hchl3.dist$Sender.income <- str_sub(input.hchl3.dist$Sender.HCHL, 1,1)
input.hchl3.dist$Receiver.income <- str_sub(input.hchl3.dist$Receiver.HCHL, 1,1)

input.hchl3.dist$Sender.HC <- str_sub(input.hchl3.dist$Sender.HCHL, 3,5)
input.hchl3.dist$Receiver.HC <- str_sub(input.hchl3.dist$Receiver.HCHL, 3,5)

#########
# adj-dist & income, global level
## Value = million US$, constant
input.hchl3.dist.global.hl.flow <- input.hchl3.dist %>% 
  group_by(Sender.income, Receiver.income, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # million dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hl.exp <- input.hchl3.dist %>% 
  group_by(Sender.income, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hl.imp <- input.hchl3.dist %>% 
  group_by(Receiver.income, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

######
# adj-dist & hotspot, global level
input.hchl3.dist.global.hc <- input.hchl3.dist %>% 
  group_by(Sender.HC, Receiver.HC, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hc.exp <- input.hchl3.dist %>% 
  group_by(Sender.HC, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hc.imp <- input.hchl3.dist %>% 
  group_by(Receiver.HC, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

######
# adj-dist & hotspot & income, global level
input.hchl3.dist.global.hchl <- input.hchl3.dist %>% 
  group_by(Sender.HCHL, Receiver.HCHL, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hchl.exp <- input.hchl3.dist %>% 
  group_by(Sender.HCHL, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

input.hchl3.dist.global.hchl.imp <- input.hchl3.dist %>% 
  group_by(Receiver.HCHL, Weight, Year) %>% 
  dplyr::summarize(n=n(),
                   # $ dollars to million dollars
                   sum.value = sum(Value),
                   avg.value.pair = sum.value/n,
                   sum.value.cnst = sum(cnst2021Value),
                   avg.value.pair.cnst = sum.value.cnst/n)

## national-level column select
input.hchl3.dist.sub <- input.hchl3.dist[,c(4,1,25,28,30,3,2,26,29,31,27,14,9,10,12,16,18,21,24)]

##########
## write.csv
input.dir <- getwd()

# national
# setwd(paste0(input.dir,"csv_adjdist_country"))
write.csv(input.hchl3.dist.sub, paste0(input.dir,"/output/ODA_Flows_national.csv"), row.names=F)


# global
# setwd(paste0(input.dir,"csv_adjdist_global"))

# income
write.csv(input.hchl3.dist.global.hl.flow, paste0(input.dir,"/output/ODA_Flows_global_HL.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hl.exp, paste0(input.dir,"/output/ODA_Flows_global_HL_exp.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hl.imp, paste0(input.dir,"/output/ODA_Flows_global_HL_imp.csv"), row.names=F)

# hotspot
write.csv(input.hchl3.dist.global.hc, paste0(input.dir,"/output/ODA_Flows_global_HC.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hc.exp, paste0(input.dir,"/output/ODA_Flows_global_HC_exp.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hc.imp, paste0(input.dir,"/output/ODA_Flows_global_HC_imp.csv"), row.names=F)


# income & hotspot
write.csv(input.hchl3.dist.global.hchl, paste0(input.dir,"/output/ODA_Flows_global_HCHL.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hchl.exp, paste0(input.dir,"/output/ODA_Flows_global_HCHL_exp.csv"), row.names=F)

write.csv(input.hchl3.dist.global.hchl.imp, paste0(input.dir,"/output/ODA_Flows_global_HCHL_imp.csv"), row.names=F)


