install.packages(c("plyr", "dplyr", "stringr"))


#import libraries
library(plyr)
library(dplyr)
library(stringr)

# rm(list=ls())

file_path <- getwd()

# Step 1: Data_Processing
# reading the large daataset and producing clean .csv files

## country.list
dist <- read.csv(paste0(file_path,"/data/World_countries_eez_matrix_ISO3__1980_2010s.csv"), header=T, stringsAsFactors=FALSE)
country <- read.csv(paste0(file_path,"/data/UNCTAD_country_code_hchl_dac_m49.csv"), header=T, stringsAsFactors=FALSE)
country.iso3 <- country$iso3
country.uncode <- country$un.code

## rbind all years (2019-2021)
# input.dir <- "Path_of_input_files"
# setwd(paste0(input.dir))

un.data <- list.files(path = paste0(file_path,"/data/raw_data"),recursive=T, pattern="*.csv")

un.data.csv <- lapply(un.data, function(i) {read.csv(paste0(file_path,"/data/raw_data/",i) , header=T, stringsAsFactors=FALSE)})
un.data.all <- do.call("rbind", un.data.csv)
un.data.all2 <- subset(un.data.all, partner2Code==0 & motCode==0 & customsCode == "C00")

## subset; period; reporterCode; flowCode; partnerCode; classificationCode; cmdCode; primaryValue
un.data.sub <- un.data.all2[,c(7:10,13,15,31)]

## country selection (139 countries in 2020)
# Reporter.ISO (reporterCode)
un.data.sub2 <- subset(un.data.sub, subset=reporterCode %in% country.uncode)

# Partner.ISO (partnerCode)
un.data.sub3 <- subset(un.data.sub2, subset=partnerCode %in% country.uncode)

## add M49 column
un.data.sub4 <- merge(un.data.sub3, unique(country[,c(1,3,8)]), by.x="reporterCode", by.y="un.code")
un.data.sub5 <- merge(un.data.sub4, unique(country[,c(1,3,8)]), by.x="partnerCode", by.y="un.code")
un.data.sub5 <- unique(un.data.sub5)

## rearrange
un.data.sub6 <- un.data.sub5[,c(2,8,9,1,10,11,3:7)]

names(un.data.sub6)[c(2,5)] <- c("Reporter.ISO3", "Partner.ISO3")
names(un.data.sub6)[c(3,6)] <- c("Reporter.M49", "Partner.M49")

#### item.code
# total
un.total <- subset(un.data.sub6, cmdCode == "STOTAL")

# food: 1, 0, 22, 4
un.food <- subset(un.data.sub6, cmdCode == "S1" | cmdCode == "S0" | cmdCode == "S22" | cmdCode == "S4")

# crop: 04 (cereals), 05, (vegetables)
un.crop <- subset(un.data.sub6, cmdCode == "S04"  | cmdCode == "S05")

# meat: 01 (meat)
un.meat <- subset(un.data.sub6, cmdCode == "S01")

# animal feeding: 08
un.feed <- subset(un.data.sub6, cmdCode == "S08")

# fertilizer: 272 (fertilizers, crude), 56 (fertilizer, except grp 272)
un.fert <- subset(un.data.sub6, cmdCode == "S272" | cmdCode == "S56" )

# forest: 24 (cork and wood), 25 (pulp and waste paper)
un.forest <- subset(un.data.sub6, cmdCode == "S24" | cmdCode == "S25")

# water: 11101 (waters)
un.water <- subset(un.data.sub6, cmdCode == "S11101")

# Coal & Oil: 32, 33, 34
un.oil <- subset(un.data.sub6, cmdCode == "S32" | cmdCode == "S33"  | cmdCode == "S34" )

# Medical: 54, 554, 74183
un.medi <- subset(un.data.sub6, cmdCode == "S54"   | cmdCode == "S554"  | cmdCode == "S74183")


### select list without quotes
list.df <- list(un.total, un.food, un.crop, un.meat, un.feed, un.fert, un.forest, un.water, un.oil, un.medi)
list.un <- c("total","food", "crop", "meat", "feed", "fert", "forest", "water", "oil", "medi")


for(i in 1:length(list.df)) {
  
  name <- noquote(list.un[[i]])
  
  # load a data.frame
  un.sub <- list.df[[i]]
  
  # Aggregate
  un.sub.ag <- aggregate(cbind(primaryValue) ~ Reporter.ISO3 + Reporter.M49 + Partner.ISO3 + Partner.M49+ period + flowCode, FUN=sum, data=un.sub, na.action=na.omit)
  
  # select export & import; M (Import, 1) & X (Export, 2)
  un.sub.ag.ex <- subset(un.sub.ag, flowCode == "X")
  un.sub.ag.im <- subset(un.sub.ag, flowCode == "M")
  
  ## merge export & import
  # (export) Reporter.ISO -- Exporter; Partner.ISO -- Importer
  # (import) Partner.ISO -- Exporter; Reporter.ISO -- Importer
  un.sub.ag.m <- merge(un.sub.ag.ex[,-6], un.sub.ag.im[,-6], by.x=c("Reporter.ISO3","Reporter.M49","Partner.ISO3","Partner.M49","period"), by.y=c("Partner.ISO3","Partner.M49","Reporter.ISO3","Reporter.M49","period"), all=T)
  
  names(un.sub.ag.m) <- c("Exporter.ISO3","Exporter.M49","Importer.ISO3", "Importer.M49", "Year", "Trade.Value.US.Exp", "Trade.Value.US.Imp")
  #assign missing value to 0
  un.sub.ag.m$Trade.Value.US.Imp[is.na(un.sub.ag.m$Trade.Value.US.Imp)] <- 0
  un.sub.ag.m$Trade.Value.US.Exp[is.na(un.sub.ag.m$Trade.Value.US.Exp)] <- 0
  
  
  # if(Trade.Value.Imp > Trade.Value.Exp, Trade.Value.Imp, Trade.Value.Exp
  un.sub.ag.m$Trade.Value.US <- ifelse(un.sub.ag.m$Trade.Value.US.Imp > 0, un.sub.ag.m$Trade.Value.US.Imp, un.sub.ag.m$Trade.Value.US.Exp)
  
  # Select Trade.Value column only
  un.sub.ag.m2 <- un.sub.ag.m[,c(-6,-7)]
  
  
  ## Delete same sender = receiver
  un.sub.ag.m2$Trade.Value.US[which(un.sub.ag.m2$Exporter.ISO3 == un.sub.ag.m2$Importer.ISO3)] <- 0
  
  ## adj, dist merge
  # sending_ISO3 -- Sender.ISO3; receiving_ISO3 -- Receiver.ISO3
  un.sub.ag.m2.dist <- merge(un.sub.ag.m2, dist, by.x=c("Exporter.ISO3","Importer.ISO3"), by.y=c("Sender.ISO3","Receiver.ISO3"), all.x=T)
  
  # distant <- NA; adjacent <- 1
  un.sub.ag.m2.dist$Weight[is.na(un.sub.ag.m2.dist$Weight)] <- "distant" # Change NA to "distant"
  un.sub.ag.m2.dist$Weight[un.sub.ag.m2.dist$Weight == 1] <- "adjacent" # Change 1 to "adjacent"
  
  ## count & aggregate
  un.sub.ag.m2.dist.ex <- un.sub.ag.m2.dist %>%
    group_by(Exporter.ISO3, Exporter.M49, Year) %>% #,Weight
    dplyr::summarize(n.exp=n(),
                     sum.export.value = sum(Trade.Value.US)/10^6,
                     avg.export.value = sum.export.value/n.exp)
  
  un.sub.ag.m2.dist.im <- un.sub.ag.m2.dist %>%
    group_by(Importer.ISO3, Importer.M49, Year) %>% #.Weight
    dplyr::summarize(n.imp=n(),
                     # $ dollars to million dollars
                     sum.import.value = sum(Trade.Value.US)/10^6,
                     avg.import.value = sum.import.value/n.imp)
  
  # merge together
  un.sub.ag.m2.dist.all <- merge(un.sub.ag.m2.dist.ex, un.sub.ag.m2.dist.im,
                                 by.x=c("Exporter.ISO3", "Exporter.M49", "Year"),
                                 by.y=c("Importer.ISO3", "Importer.M49", "Year"), all=T)
  
  names(un.sub.ag.m2.dist.all)[c(1,2)] <- c("ISO3","M49")
  un.sub.ag.m2.dist.global.hl.flow <- un.sub.ag.m2.dist %>%
    group_by(Exporter.M49, Importer.M49, Year) %>% #Weight
    dplyr::summarize(n=n(),
                     # $ dollars to billion dollars
                     sum.value = sum(Trade.Value.US)/10^9,
                     avg.value.pair = sum.value/n)
  
  un.sub.ag.m2.dist.global.hl.exp <- un.sub.ag.m2.dist %>%
    group_by(Exporter.M49, Year) %>% #Weight
    dplyr::summarize(n=n(),
                     # $ dollars to billion dollars
                     sum.value = sum(Trade.Value.US)/10^9,
                     avg.value.pair = sum.value/n)
  
  un.sub.ag.m2.dist.global.hl.imp <- un.sub.ag.m2.dist %>%
    group_by(Importer.M49, Year) %>% #Weight
    dplyr::summarize(n=n(),
                     # $ dollars to billion dollars
                     sum.value = sum(Trade.Value.US)/10^9,
                     avg.value.pair = sum.value/n)
  # Saving Outputs
  # national flows
  write.csv(un.sub.ag.m2.dist, paste0("output/Trade_Flows_national_",list.un[[i]],".csv"), row.names=F)
  
  # Output for circos
  write.csv(un.sub.ag.m2.dist.global.hl.flow, paste0("output/Trade_Flows_circo_subregion_",list.un[[i]],".csv"), row.names=F)
  #summaries
  write.csv(un.sub.ag.m2.dist.global.hl.exp, paste0("output/Trade_Export_summary_",list.un[[i]],"_exp.csv"), row.names=F)
  write.csv(un.sub.ag.m2.dist.global.hl.imp, paste0("output/Trade_Import_summary_",list.un[[i]],"_imp.csv"), row.names=F)
  print(paste0(name, " saved"))
}
