### Nicole Keefner
### Master's Thesis



## Use *** to search for errors in the code or areas that need more work



## Set Working Directory



## Load packages
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)



## Import coral dataset



## Import sponge dataset
sponge_raw <- read.csv("Guana_Sponge_data_for_analysis.csv", header=T)

# Only select rows and columns for which there are values entered (sponge_raw has extraneous rows and columns)
sponge_raw <- sponge_raw[1:557,1:65]

# Over the years, people have called sites by different names. 
# Correct these entry mistakes by making names consistent
summary(sponge_raw$Site)
sponge_raw$Site <- revalue(sponge_raw$Site, c("Long Point" = "Muskmelon", "Pelican" = "Pelican Ghut"))

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
sponge_raw$Site <- as.character(sponge_raw$Site)
sponge_raw <- sponge_raw[sponge_raw$Site == "Muskmelon" | sponge_raw$Site == "Pelican Ghut" | sponge_raw$Site == "Crab Cove" | 
                           sponge_raw$Site == "Bigelow" | sponge_raw$Site == "Monkey Pt" | sponge_raw$Site == "Guana Head" |
                           sponge_raw$Site == "White Bay" | sponge_raw$Site == "Grand Ghut", ]
sponge_raw$Site <- as.factor(sponge_raw$Site)

# Put datast into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
sponge_raw_longform <- sponge_raw %>%
  gather(key = "Taxonomic_Group", value = "Count", Agelas.clathrodes..Agelas.citrina.or.Clathria.faviformis:Black..spiny..purple.exudate.but.not.slimy)

# Double-check
summary(sponge_raw_longform)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
sponge_raw_longform$Taxonomic_Group <- as.factor(sponge_raw_longform$Taxonomic_Group)

# Can also make Year a factor instead of an integer
sponge_raw_longform$Year <- as.factor(sponge_raw_longform$Year)

# Summary Information
str(sponge_raw_longform)
summary(sponge_raw_longform)
summary(sponge_raw_longform$Site)
summary(sponge_raw_longform$Year)

# Convert to wide form
sponge_raw_wideform <- spread(sponge_raw_longform, Taxonomic_Group, Count)
# ***Error because spread function doesn't work with duplicate row identifiers***
# In this case, sometimes the same transect was recorded 3 times in the same year at a given site e.g.:
# Transect  Site      Year  Taxonomic_Group Count
# 2         Crab Cove 1998  Agelas          0
# 2         Crab Cove 1998  Agelas          0

# Check that observations were made at every site for every year
#check1 <- unique(expand.grid(sponge_raw_longform$Year, sponge_raw_longform$Site, KEEP.OUT.ATTRS = TRUE))
# ***Error: cannot allocate vector of size 3.7 Gb; not enough ram?***
# 19 years and 8 sites = 152 observations
# because this dataset has ### unique observations, every site was visited for all 19 years?



## Import fish datasets (see "Fish Metadata.docx" for more information)
fish_codes <- read.csv("fish species codes 2018.csv", header = T)
fish_list <- read.csv("Fish species list.csv", header = T)
fish_raw <- read.csv("raw fish data nicole 2018.csv", header = T)

# Over the years, people have called sites by different names. 
# Correct these entry mistakes by making names consistent
summary(fish_raw$site)
fish_raw$site <- revalue(fish_raw$site, c("bigelow" = "Bigelow", "crab" = "Crab Cove", "grand" = "Grand Ghut", 
                                          "iguana" = "Guana Head", "monkey" = "Monkey Pt", "muskN" = "Muskmelon", 
                                          "pelican" = "Pelican Ghut", "pelican   " = "Pelican Ghut", "white" = "White Bay"))

# Only retain the 8 main sites:
# Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
fish_raw$site <- as.character(fish_raw$site)
fish_raw <- fish_raw[fish_raw$site == "Muskmelon" | fish_raw$site == "Pelican Ghut" | fish_raw$site == "Crab Cove" | 
                         fish_raw$site == "Bigelow" | fish_raw$site == "Monkey Pt" | fish_raw$site == "Guana Head" |
                         fish_raw$site == "White Bay" | fish_raw$site == "Grand Ghut", ]
fish_raw$site <- as.factor(fish_raw$site)

# Only retain observations where survey==main
fish_raw$survey <- as.character(fish_raw$survey)
fish_raw <- fish_raw[fish_raw$survey == "main", ]
fish_raw$survey <- as.factor(fish_raw$survey)

# Only retain observations where depth==30 (feet)
fish_raw <- fish_raw[which(fish_raw$depth == '30'), ]

# Put fish_raw into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
fish_raw_longform <- fish_raw %>% gather(key = "KEY", value = "Count", popaa:kysea)

# Split new KEY column into species_code and age_class columns
KEY <- as.character(fish_raw_longform$KEY)
fish_split <- data.frame("Species_Code" = substr(KEY, 1, (nchar(KEY)-1)), "Age_Class" = substr(KEY, nchar(KEY), nchar(KEY)))

# Combine long-form dataset with the split columns dataframe
fish_raw_longform <- cbind(fish_raw_longform, fish_split)

# Rename column in fish_codes dataframe to use as a key to merge this information with the raw data
colnames(fish_codes)[colnames(fish_codes) == "ï..new_code"] <- "KEY"
fish_raw_longform <- merge(fish_raw_longform, fish_codes, by = "KEY", all = T)

# Rename column in fish_list dataframe to use as a key to merge this information with the raw data + fish code info
colnames(fish_list)[colnames(fish_list) == "f_code"] <- "KEY"
fish_raw_longform <- merge(fish_raw_longform, fish_list, by = "KEY", all = T)

# Remove observations where Count is NA
fish_raw_longform <- fish_raw_longform[!is.na(fish_raw_longform$Count),]

# Keep only the columns that I think might be relevant
fish_raw_longform_reduced <- fish_raw_longform[,c("ï..notes", "duration", "year", "month", "day", "site", "transect", 
                                "fixed_transect", "Count", "Species_Code", "Age_Class", "Family.x", "Family.y", "Family2", 
                                "Commonname", "scientific.name", "Trophic.Level", "Max.body.size", "Dietary.group")]

# Can also make Year a factor instead of an integer
fish_raw_longform_reduced$year <- as.factor(fish_raw_longform_reduced$year)

# Summary Information
str(fish_raw_longform_reduced)
summary(fish_raw_longform_reduced)
summary(fish_raw_longform_reduced$site)
summary(fish_raw_longform_reduced$year)

# Keep only the columns that I need to convert to wide form
fish_raw_longform_minimum <- fish_raw_longform_reduced[,c("ï..notes", "year", "month", "day", "site", "transect", 
                                                  "fixed_transect", "Count", "Species_Code", "Age_Class")]

# Convert to wide form
fish_raw_wideform <- spread(fish_raw_longform_mini, Species_Code, Count)
# Note that there are 2 times more observations now than in the original raw wide format - 
summary(fish_raw$site)
summary(fish_raw_wideform$site)
# this is because A/J are in different rows, not listed in different columns

# Check that observations were made at every site for every year
#check2 <- unique(expand.grid(fish_raw_wideform$year, fish_raw_wideform$site, KEEP.OUT.ATTRS = TRUE))
# 25 years and 8 sites = 200 observations
# because this dataset has 200 unique observations, every site was visited for all 25 years

# ***Add transect length from sponge dataset to the corresponding observations in the fish dataset?***
summary(fish_raw_longform_reduced$transect)
summary(sponge_raw_longform$Transect)
# Because the levels are not the same, this may be difficult



## Summary of Importing Datasets
# ***Check that each year for each site there are 3 transects***
# *** name of coral data for analysis
# sponge_raw_longform for analysis
# fish_raw_longform_reduced for analysis



## Total Counts
# Calculate total counts
# Total counts by year
# Total counts by site
# Total counts by Year and Site


