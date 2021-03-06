### Nicole Keefner
### Master's Thesis



## Use *** to search for errors in the code or areas that need more work



## Set Working Directory



## Load packages
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)



## Import coral dataset



## Import sponge dataset
sponge_raw <- read.csv("Guana_Sponge_data_for_analysis.csv", header = T)

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

# Some "ghost" factors are being retained for transect and observer
# Remove these ghost factors
sponge_raw$Observer <- as.character(sponge_raw$Observer)
sponge_raw <- sponge_raw[sponge_raw$Observer == "E MacLean" | sponge_raw$Observer == "L Jarecki", ]
sponge_raw$Observer <- as.factor(sponge_raw$Observer)

sponge_raw$Transect <- as.character(sponge_raw$Transect)
sponge_raw <- sponge_raw[sponge_raw$Transect == "1" | sponge_raw$Transect == "2" | sponge_raw$Transect == "3" | 
                           sponge_raw$Transect == "4" | sponge_raw$Transect == "T", ]
sponge_raw$Transect <- as.factor(sponge_raw$Transect)

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
sponge_raw <- transform(sponge_raw, Site_Year = paste(sponge_raw$Year, sponge_raw$Site, sep="_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
sponge_raw$Taxa <- "Sponge"

# Put dataset into long form (i.e. so species codes are in a single column rather than having one column for each species code)
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
# ***So, need to figure out how to group by site-year
# In this case, sometimes the same transect was recorded 3 times in the same year at a given site e.g.:
# Transect  Site      Year  Taxonomic_Group Count
# 2         Crab Cove 1998  Agelas          0
# 2         Crab Cove 1998  Agelas          0

# Check that observations were made at every site for every year
# 19 years and 8 sites = 152 observations expected
check1 <- unique(sponge_raw_longform$Site_Year)
# because check1 has 150 levels, almmost every site was visited for all 19 years
# After closer inspection, 1993_Crab Cove and 2014_Pelican Ghut are missing

# Create new subset for all the times where transect length != 30 m
not30 <-sponge_raw[sponge_raw$Transect.Length..m. != "30", 1:7]


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

# Only retain observations where survey == main
fish_raw$survey <- as.character(fish_raw$survey)
fish_raw <- fish_raw[fish_raw$survey == "main", ]
fish_raw$survey <- as.factor(fish_raw$survey)

# Only retain observations where depth == 30 (feet)
fish_raw <- fish_raw[which(fish_raw$depth == '30'), ]

# Create new column called "Site_Year" that combines the year and site as ####_Sitename
fish_raw <- transform(fish_raw, Site_Year = paste(fish_raw$year, fish_raw$site, sep = "_"))

# Create new column in all datasets called Taxa, so if dataframes are combined, I know which dataset the information came from
fish_raw$Taxa <- "Fish"

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
colnames(fish_codes)[colnames(fish_codes) == "�..new_code"] <- "KEY"
fish_raw_longform <- merge(fish_raw_longform, fish_codes, by = "KEY", all = T)

# Rename column in fish_list dataframe to use as a key to merge this information with the raw data + fish code info
colnames(fish_list)[colnames(fish_list) == "f_code"] <- "KEY"
fish_raw_longform <- merge(fish_raw_longform, fish_list, by = "KEY", all = T)

# Remove observations where Count is NA
fish_raw_longform <- fish_raw_longform[!is.na(fish_raw_longform$Count),]

# Keep only the columns that I think might be relevant
fish_raw_longform_reduced <- fish_raw_longform[,c("�..notes", "duration", "year", "month", "day", "site", "transect", 
                                "fixed_transect", "Count", "Species_Code", "Age_Class", "Family.x", "Family.y", "Family2", 
                                "Commonname", "scientific.name", "Trophic.Level", "Max.body.size", "Dietary.group", 
                                "Site_Year", "Taxa")]

# Can also make Year a factor instead of an integer
fish_raw_longform_reduced$year <- as.factor(fish_raw_longform_reduced$year)

# Summary Information
str(fish_raw_longform_reduced)
summary(fish_raw_longform_reduced)
summary(fish_raw_longform_reduced$site)
summary(fish_raw_longform_reduced$year)

# Keep only the columns that I need to convert to wide form
fish_raw_longform_minimum <- fish_raw_longform_reduced[,c("�..notes", "year", "month", "day", "site", "transect", 
                                                  "fixed_transect", "Count", "Species_Code", "Age_Class")]

# Convert to wide form
fish_raw_wideform <- spread(fish_raw_longform_minimum, Species_Code, Count)
# Note that there are 2 times more observations now than in the original raw wide format - 
summary(fish_raw$site)
summary(fish_raw_wideform$site)
# this is because A/J are in different rows, not listed in different columns

# Check that observations were made at every site for every year
# 25 years and 8 sites = 200 observations expected
check2 <- unique(fish_raw_longform_reduced$Site_Year)
# because check2 has 200 levels, every site was visited for all 25 years


# ***Add transect length from sponge dataset to the corresponding observations in the fish dataset?***
summary(fish_raw_longform_reduced$transect)
summary(sponge_raw_longform$Transect)
# Because the levels are not the same, this may be difficult

# Check that all of the counts are integers
sum(sponge_raw_longform$Count)
sum(fish_raw_longform_reduced$Count)
# ***There is definitely a more efficient way to do this***
# Because the sum of fish counts has a decimal, create a subset of non-zero counts to ID the culprit
fish_nozero <- fish_raw_longform_reduced[fish_raw_longform_reduced$Count != "0", ]
# Looks like there are some 1.5's and 1.875's, so create a subset that removes these observations to 
# determine if there are other non-integer values for Count
fish_integertest <- fish_nozero[fish_nozero$Count == 1 | fish_nozero$Count >= 2, ]
# Now try again,
sum(fish_integertest$Count)
# From a closer look at the data: 1.5, 1.875, 3.75, 4.5, 7.5, 10.5, 16.5, 19.5, 22.5, 28.5, 31.5, 37.5, and 49.5
# are identified as non-integers, but there may be more.

# Check that for each Site_Year there are 3 transects
sponge_num_transects <- as.data.frame(table(sponge_raw$Site_Year), responseName="num_transects")
fish_num_transects <- as.data.frame(table(fish_raw$Site_Year), responseName="num_transects")
# Create subsets that only include Site_Year's with more than 3 transects
sponge_num_transects <- sponge_num_transects[sponge_num_transects$num_transects > 3, ]
fish_num_transects <- fish_num_transects[fish_num_transects$num_transects > 3, ]

# ***Check that the transects within a given site-year are not repeated***


## Summary of Importing Datasets
# *** name of coral data for analysis
# sponge_raw_longform for analysis
# fish_raw_longform_reduced for analysis



## Total Counts
# Calculate total counts
sum(sponge_raw_longform$Count)
sum(fish_raw_longform_reduced$Count) #why is this not a whole #?***

# Total counts by year
aggregate(sponge_raw_longform$Count, by = list(Year = sponge_raw_longform$Year), FUN = sum)
aggregate(fish_raw_longform_reduced$Count, by = list(Year = fish_raw_longform_reduced$year), FUN = sum)
# ***1992, 1993, and 2014 are not integers
# ***2016 has a total count of zero

# Total counts by site
aggregate(sponge_raw_longform$Count, by = list(Site = sponge_raw_longform$Site), FUN = sum)
aggregate(fish_raw_longform_reduced$Count, by = list(Site = fish_raw_longform_reduced$site), FUN = sum)
# ***Grand Ghut, Monkey Pt, Muskmelon, and Pelican Ghut are not integers

# Total counts by Year and Site
sponge_total_counts <- aggregate(sponge_raw_longform$Count, by = list(Site = sponge_raw_longform$Site, Year = sponge_raw_longform$Year), FUN = sum)
fish_total_counts <- aggregate(fish_raw_longform_reduced$Count, by = list(Site = fish_raw_longform_reduced$site, Year = fish_raw_longform_reduced$year), FUN = sum)

# Graphing counts by year
# ggplot(data = sponge_total_counts, aes(sponge_total_counts$Year, sponge_total_counts$x)) +
#   geom_col()
# ggplot(data = fish_total_counts, aes(fish_total_counts$Year, fish_total_counts$x)) +
#   geom_col()

# Graphing counts by site
# ggplot(data = sponge_total_counts, aes(sponge_total_counts$Site, sponge_total_counts$x)) +
#   geom_col()
# ggplot(data = fish_total_counts, aes(fish_total_counts$Site, fish_total_counts$x)) +
#   geom_col()

# Create new column called Year_Site to use as a key when combining datasets
# fish_total_counts <- unite_(data = fish_total_counts, col = "Year_Site", c("Year", "Site"), remove = F)
# sponge_total_counts <- unite_(data = sponge_total_counts, col = "Year_Site", c("Year", "Site"), remove = F)

# Combine Datasets
all_counts <- rbind(fish_total_counts, sponge_total_counts)

# Grouped column charts
Taxa <- all_counts$Taxa
ggplot(all_counts, aes(fill = Taxa, y = all_counts$x, x = all_counts$Site)) + 
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Site") +
  ylab("Count")
ggplot(all_counts, aes(fill = Taxa, y = all_counts$x, x = all_counts$Year)) + 
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Year") +
  ylab("Count")

# ***build off the following code to create a loop so there is a graph like this for each site
Site_Names <- c("Pelican Ghut", "Grand Ghut", "Crab Cove", "Muskmelon", "Bigelow", "White Bay", "Monkey Pt", "Guana Head")
Bigelow <- all_counts[which(all_counts$Site == "Bigelow"),]
ggplot(Bigelow, aes(fill = Taxa, y = Bigelow$x, x = Bigelow$Year)) + 
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Year") +
  ylab("Count")

# ***build off the following code to create a loop so there is a graph like this for each year
year_2009 <- all_counts[which(all_counts$Year == "2009"),]
ggplot(year_2009, aes(fill = Taxa, y = year_2009$x, x = year_2009$Site)) + 
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Site") +
  ylab("Count")



