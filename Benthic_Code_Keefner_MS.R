### Nicole Keefner
### Master's Thesis



## Set Working Directory



## Load packages
library(tidyverse)
library(tidyr)
library(dplyr)



## Import coral dataset



## Import sponge dataset
sponge_raw <- read.csv("Guana_Sponge_data_for_analysis.csv", header=T)

# Only select rows and columns for which there are values entered (sponge_raw has extraneous rows and columns)
sponge_raw <- sponge_raw[1:557,1:65]

# Put datast into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
sponge_raw_long <- sponge_raw %>%
  gather(key = "Taxonomic_Group", value = "Count", Agelas.clathrodes..Agelas.citrina.or.Clathria.faviformis:Black..spiny..purple.exudate.but.not.slimy)

# Double-check
summary(sponge_raw_long)

# Taxonomic_Group is being read as a character, but it should be a factor
# overwrite column so it is a factor
sponge_raw_long$Taxonomic_Group <- as.factor(sponge_raw_long$Taxonomic_Group)



## Over the years, people have called sites by different names. The following code
## corrects these entry mistakes, so that only 8 site names are retained:
## Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head

# Long Point and Muskmelon are two names for the same site
temp = which(sponge_raw_long$Site == "Long Point")
sponge_raw_long$Site[temp] = "Muskmelon"

# Pelican and Pelican Ghut are two names for the same site
temp = which(sponge_raw_long$Site == "Pelican")
sponge_raw_long$Site[temp] = "Pelican Ghut"








#################NEED TO FIGURE OUT HOW TO REMOVE SITE LEVELS THAT DO NOT HAVE DATA


# ################CHECK WITH GRAHAM THAT
# BIGELOW != BIGELOW-sOUTH
# MONKEY PT != MONKEY PT AREA
# WHITE BAY != WHITE BAY-ALT != WHITE BAY E









## Import fish datasets (see "Fish Metadata.docx" for more information)
fish_codes <- read.csv("fish species codes 2018.csv", header = T)
fish_list <- read.csv("Fish species list.csv", header = T)
fish_raw <- read.csv("raw fish data nicole 2018.csv", header = T)

# Put fish_raw into long form (i.e. so species codes are in a single column rather than having one column for each species code)
# key = "title of new column", value = "numbers being moved around because of the key", 
# ":" specifies which columns should be included in key
fish_raw_long <- fish_raw %>% gather(key = "KEY", value = "Count", popaa:kysea)

# Split new KEY column into species_code and age_class columns
KEY <- as.character(fish_raw_long$KEY)
fish_split <- data.frame("Species_Code" = substr(KEY, 1, (nchar(KEY)-1)), "Age_Class" = substr(KEY, nchar(KEY), nchar(KEY)))
# Combine long-form dataset with the split columns dataframe
fish_raw_long_split <- cbind(fish_raw_long, fish_split)

# Rename column in fish_codes dataframe to use as a key to merge this information with the raw data
colnames(fish_codes)[colnames(fish_codes) == "ï..new_code"] <- "KEY"
fish_raw_long_split_codes <- merge(fish_raw_long_split, fish_codes, by = "KEY", all = T)

# Rename column in fish_list dataframe to use as a key to merge this information with the raw data + fish code info
colnames(fish_list)[colnames(fish_list) == "f_code"] <- "KEY"
fish_raw_long_split_codes_list <- merge(fish_raw_long_split, fish_list, by = "KEY", all = T)



## According to Dr. Forrester, I should only use observations where survey==main and depth==30 (feet)
fish_raw_30ft_main <- fish_raw_long_split_codes_list[which(fish_raw_long_split_codes_list$depth == '30' 
                         & fish_raw_long_split_codes_list$survey == 'main'), ]
# Remove observations where count is NA
fish_raw_30ft_main <- fish_raw_30ft_main[!is.na(fish_raw_30ft_main$Count), ]



## Select for 8 Sites around Guana Island: 
## Pelican Ghut, Grand Ghut, Crab Cove, Muskmelon, Bigelow, White Bay, Monkey Point, and Guana Head
summary(fish_raw_30ft_main$site) # There are 48 distinct sites.

 # Keep only the columns that I think might be relevant
subset <- fish_raw_30ft_main[,c("ï..notes", "visibility", "duration", "year", "month", "day", "site", "transect", 
                                "fixed_transect", "Count", "Species_Code", "Age_Class", "Family", "Family2", 
                                "Commonname", "scientific.name", "Trophic.Level", "Max.body.size", "Dietary.group")]





###########NOTE THAT AFTER THIS POINT, I AM HAVING A DIFFICULT TIME WITH THE SELECT FXN, SO IT'S NOT SUBSETTING PROPERLY
# Keep only the columns for the main sites
subset2 <- subset[which(subset$site == 'iguana' | subset$site == 'pelican   ' | subset$site == 'pelican' | 
                          subset$site == 'white' | subset$site == 'bigelow' | subset$site == 'Bigelow south' | 
                          subset$site == 'grand' | subset$site == 'muskN' | subset$site == 'monkey' | subset$site == 'crab'), ]

# test <- select(filter(fish_raw_30ft_main, site == 'iguana' | site == 'pelican   ' | site == 'pelican' | 
#                            site == 'white' | site == 'bigelow' | site == 'Bigelow south' | 
#                            site == 'grand' | site == 'muskN' | site == 'monkey' | site == 'crab'),c("ï..notes", "visibility", "duration", "year", "month", "day", "site", "transect", 
#                                                    "fixed_transect", "Count", "Species_Code", "Age_Class", "Family", "Family2", 
#                                                    "Commonname", "scientific.name", "Trophic.Level", "Max.body.size", "Dietary.group"))







