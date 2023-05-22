#################################################################################################
# Authors: Rachel Zilinskas, Lillian Haine, Sarah Samorodnitsky, Sharon Ling, and Maria Masotti #
#################################################################################################
# Date: May 2021                                                                                #
# Edited: Kody DeGolier,  Summer 2022                                                           #
#################################################################################################
# Purpose: a helper document that includes some data cleaning steps for the Shiny App created   #
# for SPPS to use for their Earth Science curriculum as part of a collaboration with SPPS and   #
# the UMN Biostatistics Outreach Committee                                                      #
#################################################################################################

# Tidy packages for data cleaning 
library(dplyr)
library(tidyr)
library(tidyverse)


########################################################################################################################
# YEARLY DATA - this is used for the "Human Impact Tab" and includes yearly averaged values along with other variables #
########################################################################################################################

#Changed this so it includes new mean_pm2.5_years and allows the user to select different years for comparisons #
yearly_data <- read.csv("ALL_Years_Merged.csv", row.names = 1)
yearly_data <- select(yearly_data, site.name, mean_pm2.5_2015_from2.5, mean_pm2.5_2016_from2.5, mean_pm2.5_2017_from2.5, 
                      mean_pm2.5_2018_from2.5, mean_pm2.5_2019_from2.5, mean_pm2.5_2020_from2.5, mean_pm2.5_2021_from2.5, interstate, 
                      distance_mi, name_pp, fuel_type, PP_distance_mi,med_income, pop_density)

###################################################################################################################
# MAP INFO - used for both the home/intro page to created the power plant map as well as the human impact tab map #
###################################################################################################################

map_info <- read.csv("map_info.csv", row.names = 1)

#########################################################################
# SEASONAL DATA - includes seasonal site averages for the seasonal tab #
#########################################################################

seasonal_data <- read.csv("ALL_Seasons_Merged.csv")
# Remove the rows that have missing season values.
seasonal_data <- seasonal_data[!is.na(seasonal_data$Season),]
# Storing the site names
sites <- unique(seasonal_data$site.name)
# Reorder the seasons
match_seasons <- match(c("Fall", "Winter", "Spring", "Summer"),
                       seasonal_data$Season[1:4])
# Reorder the data so the seasons are in order
seasonal_data_order <- seasonal_data
for (site in sites) {
  # Storing the current site
  current_site <- seasonal_data %>% filter(site.name == site)
  # Reordering the seasons
  current_site_order <- current_site[match_seasons,]
  # Returning the reordered data to the dataset
  seasonal_data_order[seasonal_data_order$site.name %in% site,] <- current_site_order
}

#####################################################################
# MONTHLY DATA - includes monthly site averages for the monthly tab #
#####################################################################
# Load in data
monthly_data <- read.csv("ALL_Months_Merged.csv")

# Set colors to use
myCols <- c("magenta", "red", "orange", "black", "green", "skyblue", "purple")
col.br <- colorRampPalette(myCols)

# Get range of pm2.5 values in dataset so that all figures will all have the same numbers on the y-axis
# Will need to adjust Years used below for further updates #
Years <- c(2015:2021)
all_pm2.5 <- unlist(lapply(1:length(Years), function(i) monthly_data[,paste0("monthly_mean_pm2.5_", Years[i], "_from_pm2.5")]))

# Get all site names and assign them with colors
siteNames <- unique(monthly_data$site_ID)[order(unique(monthly_data$site_ID))]
siteCol <- setNames(col.br(length(siteNames)), nm=siteNames)

# Create vector containing months' names
months_vec <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_vec_short <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")

########################################################################################################################
# WEATHER DATA - includes daily site values as well as daily weather variables for the Twin Cities for the weather tab #
########################################################################################################################

weather_data <- read.csv("All_Sites_Merged_Daily_2.5.csv", header = T)

################################################################
# DAILY DATA - includes daily site values for the Holidays tab #
################################################################

daily <- read.csv("daily_PM2.5.csv", header = T)


