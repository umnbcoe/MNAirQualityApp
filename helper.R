#################################################################################################
# Authors: Rachel Zilinskas, Lillian Haine, Sarah Samorodnitsky, Sharon Ling, and Maria Masotti #
#################################################################################################
# Date: May 2021                                                                                #
# Edited: Kody DeGolier,  Summer 2022                                                           #
#################################################################################################
# Purpose: a helper document that includes some helper functions to create plots  for the Shiny #
# App created for SPPS to use for their Earth Science curriculum as part of a collaboration     #
# with SPPS and the UMN Biostatistics Outreach Committee                                        #
#################################################################################################

# Loading required packages for some of the plotting functions

library(plotly)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(leaflet)
library(fields)
library(scales)
library(gt)


######################################################################################################################
# Human impact tab plot - plots the 2021 average PM2.5 measure for sites on the y-axis against the user's choice of: #
# 1. Distance to nearest interstate                                                                                  #
# 2. Distance to nearest power plant                                                                                 #
# 3. Population Density                                                                                              #
# 4. Median income                                                                                                   #
# Users also have the option to highlight a specific site on the plot and plotly capabilities allow hover text for   #
# further site information                                                                                           #
# Credit: Rachel Zilinskas                                                                                           #
# Update: Allows user to select year of interest                                                                     #
# Will need to add in the year being added with future data updates in section (a)                                   #
# Edited: Kody DeGolier,  Summer 2022                                                                                #
######################################################################################################################

make_impact_plot <- function(measure, mean_pm2.5_chosen, dat, plot_site){
  #fine tunes the data to the selected year #
  data1 <- select(dat, site.name, paste0(mean_pm2.5_chosen), interstate, 
                  distance_mi, name_pp, fuel_type, PP_distance_mi, med_income, pop_density) %>% na.omit()
  #Changed it so it doesn't only highlight but also changes its style to not a dot #
  # Defining colors for highlighting site and filter to only the site that was chosen:
  if(plot_site == "All Sites") {data1 = mutate(data1, color = site.name)}
  if(plot_site != "All Sites") {
    site.dat.1 = filter(data1, site.name == plot_site) %>% 
      mutate(color = "red", pch = 4) 
    site.dat.2 = filter(data1, site.name != plot_site) %>% 
      mutate(color = "grey50", pch = 1)  
    data1 = rbind(site.dat.1, site.dat.2)
  }
  #section (a) #
  if(mean_pm2.5_chosen == "mean_pm2.5_2015_from2.5") {current_year = "2015"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2016_from2.5") {current_year = "2016"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2017_from2.5") {current_year = "2017"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2018_from2.5") {current_year = "2018"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2019_from2.5") {current_year = "2019"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2020_from2.5") {current_year = "2020"}
  if(mean_pm2.5_chosen == "mean_pm2.5_2021_from2.5") {current_year = "2021"}
  # Distance to Nearest Interstate Plot
  if(measure == "Distance to Nearest Interstate"){
    dat2 = data1
    fit <- lm(dat2[[mean_pm2.5_chosen]] ~ distance_mi, data = dat2)
    x <- list(title = "Distance to Nearest Interstate (miles)")
    y <- list(title = paste0("Mean PM2.5 from ", current_year, " (micro g/(cubic m))"))
    title <- "PM2.5 vs. Distance to Interstates in Minnesota"
    text <- paste0("Site: ", dat2$site.name, ", Nearest Interstate: ", dat2$interstate)
    x.val <- dat2$distance_mi
  }
  # Distance to Nearest Power Plant Plot
  if(measure == "Distance to Nearest Power Plant"){
    dat2 <- subset(data1, PP_distance_mi <= 100)
    fit <- lm(dat2[[mean_pm2.5_chosen]] ~ PP_distance_mi, data = dat2)
    x <- list(title = "Distance to Nearest Power Plant (miles)")
    y <- list(title =paste0("Mean PM2.5 from ", current_year, " (micro g/(cubic m))"))
    title <- "PM2.5 vs. Distance to Power Plants in Minnesota"
    text <- paste0("Site: ", dat2$site.name, ", Nearest Power Plant: ", dat2$name_pp, ", Fuel Type: ", dat2$fuel_type)
    x.val <- dat2$PP_distance_mi
  }
  # Population density 
  if(measure == "Population Density"){
    dat2 = data1
    fit <- lm(dat2[[mean_pm2.5_chosen]] ~ poly(pop_density,2, raw = T), data = dat2)
    x <- list(title = "Population Density (Persons per square mile)")
    y <- list(title = paste0("Mean PM2.5 from ", current_year, " (micro g/(cubic m))"))
    title <- "PM2.5 vs. Population Density in Minnesota"
    text <- paste0("Site: ", dat2$site.name)
    x.val <- dat2$pop_density
  }
  # Median income
  if(measure == "Median Income"){
    dat2 = data1[!is.na(data1$med_income),]
    fit <- lm(dat2[[mean_pm2.5_chosen]] ~ med_income, data = dat2)
    x <- list(title = "Median Yearly Household Income (from 2015-2019 Census Data)")
    y <- list(title = paste0("Mean PM2.5 from ", current_year, " (micro g/(cubic m))"))
    title <- "PM2.5 vs. Median Income in Minnesota"
    text <- paste0("Site: ", dat2$site.name)
    x.val <- dat2$med_income
  }
  
  # Outputting plot with no sites highlighted
  if(plot_site == "All Sites"){
    fig <- plot_ly(
      dat2, x = ~x.val, y = ~round(dat2[[mean_pm2.5_chosen]], 2),
      # Hover text:
      text = ~text,
      #color = ~color,
      type = "scatter"
    ) %>% 
      layout(title = title, xaxis = x, yaxis = y) %>% 
      add_lines(x = ~x.val, y = fitted(fit) )%>% 
      config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE, dragmode = FALSE)
  }
  # Outputting plot with user selected site highlighted
  if(plot_site != "All Sites"){
    fig <- plot_ly(
      dat2, x = ~x.val, y = ~round(dat2[[mean_pm2.5_chosen]], 2),
      # Hover text:
      text = ~text,
      color = ~color, colors = c("grey50", "red"),
      symbol = ~pch, 
      type = "scatter"
    ) %>% 
      layout(title = title, xaxis = x, yaxis = y) %>% 
      add_lines(x = ~x.val, y = fitted(fit) )%>% 
      config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE, dragmode = FALSE)
  }
  # Final output
  fig
}

#######################################################################################################
# Human Impact Tab Plot Interpretations - outputs beta and/or R^2 interpretations for the chosen plot #
# for the population density plot - which has a quadratic curve - only R^2 interpretation is given due#
# to the difficulty of interpretation of a quadratic coefficient                                      #
# Credit: Rachel Zilinskas                                                                            #
# Update: Displays information based off user selected year of interest                               #
# Edited: Kody DeGolier,  Summer 2022                                                                 #
#######################################################################################################
# has the text update based off year selection #
impact_plot_text <- function(measure, var1, dat){
  #fine tunes the data to the selected year #
  dat2 <- select(dat, site.name, paste0(var1), interstate, 
                distance_mi, name_pp, fuel_type, PP_distance_mi,med_income, pop_density) %>% na.omit()
  # Distance to nearest interstate
  if(measure == "Distance to Nearest Interstate"){
    dat2 = dat
    fit <- lm(dat2[[var1]] ~ distance_mi, data = dat2)
    r2 <- round(summary(fit)$r.squared,4)*100
    beta <- -1*round(coefficients(fit)[2],4)
    text <- paste0("For every mile you move away from the interstate, the average PM2.5 measure decreases by micro g/(cubic m) ", beta, ". ", 
                   r2, "% of the variability in average PM2.5 can be explained by a site's distance to the nearest interstate.")
  }
  # Distance to Nearest Power Plant Plot
  if(measure == "Distance to Nearest Power Plant"){
    dat2 <- subset(dat, PP_distance_mi <= 100)
    fit <- lm(dat2[[var1]] ~ PP_distance_mi, data = dat2)
    r2 <- round(summary(fit)$r.squared,4)*100
    beta <- -1*round(coefficients(fit)[2],4)
    text <- paste0("For every mile you move away from a power plant, the average PM2.5 measure decreases by micro g/(cubic m) ", beta, ". ", 
                   r2, "% of the variability in average PM2.5 can be explained by a site's distance to the nearest power plant.")
  }
  # Population density
  if(measure == "Population Density"){
    dat2 = dat
    fit <- lm(dat2[[var1]] ~ poly(pop_density,2, raw = T), data = dat2)
    r2 <- round(summary(fit)$r.squared,4)*100
    text <- paste0(r2, "% of the variability in average PM2.5 can be explained by a site's population density.")
  }
  # Median Income 
  if(measure == "Median Income"){
    dat2 = dat[!is.na(dat$med_income),]
    fit <- lm(dat2[[var1]] ~ med_income, data = dat2)
    r2 <- round(summary(fit)$r.squared,4)*100
    beta <- round(coefficients(fit)[2],4)*1000
    text <- paste0("For every $1,000 increase in median income, the average PM2.5 increases by micro g/(cubic m) ", beta, ". ", 
                   r2, "% of the variability in average PM2.5 can be explained by a site's median income.")
  }
  # Final output 
  print(text)
}

###########################################################################################################
# Making map data for the Human Impact tab- this function looks up the user-inputted address on the human #
# impact tab, converts it to latitude and longitude coordinates to be added to the human impact tab map   #
# which includes both air quality monitoring sites and the nearest power plant                            #
# Credit: Rachel Zilinskas                                                                                #
###########################################################################################################

make_map_dat <- function(map_sites, point){
  map_dat <- subset(map_info, site.name %in% map_sites)
  geo_dat <- data.frame(matrix(nrow = 1, ncol =1))
  names(geo_dat) <- c("address")
  geo_dat$address <- point
  res <- geo_dat %>% geocode(address)
  new_row <- data.frame(res$address, res$lat, res$long, "point")
  names(new_row) <- c("address", "lat", "lng", "icon")
  map_dat <- bind_rows(map_dat, new_row)
  return(map_dat)
}

###########################################################################################################
# Making map for the Human Impact tab - this function creates the map for the human impact tab which      #
# includes all of the air quality monitoring sites (colored in blue), the nearest power plant (colored in #
# red), and a user inputted point (colored in green). Additional information about both the air quality   #
# monitoring sites (median income for census tract, population density) and power plants (fuel type) is   #
# also available when the user clicks on the point on the map                                             #
# Credit: Rachel Zilinskas                                                                                #
###########################################################################################################

make_map <- function(map_dat){
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(lng=subset(map_dat, icon=="site")$lng, lat=subset(map_dat, icon=="site")$lat, color = "blue", popup = subset(map_dat, icon=="site")$text)  %>%
      addCircleMarkers(lng=subset(map_dat, icon=="pp")$lng, lat=subset(map_dat, icon=="pp")$lat, color = "red", popup = subset(map_dat, icon=="pp")$text) %>%
      addCircleMarkers(lng=subset(map_dat, icon=="point")$lng, lat=subset(map_dat, icon=="point")$lat, color = "green", popup = "My Point") %>%
      leaflet::addLegend("topleft", colors = c("blue", "red"), labels = c("Air Quality Monitoring Site", "Power Plant"))
    m 
}

###########################################################################################################
# Making map data for the homepage tab - this function looks up the user-inputted address on the homepage #
# tab, converts it to latitude and longitude coordinates to be added to the homepage tab which includes   #
# all of the air quality monitoring sites                                                                 #
# Credit: Rachel Zilinskas                                                                                #
###########################################################################################################

make_homepage_dat <- function(home_point){
  map_dat <- map_info
  geo_dat <- data.frame(matrix(nrow = 1, ncol =1))
  names(geo_dat) <- c("address")
  geo_dat$address <- home_point
  res <- geo_dat %>% geocode(address)
  new_row <- data.frame(res$address, res$lat, res$long, "point")
  names(new_row) <- c("address", "lat", "lng", "icon")
  map_dat <- bind_rows(map_dat, new_row)
  return(map_dat)
}

###########################################################################################################
# Making map for the homepage tab- this function creates the map for the human impact tab which includes  #
# all of the air quality monitoring sites (colored in blue) and a user inputted point (colored in green). #
# Additional information about both the air quality monitoring sites (median income for census tract,     #
# population density) is available when the user clicks on the blue point                                 #
# Credit: Rachel Zilinskas                                                                                #
###########################################################################################################

make_homepage_map <- function(map_dat){
  library(leaflet)
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(lng=subset(map_dat, icon=="site")$lng, lat=subset(map_dat, icon=="site")$lat, color = "blue", popup = subset(map_dat, icon=="site")$site.name)  %>%
    addCircleMarkers(lng=subset(map_dat, icon=="point")$lng, lat=subset(map_dat, icon=="point")$lat, color = "green", popup = "My Point")  %>%
    leaflet::addLegend("topleft", colors = c("blue", "green"), labels = c("Air Quality Monitoring Site", "My Point"))
  
  m 
}

###############################################################################################################
# Seasonal Tab Plot - this function creates a line plot which displays the seasonal PM2.5 averages for a user #
# selected year and two user selected sites to compare. Additional information about the sites is available   #
# using the plotly hover text capabilities                                                                    #
# Credit: Sarah Samorodnitsky                                                                                 #
# Edited: Kody DeGolier,  Summer 2022                                                                         #
###############################################################################################################

seasonal_plot <- function(site1, site2, year){
  
  p <- seasonal_data_order %>% 
    mutate(label = if_else(Season %in% "Summer", as.character(site.name), NA_character_)) %>%
    filter(site.name %in% c(site1, site2)) %>%
    ggplot(aes(x = factor(Season, level = c("Fall", "Winter", "Spring", "Summer")), 
               y = get(paste0("PM2.5_", year)), group = site.name, 
               text = paste("Site:", site.name,
                            "<br>Year:", year,
                            "<br>PM2.5: ", round(get(paste0("PM2.5_", year)), 3)))) +
    #panel.background fixes the coloring for most cases since its the average for the seasons for that year#
    #ex. Doesn't work for 2021 with the Red Lake Nation site since that one has PM2.5 ~= 15 entering the Moderate air quality section#
    theme(legend.position = "bottom", panel.background = element_rect(fill= "#CCE6CC")) +
    geom_line(aes(colour = site.name), show.legend = FALSE, lwd = 0.5) + 
    geom_point(aes(colour = site.name), show.legend = FALSE, size = 1) +
    #Labels for the Graph #
    labs(x = "Seasons",
         y = "Mean PM2.5 (micro g/(cubic m))") +
    ggtitle(paste("Average PM2.5 Across Seasons in", year))
  
  #Allows the user to see information for where the cursor is#
  fig <-  ggplotly(p, tooltip = "text")
  fig <- fig %>% 
    config(displayModeBar = FALSE) %>%
    layout(dragmode = FALSE)
  fig
}

###############################################################################################################
# Monthly Tab Plot - this function creates a line plot which displays the monthly PM2.5 averages for a user   #
# selected year and user selected sites. Once more that five sites have been selected, a boxplot overlays the #
# line plot to more concisely summarize the trends without the plot becoming too cluttered                    #
# Credit: Sharon Ling                                                                                         #
# Will look different from the others because it is rendered from a base plot rather than a plot_ly           #
# Edited: Kody DeGolier,  Summer 2022                                                                         #
###############################################################################################################

monthly_plot <- function(sites, year){
  # Collect only year of interest
  monthly_data_1 <- monthly_data[which(is.na(monthly_data[,paste0("monthly_mean_pm2.5_", year, "_from_pm2.5")])==FALSE),]
  monthly_data_1$MonthNum <- match(monthly_data_1[,paste0("Month_", year, "_from_pm2.5")], months_vec)
  monthly_data_1 <- monthly_data_1[order(monthly_data_1$MonthNum),]
  
  # Collect data from only sites we're interested in
  monthly_data_2 <- monthly_data_1[which(monthly_data_1$site_ID %in% sites),]
  
  #update/fix this to match the color scale used in the following 2 tabs @@ #
  #Add the cursor info for this plot @@ #
  #The code below causes the graph used to be shaded based off the PM2.5 color scale rather than being plain # 
  plot(NA, xlab="Month", ylab="Mean PM2.5 (micro g/(cubic m))", main=year, xlim=c(1,12), ylim=range(all_pm2.5, na.rm=TRUE), xaxt="n") 
  
  # Adding color strips for color scale to make it appear the same as the other graphs #

  strips <- t(as.matrix(c(1,2)))
  colorbar.plot(6,12.1,strips,strip.width = 8, col = c("#CCE6CC", "#FFFFCC"), opacity = 0.4)
 
 
 
  # Plot points for each site separately
  for (i in 1:length(sites)) {
    monthly_data_i <- monthly_data_2[which(monthly_data_2$site_ID %in% (sites)[i]),]
    points(x=monthly_data_i$MonthNum, y=monthly_data_i[,paste0("monthly_mean_pm2.5_", year, "_from_pm2.5")], col=col.br(length(sites))[i], type="l")
    points(x=monthly_data_i$MonthNum, y=monthly_data_i[,paste0("monthly_mean_pm2.5_", year, "_from_pm2.5")], col=col.br(length(sites))[i], type="p", pch=19)
  }
  
  if (length(sites) <= 5) {
    # If we're interested in <=5 data sites, then we should not add a boxplot
  } else {
    # If we're interested in >5 sites, then we should overlay boxplots
    boxplot(monthly_data_2[,paste0("monthly_mean_pm2.5_", year, "_from_pm2.5")] ~ monthly_data_2$MonthNum, xlab="Month", ylab="pm2.5", col="gray", main=year, xlim=c(0,20), ylim=range(all_pm2.5, na.rm=TRUE), xaxt="n", add=T)
  }
  
  # Add axis and legend
  axis(side=1, at=c(1:12), labels=months_vec_short, las=0)
  legend("topright", legend=c(sites), col=col.br(length(sites)), lty=1, title="Site", bty = "n", cex=min(1, 2.49/sqrt(length(sites))), pch=19)
    
}

################################################################################################################
# Holiday Tab Plot I - this function creates a dot plot for the daily PM2.5 values for a user selected holiday #
# for the years 2015 - 2021. Users also have the option to highlight a specific site to be seen on the plots   #
# for each of the years. The background shows the EPA color scale for PM2.5 values with values ranging from    #
# healthy to unhealthy.                                                                                        #                                                  
# Credit: Lillian Haine                                                                                        #
# Edited: Kody DeGolier,  Summer 2022                                                                          #
################################################################################################################

make_holiday_plot <- function(site, holiday, data, holiday.name) {
  # filter to only the site that was chosen:
  if(site == "All Sites") {site.dat = mutate(data, color = site.name)}
  if(site != "All Sites") {
    site.dat.1 = filter(data, site.name == site) %>% 
      mutate(color = "red")
    site.dat.2 = filter(data, site.name != site) %>% 
      mutate(color = "grey50")
    site.dat = rbind(site.dat.1, site.dat.2)
    
  }
  dat.all = matrix(NA, ncol = 6)
  colnames(dat.all) = c("site.name", "year", "date", "variable", "value", "color")
  #keep the holiday dates sorted, by selecting the specified thing for each year
  for (i in 1:length(holiday) ){
    year.cur = 2014+i
    date.holiday = as.Date(holiday[i])
    dat.temp = filter(site.dat, 
                      as.Date(date) == as.Date(date.holiday)) %>% 
      select(site.name, year, date, variable, value, color)
    dat.all = rbind(dat.temp, dat.all)
  }
  dat.all = na.omit(dat.all)
  #Labels for the top graph on this tab#
  x <- list(
    title = "Year"
  )
  y <- list(
    title = paste0("Mean PM2.5 value for ", holiday.name, " (micro g/(cubic m))")
  )

#The code below causes the graph used in the 2nd holiday plot to be shaded based off the PM2.5 color scale rather than being plain # 
  if(site == "All Sites") {
    fig<-plot_ly(dat.all)
    fig<-fig%>%layout(xaxis = x, yaxis = y, shapes = list(
      list(type = "rect",
           fillcolor = "green",layer='below',
           line = list(color = "green"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 0, y1 = 12, yref = "y"),
      list(type = "rect",
           fillcolor = "yellow", layer='below',
           line = list(color = "yellow"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 12.1, y1 = 35.4, yref = "y"),
      list(type = "rect",
           fillcolor = "orange", layer='below',
           line = list(color = "orange"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 35.5, y1 = 55.4, yref = "y"),
      list(type = "rect",
           fillcolor = "red", layer='below',
           line = list(color = "red"), opacity = 0.2,
           x0 =2015, x1 =2021, xref = "x",
           y0 = 55.5, y1 = 75, yref = "y"))) %>% 
#Allows the users cursor to pull up useful information# 
      add_trace(x = ~year, y = ~value,
                # Hover text:
                text = ~paste("Site: ", site.name, 'PM2.5:', value),
                color = ~color, type = "scatter"
      ) %>% config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE, dragmode = FALSE)
  }
  if(site != "All Sites") {
    fig<-plot_ly(dat.all)
    fig<-fig%>%layout(xaxis = x, yaxis = y, shapes = list(
      list(type = "rect",
           fillcolor = "green",layer='below',
           line = list(color = "green"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 0, y1 = 12, yref = "y"),
      list(type = "rect",
           fillcolor = "yellow", layer='below',
           line = list(color = "yellow"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 12.1, y1 = 35.4, yref = "y"),
      list(type = "rect",
           fillcolor = "orange", layer='below',
           line = list(color = "orange"), opacity = 0.2,
           x0 = 2015, x1 = 2021, xref = "x",
           y0 = 35.5, y1 = 55.4, yref = "y"),
      list(type = "rect",
           fillcolor = "red", layer='below',
           line = list(color = "red"), opacity = 0.2,
           x0 =2015, x1 =2021, xref = "x",
           y0 = 55.5, y1 = 75, yref = "y"))) %>%
#Allows the users cursor to pull up useful information# 
      add_trace( x = ~year, y = ~value,
                 # Hover text:
                 text = ~paste("Site: ", site.name, 'PM2.5:', value),
                 color = ~color, colors = c("grey50", "red"), type = "scatter"
      ) %>% 
      config(displayModeBar = FALSE) %>%
      layout(showlegend = FALSE, dragmode = FALSE)
  }
  fig
}

################################################################################################################
# Holiday Tab Plot II - this function creates a dot plot for the daily PM2.5 values for a user selected year,  #
#  highlighting the user selected holiday to be compared against the rest of the values for that year. The     #
# background shows the EPA color scale for PM2.5 values with values ranging from healthy to unhealthy.         #                                                                                  #                                                  
# Credit: Lillian Haine                                                                                        #
# Edited: Kody DeGolier,  Summer 2022                                                                          #
################################################################################################################

make_plot_365 <- function(holiday, data, holiday.name, year.val){
  for (i in 1:length(holiday) ){
    year.cur = 2014+i
    cur.hol = as.Date(holiday[i])
    yr.hol = format(as.Date(cur.hol, format="%d/%m/%Y"),"%Y")
    if(yr.hol == year.val){
      date.holiday = cur.hol
    }
  }
  # filters to only the year that was chosen:
  year.dat = filter(data, year == year.val) %>% 
    mutate(day_of_year = yday(as.Date(date, format = "%Y-%m-%d")))
  hol.day = filter(year.dat,
                   as.Date(date, format = "%Y-%m-%d") == 
                     as.Date(date.holiday, format = "%m/%d/%Y")) %>%
    select("day_of_year")
  hol.day = hol.day$day_of_year[1]
  print(head(year.dat))
 #Highlights the user selected holiday on the graph # 
  hol.dat = filter(year.dat, day_of_year == hol.day) %>%
    mutate(color = "red") %>% 
    select(site.name, date, variable, value, color, day_of_year)
  non.hol.dat = filter(year.dat, day_of_year != hol.day) %>% 
    mutate(color = "grey50") %>% 
    select(site.name, date, variable, value, color, day_of_year)
  
  #keep the holiday dates sorted, by selecting the specified thing for each year
  dat.all = rbind(hol.dat, non.hol.dat)
  dat.all = na.omit(dat.all)
  colnames(dat.all) = c("site.name", "date", "variable", "value", "color", "day_of_year")
  #Labels for the X and Y variables #
  x <- list(
    title = "Day of the Year"
  )
  y <- list(
    title = paste0("Mean PM2.5 from ", year.val, " (micro g/(cubic m))")
  )

#The code below causes the graph used in the 2nd holiday plot to be shaded based off the PM2.5 color scale rather than plain in color # 
  fig<-plot_ly(dat.all)
  fig<-fig%>%layout(xaxis = x, yaxis = y, shapes = list(
    list(type = "rect",
         fillcolor = "green",layer='below',
         line = list(color = "green"), opacity = 0.2,
         x0 = 1, x1 = 366, xref = "x",
         y0 = 0, y1 = 12, yref = "y"),
    list(type = "rect",
         fillcolor = "yellow", layer='below',
         line = list(color = "yellow"), opacity = 0.2,
         x0 = 1, x1 = 366, xref = "x",
         y0 = 12.1, y1 = 35.4, yref = "y"),
    list(type = "rect",
         fillcolor = "orange", layer='below',
         line = list(color = "orange"), opacity = 0.2,
         x0 = 1, x1 = 366, xref = "x",
         y0 = 35.5, y1 = 55.4, yref = "y"),
    if(max(dat.all$value+1) < 75) {list(type = "rect",
         fillcolor = "red", layer='below',
         line = list(color = "red"), opacity = 0.2,
         x0 =1, x1 =366, xref = "x",
         #Adjusted to satisfy requests regarding graphs y-axis being large for all years when only needed for some years#
         y0 = 55.5, y1 =  75, yref = "y")},
    if(max(dat.all$value) > 75) { list(type = "rect",
         fillcolor = "red", layer='below',
         line = list(color = "red"), opacity = 0.2,
         x0 =1, x1 =366, xref = "x",
         y0 = 55.5, y1 =  max(dat.all$value+10), yref = "y")}
    )) %>%
#Allows the user to hover over data on the graph to see site name, PM2.5 levels, etc. #
    add_trace( x = ~day_of_year, y = ~value,type = 'scatter', mode = 'markers', 
               text = ~paste("Site: ", site.name, ', PM2.5: ', value, ", Date:", date),
               color = ~color, colors = c("grey50", "red"), type = "scatter"
               ,showlegend = F) %>%
    config(displayModeBar = FALSE) %>%
    layout(showlegend = FALSE, dragmode = FALSE)
  fig
  
}

#####################################################################################################################
# Weather Tab Plot - this function creates a dot plot for the PM 2.5 values, either daily or weekly (user selected) # 
# against the following weather variables:                                                                          #
# 1. Minimum Temperature (F)                                                                                        #
# 2. Maximum Temperature (F)                                                                                        #
# 3. Daily precipitation (in)                                                                                       #
# 4. Daily snow accumulation (in)                                                                                   #
# 5. Daily snow depth (in)                                                                                          #
# Users can also select whether they want to see all sites or just a chosen site on the plot. Since weather data    #
# were only available for the Twin Cities, those are the only sites available to view and choose from on the plot   #
# The background shows the EPA color scale for PM2.5 values with values ranging from healthy to unhealthy.          # 
# Credit: Maria Masotti                                                                                             #
#####################################################################################################################

make_weather_plot <- function(site, weather.var, data, year, time) {
  # filter to only the site that was chosen:
  if(site == "All Sites") {site.dat = data}
  if(site != "All Sites") {site.dat = filter(data, site.name == site) }
  
  dat<-site.dat%>%select(site.name,months_days,paste0("PM2.5_",year),paste0(weather.var,"_",year))%>%na.omit()
  
  colnames(dat)[2:4]<-c("Time","PM2.5","weather")
  
  dat<-dat%>%mutate(weather=as.numeric(ifelse(weather=="T",0,weather)))
  
  if(time=="Weekly"){
    dat<-dat%>%mutate(date=as.Date(paste0(Time,"/",year),format= "%m/%d/%Y"),Time=week(date))%>%group_by(site.name,Time)%>%
      summarise(weather=mean(weather,na.rm=T),PM2.5=mean(PM2.5,na.rm=T))
  }
  if(weather.var%in%c("Minimum.Temperature.degrees..F.","Maximum.Temperature.degrees..F.")){
    dat <- dat%>%group_by(site.name) %>%
      mutate(fit = lm(PM2.5 ~ weather+I(weather^2))$fitted.values) %>%
      ungroup()
  }else{
    dat <- dat%>%group_by(site.name) %>%
      mutate(fit = lm(PM2.5 ~ weather)$fitted.values) %>%
      ungroup()
    
  }
  
  xax <- list(
    title = str_replace_all(str_replace_all(str_replace_all(weather.var,"[.]"," "),"degrees  F","(F)")," inches","(in.)"),zeroline = FALSE,
    showline = FALSE
  )
  yax <- list(
    title = paste0("PM2.5 from ", year, " (micro g/(cubic m))") ,zeroline = FALSE,
    showline = FALSE
  )
  
  time.var<-ifelse(time=="Weekly","Week","Day")
  
#The code below causes the table used in the weather plot to be shaded based off the PM2.5 color scale rather than plain in color #
  fig<-plot_ly(dat)
  fig<-fig%>%layout(xaxis = xax, yaxis = yax,shapes = list(
    list(type = "rect",
         fillcolor = "green",layer='below',
         line = list(color = "green"), opacity = 0.2,
         x0 = min(dat$weather), x1 = max(dat$weather), xref = "xax",
         y0 = 0, y1 = 12, yref = "yax"),
    list(type = "rect",
         fillcolor = "yellow", layer='below',
         line = list(color = "yellow"), opacity = 0.2,
         x0 = min(dat$weather), x1 = max(dat$weather), xref = "xax",
         y0 = 12.1, y1 = 35.4, yref = "yax"),
    list(type = "rect",
         fillcolor = "orange", layer='below',
         line = list(color = "orange"), opacity = 0.2,
         x0 = min(dat$weather), x1 = max(dat$weather), xref = "xax",
         y0 = 35.5, y1 = 55.4, yref = "yax"),
    list(type = "rect",
         fillcolor = "red", layer='below',
         line = list(color = "red"), opacity = 0.2,
         x0 = min(dat$weather), x1 = max(dat$weather), xref = "xax",
         y0 = 55.5, y1 = 72, yref = "yax"))) %>%add_trace( x = ~weather, y = ~PM2.5,type = 'scatter', mode = 'markers', text = ~paste( "PM2.5 for",site.name,"on",time.var,Time,":", PM2.5),hoverinfo = 'text', marker = list(color='black'),showlegend = F)%>%add_lines(x = ~weather, y = ~fit,color=~site.name,showlegend=T) %>%
    config(displayModeBar = FALSE) %>%
    layout(showlegend = FALSE, dragmode = FALSE)
  
  fig
}

###########################################################################################################
# Color scale function - creates the table that acts as a legend for the plots that include the EPA color #
# scale for levels of concern for PM2.5 values.                                                           #
# Credit: Rachel Zilinskas                                                                                #
# Edited: Kody DeGolier,  Summer 2022                                                                     #
###########################################################################################################

make_color_scale <- function(){
  x <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy")
  Minimum <- c(0, 12.1, 35.5, 55.5)
  Maximum <- c(12, 35.4, 55.4, 150.4)
  tab <- cbind.data.frame(x, Minimum, Maximum)
  names(tab) <- c("Level of Concern", "Minimum", "Maximum")
  tab <- gt(tab)
  
  tab <- tab %>%  tab_style(
    style = cell_fill(color = "#CCE6CC"), # Can change the color by editing after color = " #
    locations = cells_body(
      rows = Minimum   == 0
    )) %>% tab_style(
    style = cell_fill(color = "#FFFFCC"),
    locations = cells_body(
      rows =  Minimum ==12.1
    )) %>% tab_style(
    style = cell_fill(color = "#FFEDCC"),
    locations = cells_body(
      rows =  Minimum ==35.5
    )) %>% tab_style(
    style = cell_fill(color = "#FFCCCC"),
    locations = cells_body(
      rows =  Minimum ==55.5
    ))

}

##################################################################################
# About PM2.5 text function - displays the selected category in advanced detail  #
# Website where information was obtained included below                          #
# https://www.epa.gov/pmcourse/patient-exposure-and-air-quality-index            #
# Credit: Kody DeGolier                                                          #
# Added: August 2022                                                             #
##################################################################################
pm2.5_about_text <- function(selected_level) {
  
  # Good information
  if(selected_level == "Good"){
    
    
    text <- ("From the EPA Patient Exposure and the Air Quality Index regarding good PM2.5 values: 
             Air quality is good and poses little or no risk.")
  }
  # Moderate information
  if(selected_level == "Moderate"){
    
    
    text <- ("From the EPA Patient Exposure and the Air Quality Index regarding Moderate PM2.5 values:
              Air quality is acceptable; however, there may be some health concern for a small number of unusually sensitive people.")
  }
  # Unhealthy for sensitive groups information
  if(selected_level == "Unhealthy for Sensitive Groups"){
    
    
    text <- ("From the EPA Patient Exposure and the Air Quality Index regarding Unhealthy for Sensitive Groups PM2.5 values: 
              When air quality is in this range, people who are in sensitive groups, whether the increased risk is due to 
              medical conditions, exposure conditions, or innate susceptibility, may experience health effects when engaged
              in outdoor activities. However, exposures to ambient concentrations in this range are not likely to result in
              effects in the general population.")
  }
  # Unhealthy information 
  if(selected_level == "Unhealthy"){
    
    text <- ("From the EPA Patient Exposure and the Air Quality Index regarding Unhealthy PM2.5 values:
              When air quality is in this range, everyone who is active outdoors may experience effects. Members of sensitive 
              groups are likely to experience more serious effects")
  }
  # Final output 
  print(text)
}
  
  
