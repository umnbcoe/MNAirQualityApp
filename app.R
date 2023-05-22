#################################################################################################
# Authors: Rachel Zilinskas, Lillian Haine, Sarah Samorodnitsky, Sharon Ling, and Maria Masotti #
#################################################################################################
# Original creation date: May 2021                                                              #
#################################################################################################
# Altered and updated: August 2022                                                              #
# Editor: Kody DeGolier                                                                         #
#################################################################################################
# Purpose: creates an interactive Shiny app to serve as a tool for SPPS teachers to include in  #
# their earth science curriculum as a part of the SPPS and UMN Biostat Outreach Committee       #
# collaboration. Students can use the app to explore many factors affecting air quality (PM2.5) #
# values in the state of Minnesota                                                              #
#################################################################################################

# Calling required packages 
library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyr)
library(tidyverse)
library(lubridate)


# Sourcing helper documents which involve data reading and cleaning steps and functions to create plots and images 
source("helper.R")
source("dataCleaning.R")


# User interface ----
ui <- fluidPage(
  # Creating tabs
  tabsetPanel(
    # Homepage/Intro tab
    tabPanel("Home",
             # Side panel which includes some introductory information 
             sidebarPanel(
               h2("Welcome to the Air Quality Investigation Tool!"),
               h4("This app will allow you to learn more about factors that impact air quality in Minnesota. Click on 
                  the tabs at the top to explore how seasons, weather, holidays, and human activities can affect
                  the air that we breathe in Minnesota."),
               h4("There are 27 air quality monitoring sites across the state on Minnesota. Before you get started, take
                  a look at the map to determine which sites are the most important to you. Click on any blue dot to show
                  the name of an air quality monitoring site. Remember the names of the points you are interested in 
                  so that you can explore them in the other tabs. To add a location to the map, type in an address and click
                  'Show My Point' to add a location to the map."),
               # Option for the user to type an address of their choice to add to the map
               helpText("Add a point of your choice to the map to see the Air Quality Monitoring Site closest to you!"),
               textInput("home_point",
                         label= "Type an address", 
                         value = "",
                         placeholder = "Ex: '123 Main St Minneapolis, MN' or 'US Bank Stadium'"),
               actionButton("update_home_plot", "Show My Point")
               ),
             # Creating map output 
             mainPanel(
               leafletOutput("home_map", width = "100%", height = 800),
               )),
    # PM2.5 tab
    # Added in as part of the 2022 updates
    tabPanel("PM2.5 Explained",
             # Side panel which includes some introductory information 
             sidebarPanel(
               h4("PM2.5 is the amount of small particles of air pollution in the air with diameters of 2.5 micrometers or smaller. 
                  For reference a strand of Human Hair is between 50 and 70 micrometers in diameter. These fine particles contribute to 
                  reduced visability (haze), environmental damage, and materials damage. The units for PM2.5 are micrograms per cubic meter of
                  air referred to as: micro g/(cubic m)"),
               # Allows the user to choose a level to learn more about #
               selectInput(inputId = "selected_level", "Choose a PM2.5 level to learn more about",
                           c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy")),
               h6("For more information regarding health effects and other harmful effects caused by microscopic particles in the air
                  go to https://www.epa.gov/pm-pollution/health-and-environmental-effects-particulate-matter-pm"),
             ),
             # Creating text output 
             mainPanel(
               br(),
               # Defining PM2.5 and including EPA color scale used on most graphs for level of concern for reference 
               h4("Displayed below is a color scale which is also on other tabs in this web application. The colors reflect how healthy 
                   or unhealthy the air quality measures are in terms of PM 2.5 Units:"),
               br(),
               #Showing colorscale that is being explained by ranking in PM2.5
               gt_output("colorscale_explained"),
               br(),
               #Explanation for chosen level of PM2.5
               textOutput("explanation"),
               #Defines sensitive subgroup mentioned
               h4("For particle pollution, the sensitive groups include people with heart and lung disease, older adults, children, 
                   people with diabetes, and people who are generally more exposed to hazardous conditions because of work or 
                   living circumstances."),
               #Summarizes findings
               h4("Reminder: When looking at the data in following tabs, a higher PM2.5 number indicates a worse air quality 
                  experienced by those in that area.")
             )),
    # Seasonal tab
    tabPanel("Seasonal AQI",
             sidebarLayout(
               sidebarPanel(
                 # Adding some background information for the user
                 h3("This tab allows you to explore the seasonal trends in air quality measures. Use the dropdown menus below
                    to compare the seasonal average air quality measures for each site from the years 2015 - 2021."),
                 # User chooses which year they'd like to see
                 selectInput(inputId = "year", "Choose the year you would like to see:",
                             c("2015" = "2015", "2016" = "2016", 
                               "2017" = "2017", "2018" = "2018",
                               "2019" = "2019", "2020" = "2020",
                               "2021" = "2021"),
                             selected = "2020"),
                 # User chooses the two sites they'd like to compare
                 selectInput(inputId = "site1", "Choose your first site:", choices = c(sites), selected = "Harding High School"),
                 selectInput(inputId = "site2", "Choose your second site:", choices = c(sites), selected = "Boundary Waters")
                 ),
               # Make a line graph for seasonal changes for selected year
               mainPanel(plotlyOutput("line_graph"),
                         h5("Note: if you click on a site in the legend, it will remove it from the graph. Simply click again to add it back."),
                         h4("A reminder of the PM2.5 color scale is:"),
                         gt_output("colorscale1"))
             )),
    # Monthly tab
    tabPanel("Monthly AQI",
             sidebarLayout(
               # Adding some background information for the user
               sidebarPanel(
                 h3("This tab allows you to explore the monthly trends in air quality measures. Use the dropdown menus below
                    to plot the monthly average air quality measures for sites from the years 2015 - 2021. When you choose more than five sites,
                    a boxplot of the average monthly PM2.5 measures will overlay the line graphs."),
                 # User selects year and sites they would like to see on the map
                 selectInput(inputId = "month_year", label = "Choose year:", c("2015" = "2015", "2016" = "2016", "2017" = "2017", "2018" = "2018", "2019" = "2019", "2020" = "2020", "2021" = "2021"),
                             selected = "2020"),
                 selectInput(inputId = "month_sites", label = "Choose AQI monitoring site(s): ", siteNames, selected = c("Harding High School", "Ramsey Health Center", "Boundary Waters"), multiple=TRUE),
                 # Further instructions for the drop down menu 
                 helpText("To add a site to the plot, click on the site name in the dropdown menu. To remove a site, press 'backspace' or 'delete.' ")
                 ),
               # Creating main graph output 
               mainPanel(plotOutput("graph"), br(), h4("A reminder of the PM2.5 color scale is:"),
                         gt_output("colorscale4"))
             )),
    # Weather tab
    tabPanel("Weather and AQI",
             sidebarLayout(
               # Including some background information 
               sidebarPanel(
                 h3("This tab allows you to explore trends in air quality measures related to weather. Use the dropdown menus below
                    to plot either daily or monthly air quality measures against different weather variables for each site from the 
                    years 2015 - 2021.  For this tab, only sites located in the Twin Cities area are available."),
                 helpText("Choose a variable and year to see how AQI changes with the weather!"),
                 # Allowing user to choose weather variable
                 selectInput("weather_variable", 
                             label = "Choose a Variable",
                             choices = c("Minimum Daily Temperature (F)", 
                                         "Maximum Daily Temperature (F)", 
                                         "Daily Precipitation (Inches)",
                                         "Daily Snow Accumulation (Inches)", 
                                         "Daily Snow Depth (Inches)"), 
                             selected = "Minimum Daily Temperature (F)"), 
                 # Allowing user to choose year 
                 selectInput("weather_year", 
                             label = "Choose a Year",
                             choices = c("2015", 
                                         "2016", 
                                         "2017", 
                                         "2018", 
                                         "2019",
                                         "2020",
                                         "2021"),
                             selected = "2020"),
                 # Allowing user to choose which site in the Twin Cities included on the map 
                 selectInput("weather_site", 
                             label = "Choose a Site (Option to choose All SITES)",
                             choices = c("All Sites", 
                                         "Andersen School", 
                                         "Near Road I-35/I-94", 
                                         "Harding High School",
                                         "St. Louis Park City Hall",
                                         "Ramsey Health Center"),
                             selected = "All Sites"),
                 # Allowing user to choose x axis variable 
                 selectInput("weather_time", 
                             label = "Choose a Time Variable",
                             choices = c("Daily", 
                                         "Weekly"),
                             selected = "Weekly")
                 ), 
               # Creating main plot
               mainPanel(
               # Calling weather plot function 
               plotlyOutput("weather_plot"), 
               br(),
               # Including EPA color scale for reference 
               h4("A reminder of the PM2.5 color scale is:"),
               gt_output("colorscale2")
                
                )
               )),
    # Daily/Holidays tab
    tabPanel("Daily/Holidays and AQI",
             sidebarLayout(
               sidebarPanel(
                 # Adding some background information 
                 h3("This tab allows you to explore trends in air quality measures related to specific days in the year. Choose
                    a holiday from the dropdown menu to explore how the air quality measure compares across different years. 
                    Look at the bottom plot to see how the holiday AQI values compare to other days for a chosen year."),
                 # Allowing user to choose holiday 
                 selectInput("holiday", 
                             label = "Choose a Holiday",
                             choices = c("New Year's Day", 
                                         "Martin Luther King Jr. Day", 
                                         "Cinco de Mayo", 
                                         "Memorial Day",  
                                         "Last Day of School", 
                                         "Juneteenth", 
                                         "July 4th", 
                                         "July 5th", 
                                         "End of Ramadan" , 
                                         "Labor Day", 
                                         "Rosh Hashanah",
                                         "Talk Like a Pirate Day", 
                                         "US Indigenous People's Day", 
                                         "Halloween", 
                                         "Thanksgiving", 
                                         "Day after Thanksgiving", 
                                         "Christmas Day", 
                                         "New Year's Eve"), 
                             selected = "New Year's Day"), 
                 
                 # Allowing user to choose a site to be highlighted on the holidays plot 
                 selectInput("holiday_site", 
                             label = "Choose a Site to Highlight (Option to choose All SITES)",
                             choices = c("All Sites", 
                                         "Andersen School", 
                                         "Andersen Windows North", 
                                         "Andersen Windows South", 
                                         "Anoka County Airport", 
                                         "Apple Valley" , 
                                         "B.F. Pearson School" , 
                                         "Ben Franklin School", 
                                         "Blue Mounds", 
                                         "Bottineau / Marshall Terrace", 
                                         "Boundary Waters", 
                                         "Brainerd Lakes Regional Airport", 
                                         "Fond du Lac Band", 
                                         "FWS Wetland Management District", 
                                         "Grand Portage Band", 
                                         "Great River Bluffs", 
                                         "Harding High School", 
                                         "Laura MacArthur School", 
                                         "Leech Lake Nation", 
                                         "Lyndale Neighborhood", 
                                         "Michigan Street", 
                                         "Near Road I-35" , 
                                         "Near Road I-35/I-94", 
                                         "Payne-Phalen Neighborhood", 
                                         "Ramsey Health Center", 
                                         "Red Lake Nation" , 
                                         "South St. Anthony Park", 
                                         "Southwest Minnesota Regional Airport", 
                                         "St. Louis Park City Hall", 
                                         "St. Michael Elementary School", 
                                         "Talahi School", 
                                         "U of M - Duluth", 
                                         "Virginia City Hall", 
                                         "Voyageurs NP - Sullivan Bay"),
                             selected = "All Sites"), 
                 # Allowing user to choose year 
                 selectInput("holiday_year", 
                             label = "Choose a Year to Investigate",
                             choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                             selected = "2020"),
               ),
               # Output for main panel 
               mainPanel(textOutput("text1"),
                         # Holiday plot 
                         plotlyOutput("plot1"),
                         textOutput("text2"),
                         # Plotting holiday (highlighted) with other days of the year to compare
                         plotlyOutput("plot2"), 
                         textOutput("text3"),   
                         br(),
                         # Including EPA color scale reminder 
                         h4("A reminder of the PM2.5 color scale is:"),
                         gt_output("colorscale3")
               ))
    ),
    
    # Human Impact Tab 
    tabPanel("Human Impact and AQI",
             sidebarLayout(
               sidebarPanel(
                 # Including some background information 
                 h4("This tab allows you to explore how human activities impact air quality. Activities to explore 
                    include driving, power plant operations, population density. You can also explore if air quality
                    differs between areas with different median income."),
                 # Allowing user to choose the year of interest 
                 helpText("Choose a year to explore how average air quality across Minnesota changes with time"),
                 selectInput("year_human_impact", 
                             label = "Choose a Year to Investigate",
                             choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021"), 
                             selected = "2020"),
                 # Allowing user to choose the human impact factor of interest 
                 helpText("Choose a factor to explore how it impacts average air quality measures across Minnesota"),
                 selectInput("measure", 
                             label = "Choose a factor",
                             choices = c("Distance to Nearest Interstate",
                                         "Distance to Nearest Power Plant",
                                         "Population Density", 
                                         "Median Income"), 
                             selected = "Distance to Nearest Interstate"),
                 # Allowing user to highlight a site of interest 
                 helpText("Choose a site you want to highlight on the plot"),
                 selectInput("plot_site", 
                             label = "Choose a site",
                             choices = c("All Sites", yearly_data$site.name), 
                             selected = "All Sites"),
                 # Allowing user to choose sites to include on map 
                 helpText("Choose the site(s) you want to see on the map. Its closest power plant will automatically show in red."),
                 selectInput("map_sites", 
                             label = "Minnesota Air Quality Monitoring Sites",
                             choices = map_info$site.name,
                             selected = c("Harding High School", "Ramsey Health Center"),
                             multiple = TRUE), 
                 # Allowing user to type an address to include on the map 
                 helpText("To add a site to the plot, click on the site name in the dropdown menu. To remove a site, press 'backspace' or 'delete.' "),
                 helpText("Optional: Add a point of your choice to the map!"),
                 textInput("point",
                           label= "Type an address", 
                           value = "",
                           placeholder = "Ex: '123 Main St Minneapolis, MN' or 'US Bank Stadium'"),
                 # Including a button to update the map so that the map doesn't update as the user types
                 actionButton("update_plot", "Show New Map")
                 
               ), 
               mainPanel(
                 # Human impact plot output 
                 plotlyOutput("plot"),
                 br(),
                 br(),
                 # Including slope and/or R^2 interpretations 
                 textOutput("interpretations"),
                 br(),
                 br(),
                 # Including map 
                 leafletOutput("map")
               )
             )
    ), 
#Updated layout to be more visually appealing  
    tabPanel("About the Developers",
             sidebarLayout(
               sidebarPanel(
                 h1("Meet the Team!"),
                 br(),
                 h3("We are a team of graduate students and educators from the Department of
                    Biostatistics in the School of Public Health at the University of Minnesota.
                    We are part of a committee that engages in community outreach
                    as a way to promote biostatistics eduacation and provide opportunities for all to learn
                    more about statistics and public health. We worked as a team to collaborate on creating this 
                    investigation tool as a partnership with the St. Paul Public School System."),
                 ),
               mainPanel( #Adjust width and height % to obtain desired visual display 
                 img(src = "bios1.jpg", align = "left", height = "70%", width = "50%"),
                 img(src = "bios2.jpg", align = "left", height = "70%", width = "50%"),
                 img(src = "sharon.jpg", align = "center", height = "50%", width = "80%"),
                 img(src = "bios3.jpg", align ="right", height = "15%", width = "20%")
               )
             ))
  )
)


server <- function(input, output) {
  # Human impact tab
  output$plot <- renderPlotly({
    var1 <- switch(input$year_human_impact, 
                   "2015" = "mean_pm2.5_2015_from2.5", "2016" = "mean_pm2.5_2016_from2.5", "2017" = "mean_pm2.5_2017_from2.5",
                   "2018" = "mean_pm2.5_2018_from2.5", "2019" = "mean_pm2.5_2019_from2.5", "2020" = "mean_pm2.5_2020_from2.5",
                   "2021" = "mean_pm2.5_2021_from2.5")
    make_impact_plot(input$measure, var1, yearly_data, input$plot_site) 
  })
    output$interpretations <- renderText({ 
      var2 <- switch(input$year_human_impact, 
                     "2015" = "mean_pm2.5_2015_from2.5", "2016" = "mean_pm2.5_2016_from2.5", "2017" = "mean_pm2.5_2017_from2.5",
                     "2018" = "mean_pm2.5_2018_from2.5", "2019" = "mean_pm2.5_2019_from2.5", "2020" = "mean_pm2.5_2020_from2.5",
                     "2021" = "mean_pm2.5_2021_from2.5")
      impact_plot_text(input$measure, var2, yearly_data) 
    })
  # Home page map plot call 
  new_home <- eventReactive(input$update_home_plot, {
    make_homepage_dat(input$home_point)
  }, ignoreNULL = FALSE)
  output$home_map <- renderLeaflet({
    make_homepage_map(new_home())
  })
  new <- eventReactive(input$update_plot, {
    make_map_dat(input$map_sites, input$point)
  }, ignoreNULL = FALSE)
  output$map <- renderLeaflet({
    make_map(new())
  })
  #PM2.5 text call
  output$explanation <- renderText({
    pm2.5_about_text(input$selected_level)
  })
  
  # Seasonal AQI tab plot call 
  output$line_graph <- renderPlotly({
    seasonal_plot(input$site1, input$site2, input$year)
  })
  # Monthly AQI tab plot call 
  output$graph <- renderPlot({
    monthly_plot(input$month_sites, input$month_year)
  })
  # Weather and AQI tab plot call 
  output$weather_plot <- renderPlotly({
    var <- switch(input$weather_variable, 
                  "Minimum Daily Temperature (F)" = "Minimum.Temperature.degrees..F.",
                  "Maximum Daily Temperature (F)" = "Maximum.Temperature.degrees..F.",
                  "Daily Precipitation (Inches)" = "Precipitation..inches.",
                  "Daily Snow Accumulation (Inches)"= "Snow..inches.", 
                  "Daily Snow Depth (Inches)"="Snow.Depth..inches."
    )
    make_weather_plot(input$weather_site, var, weather_data, input$weather_year,input$weather_time)
  })
  # Daily/Holidays and AQI tab text to prompt user input 
  output$text1 <- renderText( { print( "Plot the holiday choosen across the years. 
                                       Start with July 4th and July 5th and see the increase in air quality from 2019 to 2020. Any theories on why that would happen? 
                                       Then look at your favorite days from the list provided and see if there are any patterns across the years. Are there particular years that are higher or  lower than other years, 
                                       Any explanations on why?") })
  
  # Daily/Holidays and AQI tab plot 1 call 
  output$plot1 <- renderPlotly({
    date <- switch(input$holiday, 
                   "New Year's Day" = c(as.Date("01/01/2015", format = "%m/%d/%Y"), 
                                        as.Date("01/01/2016", format = "%m/%d/%Y"), 
                                        as.Date("01/01/2017", format = "%m/%d/%Y"), 
                                        as.Date("01/01/2018", format = "%m/%d/%Y"), 
                                        as.Date("01/01/2019", format = "%m/%d/%Y"), 
                                        as.Date("01/01/2020", format = "%m/%d/%Y"),
                                        as.Date("01/01/2021", format = "%m/%d/%Y")),
                   "Martin Luther King Jr. Day" = c(as.Date("01/19/2015", format = "%m/%d/%Y"), 
                                                    as.Date("01/18/2016", format = "%m/%d/%Y"), 
                                                    as.Date("01/16/2017", format = "%m/%d/%Y"), 
                                                    as.Date("01/15/2018", format = "%m/%d/%Y"), 
                                                    as.Date("01/20/2019", format = "%m/%d/%Y"),
                                                    as.Date("01/20/2020", format = "%m/%d/%Y"),
                                                    as.Date("01/18/2021", format = "%m/%d/%Y")), 
                   "Cinco de Mayo" = c(as.Date("05/05/2015", format = "%m/%d/%Y"), 
                                       as.Date("05/05/2016", format = "%m/%d/%Y"), 
                                       as.Date("05/05/2017", format = "%m/%d/%Y"), 
                                       as.Date("05/05/2018", format = "%m/%d/%Y"), 
                                       as.Date("05/05/2019", format = "%m/%d/%Y"), 
                                       as.Date("05/05/2020", format = "%m/%d/%Y"),
                                       as.Date("05/05/2021", format = "%m/%d/%Y")),
                   "Memorial Day" = c(as.Date("05/25/2015", format = "%m/%d/%Y"), 
                                      as.Date("05/30/2016", format = "%m/%d/%Y"), 
                                      as.Date("05/29/2017", format = "%m/%d/%Y"), 
                                      as.Date("05/28/2018", format = "%m/%d/%Y"), 
                                      as.Date("05/27/2019", format = "%m/%d/%Y"), 
                                      as.Date("05/25/2020", format = "%m/%d/%Y"),
                                      as.Date("05/31/2021", format = "%m/%d/%Y")),
                   "Last Day of School" = c(as.Date("06/09/2015", format = "%m/%d/%Y"), 
                                            as.Date("06/10/2016", format = "%m/%d/%Y"), 
                                            as.Date("06/09/2017", format = "%m/%d/%Y"), 
                                            as.Date("06/08/2018", format = "%m/%d/%Y"), 
                                            as.Date("06/07/2019", format = "%m/%d/%Y"), 
                                            as.Date("06/09/2020", format = "%m/%d/%Y"),
                                            as.Date("06/11/2021", format = "%m/%d/%Y")),
                   "Rosh Hashanah" = c(as.Date("09/15/2015", format = "%m/%d/%Y"), 
                                       as.Date("10/04/2016", format = "%m/%d/%Y"), 
                                       as.Date("09/22/2017", format = "%m/%d/%Y"), 
                                       as.Date("09/11/2018", format = "%m/%d/%Y"), 
                                       as.Date("10/01/2019", format = "%m/%d/%Y"), 
                                       as.Date("09/20/2020", format = "%m/%d/%Y"),
                                       as.Date("09/06/2021", format = "%m/%d/%Y")), 
                   "Juneteenth" = c(as.Date("06/19/2015", format = "%m/%d/%Y"), 
                                    as.Date("06/19/2016", format = "%m/%d/%Y"), 
                                    as.Date("06/19/2017", format = "%m/%d/%Y"), 
                                    as.Date("06/19/2018", format = "%m/%d/%Y"), 
                                    as.Date("06/19/2019", format = "%m/%d/%Y"), 
                                    as.Date("06/19/2020", format = "%m/%d/%Y"),
                                    as.Date("06/19/2021", format = "%m/%d/%Y")), 
                   "July 4th" = c(as.Date("07/04/2015", format = "%m/%d/%Y"), 
                                  as.Date("07/04/2016", format = "%m/%d/%Y"), 
                                  as.Date("07/04/2017", format = "%m/%d/%Y"), 
                                  as.Date("07/04/2018", format = "%m/%d/%Y"), 
                                  as.Date("07/04/2019", format = "%m/%d/%Y"), 
                                  as.Date("07/04/2020", format = "%m/%d/%Y"),
                                  as.Date("07/04/2021", format = "%m/%d/%Y")),
                   "July 5th" = c(as.Date("07/05/2015", format = "%m/%d/%Y"), 
                                  as.Date("07/05/2016", format = "%m/%d/%Y"), 
                                  as.Date("07/05/2017", format = "%m/%d/%Y"), 
                                  as.Date("07/05/2018", format = "%m/%d/%Y"), 
                                  as.Date("07/05/2019", format = "%m/%d/%Y"), 
                                  as.Date("07/05/2020", format = "%m/%d/%Y"),
                                  as.Date("07/05/2021", format = "%m/%d/%Y")),
                   "End of Ramadan" = c(as.Date("07/17/2015", format = "%m/%d/%Y"), 
                                        as.Date("07/06/2016", format = "%m/%d/%Y"), 
                                        as.Date("06/25/2017", format = "%m/%d/%Y"), 
                                        as.Date("06/14/2018", format = "%m/%d/%Y"), 
                                        as.Date("06/03/2019", format = "%m/%d/%Y"), 
                                        as.Date("05/23/2020", format = "%m/%d/%Y"),
                                        as.Date("05/13/2021", format = "%m/%d/%Y")),
                   "Labor Day" = c(as.Date("09/07/2015", format = "%m/%d/%Y"), 
                                   as.Date("09/05/2016", format = "%m/%d/%Y"), 
                                   as.Date("09/04/2017", format = "%m/%d/%Y"), 
                                   as.Date("09/03/2018", format = "%m/%d/%Y"), 
                                   as.Date("09/02/2019", format = "%m/%d/%Y"), 
                                   as.Date("09/07/2020", format = "%m/%d/%Y"),
                                   as.Date("09/06/2021", format = "%m/%d/%Y")), 
                   "Talk Like a Pirate Day" = c(as.Date("09/19/2015", format = "%m/%d/%Y"), 
                                                as.Date("09/19/2016", format = "%m/%d/%Y"), 
                                                as.Date("09/19/2017", format = "%m/%d/%Y"), 
                                                as.Date("09/19/2018", format = "%m/%d/%Y"), 
                                                as.Date("09/19/2019", format = "%m/%d/%Y"), 
                                                as.Date("09/19/2020", format = "%m/%d/%Y"),
                                                as.Date("09/19/2021", format = "%m/%d/%Y")),
                   "US Indigenous People's Day" = c(as.Date("10/12/2015", format = "%m/%d/%Y"), 
                                                    as.Date("10/10/2016", format = "%m/%d/%Y"), 
                                                    as.Date("10/09/2017", format = "%m/%d/%Y"), 
                                                    as.Date("10/08/2018", format = "%m/%d/%Y"), 
                                                    as.Date("10/14/2019", format = "%m/%d/%Y"), 
                                                    as.Date("10/12/2020", format = "%m/%d/%Y"),
                                                    as.Date("10/11/2021", format = "%m/%d/%Y")), 
                   "Halloween" = c(as.Date("10/31/2015", format = "%m/%d/%Y"), 
                                   as.Date("10/31/2016", format = "%m/%d/%Y"), 
                                   as.Date("10/31/2017", format = "%m/%d/%Y"), 
                                   as.Date("10/31/2018", format = "%m/%d/%Y"), 
                                   as.Date("10/31/2019", format = "%m/%d/%Y"), 
                                   as.Date("10/31/2020", format = "%m/%d/%Y"),
                                   as.Date("10/31/2021", format = "%m/%d/%Y")), 
                   "Thanksgiving" = c(as.Date("11/26/2015", format = "%m/%d/%Y"), 
                                      as.Date("11/24/2016", format = "%m/%d/%Y"), 
                                      as.Date("11/23/2017", format = "%m/%d/%Y"), 
                                      as.Date("11/22/2018", format = "%m/%d/%Y"), 
                                      as.Date("11/28/2019", format = "%m/%d/%Y"), 
                                      as.Date("11/26/2020", format = "%m/%d/%Y"),
                                      as.Date("11/25/2021", format = "%m/%d/%Y")), 
                   "Day after Thanksgiving" = c(as.Date("11/27/2015", format = "%m/%d/%Y"), 
                                                as.Date("11/25/2016", format = "%m/%d/%Y"), 
                                                as.Date("11/24/2017", format = "%m/%d/%Y"), 
                                                as.Date("11/23/2018", format = "%m/%d/%Y"), 
                                                as.Date("11/29/2019", format = "%m/%d/%Y"), 
                                                as.Date("11/27/2020", format = "%m/%d/%Y"),
                                                as.Date("11/26/2021", format = "%m/%d/%Y")), 
                   "Christmas Day" = c(as.Date("12/25/2015", format = "%m/%d/%Y"), 
                                       as.Date("12/25/2016", format = "%m/%d/%Y"), 
                                       as.Date("12/25/2017", format = "%m/%d/%Y"), 
                                       as.Date("12/25/2018", format = "%m/%d/%Y"), 
                                       as.Date("12/25/2019", format = "%m/%d/%Y"), 
                                       as.Date("12/25/2020", format = "%m/%d/%Y"),
                                       as.Date("12/25/2021", format = "%m/%d/%Y")), 
                   "New Year's Eve" = c(as.Date("12/31/2015", format = "%m/%d/%Y"), 
                                        as.Date("12/31/2016", format = "%m/%d/%Y"), 
                                        as.Date("12/31/2017", format = "%m/%d/%Y"), 
                                        as.Date("12/31/2018", format = "%m/%d/%Y"), 
                                        as.Date("12/31/2019", format = "%m/%d/%Y"), 
                                        as.Date("12/31/2020", format = "%m/%d/%Y"),
                                        as.Date("12/31/2020", format = "%m/%d/%Y")))
    make_holiday_plot(input$holiday_site, date, daily, input$holiday)
    
  })
  output$text2 <- renderText({ print( "If you select a site it will highlight it in red with all other sites in grey 
                                      in the plot above. Play around and see if there are any sites that change a lot in one day over the years or spike one day over the years. ") })
  # Daily/Holidays and AQI tab plot 2 call 
  output$plot2 <- renderPlotly({date <- switch(input$holiday, 
                                               "New Year's Day" = c(as.Date("01/01/2015", format = "%m/%d/%Y"), 
                                                                    as.Date("01/01/2016", format = "%m/%d/%Y"), 
                                                                    as.Date("01/01/2017", format = "%m/%d/%Y"), 
                                                                    as.Date("01/01/2018", format = "%m/%d/%Y"), 
                                                                    as.Date("01/01/2019", format = "%m/%d/%Y"), 
                                                                    as.Date("01/01/2020", format = "%m/%d/%Y"),
                                                                    as.Date("01/01/2021", format = "%m/%d/%Y")),
                                               "Martin Luther King Jr. Day" = c(as.Date("01/19/2015", format = "%m/%d/%Y"), 
                                                                                as.Date("01/18/2016", format = "%m/%d/%Y"), 
                                                                                as.Date("01/16/2017", format = "%m/%d/%Y"), 
                                                                                as.Date("01/15/2018", format = "%m/%d/%Y"), 
                                                                                as.Date("01/20/2019", format = "%m/%d/%Y"),
                                                                                as.Date("01/18/2021", format = "%m/%d/%Y")), 
                                               "Cinco de Mayo" = c(as.Date("05/05/2015", format = "%m/%d/%Y"), 
                                                                   as.Date("05/05/2016", format = "%m/%d/%Y"), 
                                                                   as.Date("05/05/2017", format = "%m/%d/%Y"), 
                                                                   as.Date("05/05/2018", format = "%m/%d/%Y"), 
                                                                   as.Date("05/05/2019", format = "%m/%d/%Y"), 
                                                                   as.Date("05/05/2020", format = "%m/%d/%Y"),
                                                                   as.Date("05/05/2021", format = "%m/%d/%Y")),
                                               "Memorial Day" = c(as.Date("05/25/2015", format = "%m/%d/%Y"), 
                                                                  as.Date("05/30/2016", format = "%m/%d/%Y"), 
                                                                  as.Date("05/29/2017", format = "%m/%d/%Y"), 
                                                                  as.Date("05/28/2018", format = "%m/%d/%Y"), 
                                                                  as.Date("05/27/2019", format = "%m/%d/%Y"), 
                                                                  as.Date("05/25/2020", format = "%m/%d/%Y"),
                                                                  as.Date("05/31/2021", format = "%m/%d/%Y")),
                                               "Last Day of School" = c(as.Date("06/09/2015", format = "%m/%d/%Y"), 
                                                                        as.Date("06/10/2016", format = "%m/%d/%Y"), 
                                                                        as.Date("06/09/2017", format = "%m/%d/%Y"), 
                                                                        as.Date("06/08/2018", format = "%m/%d/%Y"), 
                                                                        as.Date("06/07/2019", format = "%m/%d/%Y"), 
                                                                        as.Date("06/09/2020", format = "%m/%d/%Y"),
                                                                        as.Date("06/11/2021", format = "%m/%d/%Y")),
                                               "Rosh Hashanah" = c(as.Date("09/15/2015", format = "%m/%d/%Y"), 
                                                                   as.Date("10/04/2016", format = "%m/%d/%Y"), 
                                                                   as.Date("09/22/2017", format = "%m/%d/%Y"), 
                                                                   as.Date("09/11/2018", format = "%m/%d/%Y"), 
                                                                   as.Date("10/01/2019", format = "%m/%d/%Y"), 
                                                                   as.Date("09/20/2020", format = "%m/%d/%Y"),
                                                                   as.Date("09/06/2021", format = "%m/%d/%Y")), 
                                               "Juneteenth" = c(as.Date("06/19/2015", format = "%m/%d/%Y"), 
                                                                as.Date("06/19/2016", format = "%m/%d/%Y"), 
                                                                as.Date("06/19/2017", format = "%m/%d/%Y"), 
                                                                as.Date("06/19/2018", format = "%m/%d/%Y"), 
                                                                as.Date("06/19/2019", format = "%m/%d/%Y"), 
                                                                as.Date("06/19/2020", format = "%m/%d/%Y"),
                                                                as.Date("06/19/2021", format = "%m/%d/%Y")), 
                                               "July 4th" = c(as.Date("07/04/2015", format = "%m/%d/%Y"), 
                                                              as.Date("07/04/2016", format = "%m/%d/%Y"), 
                                                              as.Date("07/04/2017", format = "%m/%d/%Y"), 
                                                              as.Date("07/04/2018", format = "%m/%d/%Y"), 
                                                              as.Date("07/04/2019", format = "%m/%d/%Y"), 
                                                              as.Date("07/04/2020", format = "%m/%d/%Y"),
                                                              as.Date("07/04/2021", format = "%m/%d/%Y")),
                                               "July 5th" = c(as.Date("07/05/2015", format = "%m/%d/%Y"), 
                                                              as.Date("07/05/2016", format = "%m/%d/%Y"), 
                                                              as.Date("07/05/2017", format = "%m/%d/%Y"), 
                                                              as.Date("07/05/2018", format = "%m/%d/%Y"), 
                                                              as.Date("07/05/2019", format = "%m/%d/%Y"), 
                                                              as.Date("07/05/2020", format = "%m/%d/%Y"),
                                                              as.Date("07/05/2021", format = "%m/%d/%Y")),
                                               "End of Ramadan" = c(as.Date("07/17/2015", format = "%m/%d/%Y"), 
                                                                    as.Date("07/06/2016", format = "%m/%d/%Y"), 
                                                                    as.Date("06/25/2017", format = "%m/%d/%Y"), 
                                                                    as.Date("06/14/2018", format = "%m/%d/%Y"), 
                                                                    as.Date("06/03/2019", format = "%m/%d/%Y"), 
                                                                    as.Date("05/23/2020", format = "%m/%d/%Y"),
                                                                    as.Date("05/13/2021", format = "%m/%d/%Y")),
                                               "Labor Day" = c(as.Date("09/07/2015", format = "%m/%d/%Y"), 
                                                               as.Date("09/05/2016", format = "%m/%d/%Y"), 
                                                               as.Date("09/04/2017", format = "%m/%d/%Y"), 
                                                               as.Date("09/03/2018", format = "%m/%d/%Y"), 
                                                               as.Date("09/02/2019", format = "%m/%d/%Y"), 
                                                               as.Date("09/07/2020", format = "%m/%d/%Y"),
                                                               as.Date("09/06/2021", format = "%m/%d/%Y")), 
                                               "Talk Like a Pirate Day" = c(as.Date("09/19/2015", format = "%m/%d/%Y"), 
                                                                            as.Date("09/19/2016", format = "%m/%d/%Y"), 
                                                                            as.Date("09/19/2017", format = "%m/%d/%Y"), 
                                                                            as.Date("09/19/2018", format = "%m/%d/%Y"), 
                                                                            as.Date("09/19/2019", format = "%m/%d/%Y"), 
                                                                            as.Date("09/19/2020", format = "%m/%d/%Y"),
                                                                            as.Date("09/19/2021", format = "%m/%d/%Y")),
                                               "US Indigenous People's Day" = c(as.Date("10/12/2015", format = "%m/%d/%Y"), 
                                                                                as.Date("10/10/2016", format = "%m/%d/%Y"), 
                                                                                as.Date("10/09/2017", format = "%m/%d/%Y"), 
                                                                                as.Date("10/08/2018", format = "%m/%d/%Y"), 
                                                                                as.Date("10/14/2019", format = "%m/%d/%Y"), 
                                                                                as.Date("10/12/2020", format = "%m/%d/%Y"),
                                                                                as.Date("10/11/2021", format = "%m/%d/%Y")), 
                                               "Halloween" = c(as.Date("10/31/2015", format = "%m/%d/%Y"), 
                                                               as.Date("10/31/2016", format = "%m/%d/%Y"), 
                                                               as.Date("10/31/2017", format = "%m/%d/%Y"), 
                                                               as.Date("10/31/2018", format = "%m/%d/%Y"), 
                                                               as.Date("10/31/2019", format = "%m/%d/%Y"), 
                                                               as.Date("10/31/2020", format = "%m/%d/%Y"),
                                                               as.Date("10/31/2021", format = "%m/%d/%Y")), 
                                               "Thanksgiving" = c(as.Date("11/26/2015", format = "%m/%d/%Y"), 
                                                                  as.Date("11/24/2016", format = "%m/%d/%Y"), 
                                                                  as.Date("11/23/2017", format = "%m/%d/%Y"), 
                                                                  as.Date("11/22/2018", format = "%m/%d/%Y"), 
                                                                  as.Date("11/28/2019", format = "%m/%d/%Y"), 
                                                                  as.Date("11/26/2020", format = "%m/%d/%Y"),
                                                                  as.Date("11/25/2021", format = "%m/%d/%Y")), 
                                               "Day after Thanksgiving" = c(as.Date("11/27/2015", format = "%m/%d/%Y"), 
                                                                            as.Date("11/25/2016", format = "%m/%d/%Y"), 
                                                                            as.Date("11/24/2017", format = "%m/%d/%Y"), 
                                                                            as.Date("11/23/2018", format = "%m/%d/%Y"), 
                                                                            as.Date("11/29/2019", format = "%m/%d/%Y"), 
                                                                            as.Date("11/27/2020", format = "%m/%d/%Y"),
                                                                            as.Date("11/26/2021", format = "%m/%d/%Y")), 
                                               "Christmas Day" = c(as.Date("12/25/2015", format = "%m/%d/%Y"), 
                                                                   as.Date("12/25/2016", format = "%m/%d/%Y"), 
                                                                   as.Date("12/25/2017", format = "%m/%d/%Y"), 
                                                                   as.Date("12/25/2018", format = "%m/%d/%Y"), 
                                                                   as.Date("12/25/2019", format = "%m/%d/%Y"), 
                                                                   as.Date("12/25/2020", format = "%m/%d/%Y"),
                                                                   as.Date("12/25/2021", format = "%m/%d/%Y")), 
                                               "New Year's Eve" = c(as.Date("12/31/2015", format = "%m/%d/%Y"), 
                                                                    as.Date("12/31/2016", format = "%m/%d/%Y"), 
                                                                    as.Date("12/31/2017", format = "%m/%d/%Y"), 
                                                                    as.Date("12/31/2018", format = "%m/%d/%Y"), 
                                                                    as.Date("12/31/2019", format = "%m/%d/%Y"), 
                                                                    as.Date("12/31/2020", format = "%m/%d/%Y"),
                                                                    as.Date("12/31/2021", format = "%m/%d/%Y")))
  make_plot_365(date, daily, input$holiday, input$holiday_year)})
  
  output$text3 <- renderText( { print("Now choose a year and a holiday in the drop down menus. This plots EVERY day in that year, starting with January 1st. 
                                      You can hover over the points to see the site, the PM2.5 value, and the date (in format 'Year-Month-Day').
                                      The holiday you chose is now red and all the other days of the year are grey. 
                                      Do you see any spikes? See which days those are and think about why these values might be higher.") })
  
  # Creates all instances of the color scale on each tab except for the human impact one 
  output$colorscale <- 
    render_gt(make_color_scale())
  
  output$colorscale2 <- render_gt(make_color_scale())
  
  output$colorscale3 <- render_gt(make_color_scale())
  
  output$colorscale4 <- render_gt(make_color_scale())
  
  output$colorscale1 <- render_gt(make_color_scale())
  
  output$colorscale_explained <- render_gt(make_color_scale())
  
  }

# Run the app ----
shinyApp(ui = ui, server = server)

