
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(parsetR)

trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)
########################yusha_data_frame#################################
#filter useful data information
yusha_small_data <- trip_data %>% 
  select(starttime, from_station_name, to_station_name, from_station_id, to_station_id)
#convert into datetime format
yusha_small_data$starttime <- mdy_hm(yusha_small_data$starttime)
yusha_small_data$starttime <- format(yusha_small_data$starttime, "%Y-%m-%d")
yusha_small_data$starttime <- ymd(yusha_small_data$starttime)
#extract year
yusha_small_data$year <- year(yusha_small_data$starttime)
#extract month
yusha_small_data$month <- month(yusha_small_data$starttime)
###########################################################################
my_ui <- navbarPage(
  "Our Title",
  tabPanel(
    "Introduction"
  ),
  tabPanel(
    "Popular Trip",
    titlePanel("Bicycle Trip in Seattle"),
    sidebarLayout(
      sidebarPanel(
        selectInput("year",
                    "Pick a Year:",
                    c(unique(yusha_small_data$year))),
        # br() element to introduce extra vertical spacing
        br(),
        uiOutput("secondSelection"),
        
        #selectInput("month",
        #  "Pick a month:",
        # c(sort(unique(yusha_small_data$month)))),
        
        # Include clarifying text 
        helpText("Note:", strong("only")," trips have relatively large passengers (counts) and with different origine and destination
                 are plotted under the graph tab. 
                 But ", strong("all")," observed trips will be displayed under the table tab.") 
        ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", 
                   h2("What is the most popular trip?"),
                   p("In this section, we will explore bicycle trip data in Seattle area from 2014-10 to 2016-08. We only look 
                     at start points and end points, so the path is unknown. However, as long as bikers have the same origin
                     and destinition, we treat them as the same geoup. You can select which year and which month's biking trips you want to see. 
                     The graph will produce the total number of biker traveling between each stations. If you click on the graph, we can see the total number of biker travelled 
                     between specific stations and the percentage of bikers taking that trips. Stations' id are displayed, but you can
                     find the full name by checking the table data under the table tab.Only relatively large number of bikers in trips are 
                     shown. However, you can always check the table for full data."),
                   br(),
                   parsetOutput("plot"),
                   br(),
                   h3(strong("The table belows shows the trip(s) that has maximun number or passengers for the selected year and month: ")),
                   tableOutput("max"),
                   br(),
                   h4("Trip that has different origin and destinition with the largest number of passengers is the
                      most popular trip (check the above table). So more stations can be settled nearby allowing large number of bicycle to park. The company may gain some profit from this."),
                   
                   br()
                   ),
          tabPanel("table",
                   dataTableOutput("view")
          )
          
          )
        )
        )
      ), # Yusha
  tabPanel(
    "Jin"
  ), # Jin
  tabPanel(
    "Ben"
  ), # Ben
  tabPanel(
    "Kevin"
  ) # Kevin
)