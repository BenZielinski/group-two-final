
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


my_ui <- navbarPage(
  theme = shinythemes::shinytheme("cosmo"), # Change the quoted word in order to easily change the theme of the app
  
  "Seattle Bicycle Usage",
  tabPanel(
    "Introduction",
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "year_JY", # key this value will be assigned to
          "year of data", # label
          min = 2014, # minimum slider value
          max = 2016, # maximum slider value
          value = 2015 # starting value
        ),
        selectInput(
          "select_JY",
          "type of data",
          choices = c("rides per month(selection of year permitted)", "station install", "female & male using")
        )
        
      ),
      
      mainPanel(
        titlePanel("introduction of our app"),
        
        p("Based on the analysis of the data from 2014 to 2016, out app will show the attributes and details of 
          bike sharing economy. This is created by Yusha Wang, Jin Yan, Benjamin Zielinski, Kevin Li."),
        
        titlePanel("summary of the data we use"),
        
        a("Our data source", href = "https://www.kaggle.com/pronto/cycle-share-dataset/data"),
        
        h2("summary of trip data"),
        
        tableOutput("table_trip_JY"),
        
        h2("summary of station data"),
        
        tableOutput("table_station_JY"),
        
        h2("summary of weather data"),
        
        p("Riding bike is an outdoor activity. In our research, we take the weather from 10/13/2014 to
          08/31/2016 as an important parameter."),
        
        titlePanel("some general trends"),
        
        p("by adjusting the year slider on the left, the table will show you different kinds of summary of 
          the data we used."),
        
        plotOutput("plot1_JY"),
        
        textOutput("text_2_JY")
        )
      
    )
    
  ),
  tabPanel(
    "User Analysis",
    textOutput( "message"),
    br(),
    plotlyOutput( "pie" ), 
    column( width = 12,
            fluidPage( 
              column( width = 6, plotOutput( "gender")), 
              column( width = 6, plotOutput( "trip")) 
            ) )
   
  ), # Yusha
  tabPanel(
    "Weather Conditions",
    sidebarPanel(
      h3("Bicycles & Weather"),
      p("Seattle's bicylcists experience a wide range of weather conditions. 
        Exposed to the elements, bikers are more likely to react to weather patterns than drivers."),
      p("Use the controls below to learn more about how bikes in Seattle adjust to the weather."),
      selectInput( # Choose what is displayed on the x-axis
        "bz_x_axis",
        label = "Type of Weather:",
        c("Precipitation" = "rain", "Wind" = "wind", "Temperature" = "temp")
      ),
      radioButtons( # Choose what is displayed on the y-axis
        "bz_y_axis",
        label = "Type of Trip Data:",
        c("Average Trip Duration" = "dur", "Number of Trips" = "num")
      ),
      uiOutput("bz_gust_choice"), # Reactively display the choice for gust
      uiOutput("bz_slide"), # Reactively change slider boundaries
      checkboxInput( # Choose to display trendline
        "bz_smooth",
        label = strong("Include Trendline")
      )
      ),
    mainPanel(
      tags$style(type="text/css", # This will supress some random error messages I was getting that
                 ".shiny-output-error{visibility:hidden;}", # didn't affect my visualization
                 ".shiny-output-error:before{visibility:hidden;}"),
      plotOutput("bz_plot"),
      p(textOutput("bz_plot_message_1")),
      p(textOutput("bz_plot_message_2"))
    )
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
  )
)