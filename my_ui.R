
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
    
    p(strong("Pronto was an early, station-based bike share company in Seattle that operated from 2014 to 2016. 
      While this company ultimately failed, analyzing the data gathered during its operation gives great 
      insight into trends among bicyclists in Seattle. One potential use for this data is to optimize modern, 
      dockless bike share models.")),
    
    p(strong("This app provides key analysis of Pronto's data, including a look at popular bike trips, Seattle bicyclists, 
      and how they react to the weather. This report was created by Yusha Wang, Jin Yan, Benjamin Zielinski, Kevin Li.")),
    
    br(),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "year_JY", # key this value will be assigned to
          "Choose Year", # label
          min = 2014, # minimum slider value
          max = 2016, # maximum slider value
          value = 2015 # starting value
        ),
        selectInput(
          "select_JY",
          "Select Data",
          choices = c("rides per month(selection of year permitted)", "station install", "female & male using")
        ),
        div(img(src = "bike.png", height = 200, width = 240), style="text-align: center;")
        
      ),
      
      mainPanel(
        
        titlePanel("Our Data"),
        
        p("We analyzed data from 10/13/2014 to 8/31/2016, the time of Pronto's operation. 
          Pronto operated stations around Seattle, all around the city in conventient 
          locations. Users took trips from one station to another, and details about their 
          trip was recorded, such as trip duration and user information. Additionally, we 
          have data that shows information regarding the stations themselves, and information 
          about the weather for each day of operation."),
        
        p("For the source of data, look", 
          a("here.", href = "https://www.kaggle.com/pronto/cycle-share-dataset/data")),
        
        h2("Overview"),
        
        h4("Bicycle Trips"),
        
        tableOutput("table_trip_JY"),
        
        h4("Stations"),
        
        tableOutput("table_station_JY"),
        
        titlePanel("General Trends"),
        
        p("Adjust the controls on the left to update the plot and view different data summaries."),
        
        plotOutput("plot1_JY"),
        
        textOutput("text_2_JY"),
        
        br(),
        br()
        )
      
    )
    
  ),
  tabPanel(
    "User Analysis",
    strong(textOutput( "message")),
    br(),
    plotlyOutput( "pie" ), 
    column( width = 12,
            fluidPage( 
              column( width = 6, plotOutput( "gender")), 
              column( width = 6, plotOutput( "trip")) 
            ) ),
    br(),
    div(img(src = "bike.png", height = 200, width = 240), style="text-align: center;"),
    br()
   
  ),
  tabPanel(
    "Weather Conditions",
    titlePanel(strong("Bicycles & Weather")),
    sidebarLayout(
      sidebarPanel(
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
        ),
        div(img(src = "bike.png", height = 200, width = 240), style="text-align: center;")
      ),
      mainPanel(
        tags$style(type="text/css", # This will supress some random error messages I was getting that
                   ".shiny-output-error{visibility:hidden;}", # didn't affect my visualization
                   ".shiny-output-error:before{visibility:hidden;}"),
        plotOutput("bz_plot"),
        p(textOutput("bz_plot_message_1")),
        p(textOutput("bz_plot_message_2"))
      )
    )
  ), 
  tabPanel(
    "Popular Trips",
    titlePanel(strong("Bicycle Trips in Seattle")),
    sidebarLayout(
      sidebarPanel(
        selectInput("year",
                    "Pick a Year:",
                    c(unique(yusha_small_data$year))),
        # br() element to introduce extra vertical spacing
        br(),
        uiOutput("secondSelection"),
        
        # Include clarifying text 
        helpText("Note:", strong("only")," trips have relatively large passengers (counts) and with different origin and destination
                 are plotted under the graph tab. 
                 But ", strong("all")," observed trips will be displayed under the table tab."),
        div(img(src = "bike.png", height = 200, width = 240), style="text-align: center;")
        ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", 
                   h2("What is the Most Popular Trip?"),
                   p("In this section, we will explore bicycle trip data in the Seattle area from 2014-10 to 2016-08. We only look 
                     at start points and end points, so the path is unknown. However, as long as bikers have the same origin
                     and destinition, we treat them as the same group. You can select which year and which month's biking trips you want to see. 
                     The graph will produce the total number of bikers traveling between each station. If you click on the graph, we can see the total number of bikers that travelled 
                     between specific stations and the percentage of bikers taking that trips. Station IDs are displayed, but you can
                     find the full name by checking the table tab. The visualization only shows trips that relatively large numbers of bikers took. 
                     However, you can always check the table for full data."),
                   br(),
                   parsetOutput("plot"),
                   br(),
                   h4("The table below shows the trip(s) that has maximum number of passengers for the selected year and month: "),
                   tableOutput("max"),
                   br(),
                   h4("The most popular trip is defined to be the journey with a different starting and ending point with the largest number of bikers. 
                      More stations can be settled nearby to these endpoints, allowing a larger number of bicycles to park. The company may gain some profit from this."),
                   
                   br()
                   ),
          tabPanel("Table",
                   br(),
                   dataTableOutput("view")
          )
          
          )
        )
      )
  )
)