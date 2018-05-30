my_ui <- navbarPage(
  "Our Title",
  tabPanel(
    "Introduction"
  ),
  tabPanel(
    "Yusha"
  ), # Yusha
  tabPanel(
    "Jin Yan",
    
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
          bike sharing economy"),
        
        titlePanel("summary of the data we use"),
        
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
  ), # Jin
  tabPanel(
    "Ben"
  ), # Ben
  tabPanel(
    "Kevin"
  ) # Kevin
)