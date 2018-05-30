my_ui <- navbarPage(
  "Our Title",
  tabPanel(
    "Introduction"
  ),
  tabPanel(
    "Yusha"
  ), # Yusha
  tabPanel(
    sidebarlayout(
      sidebarpanel(
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
          choices = c()
        )
        
      ),
      mainpanel(
        titlePanel("introduction of our app"),
        
        p("Based on the analysis of the data from 2014 to 2016, out app will show the attributes and details of 
          bike sharing economy"),
        
        titlePanel("summary of the data we use"),
        
        p("summary of data"),
        
        tableOutput("table_station_JY"),
        
        tableOutput("table_trip_JY"),
        
        p("by adjusting the year slider on the left, the table will show you different kinds of summary of 
          the data we used."),
        
        tableOutput("table_weather_JY"),
        
        plotOutput("plot1_JY")
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