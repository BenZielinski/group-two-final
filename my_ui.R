my_ui <- navbarPage(
  "Our Title",
  tabPanel(
    "Introduction"
  ),
  tabPanel(
    "Yusha"
  ), # Yusha
  tabPanel(
    "Jin"
  ), # Jin
  tabPanel(
    "Weather Conditions",
    p("Bicyclists are more exposed to the elements than car drivers or 
       people who take public transportation. As a result, bikes are more 
       likely to be affected by changes in weather. How does this affect 
       bicyclists in Seattle?"), 
    p("Use the tabs below to explore different types of weather and find out."),
    br(),
    tabsetPanel(
      tabPanel(
        "Rain",
        br(),
        sidebarPanel(
          h4(strong("Seattle's Rain")),
          p("Seattle is known for its rain. Filter the data using the controls below to understand 
            the relationship between precipitation and its bicyclists."),
          radioButtons(
            "bz_rain_graph_type",
            label = "Trip Data Type",
            c(
              "Average Trip Duration" = "dur",
              "Number of Trips" = "num"
            )
          ),
          sliderInput(
            "bz_rain_slide",
            label = "Precipitation Range (in inches)",
            min = 0,
            max = 2.20,
            value = c(0,2.20)
          ),
          checkboxInput(
            "bz_rain_smooth",
            label = "Include Trendline",
            value = FALSE
          ),
          textOutput(
            "bz_rain_message"
          )
        ),
        mainPanel(
          plotOutput("bz_rain_plot")
        )
      ),
      tabPanel(
        "Wind",
        br(),
        sidebarPanel(
          h4(strong("Seattle's Wind")),
          p("Seattle can get windy. Filter the data to see how 
            bicyclists in Seattle react to wind."),
          radioButtons(
            "bz_wind_graph_type",
            label = "Trip Data Type",
            c(
              "Average Trip Duration" = "dur",
              "Number of Trips" = "num"
            )
          ),
          radioButtons(
            "bz_wind_type",
            label = "Wind Type",
            c(
              "Average Wind Speed" = "avg",
              "Max Gust Speed" = "gust"
            )
          ),
          sliderInput(
            "bz_wind_slide",
            label = "Wind Speed Range (in mph)",
            min = 0,
            max = 52,
            value = c(0,52)
          )
        ),
        mainPanel(
          plotOutput("bz_wind_plot")
        )
      ),
      tabPanel(
        "Temperature",
        br(),
        sidebarPanel(
          h4(strong("Seattle's Temperature")),
          p("Seattle experiences a range of temperatures. 
            Filter the data to see how bicyclists react to various temperatures"),
          radioButtons(
            "bz_temp_graph_type",
            label = "Trip Data Type",
            c(
              "Average Trip Duration" = "dur",
              "Number of Trips" = "num"
            )
          ),
          sliderInput(
            "bz_temp_range",
            label = "Temperature Range (in degrees Fahrenheit)",
            min = 33,
            max = 83,
            value = c(33,83)
          )
        )
      )
    )
  ), # Ben
  tabPanel(
    "Kevin"
  ) # Kevin
)