my_ui <- navbarPage(
  theme = shinythemes::shinytheme("united"), # Change the quoted word in order to easily change the theme of the app
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
  ), # Ben
  tabPanel(
    "Kevin"
  ) # Kevin
)