library(dplyr)
library(shiny)
library(ggplot2)
library(maps)
trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)
trip_data <- mutate(
  trip_data,
  datetxt = as.Date(stoptime, "%m/%d/%Y  %H:%M"),
  year = as.numeric(format(datetxt, format = "%Y")),
  month = as.numeric(format(datetxt, format = "%m")),
  day = as.numeric(format(datetxt, format = "%d"))
)

station_data <- mutate(
  station_data,
  install_date = as.Date(install_date, "%m/%d/%Y"),
  year = as.numeric(format(install_date, format = "%Y")),
  month = as.numeric(format(install_date, format = "%m")),
  day = as.numeric(format(install_date, format = "%d"))
)

data <- trip_data %>%
  filter(gender == "Male" | gender == "Female")

my_server <- function(input,output) {
  output$table_trip_JY <- renderTable({
    table_trip_JY <- summarize(
      trip_data,
      total_trip_num = n(),
      member_use_times = 
        nrow(filter(trip_data, usertype == "Member")),
      short_term_user_use_times = 
        nrow(filter(trip_data, usertype == "Short-Term Pass Holder")),
      male_use_times = 
        nrow(filter(trip_data, gender == "Male")),
      female_use_times = 
        nrow(filter(trip_data, gender == "Female"))
      
    )
    return (table_trip_JY)
  })
  
  output$table_station_JY <- renderTable({
    table_station_JY <- summarize(
      station_data,
      station_num = n(),
      number_active = station_num - 4,
      number_decommission = 4
    )
  })
  
  filtered <- reactive({
    if (input$select_JY == "rides per month(selection of year permitted)") {
      data <- trip_data %>%
        filter(year == input$year_JY) %>%

        group_by(month) %>%
        summarize(
          rides_per_month = n()
        ) %>%
        arrange(month)
    } else if (input$select_JY == "station install") {
      data <- station_data %>%
        group_by(year) %>%
        summarize(
          installation_number = n()
        ) %>%
        arrange(year)
    } else if (input$select_JY == "female & male using") {
      data <- trip_data %>%
        filter(gender == "Male" | gender == "Female")
    }
    
    return (data)
  })
  
  output$plot1_JY <- renderPlot({
    p <- ggplot(data = filtered())
    if (input$select_JY == "rides per month(selection of year permitted)") {
      p <- p + geom_bar(mapping = aes(x = month, y = rides_per_month), stat = "identity") +
            labs(x = "month", y = "number of rides")
    } else if (input$select_JY == "station install") {
      p <- p + geom_bar(mapping = aes(x = year, y = installation_number), stat = "identity") +
        labs(x = "year", y = "number of installations")
    } else if (input$select_JY == "female & male using") {
      p <- p + geom_bar(mapping = aes(x = month, fill = gender)) +
        labs(x = "month", y = "number of rides")
    }
    
    return (p)
  })
  
  output$text_2_JY <- renderText({
    if (input$select_JY == "rides per month(selection of year permitted)") {
      text <- "As the graph shows, the shape of uses of sharing bike versus month is parabola.
      The peak appears at July and August. This attribute is consistent in 3 years." 
    } else if (input$select_JY == "station install") {
      text <- "As the graph shows, most sharing bike stations are installed in 2014."
    } else if (input$select_JY == "female & male using") {
      text <- "As the graph shows, no matter in which month, the male users of sharing bike is outnumbered female users."
    }
    
    return (text)
  })
}