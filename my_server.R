trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)

source("bz_functions.R")

my_server <- function(input,output) {
  output$bz_rain_plot <- renderPlot({
    bz_filtered_rain <- weather_data %>% 
      filter(Precipitation_In >= input$bz_rain_slide[1] & Precipitation_In <= input$bz_rain_slide[2])
    
    bz_filtered_trip <- trip_data %>% 
      select(trip_id, starttime, tripduration)
    bz_filtered_trip$Date <- sapply(bz_filtered_trip$starttime, bz_get_first_word)
    bz_filtered_trip <- bz_filtered_trip %>% 
      select(-starttime)
    bz_filtered_trip$tripduration <- sapply(bz_filtered_trip$tripduration, "/", 60)
    
    bz_filtered_trip <- bz_filtered_trip %>% group_by(Date)
    
    if (input$bz_rain_graph_type == "dur") {
      bz_filtered_trip <- summarize(bz_filtered_trip, avg_dur = mean(tripduration))
    } else {
      bz_filtered_trip <- summarize(bz_filtered_trip, num_trip = length(tripduration))
    }
    
    bz_rain_graph_data <- left_join(bz_filtered_trip, bz_filtered_rain, by = c("Date"))
    bz_rain_graph_data <- filter(bz_rain_graph_data, !is.na(Precipitation_In))
    bz_rain_graph_data$Precipitation_In <- bz_rain_graph_data$Precipitation_In + 0.001
    bz_rain_graph_data$Month <- sapply(bz_rain_graph_data$Date, bz_get_month)
    
    bz_rain_p <- ggplot(data = bz_rain_graph_data)
    
    if (input$bz_rain_graph_type == "dur") {
      bz_rain_p <- bz_rain_p + 
        geom_point(mapping = aes(x = Precipitation_In, y = avg_dur, size = 3, color = Month)) +
        scale_x_log10() +
        #scale_color_discrete() +
        guides(size = FALSE) +
        coord_fixed(ratio = 1/20)
      if (input$bz_rain_smooth) {
        bz_rain_p <- bz_rain_p +
          geom_smooth(mapping = aes(x = Precipitation_In, y = avg_dur), method = "loess")
      }
    } else {
      bz_rain_p <- bz_rain_p + geom_point(mapping = aes(x = Precipitation_In, y = num_trip, size = 1.5, color = Month)) +
        guides(size = FALSE)
      if (input$bz_rain_smooth) {
        bz_rain_p <- bz_rain_p +
          geom_smooth(mapping = aes(x = Precipitation_In, y = num_trip), method = "loess")
      }
    }
    
    
    return(bz_rain_p)
  })
  
  output$bz_rain_message <- renderText({
    bz_rain_m <- ""
    if (input$bz_rain_graph_type == "dur" & !input$bz_rain_smooth) {
      bz_rain_m <- "This plot shows inches of precipitation on the x-axis, 
        with a logarithmic scale to spread out the data in a useful way. 
        The y-axis shows the average trip duration. Each dot on the graph
        represents a single day. Notice that the upper limits of the trip 
        duration clearly decrease as precipitation increases. To confirm 
        this, check the 'Include Trendline' box."
    } else if (input$bz_rain_graph_type == "dur" & input$bz_rain_smooth) {
      bz_rain_m <- "With the trendline, we can see that average bike trip 
        duration does indeed decrease as precipitation increases. This 
        follows intuition, as bicyclists likely wouldn't like to ride while
        it rains. Even residents of Seattle, who should be used to the rain,
        don't ride as far when it's wet."
    } else if (input$bz_rain_graph_type == "num" & !input$bz_rain_smooth) {
      bz_rain_m <- "This plot shows inches of precipitation on the x-axis
        and number of bike trips during a day on the y-axis. Each point 
        represents a single day, with the position showing the amount of 
        rain and the number of bike trips taken that day. We would expect 
        that as amount of rain increases, the amount of people biking
        should decrease. To confirm this, check the 'Include Trendline' box."
    } else if (input$bz_rain_graph_type == "num" & input$bz_rain_smooth) {
      bz_rain_m <- "The trendline shows that the average number of trips 
        per day actually doesn't change very much until the daily 
        precipitation increases above roughly a half inch."
    }
    
    if (input$bz_rain_smooth) {
      bz_rain_m <- paste(bz_rain_m, 
                         "For more information on the general graph, uncheck the 'Include Trendline' box.")
    }
    
    return(bz_rain_m)
  })
  
  output$bz_wind_plot <- renderPlot({
    bz_filtered_wind <- weather_data
    if (input$bz_wind_type == "avg") {
      bz_filtered_wind <- bz_filtered_wind %>% 
        filter(Mean_Wind_Speed_MPH >= input$bz_wind_slide[1] & Mean_Wind_Speed_MPH <= input$bz_wind_slide[2]) %>% 
        select(Date, Mean_Wind_Speed_MPH)
    } else {
      bz_filtered_wind <- bz_filtered_wind %>% 
        filter(is.numeric(as.numeric(Max_Gust_Speed_MPH))) %>% 
        filter(Max_Gust_Speed_MPH >= input$bz_wind_slide[1] & Max_Gust_Speed_MPH <= input$bz_wind_slide[2]) %>% 
        select(Date, Max_Gust_Speed_MPH)
    }
    
    bz_filtered_trip <- trip_data %>% 
      select(trip_id, starttime, tripduration)
    bz_filtered_trip$Date <- sapply(bz_filtered_trip$starttime, bz_get_first_word)
    bz_filtered_trip <- bz_filtered_trip %>% 
      select(-starttime)
    bz_filtered_trip$tripduration <- sapply(bz_filtered_trip$tripduration, "/", 60)
    bz_filtered_trip$month <- substr(bz_filtered_trip$Date, 1, 1)
    
    bz_filtered_trip <- bz_filtered_trip %>% group_by(Date)
    
    if (input$bz_wind_graph_type == "dur") {
      bz_filtered_trip <- summarize(bz_filtered_trip, avg_dur = mean(tripduration))
    } else {
      bz_filtered_trip <- summarize(bz_filtered_trip, num_trip = length(tripduration))
    }
    
    bz_wind_graph_data <- left_join(bz_filtered_trip, bz_filtered_wind, by = "Date")
    
    if (input$bz_wind_type == "gust") {
      bz_wind_graph_data <- filter(bz_wind_graph_data, !is.na(Max_Gust_Speed_MPH))
    }
    
    bz_wind_p <- ggplot(data = bz_wind_graph_data)
    
    if (input$bz_wind_graph_type == "dur" & input$bz_wind_type == "avg") {
      bz_wind_p <- bz_wind_p + geom_jitter(mapping = aes(x = Mean_Wind_Speed_MPH, y = avg_dur))
    } else if (input$bz_wind_graph_type == "dur" & input$bz_wind_type == "gust") {
      bz_wind_p <- bz_wind_p + geom_point(mapping = aes(x = Max_Gust_Speed_MPH, y = avg_dur)) +
        coord_fixed(1/3)
    }
    
    return(bz_wind_p)
  })
}