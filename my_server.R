library("dplyr")
library("shiny")
library("ggplot2")
library("maps")
library("rsconnect")
library("scales")
library("waffle")
library("plotly")
library("lubridate")
library("parsetR")

trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)

source("bz_functions.R")

trip_data_JY <- mutate(
  trip_data,
  datetxt = as.Date(stoptime, "%m/%d/%Y  %H:%M"),
  year = as.numeric(format(datetxt, format = "%Y")),
  month = as.numeric(format(datetxt, format = "%m")),
  day = as.numeric(format(datetxt, format = "%d"))
)

station_data_JY <- mutate(
  station_data,
  install_date = as.Date(install_date, "%m/%d/%Y"),
  year = as.numeric(format(install_date, format = "%Y")),
  month = as.numeric(format(install_date, format = "%m")),
  day = as.numeric(format(install_date, format = "%d"))
)

my_server <- function(input,output) {
  output$table_trip_JY <- renderTable({
    table_trip_JY <- summarize(
      trip_data_JY,
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
      station_data_JY,
      station_num = n(),
      number_active = station_num - 4,
      number_decommission = 4
    )
  })
  
  filtered <- reactive({
    if (input$select_JY == "rides per month(selection of year permitted)") {
      data <- trip_data_JY %>%
        filter(year == input$year_JY) %>%
        
        group_by(month) %>%
        summarize(
          rides_per_month = n()
        ) %>%
        arrange(month)
    } else if (input$select_JY == "station install") {
      data <- station_data_JY %>%
        group_by(year) %>%
        summarize(
          installation_number = n()
        ) %>%
        arrange(year)
    } else if (input$select_JY == "female & male using") {
      data <- trip_data_JY %>%
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
  
  split_factor <- floor( ( trip_data$birthyear %% 1900 ) / 10 )
  decade_split <- split( trip_data, split_factor )
  
  # converting the dataframes in the list to show the amount of rows in each element
  # of the list
  list_function <- function( list ) {
    nrow( list )
  }
  
  per_decade <- lapply(decade_split, list_function)
  
  # converting the list into a decade
  df_decade <- data.frame( matrix( unlist( per_decade ), nrow = 7, byrow = T ),
                           stringsAsFactors=FALSE)
  
  # adding a decades column to current dataframe
  decade <- c("30s", "40s", "50s", "60s", "70s", "80s", "90s")
  df_decade <- mutate(df_decade, decade)
  colnames( df_decade ) <- c( "total", "decade" ) 
  
  ################
  # pie chart with plotly
  #########
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)',
              'rgb(114,147,203)')
  
  p <- plot_ly(df_decade, labels = ~decade, values = ~total, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('Decade: ', decade ,
                             '\n Total trips: ', total),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    layout(title = 'Bicycle trips distribution by decade of birth',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
  ################
  #Creating plot2#
  ################
  
  gender_sort <- filter( trip_data, gender == "Male" | gender == "Female")
  time_gender <- subset(gender_sort, select = c( "tripduration", "gender" ) )
  
  gender_grouped <- group_by( time_gender, gender)
  
  
  gender_sum <- summarize( gender_grouped, total = n( ) )
  
  gender_plot <- ggplot( gender_sum, aes( x = gender, y = total, 
                                          fill = gender)) +
    geom_bar( stat = "identity") +
    labs(
      title = "Trip distributed by gender"
    )
  
  
  # plot for avg trip duration for gender
  avg_gender_trip <- summarize( gender_grouped, avg_tripduration = mean( tripduration ))
  
  trip_plot <- ggplot( avg_gender_trip, aes( x = gender, y = avg_tripduration, 
                                             fill = gender)) +
    geom_bar( stat = "identity") +
    labs( 
      title = "Average trip duration by gender"
    )
  
  output$pie <- renderPlotly({
    return(p)
  })
  
  output$gender <- renderPlot({
    return(gender_plot)
  })
  
  output$trip <- renderPlot({
    return(trip_plot)
  })
  
  shorten <- function( input ){
    substring(input, 1, 8)
  }
  
  the_message <- paste0(
    "The three data visualizations give a good idea at the user metrics of Pronto users", 
    " over the period of ", shorten( trip_data[ 1,2 ] ), " to ", 
    shorten( trip_data[ 236065, 2 ] ), ". The three metrics gives a measurements of what ",
    "the distribution of age is like over all trips on the pronto system, what share of ",
    "those trips had Male or Female riders and what the average trip duration of Male and ",
    "Female riders were."
  )
  
  the_message
  
  
  output$message <- renderText({
    return( the_message )
  })
  
  bz_plot_d <- reactive({
    bz_weather <- weather_data
    
    if (input$bz_x_axis == "rain") {
      bz_weather <- bz_weather %>%
        select(Date, Precipitation_In)
    } else if (input$bz_x_axis == "wind") {
      if (input$bz_gust == "avg") {
        bz_weather <- bz_weather %>% 
          select(Date, Mean_Wind_Speed_MPH)
      } else {
        bz_weather <- bz_weather %>% 
          select(Date, Max_Gust_Speed_MPH)
      }
    } else {
      bz_weather <- weather_data %>% 
        select(Date, Mean_Temperature_F)
    }
    
    # No matter what the column is, I give it a generic name
    colnames(bz_weather) <- c("Date", "x_data")
    
    # Not all days have a gust speed, so I want to remove those days
    if (input$bz_x_axis == "wind") {
      bz_weather$x_data <- as.numeric(bz_weather$x_data)
      bz_weather <- bz_weather[!is.na(bz_weather$x_data), ]
    }
    
    # Filter to the appropriate values based on the slider
    bz_weather <- bz_weather %>% 
      filter(x_data >= input$bz_slider[1] & x_data <= input$bz_slider[2])
    
    # I need to create a column in trip_data that has just the date, so I can do a left_join
    bz_trip <- trip_data %>% 
      select(trip_id, starttime, tripduration)
    bz_trip$Date <- sapply(bz_trip$starttime, bz_get_first_word)
    bz_trip <- bz_trip %>% select(-starttime)
    bz_trip$tripduration <- sapply(bz_trip$tripduration, "/", 60)
    bz_trip <- bz_trip %>% group_by(Date)
    
    # Because I care about the day as a whole and not the individual trips, I summarize by day
    if (input$bz_y_axis == "dur") {
      bz_trip <- summarize(bz_trip, y_data = mean(tripduration))
    } else {
      bz_trip <- summarize(bz_trip, y_data = length(tripduration))
    }
    
    # I need to left join then cut by month since I want to color by month
    bz_plot_data <- left_join(bz_trip, bz_weather, by = "Date")
    bz_plot_data <- filter(bz_plot_data, !is.na(x_data))
    bz_plot_data$Month <- sapply(bz_plot_data$Date, bz_get_month)
    bz_plot_data$Month <- cut(bz_plot_data$Month, 0:12)
    
    return(bz_plot_data)
  })
  
  # BZ: If the user is looking at wind, I want the option to look at gust to appear
  output$bz_gust_choice <- renderUI({
    if (input$bz_x_axis == "wind") {
      radioButtons(
        "bz_gust",
        label = "Wind Type:",
        c("Average Wind Speed" = "avg", "Maximum Gust Speed" = "max")
      )
    }
  })
  
  # BZ: The slider has to react to the min and max values relavent to the chosen set of data
  output$bz_slide <- renderUI({
    slide_label <- ""
    slide_min <- 0
    slide_max <- 0
    
    if (input$bz_x_axis == "rain") {
      slide_label <- "Precipitation Range:"
      slide_min <- min(weather_data$Precipitation_In)
      slide_max <- max(weather_data$Precipitation_In)
    } else if (input$bz_x_axis == "wind") {
      slide_label <- "Wind Speed Range:"
      if (input$bz_gust == "avg") {
        slide_min <- min(weather_data$Mean_Wind_Speed_MPH)
        slide_max <- max(weather_data$Mean_Wind_Speed_MPH)
      } else {
        slide_min <- min(as.numeric(weather_data$Max_Gust_Speed_MPH), na.rm = TRUE)
        slide_max <- max(as.numeric(weather_data$Max_Gust_Speed_MPH), na.rm = TRUE)
      }
    } else {
      slide_label <- "Temperature Range:"
      slide_min <- min(weather_data$Mean_Temperature_F, na.rm = TRUE)
      slide_max <- max(weather_data$Mean_Temperature_F, na.rm = TRUE)
    }
    
    sliderInput(
      "bz_slider",
      label = slide_label,
      min = slide_min,
      max = slide_max,
      value = c(slide_min, slide_max)
    )
  })
  
  # BZ: Create my visualization
  output$bz_plot <- renderPlot({
    bz_plot_plot <- as.data.frame(bz_plot_d())
    
    # I need to generate a ratio to display the plot which helps with page scaling
    bz_ratio <- 0.6 * (max(bz_plot_plot$x_data) - min(bz_plot_plot$x_data)) / (max(bz_plot_plot$y_data) - min(bz_plot_plot$y_data))
    
    bz_p <- ggplot(data = bz_plot_plot) +
      geom_point(mapping = aes(x = x_data, y = y_data, size = 2, color = Month)) +
      guides(size = FALSE) +
      coord_fixed(ratio = bz_ratio) +
      scale_color_hue(labels = c("January", "February", "March", "April", "May", 
                                 "June", "July", "August", "September", "October", 
                                 "November", "December"))
    
    # Applies a trendline when the option is chosen
    if (input$bz_smooth) {
      bz_p <- bz_p + geom_smooth(mapping = aes(x = x_data, y = y_data), method = "loess")
    }
    
    # The labels of the graph need to respond to the user's chosen features
    bz_x_label <- ""
    if (input$bz_x_axis == "rain") {
      bz_x_label <- "Precipitation (inches)"
    } else if (input$bz_x_axis == "wind") {
      bz_x_label <- "Wind Speed (mph)"
    } else {
      bz_x_label <- "Temperature (F)"
    }
    
    bz_y_label <- ""
    if (input$bz_y_axis == "dur") {
      bz_y_label <- "Average Trip Duration (minutes)"
    } else {
      bz_y_label <- "Number of Trips"
    }
    
    bz_p <- bz_p + labs(x = bz_x_label, y = bz_y_label)
    
    return(bz_p)
  })
  
  # BZ: Create a message that describes what is shown on the plot
  output$bz_plot_message_1 <- renderText({
    bz_x <- ""
    if (input$bz_x_axis == "rain") {
      bz_x <- "total amount of precipitation, in inches,"
    } else if (input$bz_x_axis == "wind") {
      if (input$bz_gust == "avg") {
        bz_x <- "average wind speed, in miles per hour"
      } else {
        bz_x <- "maximum wind gust speed, in miles per hour"
      }
    } else {
      bz_x <- "average temperature, in degrees Fahrenheit,"
    }
    
    bz_y <- ""
    if (input$bz_y_axis == "dur") {
      bz_y <- "average duration of bicycle trips, in minutes,"
    } else {
      bz_y <- "total number of bike trips taken"
    }
    
    message <- paste("In this plot, each point represents a day, with", 
                     bz_x, 
                     "for that day on the x-axis, and", 
                     bz_y, 
                     "on that day on the y-axis. The points are colored based on month.")
  })
  
  # Depending on the exact plot that is displayed, a different specialized message 
  #   is displayed that describes the trend of the data
  output$bz_plot_message_2 <- renderText({
    bz_2 <- "Change this."
    if (input$bz_x_axis == "rain" & input$bz_y_axis == "dur") {
      bz_2 <- "This graph shows that the average length of a bike 
      trip decreases rapidly as amount of precipitation increases.
      This confirms our intuition, which says that people don't 
      like to ride a bike in the rain."
    } else if (input$bz_x_axis == "rain" & input$bz_y_axis == "num") {
      bz_2 <- "From this graph, it is obvious that the number of bike 
      trips in a day decreases when the amount of precipitation 
      increases. This matches what we would think, since less people 
      would want to ride a bike when it is raining outside."
    } else if (input$bz_x_axis == "wind" & input$bz_y_axis == "dur") {
      if (input$bz_gust == "gust") {
        bz_2 <- "This plot shows that there is correlation between wind 
        gusts and length of bicycle trip, this connection is not strong.
        As wind gust speed increases, the average trip length decreases.
        This makes sense, because gustier days are likely to be days with
        storms, which bicyclists would not enjoy traveling during."
      } else {
        bz_2 <- "This plot illustrates that average wind speed on a given 
        day does affect the average duration of a bike ride. However, 
        this correlation is not very strong. As average wind speed 
        increases, the length of bike trips decreases. Since the connection 
        isn't too strong, we know that Seattle's bicyclists don't mind 
        biking in the wind too much."
      }
      } else if (input$bz_x_axis == "wind" & input$bz_y_axis == "num") {
        if (input$bz_gust == "avg") {
          bz_2 <- "This graph shows that as average wind speed increases, the 
          number of bicycle trips tends to decrease, although the connection 
          isn't too strong. Windier days are more likely to be rainy or stormy, 
          which could be why we see this correlation."
        } else {
          bz_2 <- "We can see from this graph that the number of bike trips taken 
          on a day steadily decreases as wind gust speed increases. Since gusty 
          days would have poor conditions for bike riding, it is intuitive that 
          less people choose to ride bicycles on these gustier days."
        }
        } else if (input$bz_x_axis == "temp" & input$bz_y_axis == "dur") {
          bz_2 <- "This plot reveals the obvious; as average temperature increases, 
          the average length of a bicycle ride increases. On warmer days, people 
          tend to ride bikes for longer, and therefore farther distances, proving 
          the viability of bicycles when it's warm."
        } else {
          bz_2 <- "We can see from this plot that there is as strong correlation 
          between temperature and number of bike trips in a day. This proves that 
          a large factor when people choose to ride a bike is the temperature. When 
          it's warm outside, people ride bicycles more often than when it's cold."
        }
      })
  
  #####################################yusha section####################################################
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
  
  
  output$secondSelection <- renderUI({
    selectInput("month", "Pick a Month:", choices = sort(unique(yusha_small_data$month[yusha_small_data$year ==input$year])))
  })
  
  # Assign a reactive function to the outputted filtered data frame value to produce a table
  yusha_table <- reactive({
    yusha_small_datatemp <- yusha_small_data %>%
      filter(year == input$year & month == input$month ) %>% 
      dplyr::group_by(from_station_name,  to_station_name, from_station_id, to_station_id) %>%
      dplyr::summarize(counts = n()) %>% 
      ungroup() %>%
      arrange(-counts)
    return(yusha_small_datatemp)
  })
  
  #Assign a reactive function to the outputted filtered data frame value to produce a summary table
  yusha_summary <- reactive({
    yusha_small_datatemp <- yusha_small_data %>%
      filter(year == input$year & month == input$month & from_station_id != to_station_id) %>% 
      dplyr::group_by(from_station_name,  to_station_name, from_station_id, to_station_id) %>%
      dplyr::summarize(counts = n()) %>%
      ungroup() %>%
      arrange(-counts) %>% 
      filter(counts == max(counts)) 
    return(yusha_small_datatemp)
  })
  
  # Assign a reactive function to the outputted filtered data frame value to produce a plot
  yusha_filtered <- reactive({
    yusha_small_datatwo <- yusha_small_data %>%
      filter(year == input$year & month == input$month & from_station_id != to_station_id ) %>% 
      dplyr::group_by(from_station_id, to_station_id) %>%
      dplyr::summarize(counts = n()) 
    #filter better data set
    if(input$year == 2014){
      yusha_small_datatemp <- yusha_small_datatwo %>% 
        filter(counts >= 30) 
    }
    if(input$year == 2015){
      if(input$month %in% c(12,11)){
        yusha_small_datatemp <- yusha_small_datatwo %>% 
          filter(counts >= 33)
      }
      if(input$month %in% c(5,6,7,8)){
        yusha_small_datatemp <- yusha_small_datatwo %>% 
          filter(counts >= 85)
      }
      if(input$month %in% c(1,2,3,4,9,10)){
        yusha_small_datatemp <- yusha_small_datatwo %>% 
          filter(counts >= 60)
      }
    }
    if(input$year == 2016){
      if(input$month %in% c(5,6,7,8)){
        yusha_small_datatemp <- yusha_small_datatwo %>% 
          filter(counts >= 55)
      }
      if(input$month %in% c(1,2,3,4)){
        yusha_small_datatemp <- yusha_small_datatwo %>% 
          filter(counts >= 40)
      }
    }
    
    yusha_small_datatemp <-  yusha_small_datatemp %>% 
      ungroup() %>%
      arrange(-counts)
    return(yusha_small_datatemp)
  })
  
  
  #output plot with mean eviction rate versus states according to user input
  output$plot <- renderParset({
    parset(data = yusha_filtered(), dimensions = c('from_station_id' , 'to_station_id'), 
           value = htmlwidgets::JS("function(d){return d.counts}"),
           tension = 0.5)
    
  })
  #output data table
  output$view <- renderDataTable(yusha_table())
  
  #output summary table
  output$max <- renderTable(yusha_summary())
  ######################################################################################################
}