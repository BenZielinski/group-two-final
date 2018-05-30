library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("rsconnect")
library("scales")
library("waffle")
library("plotly")

trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)

my_server <- function( input, output ) {
  
  
  ##############
  #kevin's code#
  ##############
  
  ################
  #Creating plot1#
  ################
  # splits the data frame into decades
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
  
  
  
}

