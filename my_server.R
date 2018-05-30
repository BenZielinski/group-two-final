library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(parsetR)


trip_data <- read.csv("data/trip.csv", stringsAsFactors = FALSE)
weather_data <- read.csv("data/weather.csv", stringsAsFactors = FALSE)
station_data <- read.csv("data/station.csv", stringsAsFactors = FALSE)

my_server <- function(input,output) {
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
