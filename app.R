library(dplyr)
library(shiny)
library(ggplot2)
library(maps)

source("my_ui.R")
source("my_server.R")

shinyApp(ui = my_ui, server = my_server)