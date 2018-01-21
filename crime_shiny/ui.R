## Gapminder Dashboard - UI
## Author: Esteban Angel 2018

## Usage:

## This file handles all the user interface of the Shiny app.
## The app consists in a navigation bar with two tabs: a scatter plot and a table.
## The idea is using different inputs to customize the outputs in both tabs.


## Required libraries

library(shiny)
library(colourpicker)
library(dplyr)
library(ggplot2)
library(magrittr)
library(leaflet)
library(readr)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

city_info <- read_tsv('../data/city_data.tsv')
state_list <- sort(append(unique(city_info$state),"ALL"))
city_list <- sort(append(unique(city_info$real_name),"ALL"))



shinyUI(
  navbarPage(
    header = tags$style(type="text/css", "@import url('//fonts.googleapis.com/css?family=Bungee'); .navbar-brand {font-family: 'Bungee', cursive; font-size: 15pt; font-weight: 500;line-height: 1.1;color: #ad1d28;}"),
    title = "Crime Analytics",
    inverse = TRUE,
    ## Set the navbar structure
    ## Code for the scatterplot tab.
    tabPanel("Map",icon = icon("map", lib = "font-awesome"),
            hr(),
            fluidRow(
              column(4,
                     selectInput("crimeInput", "Crime Type",
                                 choices = c("All", "Homicide", "Rape","Robbery","Aggravated Assault")),
                     checkboxInput("relCheckbox", "Relative Statistics", value = FALSE)),
              column(4,
                     sliderInput("yearInput", 
                                 label = "Year",
                                 min = 1995, max = 2015, value = 2007,step=1,animate=TRUE)
                     ),
              column(4,
                     uiOutput("stateControls"),
                     selectInput("stateInput", "State",
                                 choices = state_list))
            ),
            hr(),
            leafletOutput("mymap"),
            hr()),
    
    ## Code for the table tab.
    tabPanel("Comparison",icon = icon("bar-chart-o"),
            hr(),
            fluidRow(
              column(3,
                     selectInput("cityInput1", "City 1",
                                 choices = city_list),
                     checkboxInput("forCheckbox", "Include Forecast", value = FALSE)),
              column(3,
                     selectInput("cityInput1", "City 2",
                                 choices = city_list)),
              column(3,
                     selectInput("crimeInput2", "Crime Type",
                                 choices = c("All", "Homicide", "Rape","Robbery","Aggravated Assault"))),
              column(3,
                     sliderInput("yearInput2", 
                                 label = "Year",
                                 min = 1995, max = 2015, value = 2007,step=1,animate=FALSE)
              )
    )),
    tabPanel("About",icon = icon("info", lib = "font-awesome"))
))