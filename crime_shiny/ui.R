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

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

city_info <- read_tsv('../data/city_data.tsv')
state_list <- sort(append(unique(city_info$state),"ALL"))

shinyUI(navbarPage("Gapminder Dashboard",
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
                            leafletOutput("mymap")),
                   ## Code for the table tab.
                   tabPanel("Comparison",icon = icon("bar-chart-o"),
                            p("The purpose of this tab is doing some dynamic filtering of the countries in the Gapminder dataset"),
                            fluidRow(
                              h3("Data"),
                              column(6,
                                     selectInput("continentInput2", "Continent",
                                                 choices = c("Americas", "Europe", "Asia","Africa","Oceania"))
                              ),
                              column(6,
                                     sliderInput("yearInput2", 
                                                 label = "Years",
                                                 min = 1952, max = 2007, value = c(1952, 2007),step=5)),
                              dataTableOutput("table")
                            ))
))