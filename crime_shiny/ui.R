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


shinyUI(navbarPage("Gapminder Dashboard",
                   ## Set the navbar structure
                   ## Code for the scatterplot tab.
                   tabPanel("Scatter Plot",
                            p("The purpose of this tab is customizing a scatterplot from the Gapminder dataset. There are two ways for customizing it: changing the data or changing the aesthetics. Play with both and see the results."),
                            leafletOutput("mymap"),
                            p(),
                            actionButton("recalc", "New points"),
                            hr(),
                            fluidRow(
                              column(6,
                                     h3("Data"),
                                     selectInput("continentInput", "Continent",
                                                 choices = c("Americas", "Europe", "Asia","Africa","Oceania")),
                                     checkboxInput("checkbox", "Smoothing", value = FALSE),
                                     sliderInput("yearInput", 
                                                 label = "Years",
                                                 min = 1952, max = 2007, value = c(1952, 2007),step=5)),
                              column(6,
                                     h3("Aesthetics"),
                                     radioButtons("shapeInput", "Shape",
                                                  choices = c("Point", "Triangle", "Cross"),
                                                  selected = "Point"),
                                     colourInput("colorInput", "Select color", "#177368"),
                                     numericInput("sizeInput", p("Size"), value = 4,min = 4, max = 10,step=1))
                            ),
                            hr(),
                            plotOutput("distPlot")),
                   ## Code for the table tab.
                   tabPanel("Table",
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