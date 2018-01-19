## Gapminder Dashboard - Server
## Author: Esteban Angel 2018

## Usage:

## This file performs all the data wrangling required for building the charts and tables.


library(shiny)
library(gapminder)
library(scales)
library(dplyr)
library(ggplot2)
library(magrittr)

shinyServer(function(input, output) {
  # Define server logic required to make the scatterplot.
  output$distPlot <- renderPlot({
    
    ## Code for converting the user input into the corresponding shape number for ggplot.
    shape_input <- input$shapeInput
    switch(shape_input,
           "Point"= shape <- 16,
           "Triangle"= shape <- 17,
           "Cross"= shape <- 3)
    
    ## Data wrangling of the gapminder dataset before plotting.
    gap <- gapminder %>% filter(continent == input$continentInput,
                                year >= input$yearInput[1],
                                year <= input$yearInput[2])
    
    if(input$checkbox){
      ggplot(gap,aes(gdpPercap,lifeExp))+
        geom_point(shape = shape, size = input$sizeInput,color=input$colorInput,alpha=0.6)+
        geom_smooth(se = FALSE,color="#434747")+
        scale_x_continuous(name = "GDP per capita", labels = dollar_format())+
        scale_y_continuous(name = "Life Expectancy")+
        theme_minimal()
    }else{
      ggplot(gap,aes(gdpPercap,lifeExp))+
        geom_point(shape = shape,size = input$sizeInput,color=input$colorInput,alpha=0.6)+
        scale_x_continuous(name = "GDP per capita",labels = dollar_format())+
        scale_y_continuous(name = "Life Expectancy")+
        theme_minimal()
    }
  })
  
  # Define server logic required to make the table.
  output$table <- renderDataTable({
    gap_table <- gapminder %>% 
      select(country, continent, year, gdpPercap, lifeExp) %>% 
      filter(continent == input$continentInput2,
                                      year >= input$yearInput2[1],
                                      year <= input$yearInput2[2])
    gap_table})
})
