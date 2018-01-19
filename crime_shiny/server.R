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
library(readr)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)

##Import dataset and additional table with cities information

crime_data <- read_csv('../data/crime_dataset.csv')
city_info <- read_tsv('../data/city_data.tsv')
crime_types <- data_frame(source= c("homs_sum","rape_sum","rob_sum","agg_ass_sum","violent_crime"),
                          objective = c("Homicide","Rape","Robbery","Aggravated Assault","All"))

##Data wrangling

crime_df <- crime_data %>% 
  inner_join(city_info, by = c("ORI" = "code")) %>% 
  select(-ORI,-department_name.x,-source,-url,-department_name.y,-search_name) %>% 
  gather(crime_type,quantity,3:7)

crime_df <- crime_data %>% 
  gather(crime_type,quantity,5:9) %>% 
  select(ORI,year,total_pop,crime_type,quantity)


  

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
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  # Define server logic required to make the map.
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  observe({
    crime_map <- crime_data %>% 
      select(country, continent, year, gdpPercap, lifeExp) %>% 
      filter(continent == input$continentInput2,
             year >= input$yearInput2[1],
             year <= input$yearInput2[2])
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
})
