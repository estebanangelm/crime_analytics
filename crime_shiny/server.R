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
library(htmltools)

##Import dataset and additional table with cities information

crime_data <- read_csv('../data/crime_dataset.csv')
city_info <- read_tsv('../data/city_data.tsv')
city_info$lat <- as.numeric(city_info$lat)
city_info$long <- as.numeric(city_info$long)
crime_types <- data_frame(crime_type= c("homs_sum","rape_sum","rob_sum","agg_ass_sum","violent_crime"),
                          type = c("Homicide","Rape","Robbery","Aggravated Assault","All"))

##Data wrangling

crime_df <- crime_data %>% 
  gather(crime_type,quantity,5:9) %>% 
  select(ORI,year,total_pop,crime_type,quantity) %>% 
  inner_join(crime_types) %>% 
  select(-crime_type) %>% 
  inner_join(city_info,by=c("ORI"="code")) %>% 
  select(-department_name,-search_name) %>% 
  mutate(quantity_rel = quantity / total_pop * 100000) %>% 
  filter(real_name != "National")

  

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
  
  # Define server logic required to make the map.
  
  output$mymap <- renderLeaflet({
    test <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$stateInput != "ALL"){
      test <- test %>% filter(state==input$stateInput)
    }
    if(input$relCheckbox == TRUE){
      test <- test %>% mutate(quantity = quantity_rel)
    }
    rule <- 25/max(test$quantity,na.rm=TRUE)
    leaflet(data=test) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>% 
      addTiles() %>%
      addCircleMarkers(~long, 
                       ~lat,
                       popup = ~paste("<b>",real_name,"</b>",
                                      "</br>",year,
                                      "</br><b>Type:</b>",type,
                                      "</br><b>Quantity:</b>",round(quantity)),
                       label = ~as.character(real_name),
                       radius = ~(quantity * rule),
                       stroke = FALSE, 
                       fillOpacity = 0.5)
  })
})
