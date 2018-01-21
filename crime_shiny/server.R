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

##Create forecast

forecast <- crime_df %>% 
  filter(year %in% c(2013,2014,2015)) %>% 
  group_by(real_name,type) %>% 
  summarize(f_qty_2016 = mean(quantity,na.rm=TRUE),
            f_pop_2016 = mean(total_pop,na.rm = TRUE),
            f_rel_2016 = f_qty_2016/f_pop_2016*100000)

  
shinyServer(function(input, output) {
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
  
  comparison <- crime_df %>% 
    group_by(real_name,year,type) %>% 
    summarize(total=sum(quantity_rel))
  
  output$distPlot1 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= input$yearInput2[1],
             year <= input$yearInput2[2],
             real_name == input$cityInput1)
    
    ggplot(comparison %>% filter(real_name == input$cityInput1))+
      geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
      scale_x_continuous("Year")+
      scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
      ggtitle(paste("Relative Crime Statistics for ",input$cityInput1))+
      theme_minimal()+
      theme(legend.position = "bottom")
  })
  
  output$distPlot2 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= input$yearInput2[1],
             year <= input$yearInput2[2],
             real_name == input$cityInput2)
    
    
    ggplot(comparison %>% filter(real_name == input$cityInput2))+
      geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
      scale_x_continuous("Year")+
      scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
      ggtitle(paste("Relative Crime Statistics for ",input$cityInput2))+
      theme_minimal()+
      theme(legend.position = "bottom")
  })
  

  
  output$kpi1 <- renderUI({
    tagList(
      h3(paste(input$cityInput1)," is safer than ",input$cityInput2)
    )
  })
  
  crime_table <- crime_df %>% 
    filter(year %in% c(2000,2001),
           real_name == "Atlanta") %>% 
    select(real_name,year,type,quantity_rel) %>% 
    spread(year,quantity_rel)
  
  # Define server logic required to make the table.
  output$table1 <- renderTable({
    crime_table_1 <- crime_df %>% 
      filter(real_name == input$cityInput1,
            year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
      select(real_name,year,type,quantity_rel) %>% 
      spread(year,quantity_rel)
    
    crime_table_1})
  
  output$table2 <- renderTable({
    crime_table_2 <- crime_df %>% 
      filter(real_name == input$cityInput2,
             year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
      select(real_name,year,type,quantity_rel) %>% 
      spread(year,quantity_rel)
    
    crime_table_2})
})
