## Crime Analytics Dashboard - Server
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
library(forcats)

##Import dataset and additional table with cities information

crime_data <- read_csv('./data/crime_dataset.csv')
city_info <- read_tsv('./data/city_data.tsv')
city_info$lat <- as.numeric(city_info$lat)
city_info$long <- as.numeric(city_info$long)
crime_types <- data_frame(crime_type= c("homs_sum","rape_sum","rob_sum","agg_ass_sum","violent_crime"),
                          type = c("Homicide","Rape","Robbery","Aggravated Assault","All"))

exclude_list <- c("MD00301","CA01900","KY05680","FL01300")

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
            quantity_rel = f_qty_2016/f_pop_2016*100000,
            year = 2016) %>% 
  select(real_name,year,type,quantity_rel)

  
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
  
  # Code for bar chart 1 next to the map
  output$bar_overview_1 <- renderPlot({
    data_bar <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$relCheckbox == TRUE){
      data_bar <- data_bar %>% mutate(quantity = quantity_rel)
    }
    
    top_10 <- data_bar %>% 
      group_by(real_name) %>% 
      summarize(quantity = sum(quantity,na.rm=TRUE)) %>% 
      arrange(desc(quantity)) %>% 
      top_n(10) %>% 
      mutate(type = "Top 10")
    
    
    ggplot(top_10) +
      geom_col(aes(x=fct_reorder(real_name,desc(quantity)),y=quantity),color="#A85042",fill="#A85042",alpha=0.7)+
      ggtitle(paste("Top 10 most violent cities - by ",input$crimeInput))+
      scale_y_continuous("# of crimes")+
      scale_x_discrete("")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    })
  
  # Code for bar chart 2 next to the map
  output$bar_overview_2 <- renderPlot({
    data_bar <- crime_df %>% 
      filter(year == input$yearInput,type == input$crimeInput)
    if(input$relCheckbox == TRUE){
      data_bar <- data_bar %>% mutate(quantity = quantity_rel)
    }
    
    bot_10 <- data_bar %>% 
      group_by(real_name) %>% 
      summarize(quantity = sum(quantity,na.rm=TRUE)) %>% 
      arrange(desc(quantity)) %>% 
      top_n(-10) %>% 
      mutate(type = "Bottom 10")
    
    
    ggplot(bot_10) +
      geom_col(aes(x=fct_reorder(real_name,desc(quantity)),y=quantity),color="blue",fill="blue",alpha=0.7)+
      ggtitle(paste("Top 10 safest cities - by ",input$crimeInput))+
      scale_y_continuous("# of crimes")+
      scale_x_discrete("")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
      
  
  #Code for creating the phrase with kpi.
  output$kpi1 <- renderUI({
    test <- crime_df %>% 
      filter(year == max(crime_df$year),
             real_name %in% c(input$cityInput1,input$cityInput2),
             type == "All") %>% 
      arrange(quantity_rel)
    tagList(
      h3(paste(test$real_name[1],
               " is safer than ",
               test$real_name[2],
               ". During the last year, ",
               test$real_name[1],
               " had ",
               round(test$quantity_rel[1]),
               " crimes per 100,000 people, while ",
               test$real_name[2],
               " had ",
               round(test$quantity_rel[2]),
               ". Scroll the following plots to find more details."
      ))
    )
  })
  
  #Code for creating the plots.
  
  comparison <- crime_df %>% 
    group_by(real_name,year,type) %>% 
    summarize(total=sum(quantity_rel))
  
  output$distPlot1 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= input$yearInput2[1],
             year <= input$yearInput2[2],
             real_name == input$cityInput1)
    
    sub_forecast <- forecast %>% 
      filter(real_name == input$cityInput1)
    
    if(input$forCheckbox == TRUE){
      ggplot(comparison %>% filter(real_name == input$cityInput1))+
        geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
        geom_point(data=sub_forecast,aes(x=year,y=quantity_rel,color=type),shape=12)+
        scale_x_continuous("Year")+
        scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
        ggtitle(paste("Relative Crime Statistics for ",input$cityInput1))+
        theme_minimal()+
        theme(legend.position = "bottom")
    }else{
      ggplot(comparison %>% filter(real_name == input$cityInput1))+
        geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
        scale_x_continuous("Year")+
        scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
        ggtitle(paste("Relative Crime Statistics for ",input$cityInput1))+
        theme_minimal()+
        theme(legend.position = "bottom")
    }
  })
  
  output$distPlot2 <- renderPlot({
    comparison <- comparison %>% 
      filter(year >= input$yearInput2[1],
             year <= input$yearInput2[2],
             real_name == input$cityInput2)
    
    sub_forecast <- forecast %>% 
      filter(real_name == input$cityInput2)
    
    if(input$forCheckbox == TRUE){
      ggplot(comparison %>% filter(real_name == input$cityInput2))+
        geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
        geom_point(data=sub_forecast,aes(x=year,y=quantity_rel,color=type),shape=12)+
        scale_x_continuous("Year")+
        scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
        ggtitle(paste("Relative Crime Statistics for ",input$cityInput2))+
        theme_minimal()+
        theme(legend.position = "bottom")
      
    }else{
      ggplot(comparison %>% filter(real_name == input$cityInput2))+
        geom_line(aes(x=year,y=total, color = type),size=2,alpha=0.7)+
        scale_x_continuous("Year")+
        scale_y_continuous("# of crimes per 100k people",limits = c(0,3000))+
        ggtitle(paste("Relative Crime Statistics for ",input$cityInput2))+
        theme_minimal()+
        theme(legend.position = "bottom")
    }
  })
  

  
  # Define server logic required to make the tables.
  output$table1 <- renderTable({
    if (input$forCheckbox == TRUE){
      sub_forecast <- forecast %>% 
        filter(real_name == input$cityInput1)
      
      crime_table_1 <- crime_df %>% 
        filter(real_name == input$cityInput1,
               year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
        select(real_name,year,type,quantity_rel) %>% 
        bind_rows(sub_forecast) %>% 
        spread(year,quantity_rel)
      
      colnames(crime_table_1)[5] <- "Forecast 2016"
    }else{
      crime_table_1 <- crime_df %>% 
        filter(real_name == input$cityInput1,
               year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
        select(real_name,year,type,quantity_rel) %>% 
        spread(year,quantity_rel)
    }
    
    crime_table_1})
  
  output$table2 <- renderTable({
    if (input$forCheckbox == TRUE){
      sub_forecast <- forecast %>% 
        filter(real_name == input$cityInput2)
      
      crime_table_2 <- crime_df %>% 
        filter(real_name == input$cityInput2,
               year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
        select(real_name,year,type,quantity_rel) %>% 
        bind_rows(sub_forecast) %>% 
        spread(year,quantity_rel)
      
      colnames(crime_table_2)[5] <- "Forecast 2016"
    }else{
      crime_table_2 <- crime_df %>% 
        filter(real_name == input$cityInput2,
               year %in% c(input$yearInput2[1],input$yearInput2[2])) %>% 
        select(real_name,year,type,quantity_rel) %>% 
        spread(year,quantity_rel)
    }

    crime_table_2})
})
