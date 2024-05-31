# R-cyclistic-case-study
Capstone project for Google Data Analytics Certification

---
title: "Cyclistic Case Study - Visualizations"
author: "Brian Rosseau"
date: "2023-07-15"
output:
  html_document:
    code_folding: hide
    df_print: paged
  pdf_document: default
  html_notebook:
    code_folding: hide
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

### 1. Load cleaned data set and packages
```{r read}
total_tripdata_clean_5 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-05-23 total_tripdata_clean_5.csv")
```

```{r}
library(tidyverse)
library(data.table)
library(viridis)
library(ggpubr) 
library(leaflet)
library(scales)
```

### 2. Most popular stations
#### All stations, by number of rides
This map shows all the start stations, distinguishing them by total number of rides
```{r}
#Create a map to show the most popular stations by all riders
map_data <- total_tripdata_clean_5 %>% 
  select(
    start_station_name,
    start_lat,
    start_lng
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  mutate(
    numrides = n()
  ) %>% 
  distinct(
    start_station_name,
    .keep_all = TRUE
  )

ride_bins <- seq(0, 80000, by = 10000)

ride_palette <- colorBin(
  palette = "plasma"(15, direction = -1),
  domain = map_data$numrides,
  na.color = "transparent",
  bins = ride_bins
  )

ride_text <- paste(
  "Station name: ", map_data$start_station_name, "<br/>",
  "Number of rides: ", map_data$numrides, sep = ""
  ) %>% 
  lapply(htmltools::HTML)

plot_station_map <- leaflet(map_data) %>% 
  addTiles() %>% 
  
  setView(
    lng = -87.6298, lat = 41.8781, zoom = 11.5
  ) %>% 
  
  addProviderTiles("Esri.WorldImagery") %>% 
  
  addCircleMarkers(
    ~ start_lng, ~ start_lat,
    fillColor = ~ ride_palette(numrides),
    fillOpacity = 0.7,
    color = "white",
    radius = 7,
    stroke = FALSE,
    label = ride_text,
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
        ),
      textsize = "13px",
      direction = "auto"
      )
    ) %>% 
  
  addLegend(
    pal = ride_palette,
    values = ~ numrides,
    opacity = 0.9,
    title = "Number of rides",
    position = "bottomright"
    )

plot_station_map
```

