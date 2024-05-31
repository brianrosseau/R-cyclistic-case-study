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



#### Top 20 Casual Stations
This map filters out the start stations with the most number of rides by casual users. 

```{r}
#Create a map of the top 20 casual stations
map_data_casual <- total_tripdata_clean_5 %>%        # Data frame name reflects casual riders
  filter(                                            # Add filter for casual riders
    member_casual == "casual"
  ) %>% 
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
  ) %>% 
  arrange(                                            #Add arrange and head lines to limit top 20 n
    desc(numrides)
  ) %>% 
  head(n = 20)

ride_bins <- seq(10000, 55000, by = 5000)

ride_palette <- colorBin(
  palette = "plasma"(15, direction = -1),               
  domain = map_data_casual$numrides,
  na.color = "transparent",
  bins = ride_bins
  )

ride_text <- paste(
  "Station name: ", map_data_casual$start_station_name, "<br/>",
  "Customer type: ", "Casual riders", "<br/>",                                   # Add text line for casual
  "Number of rides: ", map_data_casual$numrides, sep = ""
  ) %>% 
  lapply(htmltools::HTML)

plot_station_map_casual <- leaflet(map_data_casual) %>% 
  addTiles() %>% 
  
  setView(
    lng = -87.6298, lat = 41.9081, zoom = 11.5
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
  
  #labs(title = "Top 20 Stations for Casual Riders")

plot_station_map_casual
```

```{r}
#Create a list of the top 20 casual stations
popular_stations_casual <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "casual"
  ) %>% 
  group_by(
    start_station_name, member_casual
  ) %>%
  summarise(
    .groups = "keep", number_of_casual_rides = n()
  ) %>% 
  arrange(
    desc(number_of_casual_rides)
  )

head(popular_stations_casual, n = 20)
```

#### Top 20 Member Stations
This map filters out the start stations with the most number of rides by members. 

```{r}
#Create a map of the top 20 member stations
map_data_member <- total_tripdata_clean_5 %>%        # Data frame name reflects members
  filter(                                            # Add filter for members
    member_casual == "member"
  ) %>% 
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
  ) %>% 
  arrange(                                            #Add arrange and head lines to limit top 20 n
    desc(numrides)
  ) %>% 
  head(n = 20)

ride_bins <- seq(14000, 26000, by = 2000)

ride_palette <- colorBin(
  palette = "plasma"(15, direction = -1),
  domain = map_data_member$numrides,
  na.color = "transparent",
  bins = ride_bins
  )

ride_text <- paste(
  "Station name: ", map_data_member$start_station_name, "<br/>",
  "Customer type: ", "Members", "<br/>",                                   # Add text line for members
  "Number of rides: ", map_data_member$numrides, sep = ""
  ) %>% 
  lapply(htmltools::HTML)


plot_station_map_member <- leaflet(map_data_member) %>% 
  addTiles() %>% 
  
  setView(
    lng = -87.6298, lat = 41.8681, zoom = 11.4
  ) %>% 
  
  addProviderTiles(
    "Esri.WorldImagery"
  ) %>% 
  
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

plot_station_map_member
```

```{r}
#Create a list of the top 20 members stations
popular_stations_member <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "member"
  ) %>% 
  group_by(
    start_station_name, member_casual
  ) %>%
  summarise(
    .groups = "keep", number_of_member_rides = n()
  ) %>% 
  arrange(
    desc(number_of_member_rides)
  ) 

head(popular_stations_member, n = 20)
```

#### Top 20 Stations Overall

Now, let's identify the top 20 stations overall, putting an * by the ones that appear on both the Top 20 Casual Stations list and Top 20 Member Stations list.

```{r}
#Most popular stations overall.

#First, create a data frame with the most popular start stations overall (highest number of total rides)
popular_stations_overall <- total_tripdata_clean_5 %>% 
  group_by(
    start_station_name
  ) %>%
  summarise(
    number_of_total_rides = n()
  ) %>% 
  arrange(
    desc(number_of_total_rides)
  )

#Join columns from the popular_stations_casual and popular_stations_member data frames
popular_stations_combined <- popular_stations_overall %>% 
  left_join(popular_stations_casual, by = "start_station_name") %>% 
  subset(select = -c(member_casual)) %>% 
  left_join(popular_stations_member, by = "start_station_name") %>% 
  subset(select = -c(member_casual)) %>% 
  head(popular_stations_combined, n = 20) #%>%   highlight(sel = c(1,3,5,6,10,12,13))

#Create a column that identifies ("*") the the stations that are on both the Top 20 Casual Stations and Top 20 Member Stations lists.
popular_stations_combined$both <- with(popular_stations_combined, ifelse(
    number_of_casual_rides >= 10552
    &
    number_of_member_rides >= 14378,
    "*", "")
  )
  
popular_stations_combined <- popular_stations_combined %>% 
     relocate(both, .after = start_station_name)

popular_stations_combined
```

```{r}

```

We should market these stations, especially those marked with an *.



### 3. Most popular times of the year - Heatmap

Let's create a heatmap that shows us the most popular times of the year for all riders, by number of rides.

```{r}
#Arrange days of week in order beginning with Monday
total_tripdata_clean_5$day_of_week <- ordered(
  total_tripdata_clean_5$day_of_week,
  levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
  )
)

#Create a data frame summarizes the number of rides by date
heat_map_data <- total_tripdata_clean_5 %>% 
  select(
    date,
    day_of_week,
    week,
    year
    ) %>% 
  group_by(
    date
    ) %>% 
  mutate(
    numrides = n()
    ) %>% 
  distinct(
    date,
    .keep_all = TRUE
    )
```

```{r}
#Create a heat map to show the most popular times of the year for all riders
plot_heat_map <- ggplot(
  heat_map_data,
  aes(
    x = week,
    y = day_of_week,
    fill = numrides
    )
  ) +
  scale_fill_viridis(
    option = "D",
    direction = 1,
    name = "Number of rides"
    ) +
  geom_tile(
    color = "white",
    na.rm = FALSE
    ) +
  facet_wrap(
    "year",
    ncol = 1
    ) +
  scale_y_discrete(
    limits = rev
    ) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
  theme_light() +
  theme(
    axis.title = element_blank()
    ) +
  labs(title = "Most Popular Times of Year - All Riders")

plot_heat_map
```

We can see that the summer months are considerably more popular among all riders.

Now, let's break this down into two separate heat maps - one for casual riders and one for members.


```{r}
#Create a data frame that summarizes the number of rides by date and type of rider
heat_map_data_member_casual <- total_tripdata_clean_5 %>% 
  
  select(
    date,
    day_of_week,
    week,
    year,
    member_casual,
  ) %>% 
  
  group_by(
    member_casual,
    date
  ) %>% 
  
  mutate(
    numrides = n()
  ) %>% 
  
  distinct(
    date,
    member_casual,
    .keep_all = TRUE
  )

#Create data frame for casual riders
casual_heat_map <- heat_map_data_member_casual %>% 
  filter(member_casual == "casual")

#Create data frame for members
member_heat_map <- heat_map_data_member_casual %>% 
  filter(member_casual == "member")
```

```{r}
head(heat_map_data_member_casual)
head(casual_heat_map)
head(member_heat_map)
```

```{r}
#Create heat map for casual riders
plot_heat_map_casual <- ggplot(
  casual_heat_map,
  aes(
    x = week,
    y = day_of_week,
    fill = numrides
  )
) +
  
  scale_fill_viridis(
    option = "C",
    direction = 1,
    name = "Number of rides"
  ) +
  
  geom_tile(
    color = "white",
    na.rm = FALSE
  ) +
  
  facet_wrap(
    "year",
    ncol = 1
  ) +
  
  scale_y_discrete(
    limits = rev
  ) + 
  
  scale_x_continuous(
    expand = c(0,0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  
  theme_light() +
  
  theme(
    axis.title = element_blank()
  ) +
  
  labs(title = "Casual Riders")
```

```{r}
#Create heat map for member riders
plot_heat_map_member <- ggplot(
  member_heat_map,
  aes(
    x = week,
    y = day_of_week,
    fill = numrides
  )
) +
  
  scale_fill_viridis(
    option = "C",
    direction = 1,
    name = "Number of trips"
  ) +
  
  geom_tile(
    color = "white",
    na.rm = FALSE
  ) +
  
  facet_wrap(
    "year",
    ncol = 1
  ) +
  
  scale_y_discrete(
    limits = rev
  ) + 
  
  scale_x_continuous(
    expand = c(0,0),
    breaks = seq(1, 52, length = 12),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  
  theme_light() +
  
  theme(
    axis.title = element_blank()
  ) +
  
  labs(title = "Members")
```

```{r}
#Combine the heat maps for the casual riders and members, using one legend
plot_heat_map_combined <- ggarrange(
  plot_heat_map_casual,
  plot_heat_map_member,
  ncol = 1,
  nrow = 2,
  common.legend = TRUE,
  legend = "right"
)
```

```{r}
#Plot the combined heat map
plot_heat_map_combined
```

Comparing the two heat maps, we see that members have many more days that have high numbers of rides. 

We should focus in on the days that are most popular with casual riders.

So let's identify the specific dates that are the most popular for casual riders, having at least 10,000 rides.

```{r}
#Create a data frame that lists the days of the year in which the number of casual rides is 10,000 or more.
casual_days <- total_tripdata_clean_5 %>% 
  filter(member_casual == "casual") %>% 
  group_by(member_casual, date, day_of_week) %>% 
  summarize(number_of_rides = n()) %>% 
  filter(number_of_rides >= 10000)

#Add a column starring the days with 14,000 rides or more.
casual_days$most_popular <- with(casual_days, ifelse(
    number_of_rides >= 14000,
    "*", "")
  )
  
casual_days
```

The chart above shows the days of the year that saw the most number of casual rides: 10,000 or more. The "most_popular" column identifies the days that have 14,000 or more casual rides.

When looking at the list, we first will note that all of the dates are on Fridays, Saturdays, Sundays, and Mondays. Fridays and Mondays on the list often correspond with holiday weekends. Certain weekends stand out, such as Memorial Day Weekend (May 28 - 30), Juneteenth Weekend (June 17 - 20), and Fourth of July Weekend (July 2 - 4). 

We should tarket these weekends to offer annual membership sales to our casual riders.




### 4. Additional ride data

#### Total Rides by customer type
Now let's see get a basic look at how many casual and member rides took place and their percentages.

```{r}
#Create a data frame showing the total number of casual and member rides and the percentage
sum_customer_rides <- total_tripdata_clean_5 %>% 
  group_by(member_casual) %>% 
  summarize(ride_count = n()) %>% 
  mutate(percent = round((ride_count / sum(ride_count)) * 100, 0))

sum_customer_rides
```

We can visualize it in a simple pie chart.

```{r}
#Create a pie chart to visualize percentage of casual and member rides
ggplot(sum_customer_rides, aes(x = "", y = ride_count, fill = member_casual)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("Percent of total rides")
```

We see that the total number of member rides outnumber the total number of casual rides.

#### Average Duration by customer type
But what about the average length of each ride?

This simple tibble and bar graph shows the average ride length in seconds for casual and member rides.
```{r}
avg_customer_rides <- total_tripdata_clean_5 %>% 
  group_by(member_casual) %>% 
  summarize(average_duration_seconds = round(mean(ride_length), 0) 
  )

avg_customer_rides

avg_customer_rides %>% 
  ggplot(aes(x = member_casual, y = average_duration_seconds, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Average Ride Length",
       x = "Customer Type",
       y = "Average Ride Length (seconds)") +
  scale_fill_discrete(name = "Customer Type") 
```

Casual rides are much longer on average than member rides.

#### Analysis per month
Let's take a closer look at number of rides, breaking it down by months.
```{r}
#Analyze ridership data by type and month

#First, order the months starting in November since this is where our dataset begins.
total_tripdata_clean_5$month <- ordered(total_tripdata_clean_5$month, levels=c("11", "12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

#Create a visualization of the total number of rides per month
vis_month_stacked <- total_tripdata_clean_5 %>% 
  ggplot(aes(x = month, fill = member_casual)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle = 45, hjust=1)) +
  scale_x_discrete(labels = c("Nov 21", "Dec 21", "Jan 22", "Feb 22", "Mar 22", "Apr 22", "May 22", "Jun 22", "Jul 22", "Aug 22", "Sep 22", "Oct 22")) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "Number of rides per month (stacked)",
       x = "Month",
       y = "Number of Rides") +
  scale_fill_discrete(name = "Customer Type") 

vis_month_stacked
```

This graph shows the total rides for all customers each month. Each bar represents a month, with the customer type stacked within each month. We can see that the summer months are the most popular.

Now let's look at the same data in a different way. We will unstack the customer type and put them side-by-side for each month.


```{r}
#First, create a tibble that we will use for visualization that calculates number of rides for each month
vt_month <- total_tripdata_clean_5 %>% 
  group_by(member_casual, month) %>%              
  summarise(
    number_of_rides = n()
    ) %>%                                        
  arrange(member_casual, month)                   
  
vt_month
```


```{r}
#Visualize the number of rides per month by customer type 
vis_month <- vt_month %>% 
  ggplot(aes(x = month,  y = number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x=element_text(angle = 45, hjust=1)) +
  scale_x_discrete(labels = c("Nov 21", "Dec 21", "Jan 22", "Feb 22", "Mar 22", "Apr 22", "May 22", "Jun 22", "Jul 22", "Aug 22", "Sep 22", "Oct 22")) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "Number of rides per month (unstacked)",
       x = "Month",
       y = "Number of Rides") +
  scale_fill_discrete(name = "Customer Type") 

vis_month
```

Here we can compare the customer types more closely. Every month has more member rides than casual rides. Also, while member rides remain high June through September, casual rides begin to show decline in August. Winter months have very low ride counts, especially for casual riders.

#### Analysis by Day of the Week
Let's look at the number of rides and average duration of rides each day of the week.
```{r}
#Analyze ridership data by type and weekday
total_tripdata_clean_5$day_of_week <- ordered(total_tripdata_clean_5$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Create a tibble that we will use for visualization that calculates number of rides and average duration
vt_num_avg <- total_tripdata_clean_5 %>% 
  group_by(member_casual, day_of_week) %>%                  #groups by usertype and weekday
  summarise(
    number_of_rides = n(), 
    average_duration_seconds = round(mean(ride_length), 0)
    ) %>%                                         #calculates the number of rides and average duration
  arrange(member_casual, day_of_week)                       #sorts
  
vt_num_avg

```

This shows the number of rides and the average duration in seconds, respectively, for casual riders and members each day of the week.

Now we will put Number of Rides in a graph.

```{r}
#Visualize the number of rides by rider type 
vis_num_rides <- vt_num_avg %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(axis.text.x=element_text(angle = 45, hjust=1)) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  labs(title = "Number of rides per day of the week",
       x = "Day of Week",
       y = "Number of Rides") +
  scale_fill_discrete(name = "Customer Type")

vis_num_rides
```

This bar graph shows that the number of rides by members far surpasses those by casual riders on Mondays through Fridays. However, casual rides slightly outnumber member rides on the weekends.
The most popular days for members are Tuesday, Wednesday, and Thursday.

Let's do the same for Average Ride Length.

```{r}
#Visualize average ride duration
vis_avg_dur <- vt_num_avg %>% 
  ggplot(aes(x = day_of_week, y = average_duration_seconds, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(axis.text.x=element_text(angle = 45, hjust=1)) +
  labs(
    title = "Average ride length per day of the week",
    x = "Day of Week",
    y = "Average Ride Length (seconds)") +
  scale_fill_discrete(name = "Customer Type")

vis_avg_dur
```

Conversely, this bar graph tells us that the average duration of casual rides is double those of member rides, every day of the week! 

A higher average ride length suggests recreational usage of the bike. If this is so, the average casual riders is more likely to use the bikes for recreational purposes. Looking at members, the average duration for members is almost identical Monday through Friday; it is slightly higher for members on the weekends. This suggests that most members use the bikes for commuting to work on the weekdays and for recreation on the weekends. 


#### Analysis by time of day
Now, let's see if there is any difference in the time of day that casual riders and members use Cyclistic bikes.
```{r}
#Convert time_of_day from Hour/Minute/Second to 12Hour AM/PM format.
total_tripdata_clean_5$am_pm_hour <- 
  format(strptime(total_tripdata_clean_5$time_of_day, '%H:%M:%S'), '%I %p')

#Convert am_pm_hour to categorical format to display hours in correct sequence
total_tripdata_clean_5 <- total_tripdata_clean_5 %>% 
  mutate(am_pm_hour = factor(am_pm_hour, levels = c("12 AM", "01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM", "07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM", "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", "08 PM", "09 PM", "10 PM", "11 PM")))

#Create tibble of total number of rides per hour
rides_ph_bar <- total_tripdata_clean_5 %>% 
  group_by(member_casual, am_pm_hour) %>%
  summarise(number_of_rides = n()) %>% 
  arrange(am_pm_hour)

rides_ph_bar
```


```{r}
#Visualize total number of rides per hour
rides_ph_bar %>% 
  ggplot(aes(x=am_pm_hour, y=number_of_rides, fill=member_casual)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")) +
  labs(
    title = "Total number of rides per hour",
    x = "Hour each ride began",
    y = "") +
  theme(axis.text.x = element_text(size = 5, angle = -40, vjust = 0.5, hjust = 0)) +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_fill_discrete(name = "Customer Type")
```

It appears that start time for member rides peaks at around 8am and 5pm. This would be the times that most people leave for and return from work. The start time for casual riders, however, is around 5pm only. This supports the ideas that members use the bikes mostly for work, and casual riders mostly for recreation, as they are getting off of work.





### 5. Summary of Observations and Conclusions:

The most popular bike stations for casual riders are along the coast and the Magnificent Mile, focusing on tourist areas, with the most popular station by far being Streeter Dr and Grand Ave on the Navy Pier. The most popular stations for members are more centrally located around business and residential areas, with the most popular station being Kingsbury St and Kinzie St.

The most popular times of the year are summer weekends and summer holidays. This is especially evident for casual riders.

While the number of member rides outnumber casual rides 3 to 2, the average ride length of casual rides doubles that of member rides.

Summer months are the most popular with all riders. However, members have a "longer" popular summer season (June - September) than casual riders (June - July).

Measuring "popular days" by those with the most number of rides, weekends are the most popular for casual riders. However, the reverse is true for members; weekdays are more popular. But the average ride length is higher on the weekends for both customer types. 

As far as start times, member rides peak around 8am and 5pm, about the times that most people leave for work and leave from work. Casual rides peak around 5pm only, suggesting they are most likely to go for a ride after work.

These observations suggest that members use the bikes to travel to work primarily, whereas casual riders use the bikes for recreational purposes mostly.



## IV.Act

What recommendations can we make based on our analysis?

1. Target year-round marketing strategies around popular stations, especially those that are popular for both members and casual riders: Streeter Dr & Grand Ave, DuSable Lake Shore Dr & North Blvd, Wells St & Concord Ln, Clark St & Elm St, Wells St & Elm St, Broadway & Barry Ave, Wabash Ave & Grand Ave.

2. Target seasonal marketing strategies around popular summer holidays: Memorial Day, Juneteenth, and Fourth of July weekends.

3. Advertise the new fleet of electric bikes and the five new charging stations: Bissell St & Armitage Ave, Green St & Randolph St, Lincoln Ave & Roscoe St, Morgan St & Lake St, and Wilton Ave & Diversey Pkwy.
