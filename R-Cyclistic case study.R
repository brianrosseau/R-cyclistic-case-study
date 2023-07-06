

#1 Install and load the necessary packages.

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

#2 Combine data frames
#2.1 Load each of the .csv files to RStudio using readr from tidyverse and assign each data frame a new name according to month.

Nov21 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202111-divvy-tripdata.csv")
Dec21 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202112-divvy-tripdata.csv")
Jan22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202201-divvy-tripdata.csv")
Feb22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202202-divvy-tripdata.csv")
Mar22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202203-divvy-tripdata.csv")
Apr22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202204-divvy-tripdata.csv")
May22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202205-divvy-tripdata.csv")
Jun22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202206-divvy-tripdata.csv")
Jul22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202207-divvy-tripdata.csv")
Aug22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202208-divvy-tripdata.csv")
Sep22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202209-divvy-publictripdata.csv")
Oct22 <- read.csv("C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\original data\\202210-divvy-tripdata.csv")


#2.2 Check column names
#Compare column names in each of the data frames to verify that column names match.This is needed before we can write a command to join them into one file.

colnames(Nov21)
colnames(Dec21)
colnames(Jan22)
colnames(Feb22)
colnames(Mar22)
colnames(Apr22)
colnames(May22)
colnames(Jun22)
colnames(Jul22)
colnames(Aug22)
colnames(Sep22)
colnames(Oct22)

#Upon running these commands, we found that the column names all match. 


#2.3 Check data structure
#Inspect the data frames to look for incongruencies prior to combining data frames.

str(Nov21)
str(Dec21)
str(Jan22)
str(Feb22)
str(Mar22)
str(Apr22)
str(May22)
str(Jun22)
str(Jul22)
str(Aug22)
str(Sep22)
str(Oct22)

#Above we saw that all column names match, and that the matching columns are of the same string type. So we can go forward with combining the data frames.


#2.4 Merge data frames
#Combine all 12 data frames into one data frame using rbind function. Assign the new data frame the name total_tripdata.

total_tripdata <- rbind(Nov21, Dec21, Jan22, Feb22, Mar22, Apr22, May22, Jun22, Jul22, Aug22, Sep22, Oct22)
View(total_tripdata)

tibble(total_tripdata)


#3 CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS 
#3.1 Inspect new data frame

# List of column names
colnames(total_tripdata)

#How many rows are in data frame?
nrow(total_tripdata)

#What are the dimensions of the dataframe?
dim(total_tripdata)

#See the first six rows of the data frame. Also last six rows.
head(total_tripdata)
tail(total_tripdata)

#See list of columns and data types (numeric, character, etc.)
str(total_tripdata)

#Statistical summary of data
summary(total_tripdata)


#3.2 Confirm objects in columns
#Group by customer type in the member_casual column to confirm that these are the only two labels for customers.
total_tripdata %>% 
  group_by(member_casual) %>% 
  count(member_casual)

#Create a table showing each customer type.
table(total_tripdata$member_casual)


------------------------------
  ###Optional change:
  #Let's change the name of the 'member_casual' column to 'customer_type' to give it more meaning.
  
  
  ##******************************************
  ##*
  ##*
  ##*
  ##*need to add code
  ##*
  ##*
##*
##*

-------------------------
  
  
  #3.3 Add columns that list the date, month, day, year, day of week, and time of day for each ride. This allows us to aggregate data at more than just the ride level.
  #Date (yyyy-mm-dd)
  total_tripdata$date <- as.Date(total_tripdata$started_at)

#Month
total_tripdata$month <- format(as.Date(total_tripdata$date), "%m")

#Day
total_tripdata$day <- format(as.Date(total_tripdata$date), "%d")

#Year
total_tripdata$year <- format(as.Date(total_tripdata$date), "%Y")

#Day of week
total_tripdata$day_of_week <- format(as.Date(total_tripdata$date), "%A")

######Week columns was added at the end
#Week
total_tripdata$week <- format(as.Date(total_tripdata$date), "%W")

#Time of day (HH:MM:SS)
total_tripdata$time_of_day <- format(as.POSIXct(total_tripdata$started_at, format= "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

#Order by date
total_tripdata <- total_tripdata %>% 
  arrange(started_at)

colnames(total_tripdata)


#3.4 Calculate ride length
#Add a column that calculates ride length
total_tripdata$ride_length <- difftime(total_tripdata$ended_at, total_tripdata$started_at)

#Inspect the structure of the columns
str(total_tripdata)

#Change ride length column to numeric string type so we can run calculations
is.factor(total_tripdata$ride_length)
total_tripdata$ride_length <- as.numeric(as.character(total_tripdata$ride_length))
is.numeric(total_tripdata$ride_length)

#Inspect structure again to confirm change
str(total_tripdata)




View(total_tripdata)

#156 line

#3.5 Remove bad data
#3.51 Remove rides with negative ride_length.
#Any rides where ride_length is negative could indicate rides where Cyclistic took bikes out of circulation for quality control purposes. These rows should be removed from the dataset.

#Create a data frame to see if there are any invalid data points with ride lengths less than 0. 
ride_length_negative <- total_tripdata %>% 
  filter(ride_length<0)

tibble(ride_length_negative$ride_length)

View(ride_length_negative)

################# NEW CLEANED DATASET IS CALLED 'total_tripdata_clean' (previously called 'total_tripdata_cleaned')


#Remove rows with ride length < 0
#We will create a new version of the dataset called total_tripdata_clean since data is being removed.
total_tripdata_clean <- total_tripdata[!(total_tripdata$ride_length<0),]

View(total_tripdata_clean)

#3.52 Remove rides with a missing station name.
#For our analysis we will want complete station name information. So, we will want to remove any rows that are missing this data.
#The following code counts the number of NA's or blanks in each column of the dataset.
colSums(is.na(total_tripdata_clean) | total_tripdata_clean == "")

#We can see that there are several columns that have a lot of missing data, especially start stations and end stations. We should see how many actual rows contain this missing data.

#Create a data frame to see how many rows have missing station names.
no_station_name <- total_tripdata_clean %>% 
  filter(
    is.na(start_station_name) |
      start_station_name == "" |
      is.na(end_station_name) |
      end_station_name == ""
  ) 

View(no_station_name)

#Upon viewing the no_station_name data frame, it looks like most of these rows that contain missing station name information are rides with electric bikes.
#Let's create another data frame to confirm this. We will group together the types of bikes used in the rideable_type column.

rideable_type_vs_no_station_name <- no_station_name %>% 
  group_by(rideable_type) %>% 
  count(rideable_type)

print(rideable_type_vs_no_station_name)

#Wow! Of the 1,345,218 rows with missing station names, 1,338,310 of these rows have electric bike as their user type! The remain 6,908 rows are shared by classic and docked bikes.
#Nearly all of the rows with missing stations names are electric bikes. This should be noted, as we will be removing this data.

#Let's return to our no_station_name data frame.

nrow(no_station_name)         #This counts the number of rows in the no_station_name data frame. There are 1,345,218 rides that are missing station name information!
nrow(total_tripdata_clean)  #This counts the number of rows in the total_tripdata_clean data frame. Altogether there are 5,755,582 rides in the dataset.

nrow(total_tripdata_clean) - nrow(no_station_name)  #The difference is 4,410,364 rows of data. This is what would be left of the total_tripdata_clean dataset if we remove the rows with missing station names.
(nrow(no_station_name) / nrow(total_tripdata_clean))*100  #Calculating the percentage, rows with missing station names represent over 23 percent of the total rides! 
#This is a significant amount of data that we would be removing. But considering that we need station name information for our analysis, and since we still have adequate data to represent the whole, we will go forward with removing the bad data.

#Remove start_station_name and end_station_name blank results.
total_tripdata_clean <- total_tripdata_clean %>% 
  filter(
    !(is.na(start_station_name) |
        start_station_name == "")
  ) %>% 
  filter(
    !(is.na(end_station_name) |
        end_station_name == "")
  )

#As a test, we will again count the number of NA's in the newly cleaned dataset. - the following tests show no remaining NA or blanks in start_station_name or end_station_name columns
colSums(is.na(total_tripdata_clean) | total_tripdata_clean == "")

#The results show no NA's or blanks in the dataset!

View(total_tripdata_clean)






#####Let's save the total_tripdata_clean dataset under another name before going forward in case we need to revert back to it.
total_tripdata_clean_2 <- total_tripdata_clean
















#Upon further review of the ride_length_negative dataframe that we created and removed in 3.51, several keywords were found in the station name and station id columns that did not fit the normal format or warranted additional review. Some of these keywords were TEST, Base, checking, charging, or all capital letters.
#Let's create some data frames that filter out these and other possible anomalies from the larger dataset to see if these are legitimate rides.
#Since it is suspected that these may be test rides, we will give the data frames the prefix 'test_station_check_'.

#We will start with "TEST" and various forms like "test," and "Test."

test_station_check_TEST <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "TEST") |
      str_detect(end_station_name, "TEST") |
      str_detect(start_station_id, "TEST") |
      str_detect(end_station_id, "TEST"),
  )

test_station_check_test <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "test") |
      str_detect(end_station_name, "test") |
      str_detect(start_station_id, "test") |
      str_detect(end_station_id, "test"),
  )

test_station_check_Test <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "Test") |
      str_detect(end_station_name, "Test") |
      str_detect(start_station_id, "Test") |
      str_detect(end_station_id, "Test"),
  )

#These all appear to be test stations that should be removed. 
#We also found additional keywords to investigate, including 'WestChi', 'DIVVY', 'Warehouse', 'Hubbard Bike' and station names with all capital letters.

#Let's filter out those with all capital letters. In our search, it appears that stations that are all capitalized may be test stations.
#Create a data frame of station names that are all capital letters to see if they are test stations.
test_station_check_capitalized <- total_tripdata_clean_2 %>% 
  filter(
    (str_detect(start_station_name, "[:upper:]")
     & !str_detect(start_station_name,"[:lower:]")
     |
       (str_detect(end_station_name, "[:upper:]")
        & !str_detect(end_station_name, "[:lower:]"))
    ))

test_station_check_capitalized %>% select(start_station_name, start_station_id, end_station_name, end_station_id)

#These are also test stations and should be removed.
#Additional keywords found that may indicate test or repair stations include 'Temp', and 'REPAIR'.

#But let's go ahead and remove the test stations we have found so far. These include rows with the keywords "TEST", "test", "Test", and all capital letters.

#Remove rows with "TEST", "test", and "Test" stations from dataset.
total_tripdata_clean_2 <- total_tripdata_clean_2 %>% 
  filter(
    !(str_detect(start_station_name, "TEST") |
        str_detect(end_station_name, "TEST") |
        str_detect(start_station_id, "TEST") |
        str_detect(end_station_id, "TEST"))
  ) 

total_tripdata_clean_2 <- total_tripdata_clean_2 %>% 
  filter(
    !(str_detect(start_station_name, "test") |
        str_detect(end_station_name, "test") |
        str_detect(start_station_id, "test") |
        str_detect(end_station_id, "test"))
  ) 

total_tripdata_clean_2 <- total_tripdata_clean_2 %>% 
  filter(
    !(str_detect(start_station_name, "Test") |
        str_detect(end_station_name, "Test") |
        str_detect(start_station_id, "Test") |
        str_detect(end_station_id, "Test"))
  )

#Remove rows with capitalized test stations from cleaned dataset
total_tripdata_clean_2 <- total_tripdata_clean_2 %>% 
  filter(
    !(str_detect(start_station_name, "[:upper:]")
      & !str_detect(start_station_name,"[:lower:]")
      |
        (str_detect(end_station_name, "[:upper:]")
         & !str_detect(end_station_name, "[:lower:]"))
    ))


#Check to make sure they are removed
test_station_check_TEST <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "TEST") |
      str_detect(end_station_name, "TEST") |
      str_detect(start_station_id, "TEST") |
      str_detect(end_station_id, "TEST"),
  )

test_station_check_test <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "test") |
      str_detect(end_station_name, "test") |
      str_detect(start_station_id, "test") |
      str_detect(end_station_id, "test"),
  )

test_station_check_Test <- total_tripdata_clean_2 %>% 
  filter(
    str_detect(start_station_name, "Test") |
      str_detect(end_station_name, "Test") |
      str_detect(start_station_id, "Test") |
      str_detect(end_station_id, "Test"),
  )

test_station_check_capitalized <- total_tripdata_clean_2 %>% 
  filter(
    (str_detect(start_station_name, "[:upper:]")
     & !str_detect(start_station_name,"[:lower:]")
     |
       (str_detect(end_station_name, "[:upper:]")
        & !str_detect(end_station_name, "[:lower:]"))
    ))

#These data frames are now empty because they have been removed.

#Let's again save our dataset to a new name in case we need to revert back to it.
total_tripdata_clean_3 <- total_tripdata_clean_2

#Now let's review some other keywords that we found to see if these are test stations, too.

test_station_check_Base <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "Base") |
      str_detect(end_station_name, "Base") |
      str_detect(start_station_id, "Base") |
      str_detect(end_station_id, "Base"),
  )

tibble(test_station_check_Base)

test_station_check_checking <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "checking") |
      str_detect(end_station_name, "checking") |
      str_detect(start_station_id, "checking") |
      str_detect(end_station_id, "checking"),
  )

tibble(test_station_check_checking)

test_station_check_charging <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "charging") |
      str_detect(end_station_name, "charging") |
      str_detect(start_station_id, "charging") |
      str_detect(end_station_id, "charging"),
  )

tibble(test_station_check_charging)

test_station_check_WestChi <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "WestChi") |
      str_detect(end_station_name, "WestChi") |
      str_detect(start_station_id, "WestChi") |
      str_detect(end_station_id, "WestChi"),
  )

tibble(test_station_check_WestChi)

test_station_check_DIVVY <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "DIVVY") |
      str_detect(end_station_name, "DIVVY") |
      str_detect(start_station_id, "DIVVY") |
      str_detect(end_station_id, "DIVVY"),
  )

tibble(test_station_check_DIVVY)

test_station_check_Warehouse <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "Warehouse") |
      str_detect(end_station_name, "Warehouse") |
      str_detect(start_station_id, "Warehouse") |
      str_detect(end_station_id, "Warehouse"),
  )

tibble(test_station_check_Warehouse)

test_station_check_Hubbard_Bike <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "Hubbard Bike") |
      str_detect(end_station_name, "Hubbard Bike") |
      str_detect(start_station_id, "Hubbard Bike") |
      str_detect(end_station_id, "Hubbard Bike"),
  )

tibble(test_station_check_Hubbard_Bike)

test_station_check_Temp <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "Temp") |
      str_detect(end_station_name, "Temp") |
      str_detect(start_station_id, "Temp") |
      str_detect(end_station_id, "Temp"),
  )

tibble(test_station_check_Temp)

test_station_check_REPAIR <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_name, "REPAIR") |
      str_detect(end_station_name, "REPAIR") |
      str_detect(start_station_id, "REPAIR") |
      str_detect(end_station_id, "REPAIR"),
  )

tibble(test_station_check_REPAIR)


#The following keywords were already deleted with the other test rides: 'Base', 'checking', 'WestChi', 'DIVVY', 'Hubbard Bike', 'REPAIR'.
#However, these keywords remain: 'charging', 'Warehouse', 'Temp'.

View(test_station_check_charging)
View(test_station_check_Warehouse)
View(test_station_check_Temp)

#Upon review of these data frames, it cannot be confirmed that these are test or maintenance stations. They will not be removed from the dataset.

#Save charging stations for future analysis.
#Let's look again at the stations with 'charging' in the station_id. We will rename this data frame charging_stations.

charging_stations <- total_tripdata_clean_3 %>% 
  filter(
    str_detect(start_station_id, "charging") |
      str_detect(end_station_id, "charging"),
  )

charging_select_col <- charging_stations[,c("start_station_name","start_station_id","end_station_name","end_station_id")]

head(charging_select_col, n=10)

nrow(charging_stations)   #We count the rows and find there are 62,463 rides which begin or end at a charging station.

#Let's group the charging stations together so we can identify them.
charging_stations_group <- charging_stations %>% 
  group_by(start_station_name, start_station_id) %>% 
  count(start_station_name) %>% 
  filter(
    str_detect(start_station_id, "charging")
  )

print(charging_stations_group)


#Looking at the results, we see that there are five charging stations: Bissell St & Armitage Ave*, Green St & Randolph St*, Lincoln Ave & Roscoe St*, Morgan St & Lake St*, and Wilton Ave & Diversey Pkwy*.
#The five charging stations seem legitimate. The one ride out of the test station does not.

#Upon further research, we learned that these five charging stations are the initial five charging stations for a new fleet of Lyft ebikes. See this article dated May 5, 2022. According to the article, the new Lyft ebike was introduced in Chicago in December of 2021, and "is the first fleet of ebike in the U.S. capable of on-street charging in docks."
# https://www.chicago.gov/city/en/depts/cdot/provdrs/future_projects_andconcepts/news/2022/may/divvy-becomes-first-u-s--bikeshare-system-to-incorporate-ebike-c.html#:~:text=The%20initial%20five%20charging%20stations%20are%20located%20at%3A,Randolph%20St%205%20Morgan%20St%20%26%20Lake%20St

#There is no reason to believe that the rides we identified coming in and out of these five charging stations are illegitimate. We will save the charging_stations dataset for future analysis, but will NOT remove their rows from the total_tripdata_clean_3 dataset.

#Consider removing station names without ampersand.
#We notice that the normal station_name format in the large dataset is two street names with an "&" between. Let's  filter out those not following this format to see if there are any anomalies.

station_name_no_ampersand_check <- total_tripdata_clean_3 %>% 
  filter(
    !(str_detect(start_station_name, "&") |
        str_detect(end_station_name, "&"))
  )

station_name_no_ampersand_group <- station_name_no_ampersand_check %>% 
  group_by(start_station_name) %>% 
  count(start_station_name)

View(station_name_no_ampersand_group) #There are 82 station names without an "&".

#When we group them together by station name and view the data frame, we see no reason to exclude these from our dataset.


#Let's again save the dataset under a new name in case we need to revert back to it.
total_tripdata_clean_4 <- total_tripdata_clean_3




#3.55 Remove error rides
#Now, check to see if there are any error rides that last 60 seconds or less in which the bike began and ended its ride at the same station, or any rides that were 30 seconds or less. This could indicate a user changing their mind or being unable to use the bicycle for some reason.
error_rides <- total_tripdata_clean_4 %>% 
  filter(
    ((ride_length <= 60)
     &
       (start_station_name == end_station_name))
    |
      (ride_length <= 30)
  )

tibble(error_rides)
#The tibble shows that there are 73,512 rows with an error ride.

#These error rides should be removed from the dataset.
total_tripdata_clean_4 <- total_tripdata_clean_4 %>% 
  filter(
    !((ride_length <= 60)
      &
        (start_station_name == end_station_name))
    &
      !(ride_length <= 30)
  )


#3.56 Remove Any Duplicates

#The ride_id column should be unique to each ride. Create a data frame to check that there are no duplicates.
duplicate_ride_id_check <- total_tripdata_clean_4 %>% 
  count(ride_id) %>% 
  filter(n > 1)

print(duplicate_ride_id_check)

#The above code returned no duplicates for ride_id.

#Let's check to make sure there are no rows that are duplicated.

duplicate_rows_check <- total_tripdata_cleaned[duplicated(total_tripdata_clean_4), ]

print(duplicate_rows_check)

#The above code returned no duplicate rows.


#We will save the final cleaned version of the dataset with a new name.
total_tripdata_clean_5 <- total_tripdata_clean_4

----------------------------
  
  
  
  
  
  
  #3.6 Understand the dataset
  #3.61 Understand the charging_stations dataset.
  
  #Save charging stations for future analysis.
  #Let's look again at the stations with 'charging' in the station_id. We will rename this data frame charging_stations, gleaned from the final cleaned version of the dataset.

charging_stations <- total_tripdata_clean_5 %>% 
  filter(
    str_detect(start_station_id, "charging") |
      str_detect(end_station_id, "charging"),
  )

charging_select_col <- charging_stations[,c("start_station_name","start_station_id","end_station_name","end_station_id")]

head(charging_select_col, n=10)

nrow(charging_stations)   #We count the rows and find there are 60,405 rides which begin or end at a charging station.

#Let's group the charging stations together so we can identify them.
charging_stations_group <- charging_stations %>% 
  group_by(start_station_name, start_station_id) %>% 
  count(start_station_name) %>% 
  filter(
    str_detect(start_station_id, "charging")
  )

print(charging_stations_group)


#Looking at the results, we see that there are five charging stations: Bissell St & Armitage Ave*, Green St & Randolph St*, Lincoln Ave & Roscoe St*, Morgan St & Lake St*, and Wilton Ave & Diversey Pkwy*.
#The five charging stations seem legitimate. The one ride out of the test station does not.

#Upon further research, we learned that these five charging stations are the initial five charging stations for a new fleet of Lyft ebikes. See this article dated May 5, 2022. According to the article, the new Lyft ebike was introduced in Chicago in December of 2021, and "is the first fleet of ebike in the U.S. capable of on-street charging in docks."
# https://www.chicago.gov/city/en/depts/cdot/provdrs/future_projects_andconcepts/news/2022/may/divvy-becomes-first-u-s--bikeshare-system-to-incorporate-ebike-c.html#:~:text=The%20initial%20five%20charging%20stations%20are%20located%20at%3A,Randolph%20St%205%20Morgan%20St%20%26%20Lake%20St

#We will save the charging_stations dataset for future analysis.


#Let's see when the charging stations began, since it is expected that they were introduced during our analysis period.


#We will begin by creating data frames for each month that list the unique station names used during that month.
charging_stations_Nov21 <- charging_stations %>% 
  filter(
    month == "11"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Dec21 <- charging_stations %>% 
  filter(
    month == "12"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Jan22 <- charging_stations %>% 
  filter(
    month == "01"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Feb22 <- charging_stations %>% 
  filter(
    month == "02"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Mar22 <- charging_stations %>% 
  filter(
    month == "03"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Apr22 <- charging_stations %>% 
  filter(
    month == "04"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_May22 <- charging_stations %>% 
  filter(
    month == "05"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Jun22 <- charging_stations %>% 
  filter(
    month == "06"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Jul22 <- charging_stations %>% 
  filter(
    month == "07"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Aug22 <- charging_stations %>% 
  filter(
    month == "08"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Sep22 <- charging_stations %>% 
  filter(
    month == "09"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )

charging_stations_Oct22 <- charging_stations %>% 
  filter(
    month == "10"
  ) %>% 
  group_by(
    start_station_name
  ) %>% 
  count(
    start_station_name
  )



#Next, we check each unique station name against the monthly data frames to see which unique stations names were used in a particular month.
#
charging_stations_group$Nov_21 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Nov21$start_station_name)

charging_stations_group$Dec_21 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Dec21$start_station_name)

charging_stations_group$Jan_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Jan22$start_station_name)

charging_stations_group$Feb_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Feb22$start_station_name)

charging_stations_group$Mar_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Mar22$start_station_name)

charging_stations_group$Apr_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Apr22$start_station_name)

charging_stations_group$May_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_May22$start_station_name)

charging_stations_group$Jun_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Jun22$start_station_name)

charging_stations_group$Jul_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Jul22$start_station_name)

charging_stations_group$Aug_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Aug22$start_station_name)

charging_stations_group$Sep_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Sep22$start_station_name)

charging_stations_group$Oct_22 <- as.integer(charging_stations_group$start_station_name
                                             %in% charging_stations_Oct22$start_station_name)

print(charging_stations_group)
View(charging_stations_group)

#It looks like the first rides coming out of these stations began in April 22 for all of the charging stations except Wilton Ave * Diversey Pkwy, which didn't see its first rides until July 22. This explains why that station's total number of rides is less than the other stations.


#Now, let's see who was using these charging stations.
table(charging_stations$member_casual)

#It appears that members have more rides coming out of the charging stations, but many casual riders and members both seem to enjoy the new stations.
#These new charging stations should be promoted as part of our marketing strategy.



#5: CONDUCT DESCRIPTIVE ANALYSIS
#5.1: Descriptive analysis on ride_length (in seconds)

#Average (total ride length / rides)
mean(total_tripdata_clean_5$ride_length) 

#Midpoint number in the ascending array of ride lengths
median(total_tripdata_clean_5$ride_length)  

#Longest ride
max(total_tripdata_clean_5$ride_length)   

#Shortest ride
min(total_tripdata_clean_5$ride_length)           

#Condenses the above calculations into one line of summary analysis
summary(total_tripdata_clean_5$ride_length)       


#5.2: Compare members and casual users

#Average ride length (in seconds)
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual, FUN = mean)

#Median ride length (in seconds)
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual, FUN = median)

#Longest ride (in seconds)
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual, FUN = max)

#Shortest ride (in seconds)
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual, FUN = min)


#5.3: Let's check out the average ride time each day of the week for members and casual riders.
#See the average ride time by each day for members vs casual users
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual + total_tripdata_clean_5$day_of_week, FUN = mean)

#Notice that the days of the week are out of order. Fix that here:
total_tripdata_clean_5$day_of_week <- ordered(total_tripdata_clean_5$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Now, run the average ride time by each day for members vs casual users.
aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual + total_tripdata_clean_5$day_of_week, FUN = mean)

#Looks like the average casual ride length is almost double that of members each day of the week!


#Analyze ridership data by type and weekday
total_tripdata_clean_5 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%      #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                      #groups by usertype and weekday
  summarise(number_of_rides = n()                           #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>%      #calculates the average duration
  arrange(member_casual, weekday)                           #sorts

#Visualize the number of rides by rider type    
total_tripdata_clean_5 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            , average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Visualize average ride duration
total_tripdata_clean_5 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(total_tripdata_clean_5$ride_length ~ total_tripdata_clean_5$member_casual + total_tripdata_clean_5$day_of_week, FUN = mean)

View(counts)







#Let's continue our analysis by looking at the rideable types and how their usage differs between casual riders and members.
#Create a table that shows rideable type usage by casual riders and members.
rideable_type_vs_user_type <- total_tripdata_clean_5 %>% 
  group_by(
    rideable_type,
    member_casual
  ) %>%
  select(
    rideable_type,
    member_casual
  ) %>% 
  count(
    rideable_type
  )

print(rideable_type_vs_user_type)

###Remember that many electric bike rides were removed because they lacked station name information.

#The resulting chart shows the rideable types "classic_bike," "docked_bike," and "electric_bike" and how much each one is used by casual riders and by members. Classic bikes are the most popular by both users. Also, it is interesting to note that docked bikes are only used by casual riders. Docked bike usage is also significantly less than the classic and electric bikes. 
#This raises the question of whether docked bikes were either introduced or phased out part way through the year, or whether this is representative of the whole year.

#Create a chart showing docked_bike usage month by month.
rideable_type_vs_user_type_monthtest <- total_tripdata_clean_5 %>% 
  filter(
    rideable_type == "docked_bike"
  ) %>% 
  group_by(
    month,
    member_casual
  ) %>% 
  select(
    month,
    rideable_type,
    member_casual
  ) %>% 
  count(
    rideable_type
  )

print(rideable_type_vs_user_type_monthtest)

#This chart of docked bike usage by month shows that docked bikes were used every month of the year and were not introduced or phased out part way through the year.


#Let's find out what are the most popular start stations overall, for members, and for casual riders.
#Most popular start stations overall.
popular_stations <- total_tripdata_clean_5 %>% 
  group_by(
    start_station_name
  ) %>%
  select(
    start_station_name
  ) %>% 
  count(
    start_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

print(popular_stations)

#Most popular start stations for members.
popular_stations_member <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "member"
  ) %>% 
  group_by(
    start_station_name
  ) %>%
  select(
    start_station_name
  ) %>% 
  count(
    start_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

print(popular_stations_member)

#Most popular start stations for casual users.
popular_stations_casual <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "casual"
  ) %>% 
  group_by(
    start_station_name
  ) %>%
  select(
    start_station_name
  ) %>% 
  count(
    start_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

print(popular_stations_casual)

#Now we'll do the same for end stations.
#Most popular end stations overall.
popular_end_stations <- total_tripdata_clean_5 %>% 
  group_by(
    end_station_name
  ) %>%
  select(
    end_station_name
  ) %>% 
  count(
    end_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

print(popular_end_stations)

#Most popular end stations for members.
popular_end_stations_member <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "member"
  ) %>% 
  group_by(
    end_station_name
  ) %>%
  select(
    end_station_name
  ) %>% 
  count(
    end_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

print(popular_end_stations_member)

#Most popular end stations for casual users.
popular_end_stations_casual <- total_tripdata_clean_5 %>% 
  filter(
    member_casual == "casual"
  ) %>% 
  group_by(
    end_station_name
  ) %>%
  select(
    end_station_name
  ) %>% 
  count(
    end_station_name
  ) %>% 
  arrange(
    desc(n)
  ) %>% 
  head(20)

popular_end_stations_casual

#Conclusion: When analyzing the popular start stations vs the popular end stations, they were very similar, with only slight occasional variances in order. The same was found when comparing the start station vs. end station for members, as well as the start vs end station names for casual users.
#Therefore, it is safe to just use the start stations when doing our analysis.



#Next, upon analysis it was discovered that any given station could have multiple latitude and longitude coordinates. The following tests show that we can know with confidence the most likely coordinates for given station names.
lat_and_long_test1 <- total_tripdata_clean_5 %>% 
  filter(
    start_station_name == "Streeter Dr & Grand Ave"
  ) %>% 
  group_by(
    start_lat,
    start_lng
  ) %>% 
  select(
    start_station_name,
    start_lat,
    start_lng
  ) %>% 
  count(
    start_lat
  ) %>% 
  arrange(
    desc(n)
  )

print(lat_and_long_test1)

lat_and_long_test2 <- total_tripdata_clean_5 %>% 
  filter(
    start_station_name == "Kingsbury St & Kinzie St"
  ) %>% 
  group_by(
    start_lat,
    start_lng
  ) %>% 
  select(
    start_station_name,
    start_lat,
    start_lng
  ) %>% 
  count(
    start_lat
  ) %>% 
  arrange(
    desc(n)
  )

print(lat_and_long_test2)

lat_and_long_test3 <- total_tripdata_clean_5 %>% 
  filter(
    start_station_name == "DuSable Lake Shore Dr & Monroe St"
  ) %>% 
  group_by(
    start_lat,
    start_lng
  ) %>% 
  select(
    start_station_name,
    start_lat,
    start_lng
  ) %>% 
  count(
    start_lat
  ) %>% 
  arrange(
    desc(n)
  )

print(lat_and_long_test3)



#6 EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#Create a csv file that we will visualize in Excel, Tableau, or my presentation software

#Cleaned Dataset
write.csv(total_tripdata_clean_5, "C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-04-23 total_tripdata_clean_5.csv", row.names = FALSE)

#Raw Dataset
write.csv(total_tripdata, "C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-04-23 total_tripdata_raw.csv", row.names = FALSE)

#Rideable type check dataset
write.csv(rideable_type_vs_user_type, "C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-04-23 rideable_type_vs_user_type.csv", row.names = FALSE)

#Charging stations dataset
write.csv(charging_stations, "C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-04-23 charging_stations.csv", row.names = FALSE)



#See below changes to total_tripdata_5 dataset
#Resave total_tripdata_5 dataset with added week column and remaned ToD column.
write.csv(total_tripdata_clean_5, "C:\\Users\\rosse\\OneDrive\\Documents\\Google Data Analytics\\Case Study - Bicycle Data\\07-05-23 total_tripdata_clean_5.csv", row.names = FALSE)




#Share
#Visualization
#See Tableau

#Act

#Recommendations:
#Advertise sales for new annual members leading up to holiday weekends such as Memorial Day, Juneteenth, and Independence Day.
#Advertise the new charging stations, as these are popular with both casual riders and members.


-----------------
  
  ###Changes to total_tripdata_5 dataset
  
  View(total_tripdata_clean_5)

######Week column was added at the end
####Inserted into section 3.3 above in total_tripdata dataset. Here it is added to total_tripdata_clean_5
#Week
total_tripdata_clean_5$week <- format(as.Date(total_tripdata_clean_5$date), "%W")

#Need to place in correct order
total_tripdata_clean_5 <- total_tripdata_clean_5 %>% 
  relocate(week, .after = year)

#Rename ToD column as 'time_of_day'
total_tripdata_clean_5 <- total_tripdata_clean_5 %>% 
  rename(time_of_day = ToD)

---------------------------
  ####Trial before changing total_tripdata_clean_5 dataset.
  add_week <- total_tripdata_clean_5

# Week 
add_week$week <- format(as.Date(add_week$date), "%W")

View(add_week)

#Need to place in correct order
add_week <- add_week %>% 
  relocate(week, .after = year)

#Rename ToD column as 'time_of_day'.
add_week <- add_week %>% 
  rename(time_of_day = ToD)



