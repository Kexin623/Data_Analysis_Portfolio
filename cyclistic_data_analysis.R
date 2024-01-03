##Loading lackages and libraries
install.packages('tidyverse')
install.packages('janitor')
install.packages('ggmap')
install.packages('lubridate')
install.packages('dplyr')

library(tidyverse)
library(janitor)
library(ggmap)
library(lubridate)
library(dplyr)

## Import 12 months Cyclistic data
nov23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202311-divvy-tripdata.csv")
oct23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202310-divvy-tripdata.csv")
sep23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202309-divvy-tripdata.csv")
aug23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202308-divvy-tripdata.csv")
jul23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202307-divvy-tripdata.csv")
jun23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202306-divvy-tripdata.csv")
may23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202305-divvy-tripdata.csv")
apr23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202304-divvy-tripdata.csv")
mar23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202303-divvy-tripdata.csv")
feb23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202302-divvy-tripdata.csv")
jan23 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202301-divvy-tripdata.csv")
dec22 <- read.csv("/Users/bluesww/Desktop/Cyclistic data/csv/202212-divvy-tripdata.csv")

## Check data structure
str(nov23)
str(oct23)
str(sep23)
str(aug23)
str(jul23)
str(jun23)  
str(may23)
str(apr23)
str(mar23)
str(feb23)
str(jan23)
str(dec22)

## Merge individual monthly data frames into one data frame
cyclistic_data <- rbind(nov23,oct23,sep23,aug23,jul23,jun23,may23,apr23,mar23,feb23,jan23,dec22)

View(cyclistic_data)
str(cyclistic_data)

## Add date, month, year, day of week columns
cyclistic_data <- cyclistic_data %>%
  mutate(year = format(as.Date(started_at), "%Y")) %>%
  mutate(month = format(as.Date(started_at), "%B")) %>%
  mutate(dat = format(as.Date(started_at), "%d")) %>%
  mutate(day_of_week = format(as.Date(started_at), "%A")) %>%
  mutate(ride_length = difftime(ended_at, started_at)) %>%
  mutate(start_time = strftime(started_at, "%H"))
   
# Convert 'ride_length' to numeric for calculation on data
cyclistic_data <- cyclistic_data %>% 
  mutate(ride_length = as.numeric(ride_length))
is.numeric(cyclistic_data$ride_length)

# Remove "bad" data
# The data frame includes a few hundred entries when bikes were taken out of docks 
# and checked for quality by Divvy where ride_length was negative or 'zero'
cyclistic_data_clean <- cyclistic_data[!(cyclistic_data$ride_length <= 0),]

## Check the cleaned data frame
str(cyclistic_data_clean)

##Analyze Part
##Check summarized details about the cleaned data set 
summary(cyclistic_data_clean)

## Conduct descriptive analysis
# Descriptive analysis on 'ride_length'
# mean = straight average (total ride length / total rides)
# median = midpoint number of ride length array
# max = longest ride
# min = shortest ride
cyclistic_data_clean %>% 
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

# Members vs casual riders difference depending on total rides taken
cyclistic_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id), ride_percentage = (length(ride_id) / nrow(cyclistic_data_clean)) * 100)

ggplot(cyclistic_data_clean, aes(x = member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casuals vs Members", y="Number Of Rides", title= "Casuals vs Members distribution")
##We can see on the Casuals vs Members distribution chart, members possesing about 36.1%, and casual riders have around 63.9% of the dataset. 
##So it is clearly visible that from 2022 to 2023 members used ride share around 27.8% more than casual riders.

##Comparison between Members Causal riders depending on ride length (mean, median, minimum, maximum)
cyclistic_data_clean %>%
  group_by(member_casual) %>% 
  summarise(total_ride_length = sum(ride_length),average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))
##From the above table we can conclude that casual riders took bike for longer rides than members, average ride length of member riders is lower than the average ride length of casual riders.

##Analyze the ride duration based on days of week and member vs casual
cyclistic_data_clean$day_of_week <- ordered(cyclistic_data_clean$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

cyclistic_data_clean %>% 
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n(),
            average_ride_length = mean(ride_length),.groups="drop") %>%
  arrange(member_casual, day_of_week)

## Visualize total rides data by type and day of week
cyclistic_data_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
##Members took consistent trips throughout the week, but there is more rides in Thursday and less rides in Sunday. 
##For casual riders the most taken rides are in weekends, starting rise in Friday followed by Saturday and Sunday.

##Visualize average ride time data by type and day of week
cyclistic_data_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time by Members and Casual riders Vs. Day of the week")
##The average ride length for members are much less than that of casual riders. 
##Also it can be seen that weekend average ride length is much higher for casual riders along with total rides. 

##Analyze the ride duration by each month for members vs casual riders
cyclistic_data_clean$month <- ordered(cyclistic_data_clean$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

cyclistic_data_clean %>%
  group_by(member_casual,month) %>%
  summarise(number_of_rides = n(), average_ride_length = mean(ride_length), .groups = "drop") %>%
  arrange(member_casual, month) %>%
  print(n=24)

##Visualize total rides data by type and month
cyclistic_data_clean %>%
  group_by(member_casual, month)%>%
  summarise(number_of_rides = n(), groups='drop') %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual))+
  labs(title = "Total rides by Members and Casual riders Vs. Month", x="Month", y="Number of Rides") +
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(width=0.5, position=position_dodge(width=0.5))+
  scale_y_continuous(labels = function(x) format(x, scientific=FALSE))
##The months June, July, August and September are the most busy time of the year among both members and casual riders. It is possible due to winter there is a significant drop in total rides in the months of November, December, January and February for both type of customers.

##Visualize average ride time data by type and month
cyclistic_data_clean %>%
  group_by(member_casual, month) %>%
  summarise(average_ride_length = mean(ride_length), groups ='drop') %>%
  arrange(member_casual, month)%>%
  ggplot(aes(x=month, y=average_ride_length, fill=member_casual)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Average ride length by Members and Casual riders Vs. Month")+
  theme(axis.text.x= element_text(angle = 30) )
##Average ride length of members is about the same <1000 secs throughout the year. While casual riders average ride length is between 1000 - 2000 secs throughout the year. 
##But in the month of February average right length is higher but total rides are lowest as compared to other months.

##Analyze and visualization on cyclistic's bike demand by hour in a day
cyclistic_data_clean %>%
  ggplot(aes(start_time, fill=member_casual))+
  labs(x = "Hour of the day", title = "Cyclistic Bike demand by hour in a day")+
  geom_bar()
##From the chart, we can see for both the member and casual riders perfer between 16pm and 18pm.

##Analyze and visualization of Rideable type Vs. total rides by Members and casual riders
cyclistic_data_clean %>%
  group_by(rideable_type) %>%
  summarise(count = length(ride_id))

ggplot(cyclistic_data_clean, aes(x=rideable_type, fill = member_casual)) +
  labs(x="Rideable type", title = "Rideable type vs. total rides by Members and casual riders") +
  geom_bar()
##From the above chart, we can see that members mostly use classic bikes, followed by electric bikes.
##Docked bikes mostly used by casual riders. Electric bikes are more favored by members.

