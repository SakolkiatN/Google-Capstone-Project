# Case Study: How Does a Bike-Share Navigate Speedy Success ?

#Disclaimer : This is a guide project by Google Data Analytics Course

#==============================================
# Step 1: Prepare dataset
#==============================================

### Install requiremnet packages
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

getwd() #to check the working directory
setwd("/Users/skk/Desktop/Data Analyst/Cyclists data/csv") #to set the working directory

# Store data set in variables to join them later
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")

## Check column names for each file
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

### Rename q2 to match columns of the rest of files
q2_2019 <- rename(q2_2019
                  ,trip_id = "01 - Rental Details Rental ID" 
                  ,start_time = "01 - Rental Details Local Start Time"
                  ,end_time = "01 - Rental Details Local End Time"
                  ,bikeid = "01 - Rental Details Bike ID"
                  ,tripduration = "01 - Rental Details Duration In Seconds Uncapped"
                  ,from_station_id = "03 - Rental Start Station ID"
                  ,from_station_name = "03 - Rental Start Station Name"
                  ,to_station_id = "02 - Rental End Station ID"
                  ,to_station_name = "02 - Rental End Station Name"
                  ,usertype = "User Type"
                  ,gender = "Member Gender"
                  ,birthyear = "05 - Member Details Member Birthday Year")


### Concatenate rows into a single dataframe

all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019)

#===============================================
# Step 2: Clean up and prepare data for analysis
#===============================================

### Inspect new table

colnames(all_trips) # List of column names
nrow(all_trips) # Number of rows in dataframe
dim(all_trips) # Dimension of the dataframe
head(all_trips) # Inspect the first six rows
summary(all_trips) # Statistical summary of data

# Check the usertype
distinct_usertype <- unique(all_trips$usertype)
print(distinct_usertype)
# There are two types of user, Subscriber and Customer

# Rename Subscriber to member and Customer to casual
all_trips <- all_trips %>%
  mutate(usertype = recode(usertype
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))

# Check new table
table(all_trips$usertype)
# there are 880,637 casual users and 2,937,367 member users!

# Add columns that list a date, month, day, year of each ride
all_trips$date <- as.Date(all_trips$start_time)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# add ride_length calculations (in seconds)
all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length) # return FALSE value
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) #convert diftime num -> chr -> num
is.factor(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# Create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 3: Conduct descriptive analysis
#=====================================

# Descriptive in ride_length (seconds)
mean(all_trips_v2$ride_length) #average duration
median(all_trips_v2$ride_length) #midpoint value in the ascendin array values
max(all_trips_v2$ride_length) #longest duration
min(all_trips_v2$ride_length) #shortest duration

# Compare member and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
# casual = 57.01802 , member = 14.32780
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
# casual = 25.83333 , member = 9.80000
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
# casual = 177200.4 , member = 150943.9
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)
# casual = 1.016667 , member = 1.016667

# Compare average ride_time for each day
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean) # the days are out of order
# Arrange the days of the week in order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# the results would be
#                 casual                   Sunday                 56.18519
#                 member                   Sunday                 15.40290
#                 casual                   Monday                 54.49989
#                 member                   Monday                 14.24928
#                 casual                  Tuesday                 57.41328
#                 member                  Tuesday                 14.15259
#                 casual                Wednesday                 60.33407
#                 member                Wednesday                 13.80984
#                 casual                 Thursday                 59.95112
#                 member                 Thursday                 13.77979
#                 casual                   Friday                 60.17561
#                 member                   Friday                 13.89748
#                 casual                 Saturday                 54.06111
#                 member                 Saturday                 16.30271
    

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(usertype, weekday)								# sorts
# result
# usertype weekday       number_of_rides. average_duration
# <chr>    <ord>             <int>            <dbl>
# casual   Sun              170173             56.2
# casual   Mon              101489             54.5
# casual   Tue               88655             57.4
# casual   Wed               89745             60.3
# casual   Thu              101372             60.0
# casual   Fri              121141             60.2
# casual   Sat              208056             54.1
# member   Sun              256234             15.4
# member   Mon              458780             14.2
# member   Tue              497025             14.2
# member   Wed              494277             13.8
# member   Thu              486915             13.8
# member   Fri              456966             13.9
# member   Sat              287163             16.3

# Visualize the number of rides by usertype

all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype, color = usertype)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Visualize average duration
all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(usertype, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(usertype, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  geom_col(position = "dodge")



