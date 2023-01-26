### Divvy_Exercise_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("D:/non-backupfolders/School/data/casestudy1/rawdata/divvy-tripdata") #sets your working directory to simplify calls to data ... make sure to use your OWN username instead of mine ;)

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
m12_2021 <- read_csv("202112-divvy-tripdata.csv")
m01_2022 <- read_csv("202201-divvy-tripdata.csv")
m02_2022 <- read_csv("202202-divvy-tripdata.csv")
m03_2022 <- read_csv("202203-divvy-tripdata.csv")
m04_2022 <- read_csv("202204-divvy-tripdata.csv")
m05_2022 <- read_csv("202205-divvy-tripdata.csv")
m06_2022 <- read_csv("202206-divvy-tripdata.csv")
m07_2022 <- read_csv("202207-divvy-tripdata.csv")
m08_2022 <- read_csv("202208-divvy-tripdata.csv")
m09_2022 <- read_csv("202209-divvy-tripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(m12_2021)
colnames(m01_2022)
colnames(m02_2022)
colnames(m03_2022)
colnames(m04_2022)
colnames(m05_2022)
colnames(m06_2022)
colnames(m07_2022)
colnames(m08_2022)
colnames(m09_2022)
colnames(m10_2022)
colnames(m11_2022)
# The columns match  so no change is required. Move forward
# Inspect the dataframes and look for incongruencies
str(m12_2021)
str(m01_2022)
str(m02_2022)
str(m03_2022)
str(m04_2022)
str(m05_2022)
str(m06_2022)
str(m07_2022)
str(m08_2022)
str(m09_2022)
str(m10_2022)
str(m11_2022)

#The data types also match so no change is required. Move forward
# Stack individual month's data frames into one big data frame
all_trips <- bind_rows(m12_2021, m01_2022, m02_2022, m03_2022, m04_2022, m05_2022, m06_2022, m07_2022, m08_2022, m09_2022, m10_2022, m11_2022)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
glimpse(all_trips)
#Summary has all the data I need, glimpse() is better in some situations. 
# only other one necessary here is head() to get a quick peek. Could turn it into a tibble

#check if the  categorization of membership is good
unique(all_trips$member_casual)

#calculate ride length and add it to the dataframe
all_trips$ride_length <- (all_trips$ended_at - all_trips$started_at)
#convert to hh:mm:ss format, using lubridate
all_trips$ride_length <- hms::as_hms(all_trips$ride_length)
#check for negative ride_length
length(all_trips$ride_length[all_trips$ride_length<0])
#remove negative ride_length rows
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
#v2 has exactly 100 rows less than v1 which is expected

#calculate day of the week using lubridate and add it to the dataframe 
all_trips_v2$day_of_week <- wday(all_trips_v2$started_at, label = TRUE, abbr = FALSE)

#repeat for date day month year
all_trips_v2$date <- date(all_trips_v2$started_at)
all_trips_v2$day <- day(all_trips_v2$started_at)
all_trips_v2$month <- month(all_trips_v2$started_at)
all_trips_v2$year <- year(all_trips_v2$started_at)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(as.numeric(all_trips_v2$ride_length))

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- setNames(aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$date, FUN = mean),
                    c("member_casual","date","ride_length"))
write.csv(counts, file = 'D:/non-backupfolders/School/data/casestudy1/avg_ride_length.csv')

#You're done! Congratulations!

# ADD LOCATION DATA TO VISUALIZATION CSV ABOVE #######
