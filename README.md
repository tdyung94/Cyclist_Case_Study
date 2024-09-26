---
title: "Google Data Analytics Capstone Project: Cyclist"
output: html_notebook
---

## Introduction
Welcome to the Cyclistic bike-share analysis case study! In this case study, you work for a
fictional company, Cyclistic, along with some key team members. In order to answer the
business questions, follow the steps of the data analysis process: Ask, Prepare, Process,
Analyze, Share, and Act. Along the way, the Case Study Roadmap tables — including guiding
questions and key tasks — will help you stay on the right path.

## Scenario
You are a junior data analyst working on the marketing analyst team at Cyclistic, a bike-share
company in Chicago. The director of marketing believes the company’s future success
depends on maximizing the number of annual memberships. Therefore, your team wants to
understand how casual riders and annual members use Cyclistic bikes differently. From these
insights, your team will design a new marketing strategy to convert casual riders into annual
members. But first, Cyclistic executives must approve your recommendations, so they must be
backed up with compelling data insights and professional data visualizations.

## About the company
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown
to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations
across Chicago. The bikes can be unlocked from one station and returned to any other station
in the system anytime.

Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to
broad consumer segments. One approach that helped make these things possible was the
flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships.
Customers who purchase single-ride or full-day passes are referred to as casual riders.
Customers who purchase annual memberships are Cyclistic members.

Cyclistic’s finance analysts have concluded that annual members are much more profitable
than casual riders. Although the pricing flexibility helps Cyclistic attract more customers,
Moreno believes that maximizing the number of annual members will be key to future growth.
Rather than creating a marketing campaign that targets all-new customers, Moreno believes
there is a solid opportunity to convert casual riders into members. She notes that casual riders
are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into
annual members. In order to do that, however, the team needs to better understand how
annual members and casual riders differ, why casual riders would buy a membership, and how
digital media could affect their marketing tactics. Moreno and her team are interested in
analyzing the Cyclistic historical bike trip data to identify trends.

I will be using the 6 phases of the data analysis process (Ask, Prepare, Process, Analyze, Share, and Act) to help guide my marketing strategy.

## Phase 1: Ask

### Identitfy the business task
Understand how casual riders and annual members use Cyclistic bikes differently in order to prepare a new marketing strategy to covert casual riders to annual members.

### Key stakeholders
  * Cyclistic: A bike-share program that features more than 5,800 bicycles and 600
docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand
tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities
and riders who can’t use a standard two-wheeled bike. The majority of riders opt for
traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more
likely to ride for leisure, but about 30% use the bikes to commute to work each day.
  * Lily Moreno: The director of marketing and your manager. Moreno is responsible for
the development of campaigns and initiatives to promote the bike-share program.
These may include email, social media, and other channels.
  * Cyclistic marketing analytics team: A team of data analysts who are responsible for
collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy.
You joined this team six months ago and have been busy learning about Cyclistic’s
mission and business goals—as well as how you, as a junior data analyst, can help
Cyclistic achieve them.
  * Cyclistic executive team: The notoriously detail-oriented executive team will decide
whether to approve the recommended marketing program.

### Key questions to guide the future marketing program
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

## Phase 2: Prepare

### Data Collection
This data has been made available by Motivate International Inc under the [license](https://divvybikes.com/data-license-agreement)

I downloaded 4 ZIP files containing the csv files for the first 6 months of 2020. Each month had a seperate csv file except for January, February, and March which were all included on the same file.

The data is first hand data as it was collected by Cyclistic itself so it is original and reliable. Personal information of individuals have been removed and is protected.

The data contains information about each bike ride such as the ride id, rideable type, start and end times, start and end stations, and member status. The data appears to be comprehensive for the most part though it does have some limitations as it contains some NULL values which were removed in the analysis.

## Phase 3: Process

This phase was used to clean the data and make any necessary modifications.

Rstudio was used for the data processing phase of this project

### Setting up my R environment
```{r}
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("readr")
install.packages("skimr")
install.packages("ggplot2")
install.packages("dplyr")
```

```{r}
library(tidyverse)
library(janitor)
library(lubridate)
library(readr)
library(skimr)
library(ggplot2)
library(dplyr)
```

### Uploading the datasets
```{r}
Jan_Feb_Mar_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")
Apr_2020 <- read_csv("202004-divvy-tripdata.csv")
May_2020 <- read_csv("202005-divvy-tripdata.csv")
Jun_2020 <- read_csv("202006-divvy-tripdata.csv")
```

Merging each individual data frame into one
```{r}
combined_trips <- bind_rows(Jan_Feb_Mar_2020,Apr_2020,May_2020,Jun_2020)
```


Check to make sure the data types match
```{r}
compare_df_cols(Jan_Feb_Mar_2020,Apr_2020,May_2020,Jun_2020)
```

### Create new columns 
I need to extract the day, month, and year from started_at
```{r}
combined_trips$date <- as.Date(combined_trips$started_at)
combined_trips$month <- format(as.Date(combined_trips$date), "%m")
combined_trips$day <- format(as.Date(combined_trips$date), "%d")
combined_trips$year <- format(as.Date(combined_trips$date), "%Y")
combined_trips$day_of_week <- format(as.Date(combined_trips$date), "%A")
```

I created another variable to determine the trip duration of each trip and covert to minutes
```{r}
combined_trips$ride_length <- difftime(combined_trips$ended_at, combined_trips$started_at)
combined_trips$ride_length <- combined_trips$ride_length/60
combined_trips$ride_length <- round(combined_trips$ride_length, 2)
```

Now I need to convert the ride_length column from factor to numeric
```{r}
combined_trips$ride_length <- as.numeric(as.character(combined_trips$ride_length))
```

### Remove bad data
I need to remove the entries that have a negative ride_length and those that were taken out of docks and checked for quality by Divvy
```{r}
combined_trips_v2 <- combined_trips[!(combined_trips$start_station_name == "HQ QR" | combined_trips$ride_length<0),]
```

I am checking to ensure the data is in the correct format and all variables are included 
```{r}
head(combined_trips_v2)
```

## Phase 4: Analyze

Descriptive analysis on ride_length (all figures are in minutes)
```{r}
summary(combined_trips_v2$ride_length)
```

### Members vs casual riders

Compare the mean, median, max, min, and number of rides of members vs causal riders
```{r}
combined_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(number_of_rides = n(), avg_ride_length = mean(ride_length), median_ride_length = median(ride_length), max_ride_length = max(ride_length), min_ride_length = min(ride_length))
```

Compare the average ride time by each day for member vs casual riders. First I need to get the days of the week and months listed in the correct order
```{r}
combined_trips_v2$day_of_week <- ordered(combined_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

combined_trips_v2$month <- ordered(combined_trips_v2$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",
                                                       "December"))
```

Now I can determine the mean by day
```{r}
aggregate(combined_trips_v2$ride_length ~ combined_trips_v2$member_casual + combined_trips_v2$day_of_week, FUN = mean)
```

All rides of day by week and member type
```{r}
combined_trips_by_day <- combined_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(num_of_rides = n(), avg_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)
```

Created visual to represent the total number of rides by day
```{r}
combined_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title="Total number of Rides by Day")
```

Created visual to represent total number of rides by month
```{r}
combined_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + labs(title="Total number of Rides by Month")
```

Created visual to represent average trip duration by month
```{r}
combined_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title="Average Trip Duration by Month")
```

Created visual to represent the average duration by day
```{r}
combined_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) + geom_col(position = "dodge") + labs(title="Average Trip Duration by Day")
```

Next I want to find out the percentage of casual riders that ride on weekends vs weekdays
```{r}
total_rides_casual_weekend <- NROW(filter(combined_trips_v2, member_casual == "casual" & (day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_casual_weekday <- NROW(filter(combined_trips_v2, member_casual == "casual" & !(day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_casual <- total_rides_casual_weekday + total_rides_casual_weekend
```

Created visual to represent percentage number of rides for casual riders on weekdays vs weekends
```{r}
slices <- c(169943, 139776)
lbls <- c("Weekday", "Weekend")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls<- paste(lbls, "%", sep = "")
pie(slices, labels = lbls, main = "Casual Riders: Weekday vs Weekend")
```

Now I am going to do the same with members. Visual was created in the next step as well
```{r}
total_rides_member_weekend <- NROW(filter(combined_trips_v2, member_casual == "member" & (day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_member_weekday <- NROW(filter(combined_trips_v2, member_casual == "member" & !(day_of_week == "Saturday" | day_of_week == "Sunday")))
total_rides_member <- total_rides_member_weekday + total_rides_member_weekend
```

```{r}
slices1 <- c(552577, 188187)
lbls1 <- c("Weekday", "Weekend")
pct1 <- round(slices1/sum(slices1)*100)
lbls1 <- paste(lbls1,pct1)
lbls1<- paste(lbls1, "%", sep = "")
pie(slices1, labels = lbls1, main = "Members: Weekday vs Weekend")
```

## Phase 5: Share

*   Casual riders avg ride time is roughly an hour while members avg is 16 minutes. Members seem to ride longer on the weekends while casual drivers ride longer on Thursday, Friday and Sunday

*   Trips tend to get shorter in the hotter months for casual riders but the opposite is true for members

*   Casual riders use the service more on weekends while members use it the most in the middle of the week

*   Casual riders appear to ride more frequently during the summer months

*   Almost half of the total number of riders for casual riders occurs on the weekends (45%). For members       only 25% of total rides occur on the weekends


## Phase 6: Act

1. Cyclistic currently offers single ride and full day passes but there is also an opportunity to provide monthly or seasonal passes. The number of rides for casual riders increases during the warmer months specifically in May and June so the marketing team could tailor a promotion during the peak summer months to increase memberships during that time

2. Another opportunity could be offering weekend passes. With almost half of the total number of rides by casual riders occur on the weekends it is apparent that casual riders use Cyclist much more frequently on the weekends

3. Creating a loyalty or bonus system could also encourage more casual riders to purchase memberships. The average ride duration of ~60 minutes for casual rider is about 4 times longer than that of a member. Having a loyalty or bonus system that offers discounts for riding a certain amount of distance could encourage more casual users to become members to reap in the benefits
