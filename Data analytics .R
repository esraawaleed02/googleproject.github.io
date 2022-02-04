#Import needed packages
library(readr)
library(tidyverse)
library(skimr)
library(janitor)
library(RColorBrewer)

#Importing datasets to R
Daily_Activity <- read.csv("dailyActivity_merged.csv")

Daily_Calories <- read_.sv("dailyCalories_merged.csv")

Daily_Intensities <- read.csv("dailyIntensities_merged.csv")

Daily_Steps <- read.csv("dailySteps_merged.csv")

Hourly_Calories <- read.csv("hourlyCalories_merged.csv")

Hourly_Intensities <- read.csv("hourlyIntensities_merged.csv")

Hourly_Steps <- read.csv("hourlySteps_merged.csv")

Sleep_Day <- read.csv("sleepDay_merged.csv")

Heart_Rate <- read.csv("heartrate_seconds_merged.csv") 

Weight <- read.csv("weightLogInfo_merged.csv")



#Summary for the following datasets
glimpse(Daily_Activity) 
glimpse(Hourly_Calories) 
glimpse(Sleep_Day) 

str(Daily_Activity)
str(Hourly_Calories)
str(Sleep_Day)

#Process phase, count the number of IDs repeated in the datasets
Activity_ID_Count <- Daily_Activity %>% 
  group_by(Id) %>% 
  summarize(count = n())
Activity_ID_Count 

Calories_ID_Count <- Hourly_Calories %>% 
  group_by(Id) %>% 
  summarize(count = n())
Calories_ID_Count

Sleep_ID_Count <- Sleep_Day %>% 
  group_by(Id) %>% 
  summarize(count = n())
Sleep_ID_Count

HeartRate_ID_Count <- Heart_Rate %>% 
  group_by(Id) %>% 
  summarize(count = n())
HeartRate_ID_Count #Only 14 records are available

Weight_ID_Count <- Weight %>% 
  group_by(Id) %>% 
  summarize(count = n())
Weight_ID_Count #Only 8 participants had their weights recorded

#Split the date column into two "date & time" in the following datasets
Hourly_Intense <- Hourly_Intensities %>% 
  extract(ActivityHour, c("Date", "Hour"), "([^ ]+) (.*)")

Hourly_Calorie <- Hourly_Calories %>% 
  extract(ActivityHour, c("Date", "Hour"), "([^ ]+) (.*)")

Hourly_Step <- Hourly_Steps %>% 
  extract(ActivityHour, c("Date", "Hour"), "([^ ]+) (.*)")

#Analyze phase, conversion of minutes asleep into hours
Hours_sleep <- Sleep_Day %>% 
  mutate(Total_Hours_Asleep = TotalMinutesAsleep/60,
         Total_Hours_in_Bed = TotalTimeInBed/60) %>% 
  extract(SleepDay, c("Date", "Hour"), "([^ ]+) (.*)") %>% 
  select(-TotalSleepRecords, -TotalMinutesAsleep, -TotalTimeInBed, -Hour)

#Mean values of the following datasets
AVG_Daily_Calories<- Daily_Calories %>% 
  group_by(Id)  %>%
  summarize(AverageConsumedCaloriesDaily = mean(Calories)) 

AVG_Daily_Steps <- Daily_Steps %>% 
  group_by(Id) %>% 
  summarize(AverageTakenStepsDaily = mean(StepTotal))

AVG_Hourly_Calories <- Hourly_Calorie %>% 
  group_by(Id, Hour) %>% 
  summarize(AverageConsumedCaloriesHourly = mean(Calories))

AVG_Hourly_Steps <- Hourly_Step %>% 
  group_by(Id, Hour) %>% 
  summarize(AeravgeStepsHourly = mean(StepTotal))

AVG_Hourly_Intense <- Hourly_Intense %>% 
  group_by(Id, Hour) %>% 
  summarize(AverageIntenseHourly = mean(AverageIntensity))

AVG_Hours_Asleep <- Hours_sleep %>% 
  group_by(Id) %>% 
  summarize(AverageHoursSleeping = mean(Total_Hours_Asleep))

AVG_Sleep_Dates <- Hours_sleep %>% 
  group_by(Date) %>% 
  summarize(AverageHoursSleeping = mean(Total_Hours_Asleep))

AVG_BPM <- Heart_Rate %>% 
  group_by(Id)  %>%
  summarize(BPM = mean(Value)) 

# % of how many days users were not using the product
Percentage_days_off <- Daily_Activity %>% 
  filter(SedentaryMinutes == 1440) %>% 
  group_by(Id) %>% 
  summarize(days_off = n(),
            percent_days_off = (days_off/31)*100) %>% 
  print()
ggplot(Percentage_days_off, aes(x = percent_days_off)) +
  geom_histogram(bins = 15, fill = "cyan", color = "black") +
  xlab("Days off taken by participants in %") +
  ylab("Number of participants") +
  labs(title = "Percentage of days off for participants") 

#Creating a pie chart
piechart <- Percentage_days_off %>% 
  group_by(days_off) %>% 
  summarize(num_of_participants = n())
# Plot of users activity
slices <- c(48, 24, 12, 16)
lbls <- c("Used the product daily", "Did not use for 1 day", "Did not use for 1-7 days", "Did not use for more than 7 days")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices,labels = lbls, col = blues9,
    main="Fitbit Usage")

#Create a new table to categorize participants activity in total and percentage terms
Activity_of_Participants <- Daily_Activity %>% 
  filter(SedentaryMinutes != 1440) %>% 
  group_by(Id) %>% 
  summarize(Total_Very_Active_Minutes = sum(VeryActiveMinutes),
            Total_Fairly_Active_Minutes = sum(FairlyActiveMinutes),
            Total_Lightly_Active_Minutes = sum(LightlyActiveMinutes),
            Total_Sendentary_Minutes = sum(SedentaryMinutes),
            Total_Minutes = sum(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes),
            Percentage_Very_Active = (Total_Very_Active_Minutes/Total_Minutes)*100,
            Percentage_Fairly_Active = (Total_Fairly_Active_Minutes/Total_Minutes)*100,
            Percent_Lightly_Active = (Total_Lightly_Active_Minutes/Total_Minutes)*100,
            Percent_Sendentary_Active = ( Total_Sendentary_Minutes/Total_Minutes)*100)

#Create a new table to differentiate between participants intense workout
Participants_Activity <- Activity_of_Participants %>% 
  mutate(Intensity =
           case_when(Percentage_Very_Active > mean(Percentage_Very_Active) ~ "Very Active",
                     Percentage_Fairly_Active > mean(Percentage_Fairly_Active) ~ "Fairly Active",
                     Percent_Lightly_Active > mean(Percent_Lightly_Active) ~ "Lightly Active",
                     Percent_Sendentary_Active > mean(Percent_Sendentary_Active) ~ "Sendentary Active"))

#New table to show how many participant in each category
Activity <- Participants_Activity %>% 
  group_by(Intensity) %>% 
  summarize(count = n())

#Comparing participants according to daily activity
ggplot(Activity, aes(x = Intensity, y = count)) +
  geom_histogram(stat = "identity", col = "red", fill = "lightblue") +
  ylab("Number of Participants") +
  xlab("Types of Intensity") +
  labs(title = "Number of Participants by Intensity") 

ggplot(AVG_Daily_Calories, aes(x = AverageConsumedCaloriesDaily)) +
  geom_histogram(bins = 8, fill = "magenta", color = "black") +
  ylab("Number of Participants") +
  xlab("Average Consumed Calories") +
  labs(title = "Average Calories Daily") 

ggplot(AVG_Daily_Steps, aes(x = AverageTakenStepsDaily)) +
  geom_histogram(bins = 8, fill = "purple", color = "black") +
  ylab("Number of Participants") +
  xlab("Average Taken Steps") +
  labs(title = "Average Taken Steps Daily") 

#Showing the activities in each hour per day
ggplot(AVG_Hourly_Calories, aes(x = AverageConsumedCaloriesHourly)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  ylab("Number of Participants") +
  xlab("Average Consumed Calories") +
  labs(title = "Average Calories per Hour") +
  facet_wrap(~Hour) 

ggplot(AVG_Hourly_Steps, aes(x = AeravgeStepsHourly)) +
  geom_histogram(bins = 10, fill = "red", color = "black") +
  ylab("Number of Participants") +
  xlab("Average Taken Steps") +
  labs(title = "Average Taken Steps Each Hour") +
  facet_wrap(~Hour)

ggplot(AVG_Hourly_Intense, aes(x = AverageIntenseHourly)) +
  geom_histogram(bins = 10, fill = "green", color = "black") +
  ylab("Number of Participants") +
  xlab("Average Intense Workout") +
  labs(title = "Average of Intense Workout Each Hour") +
  facet_wrap(~Hour)

Dates <- AVG_Sleep_Dates 
AVG_Sleep_Dates$Date <- as.Date(AVG_Sleep_Dates$Date, "%m/%d/%Y") 

ggplot(AVG_Sleep_Dates, aes(x = Date, y = AverageHoursSleeping )) +
  geom_point(alpha = .6) +
  geom_line(color = "black") +
  ylab("Average Hours of Sleep") +
  xlab("Date") +
  labs(title = "Average Hours of Sleep per day for participants") 

#Show number of hours participants sleep each day
AVG_Sleep_Days <- Dates %>% 
  mutate(Day =
           case_when(Date == "4/17/2016" |Date == "4/24/2016" |Date == "5/1/2016" |Date == "5/8/2016"  ~ "Sunday",
                     Date == "4/18/2016" |Date == "4/25/2016" |Date == "5/2/2016" |Date == "5/9/2016"  ~ "Monday",
                     Date == "4/19/2016" |Date == "4/26/2016" |Date == "5/3/2016" |Date == "4/12/2016"  ~ "Tuesday",
                     Date == "4/20/2016" |Date == "4/27/2016" |Date == "5/4/2016" |Date == "4/13/2016"  ~ "Wednesday",
                     Date == "4/21/2016" |Date == "4/28/2016" |Date == "5/5/2016" |Date == "4/14/2016"  ~ "Thursday",
                     Date == "4/22/2016" |Date == "4/29/2016" |Date == "5/6/2016" |Date == "4/15/2016"  ~ "Friday",
                     Date == "4/23/2016" |Date == "4/30/2016" |Date == "5/7/2016" |Date == "4/16/2016"  ~ "Saturday",)) %>% 
  select(-Date)
head(AVG_Sleep_Days)

Activity_Sleep <- Participants_Activity %>% 
  inner_join(AVG_Hours_Asleep)

#Plot every participant's sleeping hours 
ggplot(AVG_Hours_Asleep, aes(x = AverageHoursSleeping, y = as.factor(Id))) +
  geom_histogram(stat = "identity", fill = "lightpink", color = "black") +
  ylab("Participants by ID") +
  xlab("Average Sleeping Hours ") +
  labs(title = "Participants' Sleeping Hours")
