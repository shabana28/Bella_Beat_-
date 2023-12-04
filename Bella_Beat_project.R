#uploading file

daily_activity <- read.csv("C:/Users/Hp/Downloads/Shabana_R_Project/dailyActivity_merged.csv")
sleep_day <- read.csv("C:/Users/Hp/Downloads/Shabana_R_Project/sleepDay_merged.csv")

#installing and loading packages 
install.packages("tidyverse")
library(tidyverse)

#cleaning data(checking for missing values)
sum(is.na(daily_activity))
sum(is.na(sleep_day))

#Exploring tables
head(daily_activity, 4)
head(sleep_day, 4)

#Identifying all columns
colnames(daily_activity)
colnames(sleep_day)

#Understanding some summary statistics
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

#data transformation
daily_activity_new <- daily_activity %>%
  mutate(total_active_minutes=VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes) %>%
  select(Id,ActivityDate,TotalSteps,TotalDistance,total_active_minutes,SedentaryMinutes,Calories)
glimpse(daily_activity_new)

#changing time format from minutes to hours in daily activity and sleep day
daily_activity_cleaned <- daily_activity_new %>%
  mutate(total_active_hours=total_active_minutes/60,SedentaryHours=SedentaryMinutes/60) %>%
  select(Id,ActivityDate,TotalSteps,TotalDistance,total_active_hours,SedentaryHours,Calories)
head(daily_activity_cleaned)


Sleep_Day_Cleaned <- sleep_day %>% 
  mutate(TotalHoursAsleep=TotalMinutesAsleep/60,TotalTimeInBed=TotalTimeInBed/60) %>% 
  select(Id,SleepDay,TotalSleepRecords,TotalHoursAsleep,TotalTimeInBed)
head(Sleep_Day_Cleaned)

#Data summary
daily_activity_cleaned %>%
  select(TotalSteps,TotalDistance,total_active_hours,SedentaryHours,Calories) %>%
  summary()

Sleep_Day_Cleaned %>%
  select(TotalHoursAsleep,TotalTimeInBed) %>%
  summary()

#data aggregation(to evaluate relationship and trends)
#arranging our data in ascending and descending order in order to get trends
#IN DESCENDING ORDER
daily_activity_cleaned %>%
  select(Id,TotalSteps,TotalDistance,total_active_hours,SedentaryHours,Calories) %>% 
  arrange(-Calories) %>% group_by(Id) %>% 
  tibble()

#IN ASCENDING ORDER
daily_activity_cleaned %>%
  select(Id,TotalSteps,TotalDistance,total_active_hours,SedentaryHours,Calories) %>% 
  arrange(Calories) %>% group_by(Id) %>% 
  tibble()


#merging data
combined_data<-merge(Sleep_Day_Cleaned, daily_activity_cleaned, by='Id')

#HIGHEST ACTIVITY DAYS
combined_data %>% 
  select(Id,SedentaryHours,total_active_hours,TotalHoursAsleep) %>% group_by(Id) %>%
  arrange(-total_active_hours) %>% head()

#LOWEST ACTIVITY DAYS
combined_data %>% 
  select(Id,SedentaryHours,total_active_hours,TotalHoursAsleep) %>% group_by(Id) %>% 
  arrange(total_active_hours) %>% head()

#visualization

#correlation matrix for daily activity and calories burned

install.packages("ggcorrplot")
library(ggcorrplot)
corr_matrix <- matrix <- daily_activity_cleaned %>%
  select(TotalSteps,TotalDistance, total_active_hours,SedentaryHours,Calories) %>% 
  cor()
ggcorrplot(matrix, type = "lower", method = "circle" )+labs(title = "Daily activity correlation", caption = "Figure 01 : Correlation matrix of the daily activity and calories burned")


#relationship between daily steps and calories burned

install.packages("ggplot2")
library(ggplot2)

ggplot(daily_activity_cleaned, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Total steps vs Calories burned", caption = "Figure 02: The relationship between total steps and calories burned")

#relationshipbetween inactivity and sleep time

cor(combined_data$SedentaryHours,combined_data$TotalTimeInBed)






