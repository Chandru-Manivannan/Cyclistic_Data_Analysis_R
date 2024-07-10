library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

csv_files <- list.files(path = "C:\\Users\\sys\\Desktop\\Track A\\Copy", pattern = "*.csv", full.names = TRUE) ## Files are extracted
dir.create("C:\\Users\\sys\\Desktop\\Track A", recursive = TRUE, showWarnings = FALSE) 
columns_to_remove <- c(1,5,6,7,8,9,10,11,12)  # Columns to be removed 
for (file in csv_files) {
  b = read_csv(file)
  b = b[ , -columns_to_remove]
  b$start_date = as.Date(b$started_at) #Starting Date  
  b$start_time = format(b$started_at,"%H:%M:%S") #Starting Time 
  b$started_at = NULL #Coulmn Strated_at is removed
  b$end_date = as.Date(b$ended_at)
  b$end_time = format(b$ended_at,"%H:%M:%S")
  b$ended_at = NULL
  #As the type of time varibale is "doube", it is changed to type "POSIXct" to find the difference between starting and ending.
  time1_posix = as.POSIXct(b$start_time, format = "%H:%M:%S")
  time2_posix = as.POSIXct(b$end_time, format = "%H:%M:%S")
  #A day(86400 secs) is added to end_time if it is lesser thanstart_time.
  time2_posix[time2_posix < time1_posix] = time2_posix[time2_posix < time1_posix] + 86400
  b$usage_time = difftime(time2_posix, time1_posix, units = "mins")
  b$start_day = weekdays(b$start_date) # Day is calculated \
  output_file = file.path("C:\\Users\\sys\\Desktop\\Track A", basename(file))
  write_csv(b, path = output_file) #Saved in the provided path
}
csv_files <- list.files(path = "C:\\Users\\sys\\Desktop\\Track A", pattern = "*.csv", full.names = TRUE) #Files are combined and stored into a single file as 
combined_data <- lapply(csv_files, read_csv) %>%bind_rows()
write_csv(combined_data, "C:\\Users\\sys\\Desktop\\Track A\\final_data.csv")
### Code Ends

## Plots
Visualization of the processed data is made to draw conclusions.
```{r}
library(tibble)
library(readr)
a = read_csv("C:\\Users\\sys\\Desktop\\Track A\\final_data.csv")
# Reading the csv file
as_tibble(a)
```
First 10 rows are displayed to show how the data looks after data processing
## Total Users by User Type
```{r}
library(dplyr)
library(ggplot2)

a %>% 
  group_by(member_casual) %>% 
  summarise(total = n(), .groups = 'drop') %>% 
  ggplot(aes(x = member_casual, y = total)) + 
  geom_segment(aes(x = member_casual, xend = member_casual, y = 0, 
  yend = total), color = "grey") +geom_point(size = 4, color = "blue") +
  labs(title = "Total Users by User Type", x = "User Type",   y = "Total   Users") + theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
```
There are more number of members using bikes.Still, there are about 2.5 million casual users using bikes. So, making them member will be profitable for the company.

### Total Users By Days
```{r}
a %>% group_by(member_casual,start_day) %>% 
  summarise(total_users = n()) %>%
  ggplot(aes(x = start_day,y= total_users, fill = member_casual)) +
  geom_col(position = "dodge") + labs(x = "Week Day", 
  y ="Rides on the  day", title ="Daily Usage") +  geom_text(aes(
  label = total_users), 
  position = position_dodge(width = 0.9) , vjust = -0.5) +
  theme(plot.title = element_text(hjust = 0.5))

```
Members are the leading users of bikes. But during weekends, casuals are also looking for bikes as much as members do.

## Users with usage_time between 45 and 600 minutes
```{r}
a %>% filter(round(usage_time) > 45 & round(usage_time) < 600) %>% 
  group_by(member_casual, start_day) %>% 
  summarise(num = n()) %>% ggplot(aes(x=start_day, y=num,
  fill = member_casual )) + geom_col(position = "dodge") +
  geom_text(aes(label = num), position = position_dodge(width = 0.9) 
  , vjust = -0.5) + labs(x = "Week Day", y ="Total Users", 
  title ="Number of users with usage_time between 45 and 600 minutes") +   theme(plot.title = element_text(hjust = 0.5))
```
It's clear that, casuals are using bikes in higher range with an usage_time between 45 and 600 minutes. New membership schemes can be implemented based on hours.

### Usage Hour
```{r}
a %>% mutate(start_time = as.POSIXct(start_time, format="%Y-%m-%d %H:%M:%S"),
hour = format(start_time, "%H")) %>% group_by(member_casual,hour) %>% 
summarise( count = n()) %>% ggplot( aes(x = hour, y = count, group = member_casual, color = member_casual)) +geom_line() +geom_point() +
labs(title = "Hourly Counts by Member Type",x = "Hour of the Day",
y = "Count of Occurrences") + theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))
```
People are showing interests to make use of bikes during day time. So, membership scheme based on hours must be a good choice.

## Monthly Users
```{r}
library(lubridate)
a %>% mutate(mon = month(start_date,label = TRUE, abbr = FALSE)) %>% group_by(member_casual, mon) %>% summarise(total = n(),.groups = 'drop') %>% ggplot(aes(x= mon, y=total,group = member_casual, color = member_casual)) + geom_point(alpha = 0.5) +labs(title = "Monthly Users",x = "Month",y = "Users") +theme_minimal() +
theme(plot.title = element_text(hjust = 0.5))
```
The numbers highly vary among months. So, monthly memberships will play a vital role in attracting casual riders.

## Seasonal Users
```{r}
get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  }
}

a %>% 
  mutate(season = sapply(start_date, get_season)) %>% 
  group_by(member_casual, season) %>% 
  summarise(total = n(), .groups = 'drop') %>% 
  ggplot(aes(x = season, y = total, group = member_casual, 
  color = member_casual)) + geom_point(alpha = 0.5) +
  labs(title = "Seasonal Users", x = "Season", y = "Users") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
```
There is a drop in the usage of bikes during Winter season. So annual membership may not be an interest for the casuals. Providing them seasonal pass will be an added advantage for the company

## Conclusions

1. High Volume of Casual Riders: The analysis reveals that there are 2.5 million casual riders. This substantial user base represents a significant opportunity for conversion to membership plans.

2. Peak Usage on Weekends: Data indicates that weekends experience the highest usage rates. This trend suggests the potential for introducing a weekend-specific membership plan to cater to this demand.
Targeted Annual Membership Plans:

3. Based on hourly usage patterns, two distinct annual membership options can be introduced:
  * Option 1: For users with ride durations less than one hour.
  * Option 2: For users with ride durations up to five hours.

4. Flexible Membership Options: To attract more casual riders, the introduction of monthly and seasonal memberships is recommended. 

These flexible plans can serve as an entry point for casual users to transition to more committed membership plans.

