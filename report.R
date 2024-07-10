# Required Libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tibble)

# Data Cleaning and Data Pre-processing
csv_files = list.files(path = "C:\\Users\\sys\\Desktop\\Track A\\Copy", pattern = "*.csv", full.names = TRUE) # Files are extracted
dir.create("C:\\Users\\sys\\Desktop\\Track A", recursive = TRUE, showWarnings = FALSE) 
columns_to_remove = c(1,5,6,7,8,9,10,11,12)  # Columns to be removed 
for (file in csv_files) {
  b = read_csv(file) # reads the file 
  b = b[ , -columns_to_remove] # Columns are removed
  b$start_date = as.Date(b$started_at) # Extracts Start Date  
  b$start_time = format(b$started_at,"%H:%M:%S") # Extracts Start Time 
  b$started_at = NULL  # Coulmn Strated_at is removed
  b$end_date = as.Date(b$ended_at) # Extracts End Date  
  b$end_time = format(b$ended_at,"%H:%M:%S") # Extracts End Time
  b$ended_at = NULL # Coulmn Ended_at is removed
  # As the type of time varibale is "doube", it is changed to type "POSIXct" to find the difference between starting and ending.
  time1_posix = as.POSIXct(b$start_time, format = "%H:%M:%S")
  time2_posix = as.POSIXct(b$end_time, format = "%H:%M:%S")
  #A day(86400 secs) is added to end_time if it is lesser than start_time.
  time2_posix[time2_posix < time1_posix] = time2_posix[time2_posix < time1_posix] + 86400
  b$usage_time = difftime(time2_posix, time1_posix, units = "mins") # Usage time single user in minutes
  b$start_day = weekdays(b$start_date) # Day is calculated 
  output_file = file.path("C:\\Users\\sys\\Desktop\\Track A", basename(file))
  write_csv(b, path = output_file) # Saved in the provided path
}
csv_files <- list.files(path = "C:\\Users\\sys\\Desktop\\Track A", pattern = "*.csv", full.names = TRUE) # Pre-processed files 
combined_data <- lapply(csv_files, read_csv) %>%bind_rows() # reads each file and appending new file at the end of old file
write_csv(combined_data, "C:\\Users\\sys\\Desktop\\Track A\\final_data.csv") # comibined file is made and saved as final.csv


# Plots

a = read_csv("C:\\Users\\sys\\Desktop\\Track A\\final_data.csv") # reads the combined csv files 
as_tibble(a) # First 10 rows are displayed to show how the data processed

# Total number users as members and casuals
a %>% group_by(member_casual) %>% 
  summarise(total = n(), .groups = 'drop') %>% 
  ggplot(aes(x = member_casual, y = total)) + 
  geom_segment(aes(x = member_casual, xend = member_casual, y = 0, yend = total), color = "grey") +
  geom_point(size = 4, color = "blue") +
  labs(title = "Total Users by User Type", x = "User Type",   y = "Total   Users") + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Day based users
a %>% group_by(member_casual,start_day) %>% 
  summarise(total_users = n()) %>%
  ggplot(aes(x = start_day,y= total_users, fill = member_casual)) +
  geom_col(position = "dodge") + labs(x = "Week Day", y ="Rides on the  day", title ="Daily Usage") +  
  geom_text(aes(label = total_users), position = position_dodge(width = 0.9) , vjust = -0.5) +
  theme(plot.title = element_text(hjust = 0.5))

# Users with usage_time between 45 and 600 minutes
a %>% filter(round(usage_time) > 45 & round(usage_time) < 600) %>% 
  group_by(member_casual, start_day) %>% 
  summarise(num = n()) %>% ggplot(aes(x=start_day, y=num,fill = member_casual )) + 
  geom_col(position = "dodge") +
  geom_text(aes(label = num), position = position_dodge(width = 0.9) , vjust = -0.5) + 
  labs(x = "Week Day", y ="Total Users", title ="Number of users with usage_time between 45 and 600 minutes") +   
  theme(plot.title = element_text(hjust = 0.5))

# At which hour bike is needed:
a %>% mutate(start_time = as.POSIXct(start_time, format="%Y-%m-%d %H:%M:%S"),hour = format(start_time, "%H")) %>% 
  group_by(member_casual,hour) %>% 
  summarise( count = n()) %>% 
  ggplot( aes(x = hour, y = count, group = member_casual, color = member_casual)) +
  geom_line() + geom_point() +
  labs(title = "Hourly Counts by Member Type",x = "Hour of the Day",y = "Count of Occurrences") + 
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Monthly Users
library(lubridate)
a %>% mutate(mon = month(start_date,label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, mon) %>% summarise(total = n(),.groups = 'drop') %>% 
  ggplot(aes(x= mon, y=total,group = member_casual, color = member_casual)) + 
  geom_point(alpha = 0.5) + labs(title = "Monthly Users",x = "Month",y = "Users") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Seasonal Users
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

a %>% mutate(season = sapply(start_date, get_season)) %>% 
  group_by(member_casual, season) %>% 
  summarise(total = n(), .groups = 'drop') %>% 
  ggplot(aes(x = season, y = total, group = member_casual, 
  color = member_casual)) + geom_point(alpha = 0.5) +
  labs(title = "Seasonal Users", x = "Season", y = "Users") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
