library(tidyverse)
library(lubridate)
library(ggpubr)

#import all data
jul_2022 <- read_csv('202207-divvy-tripdata.csv')
jun_2022 <- read_csv('202206-divvy-tripdata.csv')
may_2022 <- read_csv('202205-divvy-tripdata.csv')
apr_2022 <- read_csv('202204-divvy-tripdata.csv')
mar_2022 <- read_csv('202203-divvy-tripdata.csv')
feb_2022 <- read_csv('202202-divvy-tripdata.csv')
jan_2022 <- read_csv('202201-divvy-tripdata.csv')
dec_2021 <- read_csv('202112-divvy-tripdata.csv')
nov_2021 <- read_csv('202111-divvy-tripdata.csv')
oct_2021 <- read_csv('202110-divvy-tripdata.csv')
sep_2021 <- read_csv('202109-divvy-tripdata.csv')
aug_2021 <- read_csv('202108-divvy-tripdata.csv')

listed <- list(aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022)

#check if all column names are the identical, in case need to merge data
lapply(listed, FUN = colnames) %>% sapply(FUN = identical, colnames(jul_2022)) %>% all()

#check if all columns from all the tables have the same variable types
sapply(lapply(listed, function(x) lapply(x, typeof)), FUN = identical, lapply(jul_2022, typeof)) %>% all()
listed[[length(listed)+1]] = jul_2022

#listed = lapply(listed, function(x){ select(x, -c(5:12))})
#listed = lapply(listed, function(x){ mutate(x, duration = difftime(ended_at, started_at, unit = "secs"))})

#after knowing the min/max duration calculated, a cleaning will be done for negative duration and those too high
for(i in 1:length(listed)){
  listed[[i]] = select(listed[[i]], -c(1,5:12))
  listed[[i]] = mutate(listed[[i]], duration = abs(difftime(ended_at, started_at, units = "mins")))
  listed[[i]] = listed[[i]][listed[[i]]$duration < 600 & listed[[i]]$duration > 3, ]
  listed[[i]] = distinct(listed[[i]]) #delete duplicate rows
}

all = bind_rows(listed)

#find the total number of rides for every bike type
bike_type_rides = all %>%
  group_by(bike_type = rideable_type, member_casual) %>%
  summarise(total_rides = n())

#find average duration of rides grouped by months and customer type
monthly_mean = all %>% 
  group_by(month = floor_date(started_at, 'month'), member_casual) %>% 
  summarise(average_duration = mean(duration), var_dur = var(duration), min_dur = min(duration),max_dur = max(duration),  total_rides = n())

#find average duration of rides grouped by day of the week
daily_mean = all %>% 
  group_by(day = wday(started_at, label=TRUE), member_casual) %>% 
  summarise(average_duration = mean(duration), var_dur = var(duration), min_dur = min(duration) ,max_dur = max(duration), total_rides = n())


options(scipen=5)

bike_type_plot = ggplot(bike_type_rides, aes(x = bike_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = 'identity') +
  labs(x = "Bicycle type", y = "Number of rides") + 
  scale_fill_discrete(name = "Customer Type", labels = c("casual" = "Casual","member"="Member"))
  

monthly_dur_plot = ggplot(monthly_mean) + 
  geom_line(aes(x = month, y = average_duration, 
                color = member_casual, linetype = member_casual), size = 1) +
  scale_colour_manual(values = c("casual"='orange', "member" ='blue')) + 
  scale_linetype_manual(values = c("casual"="longdash", "member" = "solid")) + 
  theme(legend.position = "none") + labs(x="", y = "Average Ride Duration(minutes)") + 
  expand_limits(y = 0)

monthly_count_plot = ggplot(monthly_mean, aes(x = month, y = total_rides)) + 
  geom_line(aes(color = member_casual, linetype = member_casual), size = 1) +
  scale_colour_manual(values = c("casual"='orange', "member" ='blue'), 
                      name = "Customer Type", 
                      labels = c("casual" = 'Casual', "member" = 'Member')) + 
  scale_linetype_manual(values = c("casual"="longdash", "member" = "solid"), 
                        name = "Customer Type", 
                        labels = c("casual" = 'Casual', "member" = 'Member')) + 
  theme(legend.position = "top") +  
  labs(x="", y = "Number of rides")

ggarrange(monthly_dur_plot, monthly_count_plot, 
          labels = c("",""),
          ncol = 1, nrow = 2) %>%
  annotate_figure(top = text_grob("Average duration per ride and total number of rides\nbetween casual customers and annual member",
                                  face = "bold", size = 14))



#plot for average duration and total rides based on day of the week
daily_dur_plot = ggplot(daily_mean, aes(x=day, y = average_duration)) + 
  geom_bar(aes(fill = member_casual), position = "dodge", stat = 'identity') + 
  theme(legend.position = "none") + labs(x="", y = "Average Ride Duration(minutes)") + 
  scale_fill_manual(values=c("casual"='orange', "member" ='blue'))

daily_count_plot = ggplot(daily_mean, aes(x=day, y = total_rides)) + 
  geom_bar(aes(fill = member_casual), position = "dodge", stat = 'identity') + 
  theme(legend.position = "top") + labs(x="Day of the week", y = "Number of rides") + 
  scale_fill_manual(values=c("casual"='orange', "member" ='blue'), name = "Customer Type", labels = c("casual" = "Casual", "member" = "Member"))

ggarrange(daily_dur_plot, daily_count_plot, 
          labels = c("",""),
          ncol = 1, nrow = 2) %>%
  annotate_figure(top = text_grob("Average duration per ride and total number of rides\nbetween casual customers and annual member",
                                  face = "bold", size = 14))
