# Google Data Analytics Course - Capstone Projects

Collection of scripts and charts from doing Google Data Analytics course capstone projects.

## Case Study 1
### Synopsis

Cyclystic, a bike-share company in Chicago gave me, a junior data analyst, a task to find advantageous marketing strategies to attract casual riders to subscribe to the annual membership. Data acquired from all the available bicycles, which are all geotracked, is provided so that the trend between the casual riders and members can be identified. Only the past year's data would be used for the analysis.

### Data processing

The data cleaning and aggregation is done under two different tools in order to practice each one.

**The first is through R:**
```r
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
  summarise(average_duration = mean(duration), max_dur = max(duration), min_dur = min(duration), total_rides = n())

#find average duration of rides grouped by day of the week
daily_mean = all %>% 
  group_by(day = wday(started_at, label=TRUE), member_casual) %>% 
  summarise(average_duration = mean(duration), max_dur = max(duration), min_dur = min(duration) , total_rides = n())
```

**Second is through SQL, with the data imported to my local MariaDB:**
```sql
/*merge data while calculating duration of each ride*/
CREATE OR REPLACE TEMPORARY TABLE tempo
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_08
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_09
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_10
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_11
     UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2021_12
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_01
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_02
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_03
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_04
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_05
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_06
    UNION
    SELECT rideable_type, started_at, ended_at, member_casual, ABS(ROUND(TIMESTAMPDIFF(SECOND, started_at, ended_at)/60,2)) AS duration
    FROM 2022_07;

/*delete rows that has null values of duration or customer type, also remove inconsistencies in duration values*/
DELETE FROM tempo
WHERE duration < 5 OR duration >600 OR CONCAT(duration, member_casual) IS NULL;

/*aggregate the data to useful ones only*/
SELECT DATE_FORMAT(started_at, '%Y-%m-01') AS mon, member_casual, ROUND(AVG(duration),2) AS average_duration, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY MONTH(started_at), member_casual;

SELECT DATE_FORMAT(started_at, '%W') AS weekday, member_casual, ROUND(AVG(duration),2) AS average_duration, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY WEEKDAY(started_at), member_casual;
```
**The data is then exported to CSV files for visualisation.**

### Data Visualisation

Data visualisation is done through 3 different tools.

**First is throught he same R session using the ggplot2 library, producing these graphs:**

<p align="left"><img width="800"src="/case-study-1/month_plot.jpeg?raw=true"></p>
<p align="left"><img width="800"src="/case-study-1/weekday_plot.jpeg?raw=true"></p>

**While the next one is using Tableau:**

<p align="left"><img width="800"src="/case-study-1/tableau-charts.png?raw=true"></p>

**The final one is using Excel, with the graphs inside the file.**

### Conclusion

All methods resulted in the same conclusions, which are:

* Casual riders  decide to use the bicycles for entertainment, since the average duration of each ride is longer, while members tend to do it to get somewhere specific regularly, such as work or school. This is further supported though the weekday graphs since they show that on weekends the number of rides from the casual riders outnumber the subscribers.
* The drop in frequency can be due to cold weather, since it happens around winter, while the drop for members is less since it might be necessary for them. The drop in average duration for non-members also support this, while it roughly stays constant for members.

## Case Study 2
### Synopsis
