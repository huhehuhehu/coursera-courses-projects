# Google Data Analytics Course - Capstone Projects

Collection of scripts and charts from doing Google Data Analytics course capstone projects.

## Case 1 - Cyclystic
### Synopsis

Cyclystic, a bike-share company in Chicago gave me, a junior data analyst, a task to find advantageous marketing strategies to attract casual riders to subscribe to the annual membership. Data acquired from all the available bicycles, which are all geotracked, is provided so that the trend between the casual riders and members can be identified. Only the past year's data would be used for the analysis.

### 1. Data processing

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
  summarise(average_duration = mean(duration), var_dur = var(duration), min_dur = min(duration), max_dur = max(duration), total_rides = n())

#find average duration of rides grouped by day of the week
daily_mean = all %>% 
  group_by(day = wday(started_at, label=TRUE), member_casual) %>% 
  summarise(average_duration = mean(duration), var_dur = var(duration), min_dur = min(duration), max_dur = max(duration), total_rides = n())
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
SELECT DATE_FORMAT(started_at, '%Y-%m-01') AS mon, member_casual, ROUND(AVG(duration),2) AS average_duration, ROUND(VAR(duration),2) AS var_dur, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY MONTH(started_at), member_casual;

SELECT DATE_FORMAT(started_at, '%W') AS weekday, member_casual, ROUND(AVG(duration),2) AS average_duration, ROUND(VAR(duration),2) AS var_dur, COUNT(*) AS total_rides, MIN(duration), MAX(duration)
FROM tempo
GROUP BY WEEKDAY(started_at), member_casual;
```
**The data is then exported to CSV files for visualisation.**

### 2. Data Visualisation

Data visualisation is done through 3 different tools.

**First is throught he same R session using the ggplot2 library, producing these graphs:**

<p align="left"><img width="800"src="/case-study-1/month_plot.jpeg?raw=true"></p>
<p align="left"><img width="800"src="/case-study-1/weekday_plot.jpeg?raw=true"></p>

**While the next one is using Tableau:**

<p align="left"><img width="800"src="/case-study-1/tableau-charts.png?raw=true"></p>

**The final one is using Excel, with the graphs inside the file.**

### 3. Conclusion

All methods resulted in the same conclusions, which are:

* Casual riders  decide to use the bicycles for entertainment, since the average duration of each ride is longer, while members tend to do it to get somewhere specific regularly, such as work or school. This is further supported though the weekday graphs since they show that on weekends the number of rides from the casual riders outnumber the subscribers. The variance on those who are members also support this hypothesis, since it is way lower than non-members, meaning they are used by members for daily commute.
* The drop in frequency can be due to cold weather, since it happens around winter, while the drop for members is less since it might be necessary for them. The drop in average duration for non-members also support this, while it roughly stays constant for members.




## Case 3 - Suicide

### Synopsis

Every year more than half a million people take their own life, and many more who attempt to do so, causing tragedies that affect families, communities, and causing long lasting effects on those who are left behind.

Suicide does not only happen in certain countries, in fact it is a global phenomenon throughout the world. However, each country has different rate of suicide, some of which have huge gaps with other countries. Happiness is an obvious main factor for the desire to commit suicide. But, what exactly makes people happy. Is it the same across all types of humans? Hence the question is, is there an actual main cause for suicides in the first place? If so, what is it? Knowing this could be the answer to tackling this issue with correct method of prevention.

I will make an attempt to determine the causes by analysing data recording the world's past suicide rate as well as other probable factors which would increase the total number of suicides.

### 1. Data Processing

**\*data obtained is downloaded from either [data.worldbank.org](data.worldbank.org) or [worldpopulationreview.com](worldpopulationreview.com)**
**\*data obtained from data.worldbank.org also include groups of countries such a North America, Europian Union etc. and those rows have been deleted manually from suicides.csv, the other files are left untouched since inner join will be used to merge them**

**Libraries used**
```{r libraries, echo=TRUE, message=FALSE}
library(tidyverse)
library(lubridate)
library(ggpubr)
```

The first assumption is that each person's purchasing power is a huge factor in determining the level of happiness. The imported data will be aggregated to only selecting the data for the period between year 2000 to 2019. Year 2020 and 2021 are removed since they haven't been able to record a complete data. They are then converted to a long format for plotting and other purposes. **The GNI (Gross National Income) data is of constant LCU in 2017, with the same currency and PPP (Purchasing Power Parity) adjusted per capita.**

```{r read_csv, warning = FALSE, message = FALSE}
suicides = read_csv('suicides.csv') %>% 
  select(!('1960':'1999') & !('2020':'2021') & !('Indicator Name':'Indicator Code') & !('...67') & !('Country Code')) %>%
  gather('Year','Suicide Rate','2000':'2019', convert = TRUE) %>%
  drop_na()

gni = read_csv('gni.csv') %>% 
  select(!('1960':'1999') & !('2020':'2021') & !('Indicator Name':'Indicator Code') & !('...67') & !('Country Code')) %>%
  gather('Year','GNI','2000':'2019', convert = TRUE) %>%
  drop_na()
```
<br/>

<p align="center"><img width="800"src="/case-study-3/1.jpeg?raw=true"></p>

<div align='center'><b>Figure 1. Line chart displaying the trend of suicide rate of each country from year 2000 to 2019.</b></div>
<br />

This line chart shows that I can use the average suicide rate between the period to find the top 10 countries with highest and lowest suicide rate, even though there are a few who have big changes, they have higher rate compared to the others, hence will definitely show in the result. The reason to only use this amount is because it will be too confusing to do it on all of them, as well as if purchasing power parity is really the main cause, it will be very visible in the result.

``` {r find_top_bottom}
mean_suicide_rate = group_by(suicides, `Country Name`) %>%
  summarise(avg_suicide_rate = mean(`Suicide Rate`))

highest_suicide_countries =  mean_suicide_rate %>%
  arrange(desc(avg_suicide_rate)) %>%
  slice(1:10) %>%
  select(1)

lowest_suicide_countries = mean_suicide_rate %>%
  arrange(avg_suicide_rate) %>%
  slice(1:10) %>%
  select(1)
```


These are then used to filter the suicide and gni table, for them to be merged later on:
``` {r filter, echo = FALSE}
highest_suicide_rate = filter(suicides, `Country Name` %in% highest_suicide_countries[['Country Name']])
lowest_suicide_rate = filter(suicides, `Country Name` %in% lowest_suicide_countries[['Country Name']])
```

### 2. Analysis

<p align="center"><img width="800"src="/case-study-3/2.jpeg?raw=true"></p>
<div align = "center"><b>Figure 2. Line chart of the trend in suicide rate for the bottom 10 countries with the lowest average suicide rate over the period.</b></div>
<br />

```{r high_suicide_year, echo = FALSE}

<p align="center"><img width="800"src="/case-study-3/3.jpeg?raw=true"></p>
<div align = "center"><b>Figure 3. Line chart of the trend in suicide rate for the top 10 countries with the highest average suicide rate over the period.</b></div>

#### 2.1 Make a deduction about the effect of purchasing power on suicidal tendencies

Next step would be to inner join the data of those countries with the GNI data to plot GNI vs suicide rate:
```{r gpd_suicide_20, echo = FALSE, warning=FALSE}
gni_suicide_high = merge(highest_suicide_rate, gni, by = c('Country Name', "Year"), all.x = FALSE, all.y = FALSE) %>%
  drop_na()

gni_suicide_low = merge(lowest_suicide_rate, gni, by = c('Country Name', "Year"), all.x = FALSE, all.y = FALSE) %>%
  drop_na()

<p align="center"><img width="800"src="/case-study-3/4.jpeg?raw=true"></p>
<div align = "center"><b>Figure 4. Scatter chart of GNI per capita vs suicide rate for countries with the top and bottom 10 average suicide rate over the period.</b></div>


And this is to compare the average of gni of each country with its average suicide rate:
```{r gni_suicide_overall}
mean_gni = group_by(gni,`Country Name`) %>%
  summarise(avg_gni = mean(GNI))

merge(mean_suicide_rate, mean_gni, by = 'Country Name', all.x = FALSE, all.y = FALSE) %>%
  ggplot(aes(x = `avg_gni`, y = `avg_suicide_rate`)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "GNI per capita, PPP", y = "suicide Rate (per 100 000)")
```
<p align="center"><img width="800"src="/case-study-3/5.jpeg?raw=true"></p>
<div align = "center"><b>Figure 5. Scatter chart of average GNI per capita vs average suicide rate for all the available countries, line of best fit added.</b></div>


As it can be seen from **Figure 4**, it cannot be said that money (or how developed the country is) is the main determinant of happiness level of a country's population. The points of the data are not clustered into two groups, in fact, only one country with a low suicide rate has high GNI. Furthermore, The line of best fit in **Figure 5** suggests that higher GNI per capita actually leads to higher suicide rate. Is it acceptable? Sure, it is possible, however looking at the gradient, it looks too low to be the main factor (r = 0.1, p = 0.17).

#### 2.2 Adding another possible factor, birth rate

Looking at the countries which surpass my expectation, there seems to be a common factor, which is the population density of each country. Hence, adding data of a new factor might help with the analysis. Since the  data for population density is calculated with the country's land area, which does not exclude inhabitable land, it might not  produce the expected results. Therefore, choosing birth rate instead, which is correlated with each other, will do.

**\*note that the unit of birth rate is the the number of births for every 1000 of the population**

```{r load_birth, message=FALSE, warning = FALSE}
birth_rate = read_csv('birth_rate.csv') %>%
  select(!('2020':'2021') & !('Indicator Name':'1999') & !('...67') & !('Country Code')) %>%
  gather('Year','Birth Rate','2000':'2019', convert = TRUE) %>%
  drop_na()

mean_birth_rate = group_by(birth_rate,`Country Name`) %>%
  drop_na() %>% 
  summarise(avg_birth_rate = mean(`Birth Rate`))

#merge the mean tables together for comparison
suicide_gni_birth = merge(mean_suicide_rate,
                          mean_birth_rate, by = 'Country Name',
                          all.x = FALSE, all.y = FALSE) %>% 
  merge(mean_gni, by = 'Country Name', all.x = FALSE, all.y = FALSE)
```

The merged data is then grouped between different ranges.
**\*note that birth rate of 10-20 is already considered low, while anything above 30 is too high (source from a study in [https://web.archive.org/web/20160526190803/http://www.childtrends.org/?indicators=fertility-and-birth-rates](web.archive.org)**
```{r grouping}
#compare birth rate + gni + suicide rate
suicide_gni_birth = merge(mean_suicide_rate,
                          mean_birth_rate, by = 'Country Name',
                          all.x = FALSE, all.y = FALSE) %>% 
  merge(mean_gni, by = 'Country Name', all.x = FALSE, all.y = FALSE)


#group the data by birth rate range
suicide_gni_birth_grouped = suicide_gni_birth %>% 
  mutate(birth_rate_range = cut(avg_birth_rate, c(0, 10, 20, 30, Inf))) %>%
  filter(`Country Name` != 'Equatorial Guinea') %>%
  arrange(desc(birth_rate_range), desc(avg_gni))
```

It is common knowledge that as a country' progress's economy progresses, there will always be a decrease in birth rate. However, the value is not always equal. That is the reason why the grouping is done.


<p align="center"><img width="800"src="/case-study-3/6.jpeg?raw=true"></p>
<div align = "center"><b>Figure 6. Scatter charts of data separated by range of the average birth rate in each country, chart plotted average GNI per capita vs average suicide rate over the year 2000 to 2019. The plots also include the income ranges deemed to be of [low, lower-middle, upper-middle, and higher](https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups).</b></div>
<br />


<p align="center"><img width="800"src="/case-study-3/7.jpeg?raw=true"></p>
<div align = "center"><b>Figure 7. Scatter chart of average GNI per capita vs average suicide rate for all the available countries grouped by different ranges of birth rate.</b></div>
<br />

```{r summary}
suicide_gni_birth %>% 
  mutate(birth_rate_range = cut(avg_birth_rate, c(0, 10, 20, 30, Inf))) %>%
  group_by(birth_rate_range) %>%
  summarise('lowest suicide' = min(avg_suicide_rate), 'average suicide' = mean(avg_suicide_rate), 'max suicide' = max(avg_suicide_rate),
            'lowest gni' = min(avg_gni), 'average gni' = mean(avg_gni), 'highest gni' = max(avg_gni))
```
<p align="left"><img width="800"src="/case-study-3/8.JPG?raw=true"></p>

As it can be seen from the summary, the average suicide rate of the countries with lower birth rate are higher, even though majority are developed countries.Same goes with those with extremely high birth rate, which has the lowest average suicide rate. Those closer to the normal birth rate are around similar average suicide rate, however, those with lower birth rate consist of countries with high GNI per capita, which supports my previous deduction, even though there are a few who still do not follow the trend.

This can mean several things, however it will not be convenient to prove since there will be inadequate data available since the other factors, such as traditions, cultures, politics etc., would be hard to gather data on due to the wide range of types.  One factor I can be sure of is the societal aspect of each country. Being developed countries yet having low birth rate means that the chances of creating a family there is low, and therefore the chance of adults being supported by others is low as well. While the opposite, which is high birth rate in a developing or even third world countries, means that the population is socialising regularly.


#### 2.3 Additional info

There is, however, another factor that is indisputable, which can help with the prevention.
```{r gender, echo=TRUE, message=FALSE}
suicide_proportion = read_csv('suicide_by_gender.csv') %>%
  mutate('Male Percentage' = round(rate2019male / (rate2019male + rate2019female) * 100,2),
         'Female Percentage' = 100 - `Male Percentage`) %>%
  select('country', 'Male Percentage', 'Female Percentage') %>%
  rename('Country' = 'country') 

summary(suicide_proportion)

filter(suicide_proportion, `Female Percentage` > `Male Percentage`)
```
<p align="left"><img width="800"src="/case-study-3/9.JPG?raw=true"></p>
<p align="left"><img width="800"src="/case-study-3/10.JPG?raw=true"></p>

As it can be seen from the summary table, the percentage of male suicide victims is significantly larger than female's. And looking at the countries with higher female ratio, only two come up,Grenada and Antigua and Barbuda, which fall under the 10 countries with lowest suicide rate, hence it's insignificant.

There can be several explanations for this, however all would lead to stress, which is made worse by the fact that men tend to hide their negative emotions, hence getting no support from others (source from [mensline.org.au](https://mensline.org.au/mens-mental-health/men-and-emotions)). Again, this could be due to traditions or cultures that demand men to reach higher goals in life as the normal. 

### 3. Conclusion

There is no definite cause of the high rate of suicide of some countries. It can be due to monetary aspect, societal aspect, tradition, culture, politics, or different combinations of them. The only thing that can increase the chance of prevention would be to teach youngsters to open up and socialise more so that others can help when they are unhappy or stressed.
