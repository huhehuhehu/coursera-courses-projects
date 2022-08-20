library(tidyverse)
library(lubridate)
library(ggpubr)


# import the csv files and cleaning them to only contain data from year 2000 to 2019, as well as getting rid of the unnecessary columns
# and countries whose data is not available
# also transforming the wide format to long
# removing country code since source is the same, hence format of country name is the same
suicides = read_csv('suicides.csv') %>% 
  select(!('1960':'1999') & !('2020':'2021') & !('Indicator Name':'Indicator Code') & !('...67') & !('Country Code')) %>%
  gather('Year','Suicide Rate','2000':'2019', convert = TRUE) %>%
  drop_na()

gni = read_csv('gni.csv') %>% 
  select(!('1960':'1999') & !('2020':'2021') & !('Indicator Name':'Indicator Code') & !('...67') & !('Country Code')) %>%
  gather('Year','GNI','2000':'2019', convert = TRUE) %>%
  drop_na()

birth_rate = read_csv('birth_rate.csv') %>%
  select(!('2020':'2021') & !('Indicator Name':'1999') & !('...67') & !('Country Code')) %>%
  gather('Year','Birth Rate','2000':'2019', convert = TRUE) %>%
  drop_na()


ggplot(suicides, aes(x = Year, y = `Suicide Rate`, group = `Country Name`)) + 
  geom_line() + labs(y = "Suicide Rate (per 100 000)", x = '')

#find the top 10 countries with highest and lowest average suicide rate within the period 
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



#find the the top 10 countries with highest and lowest average GNI per capita within the period
mean_gni = group_by(gni,`Country Name`) %>%
  summarise(avg_gni = mean(GNI))

highest_gni_countries = mean_gni %>%
  arrange(desc(avg_gni)) %>%
  slice(1:10) %>%
  select(1)

lowest_gni_countries = mean_gni %>%
  arrange(avg_gni) %>%
  slice(1:10) %>%
  select(1)



#summarise birth rate to find the average over the period
mean_birth_rate = group_by(birth_rate,`Country Name`) %>%
  drop_na() %>% 
  summarise(avg_birth_rate = mean(`Birth Rate`))

summary(mean_birth_rate)

highest_birth_rate_countries = mean_birth_rate %>%
  arrange(desc(avg_birth_rate)) %>%
  slice(1:10) %>%
  select(1)

lowest_birth_rate_countries = mean_birth_rate %>%
  arrange(avg_birth_rate) %>%
  slice(1:10) %>%
  select(1)

#plot suicide rates of countries with highest and lowest rate
highest_suicide_rate = filter(suicides, `Country Name` %in% highest_suicide_countries[['Country Name']])
lowest_suicide_rate = filter(suicides, `Country Name` %in% lowest_suicide_countries[['Country Name']])

ggplot(lowest_suicide_rate, aes(x = Year, y = `Suicide Rate`, colour = `Country Name`)) + 
  geom_line() + 
  xlim(2000,2020) +
  labs(color = 'Country', x = '', y = 'Suicide Rate (per 100 000)')

ggplot(highest_suicide_rate, aes(x = Year, y = `Suicide Rate`, colour = `Country Name`)) + 
  geom_line() + 
  xlim(2000,2020) + ylim(0,100) +
  labs(color = 'Country', x = '', y = 'Suicide Rate (per 100 000)')



#inner join countries highest and lowest suicide data with the corresponding gni per capita
gni_suicide_high = merge(highest_suicide_rate, gni, by = c('Country Name', "Year"), all.x = FALSE, all.y = FALSE) %>%
  drop_na()

gni_suicide_low = merge(lowest_suicide_rate, gni, by = c('Country Name', "Year"), all.x = FALSE, all.y = FALSE) %>%
  drop_na()

ggplot(rbind(gni_suicide_high, gni_suicide_low), 
       aes(x = `GNI`, y = `Suicide Rate`, color = `Country Name`)) +
  geom_point() +
  theme(legend.position = "none") +
  ylim(1,100) +
  labs(x = "GNI per capita, PPP", y = "Suicide Rate (per 100 000)")




#inner join countries highest and lowest gni trend with the corresponding suicide rate
suicide_gni_high = merge(
  filter(gni, `Country Name` %in% highest_gni_countries[['Country Name']]),
  filter(suicides, `Country Name` %in% highest_gni_countries[['Country Name']]),
  by = c('Country Name', "Year"),
  all.x = FALSE, all.y = FALSE) %>%
  drop_na()

suicide_gni_low = merge(
  filter(gni, `Country Name` %in% lowest_gni_countries[['Country Name']]),
  filter(suicides, `Country Name` %in% lowest_gni_countries[['Country Name']]),
  by = c('Country Name', "Year"),
  all.x = FALSE, all.y = FALSE) %>%
  drop_na()

ggplot(suicide_gni_low, aes(x = GNI, y = `Suicide Rate`)) +
  geom_point() +
  geom_point(data = suicide_gni_high)
  




options(scipen="5")
merge(mean_suicide_rate, mean_gni, by = 'Country Name', all.x = FALSE, all.y = FALSE) %>%
  ggplot(aes(x = `avg_gni`, y = `avg_suicide_rate`)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "GNI per capita, PPP", y = "Suicide Rate (per 100 000)")


merge(mean_suicide_rate, mean_birth_rate, by = 'Country Name', all.x = FALSE, all.y = FALSE) %>%
  ggplot(aes(x = `avg_birth_rate`, y = `avg_suicide_rate`)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

merge(mean_gni, mean_birth_rate, by = 'Country Name', all.x = FALSE, all.y = FALSE) %>%
  ggplot(aes(x = `avg_gni`, y = `avg_birth_rate`)) + 
  geom_point() + 
  geom_smooth(method = "loess")

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


cor.test(suicide_gni_birth$avg_gni, suicide_gni_birth$avg_suicide_rate, method = "pearson")


#plot different birth rate range separately
p1 = ggplot(filter(suicide_gni_birth_grouped, birth_rate_range == "(0,10]"), 
       aes(x = avg_gni, y = avg_suicide_rate)) +
  geom_point() +
  labs(x = '', y = "Suicide rate (per 100 000)", title = "Less than 10") +
  ylim(0,75) +
  geom_smooth(method = "lm") +
  annotate("text", x = 30000, y = 60, label = "bold('High income')", parse = TRUE) +
  geom_vline(xintercept = 1036) + geom_vline(xintercept = 4045) + geom_vline(xintercept = 12535)

p2 = ggplot(filter(suicide_gni_birth_grouped, birth_rate_range == "(10,20]"), 
       aes(x = avg_gni, y = avg_suicide_rate)) +
  geom_point() +
  labs(x = '', y = '', title = "Between 10 and 20") +
  ylim(0,75) +
  geom_smooth(method = "lm") +
  annotate("text", x = 45000, y = 60, label = "bold('High income')", parse = TRUE) +
  geom_vline(xintercept = 1036) + geom_vline(xintercept = 4045) + geom_vline(xintercept = 12535)

p3 = ggplot(filter(suicide_gni_birth_grouped, birth_rate_range == "(20,30]"), 
       aes(x = avg_gni, y = avg_suicide_rate)) +
  geom_point() +
  labs(x = "GNI per capita, PPP", y = "Suicide rate (per 100 000)", title = "Between 20 to 30") +
  ylim(0,75) +
  geom_smooth(method = "lm") +
  annotate("text", x = 9000, y = 60, label = "bold('Upper\nmiddle\nincome')", parse = TRUE,  size = 2) +
  geom_vline(xintercept = 1036) + geom_vline(xintercept = 4045) + geom_vline(xintercept = 12535)

p4 = ggplot(filter(suicide_gni_birth_grouped, birth_rate_range == "(30,Inf]"), 
       aes(x = avg_gni, y = avg_suicide_rate)) +
  geom_point() +
  labs(x = "GNI per capita, PPP", y = '', title = "More than 30") +
  ylim(0,75) +
  geom_smooth(method = "lm") +
  annotate("text", x = 9000, y = 60, label = 'bold("Upper\nmiddle\nincome")',  parse = TRUE) +
  geom_vline(xintercept = 1036) + geom_vline(xintercept = 4045) + geom_vline(xintercept = 12535)

ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2)


suicide_gni_birth_grouped %>%
  group_by(birth_rate_range) %>%
  ggplot(aes(x = avg_gni, y = avg_suicide_rate)) +
  geom_point(aes(color = birth_rate_range, shape = birth_rate_range)) + 
  theme(legend.position = c(1,1), legend.justification = c("right", "top")) +
  labs(color = 'Birth Rate (Per 1000)', shape = 'Birth Rate (Per 1000)', x = "GNI per capita, PPP", y = "Suicide rate (per 100 000)") +
  geom_vline(xintercept = 1036) + geom_vline(xintercept = 4045) + geom_vline(xintercept = 12535)

suicide_gni_birth %>% 
  mutate(birth_rate_range = cut(avg_birth_rate, c(0, 10, 20, 30, Inf))) %>%
  group_by(birth_rate_range) %>%
  summarise('lowest suicide' = min(avg_suicide_rate), 'average suicide' = mean(avg_suicide_rate), 'max suicide' = max(avg_suicide_rate),
            'lowest gni' = min(avg_gni), 'average gni' = mean(avg_gni), 'highest gni' = max(avg_gni))


View(filter(suicide_gni_birth, `Country Name` %in% highest_suicide_countries[['Country Name']] |
         `Country Name` %in% lowest_suicide_countries[['Country Name']]))


#showing suicide percentage
suicide_proportion = read_csv('suicide_by_gender.csv') %>%
  mutate('Male Percentage' = round(rate2019male / (rate2019male + rate2019female) * 100,2),
         'Female Percentage' = 100 - `Male Percentage`) %>%
  select('country', 'Male Percentage', 'Female Percentage') %>%
  rename('Country' = 'country') 

summary(suicide_proportion)

filter(suicide_proportion, `Female Percentage` > `Male Percentage`)
