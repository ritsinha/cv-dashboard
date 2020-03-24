## Explore the Corona Virus Data

# Load libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(rlang)
library(tidyverse)
library(tidyr)

dat_cases = read_csv("../../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#dat = fread("../../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

names(dat_cases)[1] = "State"
names(dat_cases)[2] = "Country"

for(i in 5:length(names(dat_cases))){
  names(dat_cases)[i] = paste("date", gsub("/", "_", names(dat_cases)[i]), sep="")
}

first_date = names(dat_cases)[5]
last_date = names(dat_cases)[length(names(dat_cases))]


top_countries = dat_cases %>% 
  group_by(Country) %>%
  summarize(total = sum(!!sym(last_date))) %>%
  arrange(desc(total)) %>%
  data.frame()
  
dat_cases_long = dat_cases %>%
  gather(Date, Value, !!sym(first_date):!!sym(last_date)) %>%
  data.frame()

dat_cases_long$State = NULL
dat_cases_long$Lat = NULL
dat_cases_long$Long = NULL

dat_cases_long = dat_cases_long %>%
  group_by(Country, Date) %>%
  summarise(total = sum(Value)) %>%
  data.frame()

dat_cases_long$Date  = as.Date(gsub("_", "-", gsub("date", "", dat_cases_long$Date)), "%m-%d-%y")

dat_cases_long = dat_cases_long %>%
  arrange(Country, Date)

top = 12

ggplot(subset(dat_cases_long, (Country %in% top_countries$Country[1:top] & total > 100)), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line() + geom_point() + theme_bw(14) + ylab("Number of Confirmed Cases")

ggplot(subset(dat_cases_long, (Country %in% top_countries$Country[1:top] & total > 100)), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line() + geom_point() + theme_bw(14) + ylab("Number of Confirmed Cases") + scale_y_log10()


dat_cases_long_selected = subset(dat_cases_long, (Country %in% top_countries$Country[1:top] & total > 100))

fit = lm(log10(total) ~ -1 + Country + Country:Date, data = dat_cases_long_selected,
         weights = )

predict_df = data.frame(expand.grid(Country = unique(dat_cases_long_selected$Country), 
                                    Date = max(dat_cases_long_selected$Date)+1:5))

predict_df$total = 10^predict.lm(fit, newdata = predict_df)
predict_df$type = "Predicted"

dat_cases_long_selected$type = "Observed"

dat_cases_long_selected_predicted = merge(dat_cases_long_selected, predict_df, all = TRUE)

ggplot(subset(dat_cases_long_selected_predicted, Country %in% c("US", "Italy", "Spain","Germany")), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line(aes(linetype= type)) + geom_point(aes(shape = type)) + theme_bw(14) + 
  ylab("Number of Confirmed Cases") + scale_y_log10()



#### Deaths  ####

dat_deaths = read_csv("../../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

names(dat_deaths)[1] = "State"
names(dat_deaths)[2] = "Country"

for(i in 5:length(names(dat_deaths))){
  names(dat_deaths)[i] = paste("date", gsub("/", "_", names(dat_deaths)[i]), sep="")
}

first_date = names(dat_deaths)[5]
last_date = names(dat_deaths)[length(names(dat_deaths))]

top_countries = dat_deaths %>% 
  group_by(Country) %>%
  summarize(total = sum(!!sym(last_date))) %>%
  arrange(desc(total)) %>%
  data.frame()

dat_deaths_long = dat_deaths %>%
  gather(Date, Value, !!sym(first_date):!!sym(last_date)) %>%
  data.frame()

dat_deaths_long$State = NULL
dat_deaths_long$Lat = NULL
dat_deaths_long$Long = NULL

dat_deaths_long = dat_deaths_long %>%
  group_by(Country, Date) %>%
  summarise(total = sum(Value)) %>%
  data.frame()

dat_deaths_long$Date  = as.Date(gsub("_", "-", gsub("date", "", dat_deaths_long$Date)), "%m-%d-%y")

dat_deaths_long = dat_deaths_long %>%
  arrange(Country, Date)

top = 12

ggplot(subset(dat_deaths_long, (Country %in% top_countries$Country[1:top] & total > 10)), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line() + geom_point() + theme_bw(14) + ylab("Number of Confirmed Cases")

ggplot(subset(dat_deaths_long, (Country %in% top_countries$Country[1:top] & total > 10)), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line() + geom_point() + theme_bw(14) + ylab("Number of Confirmed Cases") + scale_y_log10()


dat_deaths_long_selected = subset(dat_deaths_long, (Country %in% top_countries$Country[1:top] & total > 50))

fit = lm(log10(total) ~ -1 + Country + Country:Date, data = dat_deaths_long_selected)

predict_df = data.frame(expand.grid(Country = unique(dat_deaths_long_selected$Country), 
                                    Date = max(dat_deaths_long_selected$Date)+1:10))

predict_df$total = 10^predict.lm(fit, newdata = predict_df)
predict_df$type = "Predicted"

dat_deaths_long_selected$type = "Observed"

dat_deaths_long_selected_predicted = merge(dat_deaths_long_selected, predict_df, all = TRUE)

ggplot(subset(dat_deaths_long_selected_predicted, Country %in% c("US", "Italy", "Spain","Germany", 
                                                          "Iran", "China")), 
       aes(x = Date, y = total, color = Country)) + 
  geom_line(aes(linetype= type)) + geom_point(aes(shape = type)) + theme_bw(14) + 
  ylab("Number of Confirmed Cases") + scale_y_log10()

dat_deaths_daily = dat_deaths_long %>%
  group_by(Date) %>%
  summarise(daily_total = sum(total)) %>%
  data.frame()

ggplot(dat_deaths_daily, 
       aes(x = Date, y = daily_total)) + 
  geom_line() + geom_point() + theme_bw(14) + ylab("Number of Deaths") + scale_y_log10()

dat_deaths_daily$days_from_1stjan = as.numeric(dat_deaths_daily$Date - as.Date("2020-01-01"))

fit = lm(log10(daily_total) ~ poly(days_from_1stjan, 3), data = dat_deaths_daily)

predict_df = data.frame(days_from_1stjan = max(dat_deaths_daily$days_from_1stjan)+1:15,
                        Date = max(dat_deaths_long_selected$Date)+1:15)

predict_df$daily_total = 10^predict.lm(fit, newdata = predict_df)
predict_df$type = "Predicted"

dat_deaths_daily$type = "Observed"

dat_deaths_daily_predicted = merge(dat_deaths_daily, predict_df, all = TRUE)

ggplot(dat_deaths_daily_predicted, 
       aes(x = Date, y = daily_total)) + 
  geom_line(aes(linetype= type)) + geom_point(aes(shape = type)) + theme_bw(14) + 
  ylab("Number of Deaths") + scale_y_log10()

