# Summary of the Data:
# • Two datasets from two sources, one with P&C data (Auto & Home), one with HLM 
# (Health, Life, and Medicare)
# • Dimensions: date, time, product, device
# • Metrics: clicks, clicks revenue, (RPC = clicks revenue / click)
# • The data ranges from June 2nd 2020 to June 9th 2020
# • Hours are rounded to the nearest hour, so 8:00:00 includes all of 8am-8:59am
# • Unless otherwise specified, use the entirety of both datasets together

# validated data values in Excel using filter

# attach cleaning and date package
library(tidyverse)
library(lubridate)

# set working directory
setwd("/cloud/project/Analytics Questions")

# upload csv into environment
hlm <- read.csv("hlm.csv")
pc <- read.csv("pc.csv")


# create aliases to protect original data
hlm_mod <- hlm
pc_mod <- pc

# reformat dates to be consistent for combining
hlm_mod$date <- format(as.Date(hlm_mod$date,"%Y/%m/%d"))
hlm_mod$date <- ymd(hlm_mod$date)
pc_mod$date <- format(as.Date(pc_mod$date,"%m-%d-%Y"))

# combine
agg <- rbind(hlm_mod,pc_mod)

# create rpc column
agg <- agg %>% 
  mutate(rpc = clicks_rev/clicks)

# Questions
# 1. Which 3 hour window across the entire date range shows the largest amount of clicks?
#   (hint: your answer should include a date and hour range, e.g. June 4th 2pm-5pm; this 
#    constitutes the 3 hour window of 2pm-4:59pm due to aforementioned rounding)

date_hour <- agg %>% 
  unite("datetime", date:time, remove = TRUE, sep = " ") %>% 
  mutate(datetime = strptime(datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")) %>% 
  group_by(datetime) %>%
  summarise(total_clicks= sum(clicks), total_rev = sum(clicks_rev), avg_rpc = mean(rpc))

install.packages("RcppRoll")
library(RcppRoll)

# use roll sum function
# reassign selected columns to roll sum by first in data.frame
# assign three_hours_sum as rolling sum of total clicks by previous 3 hours
# fill=NA to fill in first 2 rows and align right to have back 3, defaults to align mid
# align left means next 3

# combine dates and hours
q1 <- date_hour

# set hours rolling sum

# format new dataframe with alterations of start time - 2hours to end time +59mins
# add rolling sum function for 3 previous hours using align right
q1 <- data.frame(start_time = q1$datetime - hours(2),
                end_time = q1$datetime + minutes(59), 
                total_clicks=q1$total_clicks,
                three_hours_sum = roll_sum(q1$total_clicks,3,
                                          fill=NA, align="right"))
# selects the highest 3 hour click total
best_three_hours <- q1 %>% 
  select(-total_clicks) %>% 
  arrange(desc(three_hours_sum)) %>% 
  head(1)


# 2. For the 3 hour window you found in question 1, what percent of the clicks in these 3 
# hours were “mobile”?

# create new dataframe or change q1 dataframe to include mobile?
# must store highest 3 hour click total into vector than is compatible with the df

q2 <- agg %>%
  unite("datetime", date:time, remove = TRUE, sep = " ") %>% # create datetime
  mutate(datetime = as.POSIXct(datetime, tz="UTC")) %>% # reformat to datetime type
  group_by(datetime,device) %>% # group by to agg datetime and device
  summarise(total_clicks= sum(clicks), total_rev = sum(clicks_rev), avg_rpc = mean(rpc)) %>% # create agg calcs
  arrange(device, datetime) # sort to get device rolling sums 
  

# create new data frame for rolling sum
q2_2 <- data.frame(start_time = q2$datetime - hours(2), # subtract 2 hours
                   end_time = q2$datetime + minutes(59), # add ending 59 mins
                   device = q2$device, 
                   total_clicks=q2$total_clicks,
                   three_hours_sum = roll_sum(q2$total_clicks,3, # rolling sum
                                              fill=NA, align="right"))

q2_mobile <- q2_2 %>% 
  filter(device == "mobile" & 
           q2_2$start_time == best_three_hours$start_time)
q2_all <- q2_2 %>% 
  filter(q2_2$start_time == best_three_hours$start_time)
# retrieve answer as a percentage from total mobile clicks from best 3 hours / total 3 hours
library(scales)
q2_answer <- label_percent()(q2_mobile$three_hours_sum / sum(q2_all$three_hours_sum))
q2_answer

# 3. Graph the time series of clicks on datetime; x-axis should be datetime, y-axis should be 
# clicks. nothing graphically fancy is required, just a simple timeseries!
#   (hint: you should only have 1 time-series, not multiple lines)
# date_hour gets agg of date and hour df

# try to plot date/hour on x-axis and clicks on y
date_hour$datetime <- as.POSIXct(date_hour$datetime)
date_hour %>% 
  ggplot(aes(datetime,total_clicks)) + geom_line() + 
  labs(x="Datetime", y="Clicks Total")

# 4. For each product, find the hour (time) that shows the highest average RPC

# create dataframe grouping by datetime,product and sort by highest rpc for each product
q4 <- agg %>% 
  unite("datetime", date:time, remove = TRUE, sep = " ") %>% 
  mutate(datetime = as.POSIXct(datetime,tz="UTC")) %>% 
  group_by(datetime, product) %>% 
  summarise(total_clicks= sum(clicks), total_rev = sum(clicks_rev), avg_rpc = mean(rpc)) %>% 
  arrange(desc(avg_rpc),product)

# remove duplicates in product column
q4_2 <- q4[!duplicated(q4$product),]
