setwd("~/Analytics/CSV")
# read the four csv files
q2_2019 <- read.delim("Divvy_Trips_2019_Q2.txt", 
                      header = TRUE, sep = ",")
View(head(q2_2019))
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")
View(head(q3_2019))
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

#rename columns to prep to combine
q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype)

q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype)

q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"
                   ,rideable_type = "X01...Rental.Details.Bike.ID" 
                   ,started_at = "X01...Rental.Details.Local.Start.Time"  
                   ,ended_at = "X01...Rental.Details.Local.End.Time"  
                   ,start_station_name = "X03...Rental.Start.Station.Name" 
                   ,start_station_id = "X03...Rental.Start.Station.ID"
                   ,end_station_name = "X02...Rental.End.Station.Name" 
                   ,end_station_id = "X02...Rental.End.Station.ID"
                   ,member_casual = "User.Type"
                   ,'01 - Rental Details Duration In Seconds Uncapped'= 
                    "X01...Rental.Details.Duration.In.Seconds.Uncapped"
                   ,'Member Gender'= 
                    "Member.Gender"
                   ,'05 - Member Details Member Birthday Year'=
                    "X05...Member.Details.Member.Birthday.Year")

# inspect for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# one big dataframe
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

# inspect aggregated for analysis
colnames(all_trips)
nrow(all_trips)
nrow(q1_2020)+ nrow(q2_2019) + nrow(q3_2019) + nrow(q4_2019)

# dimensions in dataframe
dim(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)

# change Customer to casual value
all_trips$member_casual[all_trips$member_casual == "Customer"] <- "casual"

# change Subscriber to member value
all_trips$member_casual[all_trips$member_casual == "Subscriber"] <- "member"
View(tail(all_trips))

# count of how many members there are to casual
all_trips %>% 
  group_by(member_casual) %>% 
  tally()

# Date formatting and separating into columns
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add calculated ride_duration
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# reformat times into numeric for calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data qhwew bikes checked for quality or length is < 0 
all_trips_v2 <- 
all_trips[!(all_trips$start_station_name == "HQ QR"| 
              all_trips$start_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" | 
              all_trips$end_station_name == "HUBBARD ST BIKE CHECKING (LBS-WH-TEST)" |
              all_trips$ride_length<0),]
# QA to ensure no ride lengths negative
all_trips_v2 %>% 
  filter(ride_length < 0) %>% 
  tally()

View(head(all_trips_v2))

# summary stats
summary(all_trips_v2$ride_length)

# compare member vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# i see that casual has higher ride lengths than members from median

# See the average ride time by each day for members vs casual users
# Returns ordered list by column DOW
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", 
                                             "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", 
                                             "Saturday"))
View(aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                 all_trips_v2$day_of_week, FUN = mean))
# casual have higher ride lengths across DOW


# analyze ridership data by type and weekday
# why do we use the wday function when we have DOW?
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = unit_format(suffix = "K", scale = 1e-3)) +
  labs(x="Weekday", y="Number of Rides", fill = "Customer Type")

# clearly shows number of ride for each member is more than that of casual


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = unit_format(suffix = "K", scale = 1e-3)) +
  labs(x="Weekday", y="Average Ride Duration (s)", fill= "Customer Type") 

# clearly shows that durations are longer for casual than for members

# change in start station (scratch)
all_trips_v2 %>% 
  group_by(start_station_name) %>% 
  tally(name = "total") %>% 
  arrange(desc(total))

# trend over time for number of rides
all_trips_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_rides, 
             group=member_casual, fill=member_casual,
             color=member_casual)) +
  geom_line() + geom_point() + 
  scale_y_continuous(labels = unit_format(suffix = "K", scale = 1e-3)) +
  labs(x="Month", y="Number of Rides")

# trend over time for avg duration
all_trips_v2 %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, 
             group=member_casual, fill=member_casual,
             color=member_casual)) +
  geom_line() + geom_point() +
  scale_y_continuous(labels = unit_format(suffix = "K", scale = 1e-3)) +
  labs(x="Month", y="Average Ride Duration (s)")

# extremely high average duration for month 1 is unbelievable
# due to longer ride lengths beginning that year(will need more context)
# members have more consistent average duration


# members fairly consistent even off peak times, casual non-existent off peak


all_trips_v2 %>% 
  group_by(member_casual,rideable_type) %>% 
  filter(started_at >= 2020-01-01) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x=rideable_type, y=average_duration, 
             fill=member_casual,
             color=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = unit_format(suffix = "K", scale = 1e-3)) +
  labs(x="Month", y="Average Ride Duration (s)")

# summary stats by member/casual
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
