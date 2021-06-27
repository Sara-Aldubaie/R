##_____----- Sara Aldubaie -----_____##


# installing the package 
install.packages("tidyverse")          
install.packages("lubridate")
install.packages("nycflights13")

# load the package 
library("tidyverse")
library("lubridate")
library("nycflights13")

# the following function is needed to solve couple of questions (provided from the book)
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

# the following table is needed to solve couple of questions (provided from the book)

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


#___________________________________________________________________________________#

#----------------------------------------------------------------------------------#
###______________________________ 16.2.4 Exercises ______________________________###
#----------------------------------------------------------------------------------#




#_____ 1.What happens if you parse a string that contains invalid dates?

ymd(c("2010-10-10", "bananas"))

#_________ solution __________#

# it return an NA and a warning message( "failed to parse." )

#___________________________________________________________________________________#

#_____ 2.What does the tzone argument to today() do? Why is it important?

#_________ solution __________#

#it help us to find the time-zone of a date, since different time-zones can have different dates, the value of today() can vary depending on the time-zone specified

#___________________________________________________________________________________#

#_____ 3.Use the appropriate lubridate function to parse each of the following dates:

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

#_________ solution __________#

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

#___________________________________________________________________________________#


#----------------------------------------------------------------------------------#
###______________________________ 16.3.4 Exercises ______________________________###
#----------------------------------------------------------------------------------#



#_____ 1.How does the distribution of flight times within a day change over the course of the year?

#_________ solution __________#


# within a day
flights_dt %>%
  mutate(date = make_date(year(dep_time),
                          month(dep_time),
                          mday(dep_time)),
         hour = hour(dep_time)) %>%
  group_by(date, hour) %>%
  filter(date == '2013-10-15') %>%
  ggplot(mapping = aes(x = hour)) +
  geom_density(alpha = .1)

# within a year
flights_dt %>%
  mutate(date = make_date(year(dep_time),
                          month(dep_time),
                          mday(dep_time)),
         hour = hour(dep_time)) %>%
  group_by(date, hour) %>%
  ggplot(mapping = aes(x = hour, group = date)) +
  geom_density(alpha = .1)

# both are alomst thr same 
#___________________________________________________________________________________#

#_____ 2.Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.

#_________ solution __________#

flights_dt %>%
  select(dep_time, sched_dep_time, dep_delay) %>%
  mutate(other_dep_time = (dep_time - sched_dep_time) / 60) %>%
  filter(dep_delay != other_dep_time) %>%
  View()

flights_dt %>%
  mutate(hour = hour(sched_dep_time)) %>%
  group_by(hour) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = hour, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "Average departure delay (in minutes)",
       x = "Hour of the day")

# its look like dep_time != (sched_dep_time + dep_delay) so there is a mistake in the data 

#___________________________________________________________________________________#

#_____ 3.Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)

#_________ solution __________#


flights_dt %>%
  left_join(airports, by = c('origin' = 'faa')) %>%
  left_join(airports, by = c('dest' = 'faa'), suffix = c('.origin','.dest')) %>%
  select(dep_time, arr_time, air_time, contains('tzone'))

# we must take the time zone in consideration 

#___________________________________________________________________________________#

#_____ 4.How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?

#_________ solution __________#

flights_dt %>%
  mutate(hour = hour(sched_dep_time)) %>%
  group_by(hour) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = hour, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(y = "Average departure delay (in minutes)",
       x = "Hour of the day")

# sched_dep_time because it will tell us how much delay we should expect at the scheduled departure time

#___________________________________________________________________________________#

#_____ 5.On what day of the week should you leave if you want to minimise the chance of a delay?

#_________ solution __________#

flights_dt %>%
  mutate(dayweek = wday(sched_dep_time, label = TRUE)) %>%
  group_by(dayweek) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  gather(key = 'delay', value = 'minutes', 2:3) %>%
  ggplot() +
  geom_col(mapping = aes(x = dayweek, y = minutes, fill = delay),
           position = 'dodge')

# saturday  

#___________________________________________________________________________________#

#_____ 6.What makes the distribution of diamonds$carat and flights$sched_dep_time similar?

#_________ solution __________#

diamonds %>%
  ggplot() +
  geom_freqpoly(mapping = aes(x = carat), binwidth = .02)

flights_dt %>%
  mutate(minutes = minute(sched_dep_time)) %>%
  ggplot() +
  geom_freqpoly(mapping = aes(x = minutes), binwidth = 1)

# both peaks at rounded numbers, its most likely caused by human factors


#___________________________________________________________________________________#

#_____ 7.Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.

#_________ solution __________#

flights_dt %>%
  mutate(delayed = dep_delay > 0,
         minutes = minute(sched_dep_time) %/% 10 * 10,
         minutes = factor(minutes, levels = c(0,10,20,30,40,50),
                          labels = c('0 - 9 mins',
                                     '10 - 19 mins',
                                     '20 - 29 mins',
                                     '30 - 39 mins',
                                     '40 - 49 mins',
                                     '50 - 50 mins'))) %>%
  group_by(minutes) %>%
  summarize(prop_early = 1 - mean(delayed, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(mapping = aes(x = minutes, y = prop_early)) +
  labs(x = 'Scheduled departure (minutes)',
       y = 'Proportion of early departures')
# This shows that the proportion of flights that are early departures is highest between minutes 20--30 and 50--60
#___________________________________________________________________________________#


#----------------------------------------------------------------------------------#
###______________________________ 16.4.5 Exercises ______________________________###
#----------------------------------------------------------------------------------#



#_____ 1.Why is there months() but no dmonths()?

#_________ solution __________#

# because their are months with 28, 29, 30, and 31 day, so month are not fixed number unlike years and days  

#___________________________________________________________________________________#

#_____ 2.Explain days(overnight * 1) to someone who has just started learning R. How does it work?

#_________ solution __________#

#  if there is an overnight flight overnight is equal to TRUE and it will added one more day 
# if there is no  overnight flight overnight is equal to FALSE and no day will be added
#___________________________________________________________________________________#

#_____ 3.Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.

#_________ solution __________#

# 2015
ymd("2015-01-01") + months(0:11)

# current year 
floor_date(today(), unit = "year") + months(0:11)

#___________________________________________________________________________________#

#_____ 4.Write a function that given your birthday (as a date), returns how old you are in years.

#_________ solution __________#

age <- function(bday) {
  (bday %--% today()) %/% years(1)
}
age(ymd("1993-06-01"))

#___________________________________________________________________________________# 

#_____ 5.Why can't (today() %--% (today() + years(1))) / months(1) work?

#_________ solution __________#

today() %--% (today() + years(1))

# it takes our current date and adds one more year, so its working  

#___________________________________________________________________________________#


