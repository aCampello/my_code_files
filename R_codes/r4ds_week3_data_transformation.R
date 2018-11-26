
#============================================#
#                                            #
#      R4DS Week 3: Data Transformation      #
#                                             #
#============================================#

#library(r4ds) # devtools::install_github("hadley/r4ds")

library(tidyverse)
library(nycflights13) # example dataset
library(Lahman)

# dplyr functions

filter()    # Subset observations based on their values
arrange()   # Reorder the rows
select()    # Pick variables by their names
mutate()    # Create new variables with functions of existing variables.
summarise() # Collapse many values down to a single summary.

# Can use these commands in conjunction with group_by() 
# This makes the function operate on the dataset group-by-group.



#= Filtering - filter() ####
#==========================#
# Alternative to subsetting - returns a new TIBBLE df
# Tibbles are a modernised type of dataframe that are good for data science.
# filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values
# (unlike base R subsetting)

library(tidyverse) # imports DS libraries including dplyr, which overwrites base R's filter function.
library(nycflights13) # flights dataset

filter(flights, origin == "LGA")  # shows flights originating from LGA only

# filter by multiple conditions (comma is same as &)
filter(flights, month == 1, day == 1) # flights that departed in month 1 and day 1
filter(flights, month == 1 | month == 2) #  flights that departed in month 1 OR month 2

# filter by a list of values using x %in% y
filter(flights, month %in% c(1, 2)) 

# De Morgan’s law
!x | !y # is the same as: 
!(x & y) 

!x & !y # is the same as:
!(x | y) 
# e.g. flights that weren’t delayed (on arrival or departure) by more than two hours
filter(flights, !(dep_delay>120 | arr_delay>120))
# same as:
filter(flights, arr_delay <= 120, dep_delay <= 120)

# to preserve missing values in filter()
filter(flights, is.na(dep_delay) | dep_delay > 120) # flights delayed by >2 hrs or with unknown delay

# examples
filter(flights, dep_delay >= 120) # flights delayed by 2+ hours
filter(flights, dest  %in% c("IAH", "HOU")) # flights to Houston (IAH or HOU)
filter(flights, carrier %in% c("UA", "AA", "DL")) # Were operated by United, American, or Delta
airlines # to get list of carrier (airlines) and their abbreviations
filter(flights, month %in% c(7, 8, 9)) # Departed in summer (July, August, and September)
filter(flights, arr_delay > 120, dep_delay <= 0) # Arrived >2h late, but didn’t leave late.
filter(flights, dep_delay >= 60, arr_delay <= dep_delay-30) # Were delayed by min 1h, but made up >30min in flight.
filter(flights, dep_time >=0000 & dep_time <= 0600) # Departed between midnight and 6am (inclusive)

# dplyr::between()
filter(flights, between(dep_time, 0000, 0600)) # Departed between midnight and 6am (inclusive)

# How many flights have a missing dep_time? 
filter(flights, is.na(dep_time)) # 8,255



#= Arrange / order by - arrange() ####
#====================================#
arrange(flights, year, month, day) # orders flights data by these variables

# to re-order by a column in descending order: desc() 
arrange(flights, desc(arr_delay))

# put all NA arrival delays first (default is at bottom)
a <- arrange(flights, desc(is.na(arr_delay)))
head(as.data.frame(a))

arrange(flights, desc(dep_delay)) # Most delayed flights. 
arrange(flights, dep_delay) # Flights that left earliest.
a <- arrange(filter(flights, !is.na(air_time)), distance/air_time) # Find the fastest flights.
head(as.data.frame(a))

a <- arrange(filter(flights, !is.na(air_time)), air_time) # Find shortest flights.
head(as.data.frame(a))

a <- arrange(filter(flights, !is.na(air_time)), desc(air_time)) # Find longest flights.
head(as.data.frame(a))




# Select columns - select() ####
#==============================#
select(flights, year, month, day)
select(flights, year:day)

# helper functions for select()
starts_with("abc") # matches names that begin with “abc”.
ends_with("xyz") # matches names that end with “xyz”.
contains("ijk") # matches names that contain “ijk”.
matches("(.)\\1") # selects variables that match a regular expression - here it selects variables that contain repeated characters.
num_range("x", 1:3) # matches x1, x2 and x3.
everything() # selects all variables (except those you specifically mention - for rearranging columns)
one_of() # selects variables in a character vector of variable names

# examples
select(flights, starts_with("ar")) # select arr_time and arr_delay columns only
select(flights, contains("delay"))
select(flights, year, month, day, contains("delay"))
select(flights, matches("(.)\\1")) # gets column names that contain double 'rr' (!)
select(flights, carrier, everything()) # move the carrier column to the start of the data frame

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars)) # selects all variables in the vars vector

select(flights, contains("Time", ignore.case=FALSE)) # can choose not to ignore the case (TIME vs time)



# Rename variables - rename() - a variant of select() ####
#========================================================#
rename(flights, tail_num = tailnum) # might not look like it's been renamed in the console output but check View(mydata).



# Add new variables - mutate() ####
#=================================#

# Note the mutate function must be vectorised: it must take a vector of values 
# as input and return a vector with the same number of values as output.

#= Some useful functions to use with mutate() include...

# Arithmetic operators: +, -, *, /, ^

# Logical comparisons, <, <=, >, >=, != (good idea to store the interim values in new variables so you can check that each step is working as expected)

# Arithmetic operators combined with aggregate functions (mean, sum etc):
x <- c(1,1,1,2,3,4,5,10,20,30,50)
x / sum(x)  # calculates the proportion of a total.
y - mean(y) # computes the difference from the mean.

# Modular arithmetic: 
%/% # integer division
%%  # remainder
  )

# Logs: log(), log2(), log10()  
  # logs are useful when data ranges across multiple orders of magnitude
  # logs also convert multiplicative relationships to additive
  # log2() is easiest to interpret: a difference of 1 on the log scale = doubling 
  # on the original scale; a difference of -1 on log scale = halving on the original.
  
# Offsets: 
lead() and lag() # to refer to leading and lagging values 
x - lag(x) # compute running differences (the previous value in the sequence)
x != lag(x) # find when values change (FALSE-TRUE when values start to differ from the first value)
lead(x) # prints the next value in the sequence

# Base R functions for running sums, products, mins and maxes: 
cumsum(), cumprod(), cummin(), cummax()
cumsum(x)

# dplyr provides cummean() for cumulative means.
cummean(x)

# RcppRoll package for rolling aggregates (sum computed over a rolling window)

# Ranking: 
min_rank(x) # default gives smallest values the small ranks
min_rank(desc(x)) # to give the largest values the smallest ranks

# variants of min_rank():
row_number(), dense_rank(), percent_rank(), cume_dist(), ntile()
# note these all use different methods to rank ties (equal values) - see ?rank

ntile(x, n=4) # x = vector of values to rank, n = number of groups to split up into.

# Examples:
  
# make smaller df to work with
flights_sml <- select(flights, year:day, contains("_time"), 
                      ends_with("delay"), distance, air_time)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

# use transmute() to keep only the new variables
transmute(flights_sml,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

# modular arithmetic to compute hour and minute from dept_time
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,    # divide by exactly 100 to get hours
          minute = dep_time %% 100    # the remaining number is the minutes
)


# reformat (sched_)dep_times to minutes-since-midnight (msm)
flights_sml_msm <- mutate(flights_sml,
       sched_dep_time_msm = (sched_dep_time %/% 100)*60 + sched_dep_time %% 100,
       dep_time_msm = (dep_time %/% 100)*60 + dep_time %% 100
)

# Compare air_time with arr_time - dep_time 
# Note these do not match: need to pull in timezone data from airports$tz for each destination (note all flights departed from New York in this dataset).
transmute(flights, air_time, arr_time, dep_time, 
          arr_dep_dt = arr_time - dep_time,
          arr_dep_nmin = (arr_dep_dt %/% 100)*60 + arr_dep_dt %% 100
          )

# Compare dep_time, sched_dep_time, and dep_delay
transmute(flights_sml_msm, sched_dep_time, dep_time,
          dep_delay,
          dep_delay_msm = dep_time_msm - sched_dep_time_msm, # must calc difference using minutes-since-midnight for it to make sense
)


# Find the 10 most delayed flights using a ranking function.
ranked_delays <- transmute(flights, year, month, day, sched_dep_time, dep_time, arr_time, dep_delay,
          rank = min_rank(desc(flights$dep_delay)))
arrange(filter(ranked_delays, rank <= 10), rank) # shows top 10 in ascending order




# Grouped summaries - summarise() ####
#====================================#

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

group_by() # converts an existing tbl (tibble) into a grouped tbl, where 
# operations are performed "by group". 
# Note this changes the original tbl! 
# ungroup() removes grouping - check using str(data)

# e.g. group by day to get the average delay per date
flights_byday <- group_by(flights, year, month, day)
summarise(flights_byday, mean_delay = mean(dep_delay, na.rm = TRUE))


#= Explore the relationship between flight distance and average delay

# 1. Group flights by destination (all departed from New York)
by_dest <- group_by(flights, dest) 

# 2. Summarise to compute number of flights, distance and mean delay.
delays <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   mean_delay = mean(arr_delay, na.rm = TRUE)
)

# 3. Filter to remove noisy points and Honolulu airport, which is ~ twice as 
#     far away as the next closest airport.
delays <- filter(delays, count > 20, dest != "HNL") 

# 4. Plot
ggplot(data = delays, mapping = aes(x = dist, y = mean_delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
# Note flights with a negative delay leave early.




# Combining multiple operations - PIPES ####
#==========================================#

# Speed up analysis as you don't need to name each intermediate df 
# (e.g. by_dest, delays...)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    mean_delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# read pipes like 'then' :
# take the flights df, THEN group it by dest, THEN summarise it, THEN filter it.

# Remove NAs
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

# Remove flights with missing values, assuming these represent cancelled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) # if only one is NA then assume flight was not cancelled.

# Repeat the summarise code without NAs
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))


### Can combine dplyr with ggplot2 using %>%                             ###
### BUT: ggplot2 does not understand pipes, so use + in the ggplot2 code ###

# E.g. Get cancelled flights
# Better to ID cancelled flights as those with NA dep_delay than NA arr_delay,
# as all flights with NA dep_delay also have NA arr_delay, but
# some flights with NA arr_delay do have a dep_delay.

# Is there a pattern in the number of cancelled flights per day? 
# sum(is.na(dep_delay)) gives NUMBER of delayed flights
flights %>% 
  group_by(year, month, day) %>%
  summarise(n_cancelled = sum(is.na(dep_delay))) %>% 
  ggplot(mapping = aes(x = day, y = n_cancelled)) +
  geom_point() + 
  geom_smooth(se = FALSE)
# looks like cancellations are more common around the 10th of the month


# Is the proportion of cancelled flights related to the average delay?
# mean(is.na(dep_delay)) gives PROPORTION of delayed flights
flights %>% 
  group_by(year, month, day) %>%
  summarise(proportion_cancelled = mean(is.na(dep_delay)),
            mean_delay = mean(dep_delay, na.rm = TRUE)) %>% # must exclude NAs
  filter(mean_delay<60) %>%  # remove outlier
  ggplot(mapping = aes(x = mean_delay, y = proportion_cancelled)) +
  geom_point() + 
  geom_smooth(se = FALSE)
# positive relationship between proportion cancelled and mean dep_delay




# Counts - summarise() ####
#==========================================#

# Whenever you do any aggregation, it’s always a good idea to include a count
# to check you’re not drawing conclusions based on v. small amounts of data.
n()             # counts the size of the current group.
sum(!is.na(x))  # counts the number of non-missing values.
n_distinct(x)   # count the number of distinct (unique) values.

# simple count() helper function in dplyr to calculate counts quickly
not_cancelled %>% 
  count(dest, sort=TRUE) # sort output in descending order of n
# same as:
not_cancelled %>% 
  group_by(dest) %>%
  summarise(n = n())

# can provide optional weight argument
# e.g. to count (sum) distance (miles) flown by each aircraft (identified by tail number)
not_cancelled %>% 
  count(tailnum, wt = distance)
# same as:
not_cancelled %>% 
  group_by(tailnum) %>%
  summarise(miles = sum(distance))




# Problems with mean() and sample size ####
#=========================================#

# E.g. when identifying planes (by tail number) with high mean delays, some 
# have an mean delay of 5h (300 mins), but based on just one or two samples.

# calculate mean delay
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

# freq polygon
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# scatterplot of N flights vs. mean delay
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()  # number of flights
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# typically, variation decreases as the sample size increases


# filter out groups with few observations to make patterns easier to see
# uses the delays df above
delays %>% 
  filter(n > 10) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


# NEW DATA : baseball from Lahman package
# This bit illustrates problem of ranking averages based on limited sample sizes

# Convert data to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting) 
# To see details about the batting dataset highlight <Lahman::Batting> + F1

# compute the batting average (N hits / N attempts) of every player.
batters <- batting %>%
  group_by(playerID) %>% 
  summarise(
    batting_average = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),   # H = Hits, AB = At Bats (attempts)
    at_bats = sum(AB, na.rm = TRUE)
  )

# filter and plot
batters %>% 
  filter(at_bats > 100) %>% 
  ggplot(mapping = aes(x = at_bats, y = batting_average)) +
  geom_point() + 
  geom_smooth(se = FALSE)
# players with high batting averages are lucky, not skilled!

batters %>% 
  arrange(desc(batting_average)) # players with low batting_average also have low number of attempts.

# We need to balance scores / averages with the uncertainty of a small N observations. 
# http://www.evanmiller.org/how-not-to-sort-by-average-rating.html # recommends the lower bound of Wilson score confidence interval
# http://varianceexplained.org/r/empirical_bayes_baseball/ # bayesian methods




# Logical subsetting - summarise() ####
#=====================================#
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

# this bit of the code above ensures that means are only calculated from 
# positive delays. [] is like a 'where' clause.
flights$arr_delay[flights$arr_delay > 0]




# Counts and proportions of logical values - summarise() ####
#===========================================================#

# When sum() and mean() are used with logical values, TRUE is converted to 1 and 
# FALSE to 0. 
# So sum(x) gives the number of TRUEs in x, and mean(x) gives the proportion.

# E.g. How many flights left before 5am? 
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))



# Grouping by multiple variables - can rogressively roll up a dataset ####
#========================================================================#

# When you group by multiple variables, each summarise function peels off one 
# level of the grouping, so you can progressively roll up a dataset:
  
daily <- group_by(flights, year, month, day)

(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# NOTE: progressively rolling up summaries is OK for sums and counts,
# but means and variances may require weighting,
# and it’s not possible to do it exactly for rank-based statistics like the median.
# I.e. the sum of groupwise sums is the overall sum, but the median of groupwise medians is not the overall median.




# Measures of spread: sd(x), IQR(x), mad(x) - summarise() ####
#============================================================#

# standard deviation (mean squared deviation)
sd(x) 

# robust equivalents to sd() that may be more useful if you have outliers:
IQR(x) # interquartile range
mad(x) # median absolute deviation 

# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))




# Measures of rank: min(x), quantile(x, 0.25), max(x) - summarise() ####
#======================================================================#

# Quantiles are a generalisation of the median. 
quantile(x, 0.25) # will find a value of x that is > 25% of the values, and < the remaining 75%.





# Measures of position: first(x), nth(x, 2), last(x) - summarise() ####
#=====================================================================#

# Work similarly to x[1], x[2], and x[length(x)] 
# but let you set a default value if that position does not exist 
# (i.e. you’re trying to get the 3rd element from a group that only has 2 elements). 

# E.g. first and last departure for each day
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

# NOTE: get the same result via filtering on ranks, but in long format 
# (i.e. two rows per day, for the first and last dep_times):
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))
  



# Grouped mutates & filters - mutate() and filter() ####
#===================================================================#

# Grouping is most useful in conjunction with summarise()
# Also possible to use group_by with mutate() and filter()
# A grouped filter is a grouped mutate followed by an ungrouped filter - only
# useful for quick/dirty manipulations - hard to check you've done it correctly.

# Find the worst members of each group (in terms of arrival delay)
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

# Standardise popular_dests to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)












