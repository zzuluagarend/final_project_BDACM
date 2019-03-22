rm(list = ls())
library("dplyr")
library("tidyverse")


??one_of
# Examples select function
iris <- tbl_df(iris) # so it prints a little nicer
select(iris, starts_with("Petal"))
select(iris, ends_with("Width"))
select(iris, contains("etal"))
select(iris, matches(".t."))
select(iris, Petal.Length, Petal.Width)
select(iris, everything())
vars <- c("Petal.Length", "Petal.Width")
select(iris, one_of(vars))

flights<-nycflights13::flights
(jan1<-filter(flights, month==1, day==1))
(nov_dec<-filter(flights, month==11 | month==12))
(nov_dec<-filter(flights, month %in% c(11,12)))
(delay_2<-filter(flights, (arr_delay > 120 | dep_delay > 120)))
(houston<-filter(flights, (dest == "IAH" | dest == "HOU")))
(ua_dl_aa<-filter(flights, (carrier == "UA" | carrier == "DL" | carrier == "AA")))
(sommer<-filter(flights, month %in% c(7,8,9)))
(del_ar<-filter(flights, arr_delay > 120 & dep_delay <= 0))
(del_1h<-filter(flights, (arr_delay > 60 | dep_delay > 60) &  air_time > 30))
(dep_mn<-filter(flights, dep_time %in% c(0:600)))
(dep_mn1<-filter(flights, is.na(dep_time)))
n_flights<-arrange(flights, year, month, day)
n_flights<-arrange(flights, desc(arr_delay))
n_flights<-arrange(flights, desc(arr_delay | dep_delay))
n_flights<-arrange(flights, desc(air_time))


# Add new variables
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, 
       gain =arr_delay - dep_delay, 
       speed = distance / air_time * 60,
       gain_per_hour = gain / speed
       )
transmute(flights_sml, 
       gain =arr_delay - dep_delay, 
       speed = distance / air_time * 60,
       gain_per_hour = gain / speed
)
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)
 transmute(flights,
           air_time,
           gain =arr_time - dep_time
 )
 
by_day <- group_by(flights, year, month, day)
s_by_day<- summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

# These functions allow you to select variables based on their names.
iris <- tbl_df(iris) # so it prints a little nicer
select(iris, starts_with("Petal"))
select(iris, ends_with("Width"))
select(iris, contains("etal"))
select(iris, matches(".t."))
select(iris, Petal.Length, Petal.Width)
select(iris, everything())
vars <- c("Petal.Length", "Petal.Width")
select(iris, one_of(vars))

# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.


# power analysis

pwr.anova.test(k=5, n=f, sig.level=.05, power=.8) 


