# I'm a code cell, click me, then run me!
256 * 60 * 24 # Children × minutes × hours

greet <- function(first_name, last_name) {
  paste("My name is ", last_name, ", ", 
        first_name, " ", last_name, "!", sep = "")
}

# Replace with your first and last name.
# That is, unless your name is already James Bond.
greet("James", "Bond")

# Reading in the global temperature data,data
global_temp <- read.csv("C:/Users/ADMIN/Downloads/global_temperature.csv")

# Take a look at the first datapoints
# .... YOUR CODE FOR TASK 3 ....

# Plotting global temperature in degrees celsius by year.
plot(global_temp$year, global_temp$degrees_celsius, 
     type = "l", col = "forestgreen", 
     xlab = "....", ylab = "....")

library(forecast)
library(ggplot2)

# Converting global_temp into a time series (ts) object.
global_temp_ts <- ts(global_temp$degrees_celsius, 
                     start = global_temp$year[1])

# Forecasting global temperature 50 years into the future 
# using an exponential smoothing state space model (ets).
temperature_forecast <- forecast( ets(global_temp_ts), h = 50)

# Plotting the forecast
autoplot(temperature_forecast)

# .... YOUR CODE FOR TASK 5 ....


# Are you ready to get started with projects?
I_am_ready <- TRUE

# Ps. 
# Feel free to try out any other stuff in this notebook. 
# It's all yours!












