################################################################
#IoT Analytics
#Visualize and Analyze Energy Data

#Missing values imputation and forecasting

#Created by Eirik Espe
################################################################

source("Initial Exploration part 2.R")

library(scales)
library(zoo)


# Before we start creating time series objects, we should handle 
# missing values



# Missing data analysis

# Take a look at the periods with missing data
powconsumption[which(is.na(powconsumption$Global_active_power)), "DateTime"]

# Power consumption after the period of missing values September 28th, 2010
powconsumption %>% 
  filter(Year == 2010 & Month == 9 & Day == 28 & Hour == 19 & 
           between(Minute, 8, 20))

# And the consumption the following two days
powconsumption %>% 
  filter(Year == 2007 & Month == 4 & between(Day, 28, 30))



# Data frame with date, hour and minute for the missing values
NAsTime <- powconsumption %>% 
  filter(is.na(Global_active_power)) %>%  
  select(DateTime, Hour, Minute, Global_active_power)


# Plot with Missing values for different time of the day
ggplot(NAsTime) + geom_bar(aes(x = Hour)) + 
  labs(title = "Time point of missing values") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~year(DateTime))


#Missing values per weekday
ggplot(NAsTime) + geom_bar(aes(x = Hour)) +
  labs(title = "Time point of missing values") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(year(DateTime)~wday(DateTime, label = TRUE, week_start = 1))



# I was first considering converting the data to time series and then 
# handle missing value.
# I chose instead to handle the missing values before I converted the data
# to time series.

# First I considered to change the missing values by creating a function
# that replace the missing values with values from the previous week.
# In the end I chose the na.approx() function from the zoo package.
# This function replace missing values by interpolation from values before
# and after



# Create time series objects
#tsY07_09Act <- ts(Y07_09$Global_active_power, 
#                frequency = 525600, start = c(as.Date("2007-01-01", "%Y-%m-%d")))
#tsY07_09React <- ts(Y07_09$Global_reactive_power, 
#                frequency = 525600, start = c(2007, 1))
#tsY07_09Volt <- ts(Y07_09$Voltage, 
#                frequency = 525600, start = c(2007, 1))
#tsY07_09Int <- ts(Y07_09$Global_intensity, 
#                frequency = 525600, start = c(2007, 1))
#tsY07_09S1 <- ts(Y07_09$Sub_metering_1, 
#                frequency = 525600, start = c(2007, 1))
#tsY07_09S2 <- ts(Y07_09$Sub_metering_2, 
#                 frequency = 525600, start = c(2007, 1))
#tsY07_09S3 <- ts(Y07_09$Sub_metering_3, 
#                 frequency = 525600, start = c(2007, 1))
#statsNA(tsY07_09Act)


#--- Time series imputation for Missing values ----

#Impute missing values
#tsY07_09Act_imp <- na.seadec(tsY07_09Act, algorithm = "locf")
#tsY07_09React_imp <- na.seadec(tsY07_09React, algorithm = "locf")
#tsY07_09Volt_imp <- na.seadec(tsY07_09Volt, algorithm = "locf")
#tsY07_09Int_imp <- na.seadec(tsY07_09Int, algorithm = "locf")
#tsY07_09S1_imp <- na.seadec(tsY07_09S1, algorithm = "locf")
#tsY07_09S2_imp <- na.seadec(tsY07_09S2, algorithm = "locf")
#tsY07_09S3_imp <- na.seadec(tsY07_09S3, algorithm = "locf")

# Put time series back to vector
#imp_Act <- unclass(tsY07_09Act_imp)
#imp_React <- unclass(tsY07_09React_imp)
#imp_Volt <- unclass(tsY07_09Volt_imp)
#imp_Int <- unclass(tsY07_09Int_imp)
#imp_S1 <- unclass(tsY07_09S1_imp)
#imp_S2 <- unclass(tsY07_09S2_imp)
#imp_S3 <- unclass(tsY07_09S3_imp)

#imputed_df07_09 <- cbind(imp_Act, imp_React, imp_Volt, imp_Int,
#                         imp_S1, imp_S2, imp_S3)

#plotna

#--- Function for imputing missing values ----

# Create a function to replace missing values with values from previous week
#impute_na <- function(impute) {     #Function name
#  for (i in 1:length(impute)) {     #Start at 1 and run through
#    if (is.na(impute[i])) {         #Look for missing value
#      impute[i] <- impute[i-10080]  #Replacing NAs with 1 week previous values
#    }                               #(60min*24h*7d = 10080min)
#  }
#  return(vector)
#}






#--- Handling missing values ----

#Subset variables with missing values
na_var <- mutate(powconsumption, 
                 Active_power = Global_active_power, 
                 Reactive_power = Global_reactive_power, 
                 VoltageImp = Voltage, 
                 Intensity = Global_intensity,
                 Submeter1 = Sub_metering_1, 
                 Submeter2 = Sub_metering_2, 
                 Submeter3 = Sub_metering_3) %>% 
  select(Active_power, Reactive_power, VoltageImp, Intensity, 
         Submeter1, Submeter2, Submeter3)

#Apply imputatation function
#imputed_var <- select(powconsumption, DateTime:Minute)
#  apply(na_var, 2, impute_na)

# Impute missing values
imputed_var <- apply(na_var, 2, na.approx)
imputed_var <- cbind(powconsumption, imputed_var)




#--- Time series objects ----

# Creating time series objects

## Create data frame with monthly sums of energy consumption for
## the time period from 2007 to 2009
Y07_09monthly <- imputed_var %>% 
  filter(between(Year, 2007, 2009)) %>% 
  group_by(Year, Month) %>% 
  summarise(SumActive = sum(Active_power),
            SumS1 = sum(Submeter1),
            SumS2 = sum(Submeter2),
            SumS3 = sum(Submeter3)) %>% 
  ungroup()


## Create TS object with active power
ts07_09monthly_Active <- ts(Y07_09monthly$SumActive, frequency = 12, 
                            start = c(2007,1))


## Decompose active power into trend, seasonal and remainder
components07_09Active_monthly <- decompose(ts07_09monthly_Active)

## Plot decomposed active power
plot(components07_09Active_monthly)

## Check summary statistics for decomposed active power 
summary(components07_09Active_monthly)



# Holt-Winters

## Seasonal adjusting of active power by subtracting the seasonal component
ts07_09monthly_Active_Adjusted <- 
  ts07_09monthly_Active - components07_09Active_monthly$seasonal

#Plot
autoplot(ts07_09monthly_Active_Adjusted)

# Check if seasonality has been removed. Decompose again.
plot(decompose(ts07_09monthly_Active_Adjusted))

## Looking at the scales of seasonal component (with 10^-12)
## the seasonality has been removed



## Holt Winters Exponential Smoothing & Plot
tsHW07_09mod1 <- HoltWinters(ts07_09monthly_Active_Adjusted, 
                             beta=FALSE, gamma=FALSE)


## HoltWinters forecast & plot
tsHW07_09mod1for <- forecast(tsHW07_09mod1, h = 12)
plot(tsHW07_09mod1for, ylab= "kilowatt", xlab= "Time - Active power")


## 2nd model Holt Winters Exponential Smoothing & Plot 
## (let beta and gamma be set by default handling)
tsHW07_09mod2 <- HoltWinters(ts07_09monthly_Active_Adjusted)


# Forecast and plot
tsHW07_09mod2for <- forecast(tsHW07_09mod2, h = 12)
plot(tsHW07_09mod2for, ylab= "kilowatt", xlab= "Time - Active power")



#--- Forecast weekly data ----

# Data frame for weekly sums
Y07_09weekly <- imputed_var %>% 
  filter(between(Year, 2007, 2009)) %>% 
  group_by(Year, Week) %>% 
  summarise(SumActive = sum(Active_power),
            SumS1 = sum(Submeter1),
            SumS2 = sum(Submeter2),
            SumS3 = sum(Submeter3)) %>% 
  ungroup()


## Create TS object with weekly sum of data - active power
ts07_09weekly_Active <- ts(Y07_09weekly$SumActive, frequency = 52, 
                            start = c(2007,1))


## Decompose active power into trend, seasonal and remainder
components07_09Active_weekly <- decompose(ts07_09weekly_Active)

## Plot decomposed active power
plot(components07_09Active_weekly)

## Check summary statistics for decomposed sub-meter 3 
summary(components07_09Active_weekly)



# Holt-Winters weekly model

## Seasonal adjusting active power by subtracting the seasonal component & plot
ts07_09weekly_Active_Adjusted <- 
  ts07_09weekly_Active - components07_09Active_weekly$seasonal

#Plot
autoplot(ts07_09weekly_Active_Adjusted)

# Check if seasonality has been removed. Decompose again.
plot(decompose(ts07_09weekly_Active_Adjusted))
## Yes, looking at the scales of the seasonal component - seasonality has
## been removed




## Holt Winters Exponential Smoothing & Plot weekly data
tsHW07_09mod3 <- HoltWinters(ts07_09weekly_Active_Adjusted, 
                             beta=FALSE, gamma=FALSE)

## HoltWinters forecast & plot 3rd model
tsHW07_09mod3for <- forecast(tsHW07_09mod3, h = 52)
plot(tsHW07_09mod3for, ylab= "kilowatt", xlab= "Time - Active power")



## 4th model Holt Winters Exponential Smoothing & Plot
## (let beta and gamma be set by default handling)
tsHW07_09mod4 <- HoltWinters(ts07_09weekly_Active_Adjusted)
plot(tsHW07_09mod4, ylab= "kilowatt", xlab= "Time - Active power")


# Forecast
tsHW07_09mod4for <- forecast(tsHW07_09mod4, h = 52)
plot(tsHW07_09mod4for, ylab= "kilowatt", xlab= "Time - Active power")


# 5th Holt-Wintersmodel
tsHW07_09mod5 <- HoltWinters(ts07_09weekly_Active_Adjusted, 
                           alpha = 0.10, beta = 0.01, gamma = 0.3335)

# Forecast
tsHW07_09mod5for <- forecast(tsHW07_09mod5, h = 52)

#Plot
plot(tsHW07_09mod5for, ylab= "kilowatt", xlab= "Time - Active power", 
     ylim = c(0, 18000))


# Same plot using autoplot
autoplot(tsHW07_09mod5for, conf.int = TRUE) + 
  labs(title = "Forecast of active power", y = "kilowatt") + 
  theme_bw() #+ scale_fill_continuous(label = tsHW07_09mod5for$level)



# Function for HW plot for using ggplot2


HWplot <- function(ts_object, n.ahead = 4, CI = .95, 
                 error.ribbon = 'green', line.size = 1){
  
  hw_object <- HoltWinters(ts_object)
  
  forecast <- predict(hw_object, n.ahead = n.ahead, 
                      prediction.interval = T, level = CI)
  
  
  for_values <- data.frame(time = round(time(forecast), 3), 
                           value_forecast = as.data.frame(forecast)$fit, 
                           dev = as.data.frame(forecast)$upr-
                             as.data.frame(forecast)$fit)
  
  fitted_values <- data.frame(time = round(time(hw_object$fitted), 3), 
                              value_fitted = 
                                as.data.frame(hw_object$fitted)$xhat)
  
  actual_values <- data.frame(time = round(time(hw_object$x), 3), 
                              Actual = c(hw_object$x))
  
  
  graphset <- merge(actual_values, fitted_values, by = 'time', all = TRUE)
  graphset <- merge(graphset, for_values, all = TRUE, by = 'time')
  graphset[is.na(graphset$dev), ]$dev <- 0
  
  graphset$Fitted <- c(rep(NA, NROW(graphset)-(NROW(for_values) + 
                                                 NROW(fitted_values))), 
                       fitted_values$value_fitted, for_values$value_forecast)
  
  
  graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id = 'time')
  
  p <- ggplot(graphset.melt, aes(x = time, y = value)) + 
    geom_ribbon(data = graphset, aes(x = time, y = Fitted, 
                                   ymin = Fitted-dev, ymax = Fitted + dev), 
                alpha = .2, fill = error.ribbon) + 
    geom_line(aes(colour = variable), size = line.size) + 
    geom_vline(x = max(actual_values$time), lty = 2) + 
    xlab('Time') + ylab('Value') + 
    opts(legend.position = 'bottom') + 
    scale_colour_hue('')
  
  result.list <- list(plot = p, forecast = for_values, 
                      fitted = fitted_values, actual = actual_values, 
                      hwobject = hw_object)
  return(result.list)
  
}


# Testing the function
HWplot(ts07_09weekly_Active, n.ahead = 52)
  





#--- External data ----

# In this section I will include temperature and electricity pricing data
# to see the relationship between the households electricity consumption and 
# these factors 



#--- Temperature ----
# Creating temperature dataset based on data from
# https://www.worldweatheronline.com/paris-weather-averages/ile-de-france/fr.aspx
# using data from 2009.

temperature <- setNames(data.frame(matrix(ncol = 4, nrow = 12)), 
                        c("month", "avg_temp", "min_temp", "max_temp"))

#Add months
temperature$month <- month(seq(as.Date("2009/1/1"), by = "month", 
                              length.out = 12), 
                          label = TRUE, abbr = FALSE)


#Add average temperature for 2009
temperature$avg_temp <- c(0, 3, 7, 12, 14, 16, 19, 20, 16, 12, 9, 3)
                           

#Min temperatures
temperature$min_temp <- c(-2, 0, 3, 8, 9, 11, 14, 14, 12, 8, 7, 1)


#Max temperature
temperature$max_temp <- rep(c(3, 7, 11, 17, 19, 21, 24, 25, 21, 16, 12, 6), 1)
                            #times = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
                             #         30, 31))   # <- Days per month


# Create data fram with monthly sums of energy consumption and add 
# the temperature data
energy_temp <- mutate(powconsumption %>% 
                       filter(Year == 2009) %>% 
                       group_by(Month) %>% 
                       summarise(SumActive = sum(Global_active_power, na.rm = TRUE),
                                 SumS1 = sum(Sub_metering_1, na.rm = TRUE),
                                 SumS2 = sum(Sub_metering_2, na.rm = TRUE),
                                 SumS3 = sum(Sub_metering_3, na.rm = TRUE)) %>% 
                       ungroup(), Temperature = temperature$avg_temp)


#Graph active power and temperature
ggplot(energy_temp, aes(x = Month)) + 
  geom_line(aes(y = SumActive, colour = "Active power")) + 
  geom_line(aes(y = Temperature*3000, colour = "Temperature")) + 
  labs(title = "Active power per month 2009", x = "", y = "kilowatt") +
  scale_y_continuous(labels = comma,
                     sec.axis = sec_axis(~./3000, name = "Temperature (ᵒC)")) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12), 
                     labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec")) + 
  scale_colour_manual(values = c("brown", "deepskyblue3"), name = "Parameter")





#--- Pricing ----

# Loading dataset with examples of prices
# The prices are collected from: 
# https://www.epexspot.com/en/market-data/dayaheadauction/auction-table/2007-09-30/FR
# Monday 24 September 2007
Prices <- read.csv2("~/Data/Prices_day_2007.csv")


# Creating data frame of sum energy consumption for September 24th 2007
price_chart <- imputed_var %>% 
  filter(Year == 2007 & Month == 9 & Day == 24) %>%  
  select(Hour, Submeter1:Submeter3) %>%
  group_by(Hour) %>% 
  summarise(sumS1 = sum(Submeter1),
            sumS2 = sum(Submeter2),
            sumS3 = sum(Submeter3)) %>%
  ungroup()

# Combine the data frame with the prices
price_chart <- cbind(price_chart, Prices)


# Plot the data
ggplot(price_chart, aes(x = Hour)) + 
  geom_line(aes(y = sumS1, colour = "Kitchen"), group = 1) +
  geom_line(aes(y = sumS2, colour = "Laundry Room"), group = 1) +
  geom_line(aes(y = Price*15, colour = "Price"), group = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Power consumption compared to price variations\nduring a day in September 2007", 
       y = "Watt/Hour") + 
  scale_y_continuous(sec.axis = sec_axis(~./15, name = "Price (€ /MWh)")) +
  guides(colour = guide_legend(title = "Parameter"))






