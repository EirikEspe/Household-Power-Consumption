################################################################
#IoT Analytics
#Visualize and Analyze Energy Data


#Created by Eirik Espe
#Last edited 7 June 2019
################################################################

source("Initial Exploration part 2.R")

#Calling on packages
library(plotly)
library(ggfortify)
library(forecast)
library(tseries)

## Data frame for the period from 2007 to end of 2009
Y07_09 <- filter(powconsumption, between(Year, 2007, 2009))

#plot(Y07_09$Sub_metering_1)


# Making some visualisations of random samples

## Subset the second week of 2008 - All Observations
houseweek <- filter(Y07_09, Year == 2008 & Week == 2)

## Plot subset houseweek
plot(houseweek$Sub_metering_1)


## Subset the 9th day of January 2008 - All observations
houseDay <- filter(Y07_09, Year == 2008 & Month == 1 & Day == 9)

## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        type = "scatter", mode = "lines")


## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,
        name = "Kitchen", type = "scatter", mode = "lines") %>%
  add_trace(y = houseDay$Sub_metering_2, name = "Laundry room", 
            mode = "lines") %>%
  add_trace(y = houseDay$Sub_metering_3, name = "Water heater / AC", 
            mode = "lines") %>%
  layout(title = "Power consumption January 9th, 2008",
         xaxis = list(title = "Time"), 
         yaxis = list(title = "Power (Watt/Hours"))



#--- Day - 10 min frequency ----

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10min <- filter(Y07_09, Year == 2008 & Month == 1 & Day == 9 &
                          (Minute == 0 | Minute == 10 | Minute == 20 |
                             Minute == 30 | Minute == 40 | Minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10min, 
        x = ~houseDay10min$DateTime, 
        y = ~houseDay10min$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10min$Sub_metering_2, name = "Laundry room", 
            mode = "lines") %>%
  add_trace(y = ~houseDay10min$Sub_metering_3, name = "Water heater / AC",
            mode = "lines") %>%
  layout(title = "Power consumption January 9th, 2008", 
         xaxis = list(title = "Time"),
         yaxis = list(title = "Power (Watt/Hours"))




#--- Week - Hour frequency ----

## Subset the second week of 2008 - Hour frequency
houseWeekHour <- filter(Y07_09, Year == 2008 & Week == 2 & Minute == 0)

## Plot sub-meter 1, 2 and 3 - Hour frequency
plot_ly(houseWeekHour, 
        x = ~houseWeekHour$DateTime, 
        y = ~houseWeekHour$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekHour$Sub_metering_2, name = "Laundry room", 
            mode = "lines") %>%
  add_trace(y = ~houseWeekHour$Sub_metering_3, name = "Water heater / AC",
            mode = "lines") %>%
  layout(title = "Power consumption week 2, 2008", 
         xaxis = list(
           type = 'date',
           #tickformat = "%d %B (%a)<br>%Y",
           title = "Time"),
         yaxis = list(title = "Power (Watt/Hours"))


#--- Week 45 2008, every second hour ----

## Subset year 2008 - Week frequency
houseYear <- filter(Y07_09, Year == 2008 & Week == 45 &
                      (Hour == 2 | Hour == 4 | Hour == 6 | Hour == 8 | 
                         Hour == 10 | Hour == 12 | Hour == 14 | Hour == 16 | 
                         Hour == 18 | Hour == 20 | Hour == 22) & 
                      Minute == 0)

## Plot sub-meter 1, 2 and 3 
plot_ly(houseYear, 
        x = ~houseYear$DateTime, 
        y = ~houseYear$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseYear$Sub_metering_2, name = "Laundry room", 
            mode = "lines") %>%
  add_trace(y = ~houseYear$Sub_metering_3, name = "Water heater / AC",
            mode = "lines") %>%
  layout(title = "Power consumption every second hour - week 45 in 2008", 
         xaxis = list(
           type = 'date',
           title = "Time"),
         yaxis = list(title = "Power (Watt/Hours)"))


# Sum monthly consumption per submeter between 2007 and 2009
Y07_09 %>%
  group_by(Month, Year) %>%
  summarise(sumS1 = sum(Sub_metering_1, na.rm = TRUE),
            sumS2 = sum(Sub_metering_2, na.rm = TRUE),
            sumS3 = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>%
  
  ggplot(aes(x = Month)) + 
  geom_line(aes(y = sumS1, colour = "Kitchen")) +
  geom_line(aes(y = sumS2, colour = "Laundry room")) +
  geom_line(aes(y = sumS3, colour = "Water heater / AC")) +
  labs(title = "Monthly sum per submeter", x = "Month", y = "Watt/Hours") +
  guides(colour = guide_legend(title = "Submeter")) +
  facet_wrap(~Year)



#--- Pie chart ----

# Making a dataframe for pie chart
piechart_df <- melt(sub_meters, variable.name = "meter_reading", 
                    value.name = "readings" )


plot_ly(piechart_df, labels = ~meter_reading, values = ~readings, 
        type = 'pie') %>%
  
  layout(title = 'Power consumption per Submeter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                      showticklabels = FALSE))


#--- Time series ----

## Subset to one observation per week on Mondays at 20:00 for 2007, 2008 and 2009
house07_09weekly <- filter(Y07_09, Weekday == "Mon" & Hour == 20 & Minute == 0 )

## Create TS object with Submeter3
tshouse07_09weekly_s3 <- ts(house07_09weekly$Sub_metering_3, frequency = 52,
                            start = c(2007, 1))


## Plot sub-meter 3 with autoplot
autoplot(tshouse07_09weekly_s3)         #from the package ggfortify

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tshouse07_09weekly_s3, colour = 'red', main = "Sub-meter 3", 
                                                   xlab = "Time", 
                                                   ylab = "Watt/Hours")


## Plot sub-meter 3 with plot.ts
plot.ts(tshouse07_09weekly_s3)


## Plot sub-meter 1 with autoplot - add labels, color
autoplot(ts(house07_09weekly$Sub_metering_1, frequency = 52,
            start = c(2007, 1)), colour = 'blue', main = "Sub-meter 1", 
         xlab = "Time", 
         ylab = "Watt/Hours")


## Plot sub-meter 2 with autoplot - add labels, color
autoplot(ts(house07_09weekly$Sub_metering_2, frequency = 52,
            start = c(2007, 1)), colour = 'green', main = "Sub-meter 2", 
         xlab = "Time", 
         ylab = "Watt/Hours")




#--- Forecasting ----

## Apply time series linear regression to the sub-meter 3 ts object and 
#   use summary to obtain R2 and RMSE from the model you built

fitS3 <- tslm(tshouse07_09weekly_s3 ~ trend + season) #from the package forecast

## Taking a look at the model output
summary(fitS3)


## Create the forecast for sub-meter 3. Forecast ahead 20 time periods
forecastFitS3 <- forecast(fitS3, h = 20)

## Plot the forecast for sub-meter 3
plot(forecastFitS3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastFitS3c <- forecast(fitS3, h = 20, level = c(80, 90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastFitS3c, ylim = c(0, 20), ylab = "Watt/Hours", xlab = "Time")
# optional addidtion shadecols = ... <= desired colours for confidence levels


#Submeter 1
tshouse07_09weekly_s1 <- ts(house07_09weekly$Sub_metering_1, frequency = 52,
                            start = c(2007, 1))

plot(forecast(tslm(tshouse07_09weekly_s1 ~ trend + season),
              h = 20, level = c(80, 90)), 
     ylim = c(0, 50), ylab = "Watt/Hours", xlab = "Time")


#Submeter 2
tshouse07_09weekly_s2 <- ts(house07_09weekly$Sub_metering_2, frequency = 52,
                            start = c(2007, 1))


plot(forecast(tslm(tshouse07_09weekly_s1 ~ trend + season),
              h = 20, level = c(80, 90)), 
     ylim = c(0, 50), ylab = "Watt/Hours", xlab = "Time")


#--- Add legend to plot from base R ---- 

# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
# Add legend
legend("topright", inset = c(-0.2, 0.2), 
       legend = c("80 %", "90 %"),
       fill=c("#B1B5CE", "#CFD1DB"), title="Confidence level")


#--- Decomposing seasonal time series ----

## Decompose Sub-meter 3 into trend, seasonal and remainder
components07_09S3weekly <- decompose(tshouse07_09weekly_s3)

## Plot decomposed sub-meter 3 
plot(components07_09S3weekly)

## Check summary statistics for decomposed sub-meter 3 
summary(components07_09S3weekly)


## Decompose Sub-meter 1 into trend, seasonal and remainder and plot the output
plot(decompose(tshouse07_09weekly_s1))
summary(decompose(tshouse07_09weekly_s1))

## Decompose Sub-meter 2 into trend, seasonal and remainder and plot the output
plot(decompose(tshouse07_09weekly_s2))
summary(decompose(tshouse07_09weekly_s2))


#--- Remove seasonal components ----

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
ts07_09s3Adjusted <- tshouse07_09weekly_s3 - components07_09S3weekly$seasonal
autoplot(ts07_09s3Adjusted)


## Test Seasonal Adjustment by running Decompose again. 
#     Note the very, very small scale for Seasonal
plot(decompose(ts07_09s3Adjusted))

## For all practical purposes the seasonality has been removed.


#--- Holt-Winters filtering & Forecasting ----

## Holt Winters Exponential Smoothing & Plot
ts07_09hwS3 <- HoltWinters(ts07_09s3Adjusted, beta = FALSE, gamma = FALSE)
plot(ts07_09hwS3, ylim = c(0, 25))

## HoltWinters forecast & plot
ts07_09hwS3Forecast <- forecast(ts07_09hwS3, h = 25)
plot(ts07_09hwS3Forecast, ylim = c(0, 25), 
     ylab = "Watt/Hours", xlab = "Time - Submeter 3")

## Forecast HoltWinters with diminished confidence levels
ts07_09hwS3ForecastC <- forecast(ts07_09hwS3, h = 25, level = c(10, 25))


## Plot only the forecasted area
plot(ts07_09hwS3ForecastC, ylim = c(0, 25), 
     ylab = "Watt/Hours", xlab = "Time - Submeter 3",
     start(2010))


#--- Add legend to plot from base R ---- 

# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
# Add legend
legend("topright", inset = c(-0.2, 0.2), 
       legend = c("10 %", "25 %"),
       fill = c("#3C53A6", "#6875B1"), title = "Confidence level")



# Sum consumption per submeter per day between 2007 and 2009
Y07_09 %>%
  group_by(Day = day(DateTime),
           Month = month(DateTime),
           Year = year(DateTime)) %>%
  summarise(SM_Kitchen = sum(Sub_metering_1),
            SM_Laundry = sum(Sub_metering_2),
            SM_HeaterAC = sum(Sub_metering_3)) %>% 
  ungroup() %>%
  mutate(YMD = paste(Year,Month,Day, sep = "-"),
         YMD = as.POSIXct(YMD, "%Y-%m-%d")) %>%
  arrange(YMD) %>%

#plot all SMs for 3 years power summed by day
plot_ly(x = ~YMD, y = ~SM_Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~SM_Laundry, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~SM_HeaterAC, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Daily Power Consumption over 3 years",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (Watt/Hours)"))



#--- Minimum consumption ----

## Looking at week with lowest power consumption
Y07_09[which.min(Y07_09$Global_active_power),]



# Identify power consumption that is not captured by the sub meters.
##   First convert global_active_power to watt-hour of active energy
###      Then subtract readings from the sub meters 
Daily <- mutate(powconsumption, Unspecified_consumption = 
                  Global_active_power * 1000/60 - 
                  (Sub_metering_1 + Sub_metering_2 + Sub_metering_3))

#Reordering the new unspecified power consumption variable to after Submeter 3
Daily <- select(Daily, 
                DateTime:Sub_metering_3, 
                Unspecified_consumption, 
                everything())


# Subset the data with sum of power consumption for each day
Daily <- Daily %>%
  group_by(Year, Month, Week, Day) %>% 
  summarise(SumActive = sum(Global_active_power, na.rm = TRUE),
            sumReactive = sum(Global_reactive_power, na.rm = TRUE),
            sumVoltage = sum(Voltage, na.rm = TRUE),
            sumIntensity = sum(Global_intensity, na.rm = TRUE),
            sumS1 = sum(Sub_metering_1, na.rm = TRUE),
            sumS2 = sum(Sub_metering_2, na.rm = TRUE),
            sumS3 = sum(Sub_metering_3, na.rm = TRUE),
            sumUnspecified = sum(Unspecified_consumption, na.rm = TRUE)) %>%
  ungroup()

## Looking at day with lowest power consumption
Daily[which.min(Daily$SumActive),]
# 29 April 2007

min(Daily$SumActive)
# 0

#Create weekday variable by pasting Year, month and day
Daily$Weekday <- wday(format(as.Date(
  paste(Daily$Year, Daily$Month, Daily$Day), "%Y %m %d")), 
  label = TRUE)

#Reordering
Daily <- select(Daily, Year:Week, Weekday,everything())

#Sum data for the first day to compare with the sums created in the Daily dataset
colSums(powconsumption %>% select_if(is.numeric) %>% filter(Year == 2006 & Day == 16))

identical(colSums(powconsumption %>% 
                    select_if(is.numeric) %>% 
                    filter(Year == 2006 & Day == 16)), Daily[1, ])
# False, data is not identical
# Turns out that the occasions where sum Acti Power equals 0 is when we
# have missig data


# Investigating original data in the cases where sumActive equals 0 
investigate <- filter(powconsumption, (Year == 2007 & Month == 4 & Day == 29) | 
                        (Year == 2009 & Month == 6 & Day == 14) | 
                        (Year == 2010 & Month == 1 & Day == 13) | 
                        (Year == 2010 & Month == 8 & Day == 18) | 
                        (Year == 2010 & Month == 8 & Day == 19) | 
                        (Year == 2010 & Month == 8 & Day == 20) | 
                        (Year == 2010 & Month == 8 & Day == 21) | 
                        (Year == 2010 & Month == 9 & Day == 26) | 
                        (Year == 2010 & Month == 9 & Day == 27))
# these are missing values

# Lowest value of available data
sort(Daily$SumActive, decreasing = FALSE)[10]

# Checking what day this was
Daily[which(Daily$SumActive == 14.218),]
# Saturday 13 June 2009


## Before handling missing values
# Top 5 min values
sort(Daily$SumActive, decreasing = FALSE)[10:14]
# Top 5 max values
sort(Daily$SumActive, decreasing = TRUE)[1:5]



#--- Missing values ----
NAs <- powconsumption %>% 
  select(DateTime, Date, Global_active_power:Sub_metering_3) %>% 
  filter(is.na(Global_active_power | Global_reactive_power | Voltage | 
                 Global_intensity | Sub_metering_1 | Sub_metering_2 |
                 Sub_metering_3))

ggplot(NAs) + geom_line(aes(x = as.Date(Date, "%d/%m/%Y"), y = ..count..), 
                        stat = "bin", binwidth = 10)  +
  scale_x_date(labels = date_format("%b %Y"), breaks = '6 months') +
  labs(title = "Distribution of missing values", x = "", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Missing values expressed as number of days
#25979 / 60 /24
# 18 days and 59 minutes        (18d*24t*60min + 59min = 25979min)

# Longest range of missing values from year 2007 to 2009 was 3723 minutes
#   = 62h and 3 minutes = 2 days, 14h and 3 min



##Subset of 2 random days - the same date for a day in January 2008 and 2010 ----
## Looking at submeter 3

# Max value from submeter 3 in January 2008
Daily %>% filter(Month == 1 & Year == 2008) %>% select(sumS3) %>% max

# Which day
Daily[which(Daily$sumS3 == 15606),]
# Saturday 26 January 2008

# In which day can I find this observation
which(Daily$sumS3 == 15606)
# Rownumber 407

# Other data from same time interval
Daily[407, c(1:5, 11)]


# Choosing another observation

# Plotting sum consumption from submeter 3 - January 10th 2008
powconsumption %>%
  filter(Year == 2008 & Month == 1 & Day == 10) %>%
  group_by(Hour) %>% 
  summarise(Sum_kitchen = sum(Sub_metering_1, na.rm = TRUE),
            Sum_laundryroom = sum(Sub_metering_2, na.rm = TRUE),
            Sum_water_ac = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  
  ggplot(aes(x = Hour, y = Sum_water_ac)) + geom_col() + 
  labs(title = "Submeter 3 consumption January 10th, 2008", y = "Watt/Hours")

# January 10th 2008
## Data frame with sum consumptions from submeters
powconsumption %>%
  filter(Year == 2008 & Month == 1 & Day == 10) %>%
  group_by(Hour) %>% 
  summarise(Sum_kitchen = sum(Sub_metering_1, na.rm = TRUE),
         Sum_laundryroom = sum(Sub_metering_2, na.rm = TRUE),
         Sum_water_ac = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() -> Day_jan2008


# January 10th 2010
## Data frame with sum consumptions from submeters
powconsumption %>%
  filter(Year == 2010 & Month == 1 & Day == 10) %>%
  group_by(Hour) %>% 
  summarise(Sum_kitchen = sum(Sub_metering_1, na.rm = TRUE),
            Sum_laundryroom = sum(Sub_metering_2, na.rm = TRUE),
            Sum_water_ac = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() -> Day_jan2010


# Combine the observations from 2008 and 2010
Jan10th <- cbind(Day_jan2010, Day_jan2008$Sum_water_ac)


## Plot both days, plotly
  
plot_ly(Jan10th, x = ~Hour, y = ~Sum_water_ac, name = '2010',
        type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~Hour, y = ~Day_jan2008$Sum_water_ac, 
            name = '2008', mode = 'lines') %>% 
  layout(title = "Power Consumption January 10th, 2010",
         xaxis = list(title = "Hour"),
         yaxis = list (title = "Watt/Hours"))
  

## Same plot with ggplot
ggplot(Jan10th) + geom_line(aes(x = Hour, y = Sum_water_ac, colour = "2010")) +
  geom_line(aes(x = Hour, y = Day_jan2008$Sum_water_ac, colour = "2008")) +
  labs(title = "Submeter 3 consumption January 10th", y = "Watt/Hours") +
  guides(colour = guide_legend(title = "Year"))

# The plots show a higher baseline consumption in 2010 than in 2008.
# Maybe an extra appliance was added to the submeter


# See if this is consistent through the whole week
## Week 2, January 2008
powconsumption %>%
  filter(Year == 2008 & Week == 2) %>%
  group_by(Hour, Weekday) %>% 
  summarise(Sum_kitchen = sum(Sub_metering_1, na.rm = TRUE),
            Sum_laundryroom = sum(Sub_metering_2, na.rm = TRUE),
            Sum_water_ac = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() -> Week_jan2008
  

## Week 2, January 2010
powconsumption %>%
  filter(Year == 2010 & Week == 2) %>%
  group_by(Hour, Weekday) %>% 
  summarise(Sum_kitchen = sum(Sub_metering_1, na.rm = TRUE),
            Sum_laundryroom = sum(Sub_metering_2, na.rm = TRUE),
            Sum_water_ac = sum(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() -> Week_jan2010

## Combine the submeter 3 data from 2008 to the data frame for Week 2 in 2010
Week2jan <- cbind(Week_jan2010, Week_jan2008$Sum_water_ac)


## Plot with ggplot
ggplot(Week2jan) + geom_line(aes(x = Hour, y = Sum_water_ac, colour = "2010")) +
  geom_line(aes(x = Hour, y = Week_jan2008$Sum_water_ac, colour = "2008")) +
  labs(title = "Submeter 3 consumption January, week 2", y = "Watt/Hours") +
  guides(colour = guide_legend(title = "Year")) +
  facet_wrap(~Weekday)

# Except from Wednesday and Thursday, where the 2010 consumption is almost 0,
# we can see that the baseline consumption in 2010 is higher.


