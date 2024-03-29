################################################################
#IoT Analytics
#Visualize and Analyze Energy Data

#Initial Exploration of the data

#Created by Eirik Espe
################################################################

#Calling on packages. Install the packages if you do not have them already.
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(lubridate)
library(reshape2)


#Import dataset - Household power consumption
# Missing values are recorded as "?"
powconsumption <- read.csv("household_power_consumption.txt", 
                           sep=";", na.strings = "?")


# Structure, head and tail of the dataset
str(powconsumption)
head(powconsumption)
tail(powconsumption)


# Records from the start and end of 2008
head(filter(powconsumption, Date == "1/1/2008"))
tail(filter(powconsumption, Date == "31/12/2008"))


# Number of observations per year:
nrow(filter(powconsumption, str_detect(Date, "2006")))      #  21 996
nrow(filter(powconsumption, str_detect(Date, "2007")))      # 525 600
nrow(filter(powconsumption, str_detect(Date, "2008")))      # 527 040 (leap year)
nrow(filter(powconsumption, str_detect(Date, "2009")))      # 525 600
nrow(filter(powconsumption, str_detect(Date, "2010")))      # 475 023


#--- Date & Time format ----

## Combine Date and Time attribute values in a new attribute column
powconsumption <- cbind(powconsumption, 
                        paste(powconsumption$Date, powconsumption$Time), 
                        stringsAsFactors = FALSE)

## Give the new attribute in the 10th column a name 
colnames(powconsumption)[10] <- "DateTime"

## Move the DateTime attribute within the dataset - to the position in first column
powconsumption <- powconsumption[, c(ncol(powconsumption), 
                                     1:(ncol(powconsumption) - 1))]


# Look at the 6 first rows of the dataset
head(powconsumption)
# Class of the new DateTime variable
class(powconsumption$DateTime)    # Data type is character


## Convert DateTime from character to POSIXct 
powconsumption$DateTime <- as.POSIXct(powconsumption$DateTime, 
                                      "%d/%m/%Y %H:%M:%S", tz = "UTC")


## Inspect the data types
str(powconsumption)


## Create "year" attribute with lubridate
powconsumption$Year <- year(powconsumption$DateTime)


# How many recordings per year
summary(factor(powconsumption$Year))


## Create "quarter" attribute with lubridate
powconsumption$Quarter <- quarter(powconsumption$DateTime)
## Create "month" attribute with lubridate
powconsumption$Month <- month(powconsumption$DateTime)
## Create "week" attribute with lubridate
powconsumption$Week <- week(powconsumption$DateTime)
## Create "weekday" attribute with lubridate
powconsumption$Weekday <- wday(powconsumption$DateTime, 
                               label = TRUE, week_start = 1)
## Create "day" attribute with lubridate
powconsumption$Day <- day(powconsumption$DateTime)
## Create "hour" attribute with lubridate
powconsumption$Hour <- hour(powconsumption$DateTime)
## Create "minute" attribute with lubridate
powconsumption$Minute <- minute(powconsumption$DateTime)



#--- Initial exploration ----

summary(powconsumption)
# The recordings happen between 16/12/2006 and 26/11/2010


#Identify missing values
colSums(is.na(powconsumption))


#Proportion of rows with missing values, looking at global active power
sum(is.na(powconsumption$Global_active_power)) / nrow(powconsumption)

# or all variables expressed as %
summarise_all(powconsumption, list(~100*mean(is.na(.))))

# Missing values are located in the energy usage variables


# Creating a data frame of the missing values
NAs <- powconsumption %>% 
  select(DateTime, Date, Global_active_power:Sub_metering_3) %>% 
  filter(is.na(Global_active_power | Global_reactive_power | Voltage | 
                 Global_intensity | Sub_metering_1 | Sub_metering_2 |
                 Sub_metering_3))


# Plot the missing values
ggplot(NAs) + geom_line(aes(x = as.Date(Date, "%d/%m/%Y"), y = ..count..), 
                        stat = "bin", binwidth = 10)  +
  scale_x_date(labels = date_format("%b %Y"), breaks = '6 months') +
  labs(title = "Distribution of missing values", x = "", y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Number of rows with missing values
nrow(NAs)     #25 979 minutes

#Missing values expressed as number of days
#25979 / 60 / 24
# 18 days and 59 minutes        (18d*24t*60min + 59min = 25979min)



#--- Exploring the sub meters ----

#Create an object for the sub meters
sub_meters <- powconsumption[, 8:10]

summary(sub_meters)



# Identify power consumption that is not captured by the sub meters.
##   First convert global_active_power to watt-hour of active energy
###      Then subtract readings from the sub meters 
sub_meters$Unspecified_consumption <- 
              powconsumption$Global_active_power * 1000/60 -
              (powconsumption$Sub_metering_1 + 
                 powconsumption$Sub_metering_2 + 
                 powconsumption$Sub_metering_3)

#Summary for the unspecified power consumption variable
summary(sub_meters$Unspecified_consumption)


# Number of observations with negative unspecified power consumption (less than 0)
nrow(filter(sub_meters, Unspecified_consumption < 0))    #Because of rounding


# Total power consumption per sub meter
colSums(sub_meters[,1:3], na.rm = TRUE)

# Sum of power consumption measured by the sub meters
sum(colSums(sub_meters[,1:3], na.rm = TRUE))

# Unspecified power consumption
sum(sub_meters$Unspecified_consumption, na.rm = TRUE)



# Total power consumption measured by sub meters in percentage
sum(colSums(sub_meters[,1:3], na.rm = TRUE)) / sum(sub_meters, na.rm = TRUE)
# The sub meters captures 48.8% of the household's power consumption


# Power consumption measured by each sub meter in percentage
colSums(sub_meters[,1:3], na.rm = TRUE) / sum(sub_meters, na.rm = TRUE)
# Sub meter 1: 6.2%         
# Sub meter 2: 7.1%        
# Sub meter 3: 35.5%                                             


# Look for correlation between the submeters
correlationMatrix <- cor(sub_meters, use = "complete.obs")
# No high correlations



#---  Histogram of active power ----

# # Histogram of active power
hist(powconsumption$Global_active_power, col = "paleturquoise4",
     main = "Active Power", xlab = "Global Active Power (kilowatts)")

## Same using ggplot
ggplot(powconsumption, aes(x = Global_active_power)) + 
  geom_histogram(binwidth = 0.5, fill = "paleturquoise4", colour = "black") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  labs(title = "Active Power", x = "Global Active Power (kilowatts)", 
       y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", 
                                               scientific = FALSE))


#Graph of power consumption measured by the submeters
#ggplot(powconsumption, aes(x = DateTime), group_by(Quarter)) + 
#  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "Kitchen"), 
#            alpha = 0.4) + 
#  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "Laundry room"), 
#            alpha = 0.4) + 
#  geom_line(aes(x = DateTime, 
#                y = Sub_metering_3, color = "Water heater / AC"), 
#            alpha = 0.4) + 
#  scale_x_datetime(labels=date_format("%b %y")) + 
#  scale_color_manual(values=c("#CC0000", "#E69F00", "#56B4E9"), 
#                     name = "Submeter") + 
#  labs(title = "Power consumption", x = "", y = "Consumption Watt/Hour") + 
#  theme_bw()



#--- Subsets of the dataset

Y2007 <- filter(powconsumption, Year == 2007)
Y2008 <- filter(powconsumption, Year == 2008)
Y2009 <- filter(powconsumption, Year == 2009)
Y2010 <- filter(powconsumption, Year == 2010)


#---  Seasonal effects  ----
#Graph of seasonal effects on active power consumption during the period

powconsumption %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>% 
  
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "mistyrose3") + 
  geom_smooth(se = F) + 
  scale_x_date(labels=date_format("%b %y")) +
  labs(title = "Mean active energy consumed", 
       x = "Time", y = "kilowatt") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  

#Seasonal effects 2007

Y2007 %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>% 
  
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "mistyrose3") +
  geom_smooth(se = F) + 
  scale_x_date(labels=date_format("%b")) +
  labs(title = "Mean active energy consumed in 2007", 
       x = "Time", y = "kilowatt") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  

#Seasonal effects 2008
Y2008 %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>% 
  
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "mistyrose3") +
  geom_smooth(se = F) + 
  scale_x_date(labels=date_format("%b")) +
  labs(title = "Mean active energy consumed in 2008", 
       x = "Time", y = "kilowatt") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
  

#Seasonal effects 2009
Y2009 %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>% 
  
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "mistyrose3") +
  geom_smooth(se = F) + 
  scale_x_date(labels=date_format("%b")) +
  labs(title = "Mean active energy consumed in 2009", 
       x = "Time", y = "kilowatt") + 
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) 
  
  
  
# Make a data frame with calculations
chart_df <- cbind(sub_meters, 
                  powconsumption$Global_active_power * 1000/60, 
                  powconsumption[, 11:18])
# Active power measured in watt/hour
colnames(chart_df)[5] <- "Active power"



# Checking Monthly Average per minute for the Submeters
## Creating data frame with monthly average energy consumption per submeter
monthly <- chart_df %>% 
  group_by(Month,Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


#Convert data to long format (all sub meter specification in one column and 
#                              all sub meter values (readings) in another column)
submeter_usage <- melt(monthly, id.vars = c("Month", "Year"),
                       variable.name = "meter_reading", 
                       value.name = "readings")

# Sort by year
submeter_usage_ar <- submeter_usage %>%
                      arrange(Year)

# First 6 records
head(submeter_usage_ar)



# Graph - power consumption measured by submeters ----
submeter_usage_ar %>%
  filter(Year > 2006) %>%
  
  ggplot(aes(x = factor(Month), y = readings, 
             group = meter_reading, color = meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption measured by sub-meters", 
       x = "", y = "Mean Watt/Hour") + 
  facet_grid(~Year) +
  scale_x_discrete(breaks = seq(2, 12, by = 3), 
                   labels = c("Jan", "Apr", "Jul", "Oct"))



# Checking Monthly Average per minute for the Submeters, 
# including unspecified consumption

## Same data frame, but including unspecified energy consumption
monthly2 <- chart_df %>% 
  group_by(Month,Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE),
            Avg_unspecified = mean(Unspecified_consumption, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


# Convert data to long format
submeter_usage2 <- melt(monthly2, id.vars = c("Month", "Year"),
                        variable.name = "meter_reading", 
                        value.name = "readings")

# Sort by year
submeter_usage_ar2 <- submeter_usage2 %>%
  arrange(Year)

head(submeter_usage_ar2)


# Adding unspecified consumption to the graph ----

submeter_usage_ar2 %>%
  filter(Year > 2006) %>%

ggplot(aes(x = factor(Month), y = readings, 
                              group = meter_reading, color=meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC", "Unspecified")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption measured by sub-meters", 
       x = "", y = "Mean Watt/Hour") + 
  facet_grid(~Year) +
  scale_x_discrete(breaks = seq(2, 12, by = 3), 
                   labels = c("Jan", "Apr", "Jul", "Oct"))
  



####        Testing for weekdays effect       ####

# Checking weekdays effect in 2007

weekday <- Y2007 %>% 
  group_by(Hour, Weekday, Week, Month, Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


# Convert data to long format
submeter_wkd_usage <- melt(weekday, id.vars = c("Hour", "Weekday", "Week", 
                                                "Month", "Year"),
                       variable.name = "meter_reading", 
                       value.name = "readings")

# Sort by year
submeter_wkd_usage_ar <- submeter_wkd_usage %>%
  arrange(Year)

head(submeter_wkd_usage_ar)


#Graphing weekdays effect 2007

ggplot(submeter_wkd_usage_ar, aes(x = Hour, y = readings, 
           group = meter_reading, color=meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption in 2007, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)




# Looking at a single week

submeter_wkd_usage_ar %>% 
  filter(Week == 35) %>% 
  
  ggplot(aes(x = Hour, y = readings, 
             group = meter_reading, color=meter_reading)) + 
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption week 35 in 2007, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)



# Checking weekdays effect in 2008, measured by Submeters
weekday2008 <- Y2008 %>% 
  group_by(Hour, Weekday, Week, Month, Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


# Convert data to long format
submeter_wkd2008_usage <- melt(weekday2008, id.vars = c("Hour", "Weekday", "Week", 
                                                "Month", "Year"),
                           variable.name = "meter_reading", 
                           value.name = "readings")

# Sort by year
submeter_wkd2008_usage_ar <- submeter_wkd2008_usage %>%
  arrange(Year)

head(submeter_wkd2008_usage_ar)



#Graphing weekdays effect 2008

ggplot(submeter_wkd2008_usage_ar, aes(x = Hour, y = readings, 
                                  group = meter_reading, color=meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption in 2008, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)
