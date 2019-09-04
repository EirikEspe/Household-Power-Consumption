################################################################
#IoT Analytics
#Visualize and Analyze Energy Data

#Initial Exploration of the data part 2

#Created by Eirik Espe
################################################################

source("Initial Exploration part 1.R")

# Checking weekdays effect in 2009, measured by Submeters
weekday2009 <- Y2009 %>% 
  group_by(Hour, Weekday, Week, Month, Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


# Convert data to long format (all sub meter specification in one column and 
#                               all sub meter values (readings) in another column)
submeter_wkd2009_usage <- melt(weekday2009, id.vars = c("Hour", "Weekday", "Week", 
                                                        "Month", "Year"),
                               variable.name = "meter_reading", 
                               value.name = "readings")


# Sort by year
submeter_wkd2009_usage_ar <- submeter_wkd2009_usage %>%
  arrange(Year)


# See first 6 recordings
head(submeter_wkd2009_usage_ar)


#Graphing weekdays effect 2009

ggplot(submeter_wkd2009_usage_ar, aes(x = Hour, y = readings, 
                                      group = meter_reading, color=meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption in 2009, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)



# Find max value for active power
chart_df[which.max(chart_df$`Active power`), ]
# Sunday, 22 February 2009, Week 8


# Find max value laundry room
chart_df[which.max(chart_df$Sub_metering_2), ]


# Find max value water heater / AC
chart_df[which.max(chart_df$Sub_metering_3), ]


# Sum power consumption sub-meter 1
sum(chart_df$Sub_metering_1, na.rm = TRUE)
# Sum power consumption sub-meter 2
sum(chart_df$Sub_metering_2, na.rm = TRUE)
# Sum power consumption sub-meter 3
sum(chart_df$Sub_metering_3, na.rm = TRUE)
# Sum power consumption unspecified
sum(chart_df$Unspecified_consumption, na.rm = TRUE)




#--- Percentages ----

# Sub-meter 1
sum(chart_df$Sub_metering_1, na.rm = TRUE) / sum(chart_df$`Active power`, 
                                                 na.rm = TRUE)    # 6.2%
# Sub-meter 2
sum(chart_df$Sub_metering_2, na.rm = TRUE) / sum(chart_df$`Active power`, 
                                                 na.rm = TRUE)    # 7.1%
# Sub-meter 3
sum(chart_df$Sub_metering_3, na.rm = TRUE) / sum(chart_df$`Active power`, 
                                                 na.rm = TRUE)    # 35.5%
# Unspecified
sum(chart_df$Unspecified_consumption, na.rm = TRUE) / sum(chart_df$`Active power`, 
                                                          na.rm = TRUE)
                                                                  # 51.2%



# Looking at week with highest observed Active power consumption

submeter_wkd2009_usage_ar %>% 
  filter(Week == 8) %>% 
  
  ggplot(aes(x = Hour, y = readings, 
             group = meter_reading, color=meter_reading)) + 
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption week 8 in 2009, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)


# Looking at week with an observation of high power consumption
weekday2010 <- Y2010 %>% 
  group_by(Hour, Weekday, Week, Month, Year) %>% 
  summarise(Avg_kitchen = mean(Sub_metering_1, na.rm = TRUE),
            Avg_laundryroom = mean(Sub_metering_2, na.rm = TRUE),
            Avg_water_ac = mean(Sub_metering_3, na.rm = TRUE)) %>%
  ungroup() %>% 
  arrange(Year)


# Convert data to long format
high_consumption_week <- melt(weekday2010, id.vars = c("Hour", "Weekday", "Week", 
                                                "Month", "Year"),
                           variable.name = "meter_reading", 
                           value.name = "readings")

# Graph

high_consumption_week %>% 
  filter(Week == 30) %>% 

ggplot(aes(x = Hour, y = readings, 
                                  group = meter_reading, color=meter_reading)) +
  geom_line() + 
  scale_color_discrete(name = "Submeter", 
                       labels = c("Kitchen", "Laundry room", 
                                  "Water heater / AC")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption week 30 in 2010, measured by sub-meters", 
       x = "Hour of day", y = "Mean Watt/Hour") + 
  facet_grid(meter_reading ~ Weekday)





