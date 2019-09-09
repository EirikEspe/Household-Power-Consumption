################################################################
#IoT Analytics
#Visualize and Analyze Energy Data

#Energy consumption during Holidays

#Created by Eirik Espe
################################################################

source("Forecasting and external data.R")


# Add unspecified consumption to impute_var data frame
imputed_var <- mutate(imputed_var, Unspecified_consumption = 
                        Active_power * 1000/60 - 
                        (Submeter1 + Submeter2 + Submeter3))


# Create a data frame summed by day
Daily_imputed <- imputed_var %>%
  group_by(Year, Month, Week, Day) %>% 
  summarise(sumActive = sum(Active_power),
            sumVoltage = sum(VoltageImp),
            sumIntensity = sum(Intensity),
            sumS1 = sum(Submeter1),
            sumS2 = sum(Submeter2),
            sumS3 = sum(Submeter3),
            sumUnspecified = sum(Unspecified_consumption)) %>%
  ungroup()


## Looking at day with lowest power consumption
Daily_imputed[which.min(Daily_imputed$sumActive),]
# 25 August 2008


#Graph the day
label <- factor(c("Submeter 1", "Submeter 2", "Submeter 3", "Unspecified"))


imputed_var %>% 
  filter(Year == 2008 & Week == 34 & Day == 25 &
           (Minute = 0 | Minute == 10 | Minute == 20 | Minute == 30 | 
              Minute == 40 | Minute == 50)) %>% 
  select(DateTime, Weekday, Hour, 
         Submeter1:Unspecified_consumption) %>% 
  
  
  melt(id.vars = c("DateTime", "Weekday", "Hour"),
       variable.name = "meter_reading", 
       value.name = "readings") %>% 
  
  ggplot() + geom_col(aes(x = Hour, y = readings, 
                           fill = meter_reading)) +
  scale_fill_manual(name = "Submeter", 
                    labels = c("Kitchen", "Laundry Room", 
                               "Water heater & AC", "Unspecified"), 
                    values = c("cyan4", "darkkhaki", "lightsteelblue3", 
                               "seashell3")) +
  facet_grid(~label[meter_reading]) + 
  labs(title = "Power consumption August 25th, 2008 ", y = "Watt/Hour")
  



# Create a data frame summed by week to look for holiday

# Graphing to look for electricity consumption during holiday
Weekly_imputed <- imputed_var %>%
  group_by(Year, Month, Week) %>% 
  summarise(sumActive = sum(Active_power),
            sumVoltage = sum(VoltageImp),
            sumIntensity = sum(Intensity),
            sumS1 = sum(Submeter1),
            sumS2 = sum(Submeter2),
            sumS3 = sum(Submeter3),
            sumUnspecified = sum(Unspecified_consumption)) %>%
  ungroup()

## Looking at week with lowest power consumption
Weekly_imputed[which.min(Weekly_imputed$sumActive),]


# Graph
imputed_var %>% 
  filter(Year == 2008 & between(Week, 14, 15)) %>%
  group_by(Date) %>% 
  summarise(sumS1 = sum(Submeter1),
            sumS2 = sum(Submeter2),
            sumS3 = sum(Submeter3),
            sumUnspecified = sum(Unspecified_consumption)) %>% 
  ungroup() %>% 
  
  
  melt(id.vars = c("Date"),
       variable.name = "meter_reading", 
       value.name = "readings") %>% 
  
  ggplot() + geom_line(aes(x = as.Date(Date, "%d/%m/%Y"), y = readings,
                           colour = meter_reading)) +
  scale_x_date(name = "", labels = date_format("%b %d"), breaks = "day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption week 14 and 15, 2008 ", y = "Watt/Hour") + 
  scale_colour_discrete(name = "Submeter", 
                       label = c("Kitchen", "Laundry Room", 
                                 "Water heater & AC", "Unspecified"))
# facet_grid(~label[meter_reading])





#--- Sequence testing ----


# Finding sequences when the submeter in the kitchen equals 0 (Sumeter 1 = 0)

# Sequences of submeter 1 (kitchen). Starting with sequences where 
# Submeter 1 equal 0, and continues to max value of Submeter 1, sorted by the 
# values of Submeter 1
SequenceS1 <- with(rle(imputed_var$Submeter1), 
                   data.frame(number = values,
                              start = cumsum(lengths) - lengths + 1,
                              end = cumsum(lengths))[order(values),])


# Including length of the sequence to the data frame
SequenceS1 <- (mutate(SequenceS1, length = end - start + 1))
# ^ With this data frame you can look up rownumbers in data frame imputed_var


# Find the 10 longest sequences for submeter 1
head(sort(SequenceS1$length, decreasing = TRUE), 10)
# This output shows that the longest sequence of Submeter 1 is 37 912 rows, and so on.


# Longest sequence
SequenceS1 %>% filter(length == 37912)
# starting at row 860 642 and stopping at 898 553


# Finding DateTime of this sequence
## Starting at
powconsumption[860642, "DateTime"]
# or using dplyr:   powconsumption %>% slice(860642) %>% select(DateTime)


## Ending at
powconsumption[898553, "DateTime"]

# That is from 5 August 2008 at 09:25:00 to 31 August 2008 at 17:16:00




# Graphing the Summer holidays 2008, including consumption before and after
# the start of the holiday

## Sum energy consumption per day
imputed_var %>% 
  filter(Year == 2008 & Month == 8 & between(Day, 4, 31)) %>% 
  #between(Week, 32, 35)) %>%
  group_by(Date) %>% 
  summarise(sumS1 = sum(Submeter1),
            sumS2 = sum(Submeter2),
            sumS3 = sum(Submeter3),
            sumUnspecified = sum(Unspecified_consumption)) %>% 
  ungroup() %>% 
  
  
  ## Converting the data to long format
  melt(id.vars = c("Date"),
       variable.name = "meter_reading", 
       value.name = "readings") %>% 
  
  ## Plotting
  ggplot() + geom_line(aes(x = as.Date(Date, "%d/%m/%Y"), y = readings,
                           colour = meter_reading)) +
  scale_x_date(name = "", labels = date_format("%b %d"), breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Power consumption August 2008 ", y = "Watt/Hour") + 
  scale_colour_discrete(name = "Submeter", 
                        label = c("Kitchen", "Laundry Room", 
                                  "Water heater & AC", "Unspecified"))




