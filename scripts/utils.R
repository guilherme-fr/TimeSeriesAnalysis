getSeason <- function(dates, north_hemisphere = TRUE) {
  season <- NULL
  
  if (north_hemisphere) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  } else {
    SS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Fall Equinox
    WS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Spring Equinox
  }
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

featureEngineering <- function(energyData) {
  
  energyData <- mutate(energyData, 
                       Date_time = as.POSIXct(paste(Date, Time, sep = " "), 
                                                format = "%Y-%m-%d %H:%M:%S", 
                                              tz = "Europe/Paris"))
  #Filling the missing minutes of the dataset
  energyData <- pad(energyData, by = "Date_time", break_above = 3)
  
  #Changing Date and Time to appropriate data type
  energyData <- mutate(energyData, 
                       Date = chron(dates = Date, format = "y-m-d"),
                       Time = chron(times = Time))
  
  #Filling the Date and Time NAs produced in pad function
  na_date_index <- which(is.na(energyData$Date))
  energyData$Date[na_date_index] <- chron(dates = 
                                            strftime(energyData$Date_time[na_date_index], 
                                                     format = "%Y-%m-%d"),
                                          format = "y-m-d")
  
  na_time_index <- which(is.na(energyData$Time))
  energyData$Time[na_time_index] <- chron(times = strftime(energyData$Date_time[na_time_index], 
                                                           format = "%H:%M:%S"))
  
  #Storing the hours to make it easyer to split into period of the day in code below
  hours <- chron::hours(energyData$Time)
  
  mean_global_active_power <- mean(energyData$Global_active_power, na.rm = TRUE)
  
  energyData <- energyData %>% 
    
    #New columns for Month, Week day and Season
    mutate(Month = months(Date), 
           Week_day = weekdays(Date), 
           Season = as.factor(getSeason(Date))) %>%
    
    #New column for period of the day
    mutate(Period_of_day = ifelse(hours >= 0 & hours < 6, "Dawn", 
                                  ifelse(hours >= 6 & hours < 12, "Morning", 
                                         ifelse(hours >= 12 & hours < 18, "Afternoon", "Night")))) %>%
    
    #Filling the missing values of Global_active_power with the mean
    mutate(Global_active_power = ifelse(is.na(Global_active_power), 
                                  mean_global_active_power, Global_active_power)) %>%
    
    #New column for energy consumption (kWh)
    mutate(Global_energy = Global_active_power * (1/60)) %>%
    
    #New column for energy consumption not measured by any sub-meterings (Wh)
    mutate(Reminder_energy = 
             Global_active_power * 1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3) %>%
    
    #New column with the average cost
    mutate(Cost = Global_energy * 0.1754) %>%
    
    #Ordering by Date and time
    arrange(Date, Time)
  
  energyData
}

totalEnergyConsumTimeSeries <- function(energyData, granularity = "month", ...) {
  time_series <- NULL
  if (tolower(granularity) == "day") {
    energyGrouped <- totalEnergyConsumByDay(energyData)
    time_series <- ts(energyGrouped$Total_energy_day, frequency = 365, ...)
  } else if (tolower(granularity) == "week") {
    energyGrouped <- totalEnergyConsumByWeek(energyData)
    time_series <- ts(energyGrouped$Total_energy_week, frequency = 52, ...)
  } else if (tolower(granularity) == "month") {
    energyGrouped <- totalEnergyConsumByMonth(energyData)
    time_series <- ts(energyGrouped$Total_energy_month, frequency = 12, ...)
  } else {
    energyGrouped <- totalEnergyConsumByMonth(energyData)
    time_series <- ts(energyGrouped$Total_energy_month, frequency = 12, ...)
  }
  
  time_series
}

totalCostTimeSeries <- function(energyData, granularity = "month", ...) {
  time_series <- NULL
  if (tolower(granularity) == "day") {
    costGrouped <- totalCostByDay(energyData)
    time_series <- ts(costGrouped$Total_cost_day, frequency = 365, ...)
  } else if (tolower(granularity) == "week") {
    costGrouped <- totalCostByWeek(energyData)
    time_series <- ts(costGrouped$Total_cost_week, frequency = 52, ...)
  } else if (tolower(granularity) == "month") {
    costGrouped <- totalCostByMonth(energyData)
    time_series <- ts(costGrouped$Total_cost_month, frequency = 12, ...)
  } else {
    costGrouped <- totalCostByMonth(energyData)
    time_series <- ts(costGrouped$Total_cost_month, frequency = 12, ...)
  }
  
  time_series
}

splitTrainTestTimeSeries <- function(time_series, start_train, end_train, start_test, end_test) {
  train <- window(time_series, start = start_train, end = end_train)
  test <- window(time_series, start = start_test, end = end_test)
  
  split <- list(train = train, test = test)
  
  split
}