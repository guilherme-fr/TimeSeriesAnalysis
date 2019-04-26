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
  
  #Changing Date and Time to appropriate data type
  energyData <- mutate(energyData, 
                       Date = chron(dates = Date, format = "y-m-d"), 
                       Time = chron(times = Time))
  
  #Storing the hours to make it easyer to split into period of the day in code below
  hours <- chron::hours(energyData$Time)
  
  energyData <- energyData %>% 
    
    #Ordering by Date and time
    arrange(Date, Time) %>% 
    
    #New columns for Month, Week day and Season
    mutate(Month = months(Date), Week_day = weekdays(Date), Season = as.factor(getSeason(Date))) %>%
    
    #Nem column for period of the day
    mutate(Period_of_day = ifelse(hours >= 0 & hours < 6, "Dawn", 
                                  ifelse(hours >= 6 & hours < 12, "Morning", 
                                         ifelse(hours >= 12 & hours < 18, "Afternoon", "Night")))) %>%
    
    #Nem column for energy consumption (kWh)
    mutate(Global_energy = Global_active_power * (1/60)) %>%
    
    #New column for energy consumption not measured by any sub-meterings (Wh)
    mutate(Reminder_energy = 
             Global_active_power * 1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
    
  
  energyData
}