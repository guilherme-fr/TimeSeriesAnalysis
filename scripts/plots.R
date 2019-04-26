
#Average Energy Consumption for each day of week in a year
avgEnergyForDay <- function(yrPrepData) {
  
  #tempData holds the total energy consumption for all week days in the data
  tempData <- yrPrepData %>% 
    mutate(Week = week(Date)) %>%
    group_by(Week, Week_day) %>%
    summarise(Total_energy = sum(Global_energy),
              Energy_sub_1 = sum(Sub_metering_1),
              Energy_sub_2 = sum(Sub_metering_2),
              Energy_sub_3 = sum(Sub_metering_3),
              Energy_reminder = sum(Reminder_energy))
  
  #avgEnerg_dar holds the average energy consumption for each day in a week regarding the whole
  #data period
  avgEnerg_day <- tempData %>% 
    group_by(Week_day) %>% 
    summarise(Total_energy_avg = mean(Total_energy),
              Energy_Sub1_avg = mean(Energy_sub_1),
              Energy_Sub2_avg = mean(Energy_sub_2),
              Energy_Sub3_avg = mean(Energy_sub_3),
              Energy_reminder_avg = mean(Energy_reminder))
  
  avgEnerg_day
}

totalEnergyForMonths <- function(yrPrepData) {
  energy_month <- yrPrepData %>% 
    group_by(Month) %>% 
    summarise(Total_energy = sum(Global_energy),
              Energy_sub_1 = sum(Sub_metering_1),
              Energy_sub_2 = sum(Sub_metering_2),
              Energy_sub_3 = sum(Sub_metering_3),
              Energy_reminder = sum(Reminder_energy))
  
  energy_month
}

totalEnergyForSeasons <- function(yrPrepData) {
  energy_season <- yrPrepData %>% 
    group_by(Season) %>% 
    summarise(Total_energy = sum(Global_energy),
              Energy_sub_1 = sum(Sub_metering_1),
              Energy_sub_2 = sum(Sub_metering_2),
              Energy_sub_3 = sum(Sub_metering_3),
              Energy_reminder = sum(Reminder_energy))
}

energyConsumDay <- function(yrPrepData, day) {
  ecd <- yrPrepData %>% 
    filter(days(Date)  == days(day) & month(Date) == month(day)) %>%
    summarise(Total_energy = sum(Global_energy),
              Energy_subm1 = sum(Sub_metering_1),
              Energy_subm2 = sum(Sub_metering_2),
              Energy_subm3 = sum(Sub_metering_3),
              Energy_reminder = sum(Reminder_energy))
  
  ecd
}

avgEnergyForDayInMonths <- function(months) {
  tempData <- months %>%
    mutate(Year = year(Date), Week = week(Date)) %>%
    group_by(Year, Week, Week_day) %>%
    summarise(Total_energy = sum(Global_energy),
              Energy_subm1 = sum(Sub_metering_1),
              Energy_subm2 = sum(Sub_metering_2),
              Energy_subm3 = sum(Sub_metering_3),
              Energy_reminder = sum(Reminder_energy))
  
  aedm <- tempData %>%
    group_by(Week_day) %>%
    summarise(Total_energy_avg = mean(Total_energy) * 1000,
              Energy_Sub1_avg = mean(Energy_subm1),
              Energy_Sub2_avg = mean(Energy_subm2),
              Energy_Sub3_avg = mean(Energy_subm3),
              Energy_reminder_avg = mean(Energy_reminder))
 aedm 
}


