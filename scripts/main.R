library(RMySQL)
library(dplyr)
library(chron)
library(lubridate)
source("scripts/utils.R")

conn <- dbConnect(MySQL(), 
                  user = "deepAnalytics",
                  password = "Sqltask1234!",
                  dbname = "dataanalytics2018",
                  host = "data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")

dataBaseNames <- c("yr_2006", "yr_2007", "yr_2008", "yr_2009", "yr_2010")

yr2006 <- dbGetQuery(conn, paste("SELECT * FROM", dataBaseNames[1], sep = " "))

yr2006Prep <- featureEngineering(yr2006)

energyByMonth <- yr2006Prep %>% group_by(Month) %>% summarise(TotalEnergyConsumption = sum(Global_active_power))

                                  