
pacman::p_load(RMySQL, dplyr, chron, lubridate, ggfortify, padr, forecast, tseries)
source("scripts/utils.R")
source("scripts/plots.R")

question_analysis <- "What do you want to analyze?"
options_analysis <- c("Energy", "Cost")

question_granularity <- "Choose the granularity of the data by typing its number option:"
options_granularity <- c("Days", "Weeks", "Months")

question_data_transf <- "Do you want to make any transformation in the data?"
options_data_transf <- c("YES", "NO")

question_select_transf <- "Select the transformation:"
options_select_transf <- c("Simple Diff", "Seasonal Diff", "Log")

question_keep_transf <- "Do you want to keep the transformations in the data?"
options_keep_transf <- c("YES", "NO")

question_forecast_alg <- "Choose the forecast algorithm:"
options_forecast_alg <- c("ARIMA", "Holt Winters")

question_arima_type <- "Select the type of ARIMA:"
options_arima_type <- c("Auto ARIMA", "Non Seasonal ARIMA", "Seasonal ARIMA")

question_arima_param1 <- "Type the non seasonal parameters p, d , q: (Separated by commas)"

question_arima_param2 <- "Type the seasonal parameters P, D, Q: (Separated by commas)"

OPTION_INVALID <- "-INVALID-"

ts_main <- function() {
  if (!exists("energy_data") || is.null(energy_data)) {
    cat("Energy data not in memory. Trying to obtain it from the data base.\n")
    conn <- dbConnect(MySQL(), 
                      user = "deepAnalytics",
                      password = "Sqltask1234!",
                      dbname = "dataanalytics20118",
                      host = "data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com")
    
    dataBaseNames <- c("yr_2007", "yr_2008", "yr_2009", "yr_2010")
    energy_data <- data.frame()
    for (dataBase in dataBaseNames) {
      cat(paste("Downloading the '", dataBase, "' data...\n", sep = ""))
      partial_data <- dbGetQuery(conn, paste("SELECT * FROM", dataBase, sep = " "))
      cat(paste("Download of '", dataBase, "' completed successfully\n", sep = ""))
      
      cat(paste("Applying feature engineering in the '", dataBase, "' data...\n", sep = ""))
      partial_data <- featureEngineering(partial_data)
      cat(paste("Feature engineering of '", dataBase, "' data finished\n\n", sep = ""))
      
      energy_data <- rbind(energy_data, partial_data)
    }
    partial_data <- NULL #Memory cleaning
  }
  
  user_input_analysis <- get_user_option(question_analysis, options_analysis)
  if (user_input_analysis == OPTION_INVALID) {
    #TODO Invalid option. Should throw an error message and show the options again
  }
  user_input_granularity <- get_user_option(question_granularity, options_granularity)
  if (user_input_granularity == OPTION_INVALID) {
    #TODO Invalid option. Should throw an error message and show the options again
  }
  
  granularity <- NULL
  frequency <- NULL
  start_ts = c(2007, 1)
  start_train <- c(2007, 1)
  start_test <- c(2010, 1)
  end_train <- NULL
  end_test <- NULL
  data_time_series <- NULL
  if (user_input_granularity == options_granularity[1]) {
    #User chose "Days" as granularity
    granularity <- "day"
    frequency <- 365
    end_train <- c(2009, 365)
    if (user_input_analysis == options_analysis[1]) {
      #User chose "Energy" to analyze
      data_time_series <- totalEnergyConsumByDay(energy_data)
      data_time_series <- data_time_series$Total_energy_day
    } else if (user_input_analysis == options_analysis[2]) {
      #User chose "Cost" to analyze
      data_time_series <- totalCostByDay(energy_data)
      data_time_series <- data_time_series$Total_cost_day
    } 
  } else if (user_input_granularity == options_granularity[2]) {
    #User chose "Weeks" as granularity
    granularity <- "week"
    frequency <- 52
    end_train <- c(2009, 52)
    if (user_input_analysis == options_analysis[1]) {
      #User chose "Energy" to analyze
      data_time_series <- totalEnergyConsumByWeek(energy_data)
      data_time_series <- data_time_series$Total_energy_week
    } else if (user_input_analysis == options_analysis[2]) {
      #User chose "Cost" to analyze
      data_time_series <- totalCostByWeek(energy_data)
      data_time_series <- data_time_series$Total_cost_week
    } 
  } else if (user_input_granularity == options_granularity[3]) {
    #User chose "Months" as granularity
    granularity <- "month"
    frequency <- 12
    end_train <- c(2009, 12)
    if (user_input_analysis == options_analysis[1]) {
      #User chose "Energy" to analyze
      data_time_series <- totalEnergyConsumByMonth(energy_data)
      data_time_series <- data_time_series$Total_energy_month
    } else if (user_input_analysis == options_analysis[2]) {
      #User chose "Cost" to analyze
      data_time_series <- totalCostByMonth(energy_data)
      data_time_series <- data_time_series$Total_cost_month
    } 
  }
  
  y_label_ts <- NULL
  if (user_input_analysis == options_analysis[1]) {
    y_label_ts <- "Energy (kWh)"
    title_ts <- "Energy Consumption Time Series"
  } else if (user_input_analysis == options_analysis[2]) {
    y_label_ts <- "Euros"
    title_ts <- "Energy Cost Time Series"
  } 
  
  time_series <- toTimeSeries(data_time_series, granularity, start = start_ts)
  
  time_series_dec <- stl(time_series, s.window = "periodic")
  time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)
  
  print(autoplot(time_series_dec)) #Shows graphs of the time series and its components
  ggtsdisplay(time_series)  #Shows the time series and its ACF and PACF graphs
  cat("\nRelative variance of the time series components: \n")
  print(time_series_relative_variance)
  
  adf_test <- adf.test(time_series)
  cat("\nADF Test: \n")
  print(adf_test)
  
  cat("\n#####################################################\n")
  
  user_input_do_transform <- get_user_option(question_data_transf, options_data_transf)
  if (user_input_do_transform == OPTION_INVALID) {
    #TODO Invalid option. Should throw an error message and show the options again
  }
  
  did_transformation <- FALSE
  if (user_input_do_transform == options_data_transf[1]) {
    #User wants to make transformation in the data
    did_transformation <- TRUE
    data_transformed <- data_time_series
    while (user_input_do_transform == options_data_transf[1]) {
      #Show possible transformations
      user_input_transformation <- get_user_option(question_select_transf, options_select_transf)
      if (user_input_transformation == OPTION_INVALID) {
        #TODO Invalid option. Should throw an error message and show the options again
      }
      
      if (user_input_transformation == options_select_transf[1]) {
        #User selected simple diff transformation
        data_transformed <- diff(data_transformed)
      } else if (user_input_transformation == options_select_transf[2]) {
        #User selected seasonal diff transformation
        data_transformed <- diff(data_transformed, lag = frequency)
      } else if (user_input_transformation == options_select_transf[3]) {
        #User selected log transformation
        data_transformed <- log(data_transformed)
      } 
      
      time_series <- toTimeSeries(data_transformed, granularity, start = start_ts)
      
      time_series_dec <- stl(time_series, s.window = "periodic")
      time_series_relative_variance <- apply(time_series_dec$time.series, 2, var) / var(time_series)
      
      #Shows graphs of the time series and its components
      print(autoplot(time_series_dec))
      
      #Shows the time series and its ACF and PACF graphs
      ggtsdisplay(time_series)
      
      cat("\nRelative variance of the time series components: \n")
      print(time_series_relative_variance)
      
      adf_test <- adf.test(time_series)
      cat("\nADF Test: \n")
      print(adf_test)
      
      cat("\n#####################################################\n")
      
      user_input_do_transform <- get_user_option(question_data_transf, options_data_transf)
      if (user_input_do_transform == OPTION_INVALID) {
        #TODO Invalid option. Should throw an error message and show the options again
      }
    }
    
  }
  
  if (did_transformation) {
    user_input_keep_transf <- get_user_option(question_keep_transf, options_keep_transf)
    if (user_input_keep_transf == OPTION_INVALID) {
      #TODO Invalid option. Should throw an error message and show the options again
    }
    
    if (user_input_keep_transf == options_keep_transf[2]) {
      #User wants to discard the all the transformations made
      time_series <- toTimeSeries(data_time_series, granularity, start = start_ts)
    }
  }
  
  time_series_split <- splitTrainTestTimeSeries(time_series, 
                                                start_train = start_train, 
                                                end_train = end_train, 
                                                start_test = start_test, 
                                                end_test = end_test)
  
  user_input_algorithm <- get_user_option(question_forecast_alg, options_forecast_alg)
  if (user_input_algorithm == OPTION_INVALID) {
    #TODO Invalid option. Should throw an error message and show the options again
  }
  
  model <- NULL
  forecast <- NULL
  forecast_length = length(time_series_split$test) + 2
  if (user_input_algorithm == options_forecast_alg[1]) {
    #Show ARIMA options
    user_input_arima_type <- get_user_option(question_arima_type, options_arima_type)
    if (user_input_arima_type == OPTION_INVALID) {
      #TODO Invalid option. Should throw an error message and show the options again
    }
    
    if (user_input_arima_type == options_arima_type[1]) {
      #Run auto arima
      model <- auto.arima(time_series_split$train)
      forecast <- forecast:::forecast.Arima(model, h = forecast_length)
    } else if (user_input_arima_type == options_arima_type[2]) {
      #Show Non seasonal ARIMA options
      
      #Ask user for non seasonal ARIMA parameters
      user_input_arima_param1 <- get_user_input(question_arima_param1)
      arima_params1 <- strsplit(user_input_arima_param1, ",")
      arima_p <- as.integer(arima_params1[[1]][1])
      arima_d <- as.integer(arima_params1[[1]][2])
      arima_q <- as.integer(arima_params1[[1]][3])
      
      model <- Arima(time_series_split$train, order = c(arima_p, arima_d, arima_q))
      forecast <- forecast:::forecast.Arima(model, h = forecast_length)
    } else if (user_input_arima_type == options_arima_type[3]) {
      #Show seasonal ARIMA options
      
      #Ask user for NON SEASONAL ARIMA parameters
      user_input_arima_param1 <- get_user_input(question_arima_param1)
      arima_params1 <- strsplit(user_input_arima_param1, ",")
      arima_p <- as.integer(arima_params1[[1]][1])
      arima_d <- as.integer(arima_params1[[1]][2])
      arima_q <- as.integer(arima_params1[[1]][3])
      
      #Ask user for SEASONAL ARIMA parameters
      user_input_arima_param2 <- get_user_input(question_arima_param2)
      arima_params2 <- strsplit(user_input_arima_param2, ",")
      arima_P <- as.integer(arima_params2[[1]][1])
      arima_D <- as.integer(arima_params2[[1]][2])
      arima_Q <- as.integer(arima_params2[[1]][3])
      
      model <- forecast::Arima(time_series_split$train, 
                               order = c(arima_p, arima_d, arima_q),
                               seasonal = c(arima_P, arima_D, arima_Q),
                               lambda = 0)
      forecast <- forecast:::forecast.Arima(model, h = forecast_length)
    }
    
  } else if (user_input_algorithm == options_forecast_alg[2]) {
    #Run Holt Winters
    model <- HoltWinters(time_series_split$train)
    forecast <- forecast:::forecast.HoltWinters(model, h = forecast_length)
  } 
  
  cat("\nModel Metrics:\n")
  print(model)
  
  model_accuracy <- accuracy(forecast, time_series_split$test)
  cat("\nModel accuracy: \n")
  print(model_accuracy)
  
  cat("\nCheck Residuals: \n")
  checkresiduals(forecast)
  
  line_width = 1.3
  print(autoplot(time_series, size = line_width, main = title_ts) + 
          autolayer(forecast$mean, size = line_width, series = "Forecast") + 
          ylab(y_label_ts) + xlab("Year") +
          guides(colour = guide_legend(title = "")) +
          theme(text = element_text(size = 15))
  )
}

get_user_option <- function(question_str, question_options) {
  options_str <- ""
  #Loop for build the answer options
  for (i in 1:length(question_options)) {
    options_str <- paste(options_str, " (", i, ") ", question_options[i], " \n", sep = "")
  }
  
  cat(paste("\n", question_str, "\n", options_str, sep = ""))
  user_input <- readline()
  user_input <- as.integer(user_input)
  
  answer <- NULL
  if (user_input %in% 1:length(question_options)) {
    answer <- question_options[user_input]
  } else {
    answer <- "-INVALID-"
  }
  
  answer
}

get_user_input <- function(question_str) {
  cat(paste("\n", question_str, "\n", sep = ""))
  user_input <- readline()
  
  user_input
}

ts_main()
