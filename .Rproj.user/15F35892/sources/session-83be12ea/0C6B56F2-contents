library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(zoo)
library(tseries)
library(urca)
## Import data

time_series <- ts_d_US

# Perform the PP test

perform_pp_test <- function(column) {
  pp_result <- pp.test(column)
  print(pp_result)
  cat("---------------------------------\n")
  
  if (pp_result$p.value < 0.05) {
    cat("Conclusion: Reject the null hypothesis. The time series may be stationary.\n")
  } else {
    cat("Conclusion: Fail to reject the null hypothesis. The time series may not be stationary.\n")
  }
}


## Apply the test to each column of the data_frame

apply(time_series[, 2:ncol(time_series)], 2, perform_pp_test)