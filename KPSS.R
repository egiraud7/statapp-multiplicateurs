library(dplyr)
library(readxl)
library(magrittr)
library(tidyr)
library(zoo)
library(tseries)
library(urca)
## Import data

time_series <- ts_d_US

# Perform the KPSS test

perform_kpss_test <- function(column) {
  kpss_result <- kpss.test(column)
  print(kpss_result)
  cat("---------------------------------\n")
  
  if (kpss_result$p.value < 0.05) {
    cat("Conclusion: Reject the null hypothesis. The time series may not be stationary.\n")
  } else {
    cat("Conclusion: Fail to reject the null hypothesis. The time series may  be stationary.\n")
  }
}


## Apply the test to each column of the data_frame

apply(time_series[, 2:ncol(time_series)], 2, perform_kpss_test)