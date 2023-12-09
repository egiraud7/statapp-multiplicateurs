#importation donnees

d_US <- read.xlsx("DATA_USA.xlsx", na.strings = ".", colNames=TRUE, rowNames=FALSE, detectDates = TRUE)

#transfo en time serie



ts_US = ts(d_US, start=c(1990,1), frequency=4)

par(mfrow = c(2,2))

for (i in 2:ncol(ts_US)) {
  ts.plot(ts_US[, i], gpars = list(main = colnames(ts_US)[i], ylab=""))
}



#stationnary


ts_stationary_US <- sapply(ts_US, diff)

par(mfrow = c(2,2))

for (i in 2:ncol(ts_stationary_US)) {
  ts.plot(ts_stationary_US[, i], gpars = list(main = paste("stationary",colnames(ts_US)[i]), ylab=""))
}



