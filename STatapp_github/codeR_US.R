#Importation des donnees
d_US <- read.xlsx("DATA_USA.xlsx", na.strings = ".", colNames=TRUE, rowNames=FALSE, detectDates = TRUE)

#Transformation en time series
ts_US = ts(d_US, start=c(1990,1), frequency=4)

#Affichage en 2x2 graphiques
par(mfrow = c(2,2)) 

#Creation de graphiques séparés pour l'ensemble des colonnes du dataframe avec en titre le nom des colonnes de l'Excel
for (i in 2:ncol(ts_US)) {
  ts.plot(ts_US[, i], gpars = list(main = colnames(ts_US)[i], ylab=""))
}

#Stationnarisation des séries
ts_stationary_US <- sapply(ts_US, diff)

#Creation de graphiques séparés des nouvelles séries stationnarisées avec en titre le nom des colonnes de l'Excel
par(mfrow = c(2,2))
for (i in 2:ncol(ts_stationary_US)) {
  ts.plot(ts_stationary_US[, i], gpars = list(main = paste("stationary",colnames(ts_US)[i]), ylab=""))
}
