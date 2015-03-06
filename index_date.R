#Function to calculate an index date.  Point the date of the nearest use 
# of something from an initial window (date)

index.date <- function(d, ini.window) {
  require('plyr')
  out1 <- ddply(d, .(id, rcat), summarise, index = min(tdiff))
  ndate <- as.numeric(as.Date(ini.window)) + out1[['index']]
  out1$index.date <- as.Date(ndate, origin = '1970-01-01')
  out1 <- out1[, -3]
  out1
}
