library(data.table)
library(quantmod)
library(moments)
library(ggplot2)

########################################
date.from <- "2007-12-31"
date.to   <- "2022-12-31"
vfiax <- getSymbols("vfiax",src="yahoo",
                    from=date.from,
                    to=date.to,
                    auto.assign=FALSE)[,6]


vfiax <- na.omit(vfiax)
# convert to data.table
data.yahoo <- as.data.table(vfiax)
names(data.yahoo)[1] <- 'date'
head(data.yahoo)
data.yahoo[, logret := c(NA,diff(log(data.yahoo$VFIAX.Adjusted)))]
data.yahoo <- data.yahoo[-1] 
data.yahoo[, ret := exp(logret)-1 ]

logret.xts <- xts(data.yahoo$logret,order.by=data.yahoo$date)
names(logret.xts) = c('logret')
str(logret.xts)

ggplot(logret.xts)+
  geom_point(aes(x = index(logret.xts), y = logret))+
  theme_minimal()



# Skewness
cat("Skewness: ", round(skewness(data.yahoo$logret),4))
cat("Skewness: ", round(kurtosis(data.yahoo$logret),4))
## Jarque-Bera Test for Normality
jarque.test(as.vector(data.yahoo$logret))





































