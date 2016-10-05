require(lubridate)
library(car)
summary(table_big)
table_big_archiv <- table_big
# Variablen Verarbeitung

table_big$Datum_2 <- ""
table_big$Datum_2[1:(length(table_big$Datum_2)-3)] <- as.character(table_big$Datum[4:length(table_big$Datum_2)])

table_big$Datum_2 <- ifelse(table_big$Datum_2 == "", table_big$Datum_2[(length(table_big$Datum_2)-3)], as.character(table_big$Datum_2))
table(table_big$Datum_2)

table_big$Datum_2 <- paste(substr(table_big$Datum_2, 7,10), substr(table_big$Datum_2, 4,5), substr(table_big$Datum_2, 1,2), sep = "-")

table_big$Datum_2 <- paste(table_big$Datum_2, table_big$Startzeit)


table_big$Datum <- as.POSIXct(strptime(table_big$Datum_2, "%Y-%m-%d %H:%M:%S"), tz="GMT")
summary(table_big$Datum)


t <- as.POSIXlt(table_big$Datum)

table_big$weekday <- t$wday

table_big$weekday <- ifelse(table_big$weekday == 0, 7, table_big$weekday)

table_big$monthday <- t$mday
table_big$yearday <- t$yday
table_big$year <- t$year
table_big$month <- t$mon + 1

table(table_big$month)
table_big$hour <- t$hour
table(table_big$hour)

table_big$year <- table_big$year - min(table_big$year) +1
table(table_big$year)

f_test <- sin(1*2*pi*table_big$weekday[1]/7)

PARAMETER_FUER_AG <- 4


for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$hour/24)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "h", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$hour/24)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "h", "cos", i, sep = "_")
  
  
}

for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$weekday/7)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "w", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$weekday/7)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "w", "cos", i, sep = "_")
  
  
}

for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$monthday/30.5)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "d", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$monthday/30.5)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "d", "cos", i, sep = "_")
  
  
}


for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$month/12)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "m", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$month/12)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "m", "cos", i, sep = "_")
  
  
}



table(table_big$year)

year_diff <- max(table_big$year) - min(table_big$year) + 1

for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$year/year_diff)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "y", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$year/year_diff)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "y", "cos", i, sep = "_")
  
  
}


names(table_big) 
#    table_big <- table_big[,1:30]

table_big$nn <- 1:nrow(table_big)
table_big$nn_log10 <- log10(table_big$nn)
table_big$nn_log10_2 <- table_big$nn_log10^2

for(i in 31:70){
  fur_nn_log <- table_big[,i] * table_big$nn_log10
  table_big[,(ncol(table_big)+1)] <- fur_nn_log
  names(table_big)[ncol(table_big)] <- paste(names(table_big)[i], "nlog", sep = "_")
  
}


cor(table_big[31:70])

data_regression <- table_big[,31:113]
names(data_regression)



cc <- as.data.frame(cor(data_regression[,1:43]))
summary(cc)

for(i in 1:ncol(cc)){
  cc[i,i] <- 0
}

for(i in 1:ncol(cc)){
  
  cc[,i] <- ifelse(cc[,i] < 0.95,"", cc[,i])
}



for(i in ncol(cc):1){
  t <- cc
}

fit <- lm(f_y_cos_2 ~ nn_log10 + nn_log10_2 + nn, data = data_regression)
vif_value <- vif(fit)
vif_value_df <- as.data.frame(vif_value)





test_vif <- data_regression[,1:10]
test_vif$f_h_sin_1_korr <- test_vif$f_h_sin_1 * 2 + rnorm(nrow(test_vif), 0, 0.001)

names(test_vif)




vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

