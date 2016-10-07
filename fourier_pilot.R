require(lubridate)
library(car)
library(corrplot)
source("timeFunctions.R")
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

table_big$Datum_2 <- NULL



t <- as.POSIXlt(table_big$Datum)

table_big$weekday <-  calc_Week(t)

#table_big$weekday <- ifelse(table_big$weekday == 0, 7, table_big$weekday)

table_big$monthday <- t$mday - 1    #  день месяца
table_big$yearday <- t$yday -1     #  день года
table_big$year <- calc_Year(t)    #  год с дробью

table_big$month <-  calc_Month(t)   # месяц с дробью
table_big$hour <- calc_Hour(t)      # часы с дробью

'summary(table_big$month)
summary(table_big$year)
table_big$year[1:100]
summary(table_big$month)

table(table_big$hour)'

table_big$year <- table_big$year - min(table_big$year) 
#table(table_big$year)


table_big$nn_log10 <- log10(1:nrow(table_big))



#feiertage einstellen

feiertage <- read.csv("feiertage.csv", header = T, sep = ";")
feiertage$date <- as.Date(feiertage$TIMESTMP, "%d.%m.%Y") 


table_big$day <- as.Date(table_big$Datum)
summary(table_big$day)


tt <- match(table_big$day, feiertage$date)
tt[1:100]
table_big$feiertage <- feiertage$FEIERTAG[tt]
table(table_big$feiertage)
table_big$feiertage[1:100]
table_big$day <- NULL

#summary(table_big$monthday)




PARAMETER_FUER_AG <- 4

# hour

for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$hour/24)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "h", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$hour/24)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "h", "cos", i, sep = "_")
  
  
}

# week day


for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$weekday/7 )
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "w", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$weekday/7)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "w", "cos", i, sep = "_")
  
  
}

# monthday

year_month_param <- 365/12 -1


for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$monthday/year_month_param)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "d", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$monthday/year_month_param)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "d", "cos", i, sep = "_")
  
  
}
# month

for(i in 1:PARAMETER_FUER_AG){
  fur <- sin(i*2*pi*table_big$month/12)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "m", "sin", i, sep = "_")
  fur <- cos(i*2*pi*table_big$month/12)
  table_big[,(ncol(table_big)+1)] <- fur
  names(table_big)[ncol(table_big)] <- paste("f", "m", "cos", i, sep = "_")
  
  
}



table(table_big$year)


# year

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


#table_big$nn_log10_2 <- table_big$nn_log10^2
names(table_big) 
'for(i in 31:70){
  fur_nn_log <- table_big[,i] * table_big$nn_log10
  table_big[,(ncol(table_big)+1)] <- fur_nn_log
  names(table_big)[ncol(table_big)] <- paste(names(table_big)[i], "nlog", sep = "_")
  
}'



# ICH HABE FERTIG!!!
names(table_big)
data_predictor <- table_big[,30:71]
M <- cor(data_predictor)

data_m <- as.data.frame(as.matrix(M))




corrplot(M, method="square")

vif_param <- 4


data_regression <- cbind(table_big$`F 30-49`, data_predictor)
names(data_regression)[1] <- "dependet"
names(data_regression)

fit <- lm(dependet ~ ., data = data_regression)

vif_value <- vif(fit)
vif_value_df <- as.data.frame(vif_value)

summary(fit)





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


summary(table_big)

