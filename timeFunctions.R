library(chron)
'timeFunctions <- function(x, method){
  eval(parse(text = (paste0(method, "(x)"))))
}'




# year
'n <- yday(t)
dn <- n/365'

calc_Year <- function(x){
  df <- data.frame(x)
  
  df$n <- yday(x)
  df$year <- year(x)
  df$rest <- df$year %% 4
   
  df$dayyear <- ifelse(df$rest == 0, 366, 365) 
  df$dn <- df$n / df$dayyear
  return(df$year + df$dn)
}

calc_Month <- function(x){
  m31 <- c(1,3,5,7,8,10,12)
  m30 <- c(2,4,6,9,11)
  df <- data.frame(x)
  df$n <- mday(x)
  df$month <- month(x)
  df$rest <- year(x) %% 4
  
  #if(rest == 0)  daymonth2 <- 29 else daymonth2 <- 28
'  if(month %in% m31) maxmonth <- 31
  if(month %in% m30) maxmonth <- 30
  if(month == 2 & rest == 0) maxmonth <- 29
  if(month == 2 & rest != 0) maxmonth <- 28'
  
  df$maxmonth <- ifelse(df$month %in% m31, 31, 30)
  df$maxmonth <- ifelse(df$month == 2 & df$rest == 0, 29, df$maxmonth)
  df$maxmonth <- ifelse(df$month == 2 & df$rest != 0, 28, df$maxmonth)
  
  df$dn <- df$n / df$maxmonth
  return(df$month + df$dn - 1)
}


calc_Week <- function(x){
  n <- hour(x)
  weekday <- wday(x)
  #print(weekday)
  dn <- n / 24
  return(weekday + dn - 2)
}


calc_Hour <- function(x){
  n <- minutes(x)
  hour <- hour(x)
  #print(weekday)
  dn <- n / 60
  return(hour + dn )
}

#t <- table_big$Datum[362*34]



'calc_Year(t)
calc_Month(t)
calc_Week(t)
'


# To Do: für stunden 

'
as.function(eval("calc_Year"))
eval(parse(text = "calc_Year"))
t <- Sys.time()
calc_Hour(t)

t <- as.POSIXct(strptime("2016-10-03 01:00:00", "%Y-%m-%d %H:%M:%S"), tz="GMT")
weekdays(t)

1966 %% 4


timeFunctions(t, "year")
timeFunctions(t, "mday")
timeFunctions(t, "yday")
timeFunctions(t, "month")
timeFunctions(t, "wday")
timeFunctions(t, "hour")

'
#timeFunctions(t, "minutes")









