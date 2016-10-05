
#########  test load

test_table_14 <- readLines("Saisontabelle_TVGesamt_tagesweise_2014.txt")
length(test_table_14)
nrows_to_read <- length(test_table_14) - 6


table_14 <- read.table("Saisontabelle_TVGesamt_tagesweise_2014.txt", header = F, skip = 3, nrows = nrows_to_read, sep = "\t", dec = ",")

header_table_14 <- read.table("Saisontabelle_TVGesamt_tagesweise_2014.txt", header = F, skip = 1, nrows = 1, stringsAsFactors = F, sep = "\t")
names(table_14) <- paste(as.character(header_table_14[1,]))

summary(table_14)


write.table(file = "table_14.csv", table_14, row.names = F)


############# load vzroslie

#     rm(list = ls())
dir_pilot <- dir("pilot")

dir_pilot <- paste0("pilot/", dir_pilot)


#dir_pilot_vzr <- dir_pilot[12:22]
#dir_pilot_vzr <- dir_pilot[substr(dir_pilot,1,1) == "S"]
#i_visikos <- c(1,5,9,12,16,20)
for(i in 12:length(dir_pilot)){
  test_table_14 <- readLines(dir_pilot[i])
  #length(test_table_14)
  nrows_to_read <- length(test_table_14) - 6

  table <- read.table(dir_pilot[i], header = F, skip = 3, nrows = nrows_to_read, sep = "\t", dec = ",")
  
  header_table <- read.table(dir_pilot[i], header = F, skip = 1, nrows = 1, stringsAsFactors = F, sep = "\t")
  names(table) <- paste(as.character(header_table[1,]))
  if(i == 12){
    table_big_vzr <- table
  } else table_big_vzr <- rbind(table_big_vzr, table)
  
  print(nrow(table_big_vzr))
  
}

####################   kinder

children_row <- c(8787, 8763, 8763, 8763, 8787, 8763, 8763, 8763,8787, 8763, 8763 )


for(i in 1:11){
  #test_table_14 <- readLines(dir_pilot[i])
  #length(test_table_14)
  nrows_to_read <- children_row[i] - 3
  
  table <- read.table(dir_pilot[i], header = F, skip = 3, nrows = nrows_to_read, sep = "\t", dec = ",")
  
  header_table <- read.table(dir_pilot[i], header = F, skip = 1, nrows = 1, stringsAsFactors = F, sep = "\t")
  names(table) <- paste(as.character(header_table[1,]))
  if(i == 1){
    table_big_children <- table
  } else table_big_children <- rbind(table_big_children, table)
  
  print(nrow(table_big_children))
  
}

identical(table_big_children$Datum, table_big_vzr$Datum)


names(table_big_children) #  8 -14

table_big <- cbind(table_big_vzr, table_big_children[,8:14])

write.table(file = "table_big.csv", table_big, row.names = F, sep = ";", dec = ",")


rm(table, table_big_vzr, table_big_children)

