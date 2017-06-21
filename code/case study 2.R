

loan <- read.csv("loan.csv", stringsAsFactors = TRUE)


columns <- as.vector(which(colSums(is.na(loan)) != nrow(loan)))

data <- as.data.frame(loan[,columns])


columns1 <- as.vector(which(sapply(data, function(x)length(unique(x))) != 1))

data1 <- as.data.frame(data[,columns1])


columns2 <- as.vector(which(sapply(data, function(x)length(unique(x))) == 2))

data2 <- as.data.frame(data[,columns2])

lapply(data2, function(x) unique(x))

columns3 <- as.vector(c("collections_12_mths_ex_med", "chargeoff_within_12_mths", "tax_liens"))


data3<- data1[, ! names(data1) %in% columns3, drop = F]


lapply(data3, function(x) unique(x))


summary(as.factor(data3$emp_title))

?sapply

library(dplyr)
data %>% summarise_all(funs(n_distinct(.)))
