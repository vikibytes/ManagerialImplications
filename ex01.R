# 0. Loading required libraries ----
source('myLibs.R')

# 1. Let's import datasets ----
# raw.traindata <- read.csv(file = "rawData/train.csv")
# raw.storedata <- read.csv(file = 'rawData/store.csv')
test <- fread(input = "rawData/test.csv")
train <- fread("rawData/train.csv")
store <- fread("rawData/store.csv")

# 2. Data exploration and cleaning ----
str(train)
str(test)
# 2.1 Convert Date column to Date format ----
train[, Date := as.Date(Date)]
test[, Date := as.Date(Date)]
summary(train)
summary(test)

# 2.2 sort data by Date
train <- train[order(Date)]
test <- test[order(Date)]
summary(train)

#2.3 Checking NA's blanks
sum(is.na(test$Open))
test[is.na(test)] <- 1

train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]
# All test stores are also in the train data
sum(unique(test$Store) %in% unique(train$Store))
# 259 train stores are not in the test data
sum(!(unique(train$Store) %in% unique(test$Store)))
table(train$Open) / nrow(train) # Percent Open Train
table(test$Open) / nrow(test) # Percent Open Test 
table(train$Promo) / nrow(train) # Percent of the time promo in train
table(test$Promo) / nrow(test) # Percent of the time promo in test
table(train$StateHoliday) / nrow(train) # Percent of the time holiday in train
table(test$StateHoliday) / nrow(test) # no b and c = no easter holiday and no christmas
table(train$SchoolHoliday) / nrow(train) # Percent of the time school holiday in train
table(test$SchoolHoliday) / nrow(test) # Percent of the time school holiday in test
plot(train$Date, type = 'l')
plot(test$Date, type = "l")

# 3. Data Visualization ----
# 3.1 Mean sales per store when store was not closed ----
hist(aggregate(train[Sales != 0]$Sales, 
               by = list(train[Sales != 0]$Store), mean)$x, 100, 
     main = "Mean sales per store when store was not closed")

# 3.2 Mean customers per store when store was not closed ----
hist(aggregate(train[Sales != 0]$Customers, 
               by = list(train[Sales != 0]$Store), mean)$x, 100,
     main = "Mean customers per store when store was not closed")

# 3.3 Customers VS Sales ----
# m <- ggplot(train[train$Sales != 0 & train$Customers != 0],
#        aes(x = log(Customers), y = log(Sales))) + 
#   geom_point(alpha = 0.2) + geom_smooth()
# m


# Season Plot
temp <- train
temp$year <- format(temp$Date, "%Y")
temp$month <- format(temp$Date, "%m")
temp[, StoreMean := mean(Sales), by = Store]
temp <- temp[, .(MonthlySalesMean = mean(Sales / (StoreMean)) * 100), by = .(year, month)]
temp <- as.data.frame(temp)

SalesTS <- ts(temp$MonthlySalesMean, start=2013, frequency=12)
col = rainbow(3)
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)