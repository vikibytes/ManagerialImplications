library(dplyr)
msleep <- read.csv("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv")
head(msleep)
names(msleep)

# dplyr verbs in action ----
# The two most basic functions are select() and filter() 
#   which selects columns and filters rows, respectively.
# Selecting columns using select()
# Select a set of columns: the name and the sleep_total columns.
sleepData <- select(.data = msleep, name, sleep_total)
head(select(msleep, -name))
