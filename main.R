# Clearing the R console
cat("\014")  


# Clearing the Environment

rm(list = ls())


# Get data from UCI Machine Learning Repository

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"

col_names <- c("mpg","cylinders","displacement",
            "horsepower","weight","acceleration",
            "modelyear","origin", "carname")

dataFrame <- read.table(url, fileEncoding = "UTF-8", dec=',',
                        col.names = col_names)


