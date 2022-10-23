# Clearing the R console
cat("\014")  


# Clearing the Environment

rm(list = ls())


# Get data from UCI Machine Learning Repository

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"

col_names <- c("mpg","cylinders","displacement",
            "horsepower","weight","acceleration",
            "modelyear","origin", "carname")

dataFrame <- read.table(url, fileEncoding = "UTF-8", dec=",", col.names = col_names)

rows = nrow(dataFrame)
cols = ncol(dataFrame)

# Change data type

# for (i in 1:cols) {
#     print(typeof(dataFrame[, i]) )
# }

dataFrame[,c(1,2,3,4,5)] <- suppressWarnings(apply(dataFrame[,c(1,2,3,4,5)], 2, as.integer))

print("Our dataset has a missing property in at least 1 sample. In this case,
      N/A appears in 5th column (horsepower)")

# Package to .csv file

fileName <- "./data/auto-mpg.csv"

if (is.null(
            try(
                write.csv(dataFrame,
                          file = fileName,
                          row.names = TRUE,
                          na = ""))) != FALSE)
    print("Writing to CSV successfully.")



