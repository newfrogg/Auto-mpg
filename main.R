# Import needed library
library(tidyverse)



# Clearing the R console
cat("\014")  


# Clearing the Environment

rm(list = ls())


# Get data from UCI Machine Learning Repository

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"

col_names <- c("mpg","cylinders","displacement",
            "horsepower","weight","acceleration",
            "modelyear","origin", "carname")

dframe <- read.table(url, fileEncoding = "UTF-8", dec=",", col.names = col_names)

rows = nrow(dframe)
cols = ncol(dframe)

# Change data type

# for (i in 1:cols) {
#     print(typeof(dframe[, i]) )
# }

dframe[,c(1,2,3,4,5)] <- suppressWarnings(apply(dframe[,c(1,2,3,4,5)], 2, as.integer))

dframe[,6] <- as.numeric(dframe[, 6])

message("Our dataset has a missing property in at least 1 sample. In this case, N/A appears in 5th column (horsepower)")

# Package to .csv file

fileName <- "./data/auto-mpg.csv"

if (is.null(
            try(
                write.csv(dframe,
                          file = fileName,
                          row.names = TRUE,
                          na = ""))) 
                                            != FALSE)
    print("Writing to CSV successfully.")

# Cleaning data 

## Get the domain knowledge
cat("\n\tGlimpse of data set\n")
dframe %>% glimpse()

cat("\n\tSummary:\n")
print(dframe %>% summary())


