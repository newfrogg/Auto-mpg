# Import needed library
library(tidyverse)
library(stringr)
library(tidymodels)

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

dframe[,c(2,3,4,5)] <- suppressWarnings(apply(dframe[,c(2,3,4,5)], 2, as.integer))

dframe[,6] <- as.numeric(dframe[, 6])
dframe[,1] <- as.numeric(dframe[, 1])
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
dframe %>% summary()

## Exploratory data

### mpg 
cat("\tMpg\n")

print(dframe$mpg %>% summary())

hist_mpg <- ggplot(dframe, aes(x = mpg, color = "black")) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=2) + 
    labs(title="Mile Per Gallon Histogram Plot", x="Miles Per Gallon", y="Count") +
    theme_light()

box_mpg <- ggplot(dframe, aes(x = mpg)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title="Mile Per Gallon Box Plot", x="Miles Per Gallon", y="Count") +
    theme_light()

print(hist_mpg)
print(box_mpg)

### horsepower

cat("\tHorsepower\n")

print(dframe$horsepower %>% summary())

hist_horse <- ggplot(dframe, aes(x = horsepower)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=5) + 
    labs(title="Horsepower Histogram Plot", x="Horsepower", y="Count") +
    theme_light()

box_horse <- ggplot(dframe, aes(x = horsepower)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title="Horsepower Box Plot", x="Horsepower", y="Count") +
    theme_light()

print(hist_horse)
print(box_horse)

### carname
cat("\tCar name\n")

#### Split car name as their brand
dframe$carname <- str_split(dframe$carname, pattern=" ", simplify=TRUE)[, 1]

dframe$carname <- as.factor(dframe$carname)

# print(dframe$carname %>% summary())

#### Fix misspell error
dframe$carname <- as.character(dframe$carname)

dframe$carname[dframe$carname == "chevroelt"] <- "chevrolet"
dframe$carname[dframe$carname == "hi"] <- NA
dframe$carname[dframe$carname == "maxda"] <- "mazda"
dframe$carname[dframe$carname == "mercedes-benz"] <- "mercedes"
dframe$carname[dframe$carname == "toyouta"] <- "toyota"
dframe$carname[dframe$carname == "vokswagen"] <- "volkswagen"

dframe$carname <- as.factor(dframe$carname)

print(dframe$carname %>% summary())

#### Display Histogram
stat_carname <- ggplot(dframe, aes(x=carname)) + 
    stat_count(color="#F70F26", fill="#F6BAC0") + 
    labs(title="Car name Histogram Plot", x="Car name", y="Count") +
    coord_flip()
    theme_light()

print(stat_carname)


# Modeling data

set.seed(123)
## Split Training/Testing data set

dframe_split <- initial_split(dframe, prop=0.503)
# Training dataset
auto_mpg1 <- training(dframe_split)
# Testing dataset
auto_mpg2 <- testing(dframe_split)






