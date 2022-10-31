# Import needed library
library(tidyverse)
library(stringr)
library(tidymodels)


# Clearing R console
cat("\014")  

# Clearing the Environment
rm(list = ls())


# Get data from UCI Machine Learning Repository

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"

col_names <- c("mpg","cylinders","displacement",
            "horsepower","weight","acceleration",
            "modelyear","origin", "carname")

auto <- read.table(url, fileEncoding = "UTF-8", dec=",", col.names = col_names)

rows = nrow(auto)
cols = ncol(auto)

# Change data type

# for (i in 1:cols) {
#     print(typeof(auto[, i]) )
# }

auto[,c(2,3,4,5)] <- suppressWarnings(apply(auto[,c(2,3,4,5)], 2, as.integer))


auto[,6] <- as.numeric(auto[, 6])
auto[,1] <- as.numeric(auto[, 1])
message("Our dataset has a missing property in at least 1 sample. In this case, N/A appears in 5th column (horsepower)")

# Package to .csv file
fileName <- "./data/auto-mpg.csv"

if (is.null(
            try(
                write.csv(auto,
                          file = fileName,
                          row.names = TRUE,
                          na = ""))) 
                                            != FALSE)
    print("Writing to CSV successfully.")

# Cleaning data 

## Get the domain knowledge
cat("\n\tGlimpse of data set\n")
auto %>% glimpse()

cat("\n\tGlimpse of data set\n")
auto %>% glimpse()

cat("\n\tSummary:\n")
auto %>% summary()

## Exploratory data
### mpg 
cat("\tMpg\n")

print(auto$mpg %>% summary())

hist_mpg <- ggplot(auto, aes(x = mpg, color = "black")) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=3) + 
    labs(title="Mile Per Gallon Histogram Plot", x="Miles Per Gallon", y="Count") +
    theme_light()

box_mpg <- ggplot(auto, aes(x = mpg)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title="Mile Per Gallon Box Plot", x="Miles Per Gallon", y="Count") +
    theme_light()

print(hist_mpg)
print(box_mpg)

### cylinder
cat("\tCylinder\n")

auto$cylinders <- as.factor(auto$cylinders)

print(auto$cylinders %>% summary())

hist_cylinder <- ggplot(auto, aes(x = cylinders)) +
    stat_count(color="#F70F26", fill="#F6BAC0") + 
    labs(title = "cylinders Histogram Plot", x = "cylinders", y = "Count") +
    theme_light()

print(hist_cylinder)

### displacement
cat("\tDisplacement\n")
print(auto$displacement %>% summary())

hist_disp <- ggplot(auto, aes(x = displacement)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=30) + 
    labs(title = "Displacement Histogram Plot", x = "displacement", y = "Count") +
    theme_light()

box_disp <- ggplot(data = auto, aes(x = displacement)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Displacement Box Plot", x = "displacement", y = "Count") +
    theme_light()

print(hist_disp)
print(box_disp)

### horsepower
cat("\tHorsepower\n")

print(auto$horsepower %>% summary())

hist_horse <- ggplot(auto, aes(x = horsepower)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=6) + 
    labs(title="Horsepower Histogram Plot", x="Horsepower", y="Count") +
    theme_light()

box_horse <- ggplot(auto, aes(x = horsepower)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title="Horsepower Box Plot", x="Horsepower", y="Count") +
    theme_light()

print(hist_horse)
print(box_horse)

### weight

cat("\tWeight\n")

print(auto$weight %>% summary())

hist_weight <- ggplot(data = auto, aes(x = weight)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=30) +
    labs(title = "Weight Histogram Plot", x = "weight", y = "Count") +
    theme_light()

box_weight <- ggplot(data = auto, aes(x = weight)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Weight Box Plot", x = "weight", y = "Count") +
    theme_light()

print(hist_weight)
print(box_weight)

### acceleration

cat("\tAcceleration \n")

print(auto$weight %>% summary())

hist_acce <- ggplot(data = auto, aes(x = acceleration)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=30) +
    labs(title = "Acceleration Histogram Plot", x = "acceleration", y = "Count") +
    theme_light()

box_acce <- ggplot(data = auto, aes(x = acceleration)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Acceleration Box Plot", x = "acceleration", y = "Count") +
    theme_light()

print(hist_acce)
print(box_acce)

print(hist_weight)
print(box_weight)

### model year
cat("\tModel year\n")
print(auto$modelyear %>% summary())

hist_modelyear <- ggplot(data = auto, aes(x = modelyear)) +
    geom_histogram(color="#F70F26", fill="#F6BAC0", binwidth=30) +
    labs(title = "Model year Histogram Plot", x = "model year", y = "Count") +
    theme_light()

box_modelyear <- ggplot(data = auto, aes(x = modelyear)) +
    geom_boxplot(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Model year Box Plot", x = "model year", y = "Count") +
    theme_light()

points_modelyear <- ggplot(data = auto, aes(x = modelyear, y = mpg)) +
    geom_point(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Model year Geom Point", x = "model year", y = "mpg") +
    theme_light()

print(hist_modelyear)
print(box_modelyear)
print(points_modelyear)

### origin
cat("\tOrigin\n")
auto$origin <- as.factor(auto$origin)
print(auto$origin %>% summary())

stat_origin <- ggplot(data = auto, aes(x = origin)) +
    stat_count(color="#F70F26", fill="#F6BAC0") +
    labs(title = "Origin Histogram Plot", x = "origin", y = "Count") +
    theme_light()

print(stat_origin)


### carname
cat("\tCar name\n")
#### Split car name as their brand
auto$carname <- str_split(auto$carname, pattern=" ", simplify=TRUE)[, 1]

auto$carname <- as.factor(auto$carname)

# print(auto$carname %>% summary())


#### Fix misspell error
auto$carname <- as.character(auto$carname)


auto$carname[auto$carname == "chevroelt"] <- "chevrolet"
auto$carname[auto$carname == "hi"] <- NA
auto$carname[auto$carname == "maxda"] <- "mazda"
auto$carname[auto$carname == "mercedes-benz"] <- "mercedes"
auto$carname[auto$carname == "toyouta"] <- "toyota"
auto$carname[auto$carname == "vokswagen"] <- "volkswagen"

auto$carname <- as.factor(auto$carname)

print(auto$carname %>% summary())

# Display Histogram


#### Display Histogram
stat_carname <- ggplot(auto, aes(x=carname)) +
    stat_count(color="#F70F26", fill="#F6BAC0") +
    labs(title="Car name Histogram Plot", x="Car name", y="Count") +
    coord_flip()
    theme_light()

print(stat_carname)


# Modeling data
set.seed(123)
## Split Training/Testing data set

auto_split <- initial_split(auto, prop=0.503)
print(auto_split)
### Training dataset
auto_mpg1 <- training(auto_split)
### Testing dataset
auto_mpg2 <- testing(auto_split)










