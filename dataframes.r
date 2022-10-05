


# Diamonds dataset from tidyverse
data("diamonds") # load dataset
View(diamonds) # preview entire data frame
head(diamonds) # preview first 6 rows
str(diamonds) # hightlight the structure of data
glimpse(diamonds)
colnames(diamonds) # view names of the coumns

mutate(diamonds, carat_2 = carat * 100) # add a column to data frame



# Hands on activity
library(tidyverse)
names <- c("Alex", "Adam", "Eve", "Annie")
age <- c(22, 23, 22, 21)
people <- data.frame(names, age)

head(people)
glimpse(people)
colnames(people)

mutate(people, age_in_20 = age + 20)

fruits <- c("apples", "bananas", "cherries", "oranges", "peaches")
score <- c(3, 4, 1, 2, 5)
fruit_ranks <- data.frame(fruits, score)
head(fruit_ranks)



# Tibbles
as_tibble(diamonds)

data(mtcars)
View(mtcars)
colnames(mtcars)



# Readr package for import
readr_example()
cars <- read_csv("/Users/yuriibondar/Desktop/automobile_data.csv")
spec(cars)
colnames(cars)

#