install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")

library("here")
library("skimr")
library("janitor")
library("dplyr")

library("palmerpenguins")

skim_without_charts(penguins) # gives a comprehansive summary of a dataset
glimpse(penguins) # get a quick summary of a data
head(penguins) # get a preview of a first few rows
colnames(penguins) # get colnames of a table
select(penguins, species) # specify certain columns or exclude them
select(penguins, -species) # or select(!species) exclude columns from scope

rename(penguins, island_new = island) # rename a column
rename_with(penguins, tolower) # reformat column names
clean_names(penguins) # trim to only characters, numbers and underscores