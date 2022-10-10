


install.packages("Tmisc")
install.packages("datasauRus")
install.packages("SimDesign")

library(tidyverse)
library(Tmisc)
library(datasauRus)
library(SimDesign)


# Misleading data example; library = [Tmisc]
data(quartet)
View(quartet)
?quartet
quartet_summarised <- quartet %>%
 group_by(set) %>%
 summarise(
  mean(x),
  sd(x),
  mean(y),
  sd(y),
  cor(x, y)
 )
head(quartet_summarised)

ggplot(quartet, aes(x, y)) +
 geom_line() +
 geom_point() +
 geom_smooth(method = lm, se = FALSE) +
 facet_wrap(~set)



# Geom structures; library = [datasauRus]
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
 geom_point() +
 theme_void() +
 theme(legend.position = "none") +
 facet_wrap(~dataset, ncol = 3)



# bias() function; library = [SimDesign]
actual_temp <- c(68.3, 70, 72.4, 71, 67, 70)
predicted_temp <- c(67.9, 69, 71.5, 70, 67, 69)
bias(actual_temp, predicted_temp)

actual_sales <- c(150, 203, 137, 247, 116, 287)
predicted_sales <- c(200, 300, 150, 250, 150, 300)
bias(actual_sales, predicted_sales)

#