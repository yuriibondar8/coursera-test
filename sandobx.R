library(tidyverse)

# Viewing data
head(diamonds)
str(diamonds)
glimpse(diamonds)
colnames(diamonds)

# Cleaning data
rename(diamonds, carat_new = carat)
rename(diamonds, carat_new = carat, cut_new = cut)
summarise(diamonds, mean_carat = mean(carat))

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point()
ggplot(data = diamonds, aes(x = carat, y = price, colour = cut)) + geom_point()
ggplot(data = diamonds, aes(x = carat, y = price)) +
 geom_point() +
 facet_wrap(~cut)


# ggplot2 basics
data()
BOD
?BOD

ggplot(data = BOD, mapping = aes(x = Time, y = demand, colour = demand)) +
 geom_line(colour = "orange") +
 geom_point(size = 2)

ggplot(BOD, aes(Time, demand)) +
 geom_line() +
 geom_point()


view(CO2)
CO2 %>%
 ggplot(aes(x = conc, y = uptake, colour = Treatment)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = lm, se = T) +
  facet_wrap(~Type) +
  labs(title = "Concentration of CO2") +
  theme_minimal()

CO2 %>%
 ggplot(aes(Treatment, uptake)) +
  geom_boxplot() +
  geom_point(alpha = 0.5, aes(size = conc, colour = Plant)) +
  facet_wrap(~Type) +
  coord_flip() +
  labs(title = "Chilled vs Non-chilled") +
  theme_bw()


view(mpg)
colnames(mpg)
mpg %>%
 filter(cty < 25) %>%
 ggplot(aes(displ, cty)) +
  geom_point(aes(colour = drv, size = trans), alpha = 0.5) +
  geom_smooth(method = lm) +
  facet_wrap(~year, nrow = 1) +
  labs(x = "Engine size", y = "MPG in the city", title = "Fuel efficiency") +
  theme_bw()

# manufacturer
# manufacturer name

# model
# model name

# displ
# engine displacement, in litres

# year
# year of manufacture

# cyl
# number of cylinders

# trans
# type of transmission

# drv
# the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd

# cty
# city miles per gallon

# hwy
# highway miles per gallon

# fl
# fuel type

# class
# "type" of car


mpg %>%
 # fuels <- c("p", "r") %>%
 # filter(year == 2008, mpg$fl == "p" & mpg$fl=="r") %>%
 # filter(fl == fuels) %>%
 # filter(mpg$fl == "p" & mpg$fl=="r") %>%
 ggplot(aes(displ, cty)) +
  geom_point(aes(colour = fl, size = displ), alpha = .5) +
  geom_smooth(method = lm) +
  labs(x = "Engine size",
      y = "MPG in the city",
      title = "Fuel efficiency by nof cylinders") +
  facet_wrap(~cyl) +
theme_minimal()

typeof(mpg$fl)







