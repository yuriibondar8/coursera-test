


# 2022-10-08 Relig vizualization test

install.packages("ggthemes")
install.packages("RColorBrewer")

library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(here)

setwd("/Users/yuriibondar/Desktop/c")

relig_v <- read_delim("relig_v.csv")
relig_int <- read_delim("relig_int.csv", delim = ";")

relig_viz_int <- relig_int %>%
 pivot_longer(
  !religion,
  names_to = "income",
  values_to = "percentage"
 )
head(relig_viz_int)

ggplot(
 data = relig_viz_int,
 mapping = aes(x = religion, y = income, group = religion, fill = income)
) +
 geom_bar(
  data = relig_viz_int,
  mapping = aes(x = religion, y = percentage, group = religion),
  position = "fill",
  stat = "identity"
 ) +
 theme(legend.position = "none") +
 scale_fill_brewer(palette = "Set3") +
 theme_gdocs()

 #