


# 2022-10-18 Course challenge 7

library(tidyverse)

# Importing data
setwd("/Users/yuriibondar/Desktop/c")
bars_df <- read.csv("flavors_of_cacao.csv")
str(bars_df)
colnames(bars_df)

# Data cleaning
# Rename sletchy column
bars_clean <- bars_df %>%
  rename(Company = Company...Maker.if.known.)

# Summarise by mean Rating
trimmed_flavors_df <- bars_clean %>%
  select(
    Company,
    Rating,
    Cocoa.Percent
  ) %>%
  summarise(
    mean(Rating)
  )

# Filter by variables
best_trimmed_flavors_df <- bars_clean %>%
  filter(Rating >= 3.75, Cocoa.Percent >= 80)
View(best_trimmed_flavors_df)


# Data visualization
# Geom bar chart
ggplot(data = best_trimmed_flavors_df) +
  geom_bar(
    mapping = aes(
      x = Company.Location,
      alpha = Rating
    )
  ) +
  facet_wrap(~Rating)

# Scatter plot
ggplot(data = bars_clean) +
  geom_point(
    mapping = aes(
      x = Cocoa.Percent,
      y = Rating
    )
  ) +
  labs(
    title = "Suggested Chocolate"
  )


# Save the viz
ggsave(“chocolate.jpeg”)



#