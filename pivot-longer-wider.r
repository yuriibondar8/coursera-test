


# 2022-10-07 Pivot longer; Pivot wider

library(tidyr)
head(relig_income)

# pivot_loner() example
# pivot_longer(data, columns, names_to = "", values_to = "") struct
?pivot_longer

View(relig_income)
relig_income_long <- relig_income %>%
 pivot_longer(
  !religion,
  names_to = "income",
  values_to = "count"
 )
View(relig_income_long)



# pivot_wider() example
?pivot_wider

relig_income_wide <- relig_income_long %>%
 pivot_wider(
  religion,
  names_from = "income",
  values_from = "count"
 )
View(relig_income_wide)



# advanced conditions
View(billboard)
colnames(billboard)
billboard %>%
 pivot_longer(
  cols = starts_with("wk"),
  names_to = "week",
  values_to = "rank",
  values_drop_na = TRUE
 )



# Data viz
relig_viz <- relig_income %>%
 select(-"Don't know/refused") %>%
 filter(
  religion != "Don’t know/refused",
  religion != "Unaffiliated",
  religion != "Other World Religions",
  religion != "Other Christian",
  religion != "Other Faiths"
 ) %>%
 pivot_longer(
  !religion,
  names_to = "income",
  values_to = "count"
 )
View(relig_viz)

ggplot(
 data = relig_viz,
 aes(x = income, y = count, group = religion)
) +
 geom_step(
  data = relig_viz,
  mapping = aes(x = income, y = count, linetype = religion)
 ) +
 facet_wrap(~religion, ncol = 2) +
 labs(
  x = "Income",
  y = "Count",
  title = "Average income vs religion"
 ) +
 theme_bw()

#