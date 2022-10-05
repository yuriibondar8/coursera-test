


library(palmerpenguins)
library(tidyverse)

# ARRANGE
penguins %>% arrange(bill_length_mm) # sorted by bill len ASC
penguins %>% arrange(-bill_length_mm) # sorted by bill len DESC
penguins2 <- arrange(penguins, -bill_length_mm)
view(penguins2)

# GROUP BY
penguins %>%
 group_by(island) %>%
 drop_na() %>%
 summarise(
  mean_bill_length_mm = mean(bill_length_mm), #Average
  mean_body_mass_g = mean(body_mass_g)
 )

penguins %>%
 group_by(species, island) %>%
 drop_na() %>%
 summarise(
  max_bl = max(bill_length_mm),
  mean_bl = mean(bill_length_mm)
 )

# FILTER
penguins %>%
 filter(species == "Adelie")

#