head(diamonds)

ggplot(data = diamonds, aes(x = carat, y = price, color = cut)) + geom_point()

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point(alpha = 0.3)

colnames(diamonds)

diamonds %>%
 ggplot(aes(carat, price, colour = cut)) +
  geom_boxplot() +
  geom_point(alpha = 0.5) +
  facet_wrap(~cut, nrow = 2, ncol = 5)
