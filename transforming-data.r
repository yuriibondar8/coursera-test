


# 2022-10-07 Activity: Transforming data

# separate(); unite() functions
id <- c(1:10)
name <- c(
 "John Mendes",
 "Rob Stewart",
 "Rachel Abrahamson",
 "Christy Hickman",
 "Johnson Harper",
 "Candace Miller",
 "Carlson Landy",
 "Pansy Jordan",
 "Darius Berry",
 "Claudia Garcia"
)
job_title <- c(
 "Professional",
 "Programmer",
 "Management",
 "Clerical",
 "Developer",
 "Programmer",
 "Management",
 "Clerical",
 "Developer",
 "Programmer"
)
employee <- data.frame(id, name, job_title)
print(employee)

separate(employee, "name", into = c("first_name", "last_name"), sep = " ")
unite(employee, "id_name", id, name, sep = "-")


# mutate(); transmute() functions
view(penguins)
glimpse(penguins)
penguins_mod <- penguins %>%
 mutate(
  body_mass_kg = body_mass_g / 1000,
  flipper_length_m = flipper_length_mm / 1000
 )
glimpse(penguins_mod)
View(penguins_mod)
# transmute(bill_area_mm = bill_length_mm * bill_depth_mm) -- drop columns



#