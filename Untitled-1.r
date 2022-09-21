# basic calculations
quarter_1_sales <- 35657.98
quarter_2_sales <- 43810.55
midyear_sales <- quarter_1_sales + quarter_2_sales
yearend_sales <- midyear_sales * 2


# logical operators and conditional statements
# AND (sometimes represented as & or && in R)
# OR (sometimes represented as | or || in R)
# NOT (!)

# AND operator “&”
TRUE & TRUE
TRUE & FALSE
FALSE & TRUE
FALSE & FALSE

x <- 10
x > 3 & x < 12
x <- 20
x > 3 & x < 12

# OR operator “|”
TRUE & TRUE
TRUE & FALSE
FALSE & TRUE
FALSE & FALSE

y <- 7
y < 8 | y > 16
y <- 12
y < 8 | y > 16

# NOT operator “!”
!TRUE
!FALSE

x <- 2
!x

# Conditional statements

# if statement
# if (condition) {
#  expr
# }
a <- 4
if (a > 0) {
  print("a is a positive number")
}

# else statement
# if (condition) {
#   expr1
# } else {
#  expr2
# }
b <- -7
if (b > 0) {
 print("b is a positive number")
} else {
 print("b is either a negative number or zero")
}

# else if statement
# if (condition1) {
#  expr1
# } else if (condition2) {
#  expr2
# } else {
#  expr3
# }
c <- 0
if (c < 0) {
 print("c is a negative number")
} else if (c == 0) {
 print("c is zero")
} else {
 print("c is a positive number")
}
