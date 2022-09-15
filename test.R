# Var example 
first_var <- "Var 1"
second_var <- 12.5

print(first_var)
print(second_var)

vec_1 <- c(13, 48.5, 71, 101.5, 2)
?c


v1 <- c("One", "Two", "Three", "Four")
v2 <- c(1, 2, 3, 4)

names(v2) <- v1
v2

v3 <- c(five=5, six=6, seven=7)
v3


# Vectors -- basic stuff
vec <- c(2,3,5,7,11,13,17,19,23,101)
vec[3]

look.at <- 1:4
vec[look.at]

length(vec)
head(vec, 2)
tail(vec, 3)

is.integer(vec)
typeof(vec)
# Nested length() function
length(tail(vec[look.at], 2))

# Vectors -- naming
vec1 <- c(1L, 2L, 3L)
names(vec1) <- c("one", "two", "three")
vec1

vec3 <- c(1L, 2L, 3L)
vec2 <- c("one", "two", "three")
names(vec3) <- vec2
vec3

vec4 <- c(one=1L, two=2L, three=3L)
vec4


# Lists
list1 <- list(list(list(1 , 3, 5)))
str(list1)

list3 <- list("a", 1L, 1.5, TRUE)
str(list3)

list2 <- list('Chicago' = 1, 'New York' = 2, 'Los Angeles' = 3)
str(list2)
list2

# Dates and time
# date format "2016-08-16"
# time format "20:11:59 UTC"
# date-time "2018-03-31 18:15:48 UTC"

today()
now()
as_date(now())

# Convering dates from strings
ymd("2021-01-20")
mdy("January 20th, 2021")
dmy("20-Jan-2021")
ymd(20210120)

# Creating date-time components
ymd_hms("2021-01-20 20:11:59")
mdy_hm("01/20/2021 08:01")

# Data frames
name <- c("Anna", "Pete", "Frank", "Julia", "Samuel")
age <- c(28, 30, 21, 39, 35)
child <- c(FALSE, TRUE, TRUE, FALSE, TRUE)

df <- data.frame(name, age, child)
str(df)

data.frame(x = c(1, 2, 3) , y = c(1.5, 5.5, 7.5))


# Matrices
matrix(c(1:6), nrow=2)
matrix(c(1:8), ncol=2)

arr <- c(1, 2, 3, 1.5, 5.5, 7.5)
length(arr)
matrix(arr, nrow = 2, byrow = TRUE)

cbind(arr, var = 1:6)
rbind(arr) 

cb1 <- cbind(arr, var1 = 1:6)
cb2 <- cbind(var2 = 1:6, var3 = 7:12)
matrix(cb2, nrow = 2, byrow = TRUE)
m <- matrix(cb2, byrow = TRUE, nrow = 2)
cbind(m, 13:16)
rownames(m) <- c("row1", "row2")






m

mat <- matrix(1:6, byrow = TRUE, nrow = 2)
rownames(mat) <- c("r1", "r2")
colnames(mat) <- c("c1", "c2", "c3")
mat

matr <- matrix(LETTERS[1:6], byrow = TRUE, nrow = 2, dimnames = list(c("row1", "row2"), c("col1", "col2", "col3")))
matr

plot(table(rpois(100, 5)), type = "b", col = "red", 
    lwd = 10, 
    main = "rpois(100, lambda = 5)")





x <- c(1,2,3)
str(x)
print("hello")