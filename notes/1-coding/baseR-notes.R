
# Week 1: Thursday

## R basics

### Calculator 
3 + 4
8 * 5 

### Basic objects (global environment)
x = 8
print(x)

### Generate random data 
rnorm(3)

### Logical operators

# & | <= >= ==
x = TRUE
x == TRUE
x == FALSE
3 == 3
(6 <= 3) & (4 <= 7)
(6 <= 3) | (4 <= 7)

## Vectors 

### Atomic vectors 
x = c(1, 2, 5, 7)
x
y = c(TRUE, FALSE)
y
z = c("a", "b", "c")
z

#### Type and length 
a = c(TRUE, FALSE)
b = 1:10
d = c(b, 10.5)
e = letters
typeof(a)
typeof(b)
typeof(d)
typeof(e)

typeof(3)
typeof(3L)

#### Coercion 

##### explicit coercion
as.character(a)
as.character(d)

##### implicit coercion
c(a, b)
c(b, d)
c(d, e)
e = c(e, "afierhfo")

TRUE & 1
TRUE & 0

##### length coercion
b
length(b)

cbind(b, 1)
cbind(b, c(1, 2))
cbind(b, c(1, 2, 3))

### Generic vectors 

#### Lists 
x = list(A = a, 
     B = b, 
     D = d, 
     E = e)
length(x)

#### Data frames 
y = data.frame(
  x1 = rnorm(n = 10), 
  x2 = rnorm(n = 10),
  x3 = rnorm(n = 10)
)
typeof(y)


#### Model objects
z = data.frame(
  y = rnorm(n = 10),
  x1 = rnorm(n = 10), 
  x2 = rnorm(n = 10),
  x3 = rnorm(n = 10)
)
z

mod = lm(y ~ ., data = z)
typeof(mod)
mod
names(mod)


#### Subsetting 
mod$coefficients

x = list(a = 1:10, 
         b = letters, 
         c = c(TRUE, FALSE))
x[1]
x[[1]]

#### Logical subsetting
?iris
iris[, 2] 
iris[, 2] >= 3.5
iris[iris[, 2] >= 3.5, ]


## Functions 

### Built-in functions 
log(5)
log(b)

#### Calculate proportion (logical subsetting)
mean(iris[, 2] >= 3.5)
?mean

### Functions in packages 
# install.packages("MASS")
library(MASS)
?geyser
?bcv
bcv(geyser$duration)
bcv(geyser$duration,n = 10)

?stepAIC
quine.hi <- lm(log(Days + 2.5) ~ .^4, quine)
quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
quine.stp <- stepAIC(quine.nxt,
                     scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
                     trace = FALSE)

### Write your own functions 
f = function(x) {
  x ^ 2
}
f(x = 10)

## Logical control flow 

### if, else
x = 6
if(x > 10) {
  print("Big")
} else {
  print("medium")
}

# notice that the following chain breaks 
# as soon as the first TRUE is supplied
x = 6
if(x >= 12) {
  print("Very Big")
} else if(x >= 10){
  print("Big")
} else if(x >= 6) {
  print("medium")
} else if(x >= 0) {
  print("small")
} else if(x >= - 10) {
  print("very small")
} else {
  print("tiny")
}


### lapply 
b = 1:10
y = lapply(b, FUN = f)
unlist(y)
?lapply
sapply(b, FUN = f)

#### Model objects
n = 100 
z = data.frame(
  y = rnorm(n = n),
  x1 = rnorm(n = n), 
  x2 = rnorm(n = n),
  x3 = rnorm(n = n), 
  x4 = c("a","b")
)

foo = split(z, as.factor(z$x4))
lapply(foo, FUN = function(x){
  mean(x[, 2])
} )

### Loops 

#### For loop 
for(i in 1:6) {
  print(mean(1:i))
}

n = 1e6
x = double(length = n)
for(i in seq_along(x)) {
  x[i] = i
}
x

#### Do not grow vector 
x = 1
for(i in 2:1e6) {
  x = c(x, i)
}
x


#### While loop 
x = 6
while(x <= 10) {
  x = x + 1
  print(x)
}

### ifelse
iris$Size = ifelse(iris$Sepal.Width >= 3.2, "Big", "Small")
iris

## Basic optimization
f(6)
?optim
optim(par = 4, fn = f)
optim(par = 4, fn = f, method = "Brent", lower = -10, upper = 10)


## Basic plots 

###iris Sepal Length; Petal Length
?plot

plot(iris$Sepal.Length, iris$Petal.Length)
points(iris$Sepal.Length, iris$Petal.Length, col = "red", pch = 19)

plot(iris$Sepal.Length, iris$Petal.Length)
table(iris$Species)
iris_setosa = iris[iris$Species == "setosa", ]
## ...

iris_species = split(iris, f = as.factor(iris$Species))
points(iris_species[[1]][, c("Sepal.Length", "Petal.Length")], col = "blue", pch = 19)
points(iris_species[[2]][, c("Sepal.Length", "Petal.Length")], col = "green", pch = 19)



