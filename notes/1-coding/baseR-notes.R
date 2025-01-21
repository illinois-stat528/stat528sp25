
# Week 1: Tuesday

## R basics

### Calculator 

### Basic objects (global environment)

### Generate random data 


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

#### numeric

#### logical

#### character

#### Type and length 
typeof(3)
typeof(3L)

#### Coercion 

##### explicit coercion

##### implicit coercion
TRUE & 1
TRUE & 0

##### length coercion
cbind(b, 1)


### Generic vectors 

#### Lists 


#### Data frames 

#### Data frames in packages
install.packages("Lahman")
library(Lahman)
?Batting

#### Model objects


#### Subsetting 

##### Logical subsetting

##### Extracting elements from generic vector (list)
x = list(a = 1:10, 
         b = letters, 
         c = c(TRUE, FALSE))
x[1]
x[[1]]

##### Extracting elements from named generic vector (list)

##### model object


## Functions 

### Built-in functions 
log(5)

### Vectorization 
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
bcv(geyser$duration, nb = 10)


### Write your own functions 

#### Basic example



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


### apply functions
?lapply
?sapply


#### data frame example
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


## Very basic optimization
g = function(x) {
  (x - 5)^2 + x
}
?optim
optim(par = 4, fn = g)
optim(par = g, fn = g, method = "Brent", lower = -10, upper = 10)


## Basic plots 

### plot


### histogram
Batting_fulltime = Batting[Batting$AB >= 400, ]

hist(Batting_fulltime[Batting_fulltime$yearID == 2019, ]$HR)

hist(Batting_fulltime[Batting_fulltime$yearID == 2019, ]$HR, 
     main = "Histogram of Home Runs in 2019", 
     xlab = "Home Runs")



