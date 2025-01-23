
# Week 1: Tuesday

## R basics

### Calculator 
3 + 5
3 * 8

### Basic objects (global environment)
x = 5
a = "A"
z = 5L

### Generate random data 
set.seed(528)
y = rnorm(n = 1)

### Logical operators

# & | <= >= ==
x = TRUE
x == TRUE
x == FALSE
x != FALSE
3 == 3
(6 <= 3) & (4 <= 7)
(6 <= 3) | (4 <= 7)

## Vectors 

### Atomic vectors 
x = c(1,2,3,4)

#### numeric
x = 1:4

#### logical
x = c(TRUE, FALSE, TRUE)

#### character
x = letters

#### Type and length 
typeof(3)
typeof(3L)
typeof(x)
length(x)

#### Coercion 

##### explicit coercion
y = c("1","2","3","47")
typeof(y)
z = as.double(y)

##### implicit coercion
TRUE & 1
TRUE & 0

##### length coercion
x = rnorm(n = 10)
cbind(1, x)


### Generic vectors 

#### Lists 
z = list(a = 1:10, 
         b = letters[1:5],
         c = c(TRUE, FALSE))
length(z)

#### Data frames 
z = list(a = 1:10, 
         b = rnorm(10),
         c = rep(TRUE, 10))
length(z)
as.data.frame(z)
typeof(z$b)

#### Data frames in packages
#install.packages("Lahman")
library(Lahman)
?Batting
Batting



#### Basic subsetting 


#### Logical subsetting


#### Extracting elements from generic vector (list)
x = list(a = 1:10, 
         b = letters, 
         c = c(TRUE, FALSE))
x[1]
x[[1]]
x[1:2]
x[[1:2]]

#### Extracting elements from named generic vector (list)


#### Model objects


#### Calculate proportion (logical subsetting)
mean(Batting$HR >= 30)


## Functions 

### Built-in functions 
log(5)

### Vectorization 
log(x[1])
log(x[[1]])



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

n = 3e4
system.time({
  y = double(length = n)
  for(i in seq_along(y)) {
    y[i] = i
  }
  print(y)
})


#### Do not grow vector 
y2 = 1
system.time({
  for(i in 2:n) {
    y2 = c(y2, i)
  }
})

all.equal(y, y2)


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



