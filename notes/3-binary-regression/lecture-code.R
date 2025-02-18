
library(tidyverse)

# Get CCSO data
library(stat528materials)
data("CCSO")
CCSO
?CCSO

# restrict attention to "other traffic offenses"
CCSO %>% 
  group_by(crimeCode) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# create at least one day in jail 
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0))

# propensity of at least day in jail across demographic
# variables

## biological sex

## race

## age at arrest

## released reason

## transfers or sentence

# data transformation (simple demographic variables)
CCSO_small = CCSO %>% 
  mutate(atleastone = ifelse(daysInJail > 0,1,0)) %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>%  
  filter(race %in% c("Asian/Pacific Islander","Black","White","Hispanic")) %>% 
  filter(sex %in% c("Female","Male")) %>% 
  dplyr::select(atleastone, arrestAge, sex, race, bookingDate) %>%  
  mutate(race = fct_drop(race), sex = fct_drop(sex))
CCSO_small = CCSO_small[complete.cases(CCSO_small), ]
head(CCSO_small)
dim(CCSO_small)

# fit a basic logistic regression model (race, sex, arrestAge)
?glm

## x = TRUE returns a model matrix
M = m1$x
head(M)

# summary table
names(summary(m1))
is.list(summary(m1))
class(summary(m1))
summary(m1)

## baseline propensities for racial subgroups

## check standard errors against vcov (all.equal)
?vcov

?all.equal
sqrt(.Machine$double.eps)
?.Machine

## compare with saturated model 

## Why does Deviance = -2*log likelihood?

## compare with intercept only model (what is this?)


### deviance test calculated by hand
logLik_intercept = sum(y*log(pintercept) + (1-y)*log(1-pintercept))
logLik_intercept
deviance_intercept = -2*logLik_intercept
deviance_intercept


### using anova
mintercept = glm(atleastone ~ 1, data = CCSO_small, 
                 family = "binomial")
anova(mintercept, m1, test = "LRT")

### estimated probabilities the same as the mean


## investigate the influence of biological sex


# careful with interpretation
## fit model with intercept

## orthogonal transformation
M2 = M %*% eigen(var(M))$vec
m3 = glm(CCSO_small$atleastone ~ -1 + M2, family = "binomial")
cbind(betahat, coef(m2), coef(m3))

### same log likelihood

### same mean-value parameter estimates

# sub model canonical parameter vector (beta)
betahat

# saturated model canonical parameter vector (theta)
thetahat = as.vector(M %*% betahat)
thetahat

# saturated model mean-value parameter vector (mu; p)
muhat = 1/(1 + exp(-thetahat))
phat = predict(m1, type = "response")
all.equal(muhat, as.vector(phat))

# sub model mean-value parameter vector (tau)
as.vector(t(M) %*% muhat)
crossprod(M, muhat)
?crossprod

tauhat = crossprod(M, muhat)
tauhat

## observed = expected (full regular exponential family fact)
cbind(tauhat, crossprod(M, y))


# Esarey and Pierce (2012) diagnostic plot
library(heatmapFit)
y = CCSO_small$atleastone
p1 = predict(m1, type = "response")
heatmap.fit(y = y, pred = p1)

psmall = predict(msmall, type = "response")
heatmap.fit(y = y, pred = psmall)
