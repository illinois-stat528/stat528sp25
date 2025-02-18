
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
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0)) %>% 
  group_by(sex) %>% 
  summarise(n = n(), mean(atleastone))

## race
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0)) %>% 
  group_by(race) %>% 
  summarise(n = n(), mean(atleastone))

## arrestAge
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0)) %>% 
  filter(arrestAge <= 90) %>% 
  ggplot() + 
  aes(x = arrestAge, y = atleastone) + 
  geom_smooth(method = "lm") +
  geom_point() +
  theme_minimal()

## released reason
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0)) %>% 
  group_by(releasedReason) %>% 
  summarise(n = n(), mean(atleastone)) %>% 
  as.data.frame()

## transfers or sentence
CCSO %>% 
  filter(crimeCode == "OTHER TRAFFIC OFFENSES") %>% 
  mutate(atleastone = ifelse(daysInJail >= 1,1,0)) %>% 
  filter(grepl("Transfer|Sentence", releasedReason)) %>% 
  group_by(race) %>% 
  summarise(n = n(), mean(atleastone))



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
m1 = glm(atleastone ~ -1 + race + sex + arrestAge, data = CCSO_small, 
         family = "binomial")
?glm

## x = TRUE returns a model matrix
m1 = glm(atleastone ~ -1 + race + sex + arrestAge, data = CCSO_small, 
         family = "binomial", x = TRUE)
M = m1$x
head(M)

# summary table
names(summary(m1))
is.list(summary(m1))
class(summary(m1))
summary(m1)

## baseline propensities for racial subgroups
betahat = coef(m1)
1/(1 + exp(-betahat[1:4]))

## check standard errors against vcov (all.equal)
?vcov
sqrt(diag(vcov(m1)))
all.equal(summary(m1)$coef[, 2], sqrt(diag(vcov(m1))))
?all.equal
sqrt(.Machine$double.eps)
?.Machine

## compare with saturated model 
summary(m1)
pchisq(m1$deviance, m1$df.residual, lower = FALSE)

## Why does Deviance = -2*log likelihood?
-2 * logLik(m1)
phat = predict(m1, type = "response")
y = CCSO_small$atleastone
-2 * sum(y*log(phat) + (1-y)*log(1-phat))

## compare with intercept only model (what is this?)
pintercept = CCSO_small %>% 
  pull(atleastone) %>% 
  mean()

### deviance test calculated by hand
logLik_intercept = sum(y*log(pintercept) + (1-y)*log(1-pintercept))
logLik_intercept
deviance_intercept = -2*logLik_intercept
deviance_intercept

summary(m1)
pchisq(deviance_intercept - m1$deviance, 5, lower = FALSE)


### using anova
mintercept = glm(atleastone ~ 1, data = CCSO_small, 
                 family = "binomial")
anova(mintercept, m1, test = "LRT")

### estimated probabilities the same as the mean
unique(predict(mintercept, type = "response"))


## investigate the influence of biological sex
m2 = glm(atleastone ~ race + arrestAge, data = CCSO_small, 
         family = "binomial")
summary(m2)
anova(m2, m1, test = "LRT")
AIC(mintercept)
AIC(m1)
AIC(m2)

# careful with interpretation
## fit model with intercept
summary(m1)
m2 = glm(atleastone ~ race + sex + arrestAge, data = CCSO_small, 
         family = "binomial")
cbind(betahat, coef(m2))

## orthogonal transformation
M2 = M %*% eigen(var(M))$vec
m3 = glm(CCSO_small$atleastone ~ -1 + M2, family = "binomial")
cbind(betahat, coef(m2), coef(m3))

### same log likelihood
logLik(m1)
logLik(m2)
logLik(m3)

### same mean-value parameter estimates
all.equal(predict(m1, type = "response"),
predict(m3, type = "response"))

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


###############
### next time
###############

# Esarey and Pierce (2012) diagnostic plot
library(heatmapFit)
y = CCSO_small$atleastone
p1 = predict(m1, type = "response")
heatmap.fit(y = y, pred = p1)

psmall = predict(msmall, type = "response")
heatmap.fit(y = y, pred = psmall)
