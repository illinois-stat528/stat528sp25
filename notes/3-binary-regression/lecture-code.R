
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


# profile likelihood 
beta.profile = confint(m1)
beta.profile

se = sqrt(diag(vcov(m1)))
cbind(betahat + qnorm(0.025) * se,
      betahat + qnorm(0.975) * se)

# Esarey and Pierce (2012) diagnostic plot
library(heatmapFit)
y = CCSO_small$atleastone
p1 = predict(m1, type = "response")
heatmap.fit(y = y, pred = p1)

psmall = predict(msmall, type = "response")
heatmap.fit(y = y, pred = psmall)


# casual inference example: in-person learning
dat = read.csv("stat528sp25/notes/3-binary-regression/online.csv", 
               header = TRUE)[, -1]
head(dat)

dat_small = dat %>% dplyr::select(Online, ACTMath, ACTMajor, ACT, Gender, 
  International, F17, S18, S19, Fa19, FR, SO, JR)

# prop score model
m = glm(Online ~., data = dat_small, family = "binomial")
trt = dat_small$Online
preds = predict(m, type = "response")

## seems okay
trt = dat_small$Online
heatmap.fit(trt, preds)

## inverse propensity score weights
weights_IPW = (trt / preds - (1-trt) / (1 -preds))
mean(weights_IPW * dat$ObjExam)

### they are not too large
max(abs(weights_IPW))
weights_IPW[abs(weights_IPW) > 8]

## estimate the stabilized IPW weights
weights_alt_trt = 1 / sum(trt / preds) * trt /preds
weights_alt_notrt = 1 / sum((1 - trt)/(1 - preds)) * (1-trt)/(1-preds)
dat = data.frame(dat, weights = weights_alt_trt - weights_alt_notrt)

# check balance for gender and international (other variables are also balanced)
## balance is pretty good
dat %>% group_by(Gender, Online) %>% summarise(sum(weights))
dat %>% group_by(International, Online) %>% summarise(sum(weights))

## ATE alt
ATE_alt = sum(weights_alt_trt * dat$ObjExam) -
  sum(weights_alt_notrt * dat$ObjExam)
ATE_alt

## DR estimate
m_trt = lm(ObjExam ~ ACTMath + ACTMajor + ACT + International + Gender +
             FR + SO + JR + F17 + S18 + S19,
           data = dat[trt == 1, ])
Y_trt = predict(m_trt, newdata = dat)
m_notrt = lm(ObjExam ~ ACTMath + ACTMajor + ACT + International + Gender +
               FR + SO + JR + F17 + S18 + S19,
             data = dat[trt == 0, ])
Y_notrt = predict(m_notrt, newdata = dat)
ATE_DR = mean( (dat$ObjExam * trt - (trt - preds) * Y_trt) / preds -
                 (dat$ObjExam * (1 - trt) + (trt - preds)*Y_notrt) / (1 - preds))
ATE_DR

## nothing 
m1 = lm(ObjExam ~ trt + ACTMath + ACTMajor + ACT + International + Gender +
     FR + SO + JR + F17 + S18 + S19,
   data = dat)
summary(m1)



# nonparametric bootstrap (be mindful of sparse categorical 
# level in this demonstration)

head(CCSO_small)

## reorder factor variable levels
CCSO_small$race = 
  relevel(CCSO_small$race, ref = 2)

## remove sparse category
CCSO_small2 = CCSO_small %>% 
  filter(race != "Asian/Pacific Islander")

## slow
betastar = function(){
  mod = glm(atleastone ~ race + sex + arrestAge, 
            data = CCSO_small[sample(1:nrow(CCSO_small), replace = TRUE), ], 
            family = "binomial")
  coef(mod)
}
B = 2e3
system.time({
  bootsamp = do.call(rbind, lapply(1:B, function(j) betastar() ))  
})
bootse = sqrt(diag(var(bootsamp)))

m1 = glm(atleastone ~ race + sex + arrestAge, 
         data = CCSO_small, 
         family = "binomial")

cbind(bootse, summary(m1)$coef[, 2])


## faster (mclapply)
library(parallel)
ncores = detectCores() - 2

system.time({
  bootsamp2 = do.call(rbind, mclapply(1:B, function(j) betastar(), 
                                      mc.cores = ncores))  
})
bootse2 = sqrt(diag(var(bootsamp2)))
cbind(bootse, bootse2, summary(m1)$coef[, 2])

## faster (foreach)
library(foreach)
library(doParallel)
system.time({
  myCluster = makeCluster(ncores) # number of cores to use
  registerDoParallel(myCluster)
  bootsamp = foreach(i=1:2e3, .combine=rbind) %dopar% betastar()  
  stopCluster(myCluster) # stop cluster when done
})
bootse3 = sqrt(diag(var(bootsamp)))


## compare variability
round(cbind(summary(m1)$coef[, 2], 
            bootse,
            bootse2,
            bootse3), 3)


