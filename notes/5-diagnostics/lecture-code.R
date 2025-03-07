
library(tidyverse)

# implement Esarey and Pierce (2012) by hand

## true data
set.seed(13)
p = 3
n = 5e3
beta = rep(1,p+1)
X = cbind(1, matrix(rnorm(n*p), nrow = n, ncol = p))
Y = rbinom(n, size = 1, prob = 1/(1 + exp(-X %*% beta)))

## conditional success probability estimates under true model
ptrue = predict(glm(Y ~ -1 + X, family = "binomial"), 
                type = "response")


## loess kernel smoother as implemented by EP2012
K = function(x) (1 - abs(x)^3)^3

## implement smoothed local linear regression by hand
wls = function(m, p){
  x = m-p
  w = sapply(x, K)
  as.numeric(coef(lm(Y ~ x, weights = w))[1])
}
m = seq(from = 0, to = 1, by = 1/1000)
eta0 = sapply(m, function(x) wls(x, p=ptrue))  
foo = data.frame(x = x, eta0 = eta0)

## make plot
ggplot(foo) + 
  aes(x = x, y = eta0) +
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) + 
  theme_minimal()


## conditional success probability estimates under incorrect model
pfalse = predict(glm(Y ~ X[, 2], family = "binomial"), type = "response")

## implement smoothed local linear regression by hand
m = seq(from = 0, to = 1, by = 1/1000)
eta0 = sapply(m, function(x) wls(x,p=pfalse))  
foo = data.frame(x = x, eta0 = eta0)

## make plot
ggplot(foo) + 
  aes(x = x, y = eta0) +
  geom_line() + 
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) + 
  theme_minimal()



# implement QERDF with code from Lu Yang

library(tidyverse)
source("stat528sp25/notes/5-diagnostics/yang2021supp.R")


## true model
set.seed(13)
n = 500
p = 2
beta = rep(1,p+1)
M = cbind(1, matrix(rnorm(n*p), nrow = n, ncol = p))
Y = rpois(n = n, lambda = exp(M %*% beta))
m1 = glm(Y ~ -1 + M, family = "poisson")
lambdahat = m1$fitted.values

## grid points as a function of k and x
Delta = function(k, x){
  cumsum(dpois(0:k, lambda = exp(beta %*% x)) )
}

## plot Delta(x) at k = 10
gridpts = sort(unlist(do.call(rbind, lapply(1:1e2, function(j){ 
  Delta(k = 10, x = c(1, rnorm(p))) 
  }))))
hist(gridpts, main = expression(Delta(x) ~ "at" ~ k == 10 ~ ", random x"), 
     xlab = expression(F(k == 10 ~ "|" ~ x)))


## calculate bandwidth parameter to implement QERDF
h = bandwidthp(y = Y, lambdaf = lambdahat)

## diagnostic plot using the marginesti function
data.frame(s = seq(0,1,length = 1001)) %>% 
  mutate(y = sapply(s, function(u) {
    marginesti(u, y = Y, lambdaf = lambdahat)
  })) %>% 
  ggplot() + 
  aes(x = s, y = y) + 
  geom_line() + 
  geom_abline(col = "red", lty = 2) + 
  labs(title = "QERDF for Poisson (true model)", 
       x = "s", y = "U(s)") + 
  theme_minimal()

## wrong model
m2 = glm(Y ~ -1 + M[, -p], family = "poisson")
lambdahat = m2$fitted.values

## calculate bandwidth parameter
h = bandwidthp(y = Y, lambdaf = lambdahat)

## diagnostic plot using the marginesti function
data.frame(s = seq(0,1,length = 1001)) %>% 
  mutate(y = sapply(s, function(u) {
    marginesti(u, y = Y, lambdaf = lambdahat)
  })) %>% 
  ggplot() + 
  aes(x = s, y = y) + 
  geom_line() + 
  geom_abline(col = "red", lty = 2) + 
  labs(title = "QERDF for Poisson (missing covariate)", 
       x = "s", y = "U(s)") + 
  theme_minimal()


### Try this new code for DPIT (solder example)

# solder data from last time
devtools::install_github("jhlee1408/assessor")
library(assessor)
library(faraway) 
library(MASS)
data("solder") # from faraway package


## Negative Binomial
modpnb = glm.nb(skips~.,data=solder)
summary(modpnb)

modpnb2 = glm.nb(skips~. + I(Opening:Mask),data=solder)
modp = glm(skips~.,famil=poisson(link="log"),data=solder)
modp2 = glm(skips~.^2,famil=poisson(link="log"),data=solder)


## resid plots from last time
## make dataframe and ggplot to display residuals
dat = data.frame(mu =  predict(modpnb, type = "response"), 
                 deviance_residuals = residuals(modpnb), 
                 pearson_residuals = residuals(modpnb, "pearson")) %>%
  pivot_longer(., cols = deviance_residuals:pearson_residuals, 
               names_to = "residuals")
dat$residuals = 
  factor(dat$residuals, 
         levels = c("deviance_residuals", "pearson_residuals"), 
         labels = c("Deviance", "Pearson"))
ggplot(dat) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 


dat2 = data.frame(mu =  predict(modpnb2, type = "response"), 
                  deviance_residuals = residuals(modpnb2), 
                  pearson_residuals = residuals(modpnb2, "pearson")) %>%
  pivot_longer(., cols = deviance_residuals:pearson_residuals, 
               names_to = "residuals")
dat2$residuals = 
  factor(dat2$residuals, 
         levels = c("deviance_residuals", "pearson_residuals"), 
         labels = c("Deviance", "Pearson"))

## residual plots
ggplot(dat2) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

## QQ-plot
par(mfrow=c(3,2))
poi.resid = resid_disc(modp,plot = TRUE)
poi2.resid = resid_disc(modp2,plot = TRUE)
norm.resid = resid_disc(modpnb,plot = TRUE, scale = "normal")
unif.resid = resid_disc(modpnb,plot = TRUE, scale = "uniform")
norm2.resid = resid_disc(modpnb2,plot = TRUE, scale = "normal")
unif2.resid = resid_disc(modpnb2,plot = TRUE, scale = "uniform")


# classification example
library(lubridate)
library(caret)
library(pROC)
library(PresenceAbsence)

## balls put into play 2022 season
dat = read_csv("stat528sp25/notes/5-diagnostics/sc-hr-2022.csv")
nrow(dat)
head(dat)

m1 = glm(HR ~ launch_speed + launch_angle, data = dat, 
         family = "binomial")
summary(m1)
pchisq(deviance(m1), df.residual(m1), lower = FALSE)

preds_m1 = predict(m1, type = "response")
mean(preds_m1 >= 0.999)
mean(preds_m1 <= 0.000000001)
dat %>% 
  arrange(desc(launch_speed))

dat %>% 
  filter(HR == 1) %>%  
  arrange(desc(launch_speed))

y = dat$HR
confusionMatrix(
  data = as.factor(as.numeric(preds_m1 >= 0.5)), 
  reference = as.factor(y))

## add a quadratic term for launch angle
m2 = glm(HR ~ launch_speed + poly(launch_angle, 2), 
         data = dat, family = "binomial")
anova(m1, m2, test = "LRT")
AIC(m1, m2)

preds_m2 = predict(m2, type = "response")
confusionMatrix(
  data = as.factor(as.numeric(preds_m2 >= 0.5)), 
  reference = as.factor(y))



roc_m1 = roc(y, preds_m1)
roc_m1
plot(roc_m1, print.auc = TRUE)

roc_m2 = roc(y, preds_m2) 
roc_m2
plot(roc_m2, print.auc = TRUE)

guesses = rbinom(nrow(dat), size = 1, prob = mean(y))
confusionMatrix(
  data = as.factor(as.numeric(guesses >= 0.5)), 
  reference = as.factor(y))

confusionMatrix(
  data = as.factor(c(rep(0,nrow(dat)-1),1)), 
  reference = as.factor(y))

roc_guess = roc(y, guesses) 
plot(roc_guess, print.auc = TRUE)

library(heatmapFit)
heatmap.fit(y = y, preds_m1)
heatmap.fit(y = y, preds_m2)


dat_aug = cbind(dat, preds_m2)
dat_aug %>% arrange(desc(preds_m2))
#https://www.reddit.com/r/baseball/comments/xll84y/would_it_dong_aaron_judge_vs_matt_barnes_repbx/
#https://youtu.be/0XUgX8Rvz-E?start=466&end=479


library(PresenceAbsence)
optimal.thresholds(DATA = data.frame(ID = seq_along(nrow(dat)), 
                                     obs = y, 
                                     pred = preds_m2)) %>% 
  as.data.frame() 

## out-of-sample classification accuracy
index = sample(1:nrow(dat), size=round(0.75*nrow(dat)), replace = FALSE)
train = dat[index, ]
test = dat[-index, ]
m_train = glm(HR ~ launch_speed + poly(launch_angle, 2), 
              data = train, family = "binomial")
y_test = test$HR
preds_test = predict(m_train, newdata = test)
confusionMatrix(
  data = as.factor(as.numeric(preds_test >= 0.5)), 
  reference = as.factor(y_test))

roc_test = roc(y_test, preds_test)
roc_test
plot(roc_test, print.auc = TRUE)


## Jim Albert's work
## https://bayesball.github.io/BLOG/homeruns.html

### A rectangular region is drawn where the launch_angle is 
### between 20 and 35 degrees and the launch_speed is 
### between 95 and 110 mph
foo = dat %>% mutate(red_zone = 
  ifelse(launch_angle >= 20 & 
           launch_angle <= 35 & 
           launch_speed >= 95,1,0))
m3 = glm(HR ~ launch_speed + poly(launch_angle, 2) + 
           red_zone, 
         data = dat, family = "binomial")
preds_m3 = predict(m3, type = "response")
summary(m3)
anova(m1, m2, m3, test = "LRT")

heatmap.fit(y = y, preds_m3)
roc_m3 = roc(y, preds_m3) 
roc_m3
plot(roc_m3, print.auc = TRUE)

confusionMatrix(
  data = as.factor(as.numeric(preds_m3 >= 0.50)), 
  reference = as.factor(y))


dat_aug = cbind(dat, preds_m3)
dat_aug %>% arrange(desc(preds_m3))



### out-of-sample classification accuracy
#### note that the test/train data sets are exactly the same 
#### as before. The only difference is the inclusion of the 
#### red_zone variable
train_m3 = foo[index, ]
test_m3 = foo[-index, ]
m3_train = glm(HR ~ launch_speed + poly(launch_angle, 2) + 
                 red_zone, 
              data = train_m3, family = "binomial")
y_test = test_m3$HR
preds_test = predict(m3_train, newdata = test_m3)
confusionMatrix(
  data = as.factor(as.numeric(preds_test >= 0.5)), 
  reference = as.factor(y_test))

roc_test = roc(y_test, preds_test)
roc_test
plot(roc_test, print.auc = TRUE)

