
# load in software
library(tidyverse)
library(faraway)
head(gala)
?gala

# discretize Area
gala = gala %>% 
  mutate(Size = as.factor(1 + ifelse(Area > 1,1,0) + ifelse(Area > 25,1,0)))

# fit model (main effects only; no Area; no Endemics)
m1 = glm(Species ~ Elevation + Nearest + Scruz + Adjacent + Size, 
         family = "poisson", data = gala, x = TRUE)

## model matrix
M = m1$x
M

## summary table
summary(m1)

# parameterizations

# beta -> theta
beta = coef(m1)
theta = as.numeric(M %*% beta)

# theta -> mu
mu = exp(theta)
all.equal(mu, as.numeric(predict(m1, type = "response")))

# test against intercept only (null) model

## using glm
m_null = glm(Species ~ 1, family = "poisson", data = gala, x = TRUE)
anova(m_null, m1, test = "LRT")
all.equal(m_null$deviance, m1$null.deviance)
pchisq(m1$null.deviance - m1$deviance, df = m1$df.null - m1$df.residual, 
       lower = FALSE)


## null model is iid Poisson model (observed = expected property)
mean(gala$Species)
exp(coef(m_null))
summary(m_null)

## AIC/BIC
AIC(m_null, m1)
BIC(m_null, m1)

# test against a smaller model with Elevation removed
summary(m1)
m2 = glm(Species ~  Nearest + Scruz + Adjacent + Size, 
         family = "poisson", data = gala, x = TRUE)

## AIC/BIC
AIC(m_null, m1, m2)
BIC(m_null, m1, m2)

# test levels of the Size variable
summary(m1)

## Delta method
beta = coef(m1)
cand = c(0,0,0,0,0,-1,1)
grad = cand * exp(beta)
est = sum(grad)
est

FI = vcov(m1)
SE = as.vector(sqrt(t(grad) %*% FI %*% grad))

## effect size and CI
est/SE
est + c(-1,1) * qnorm(0.975) * SE


## Bonferroni correction (three natural tests to conduct)
est + qnorm(c(0.025/3, 1-0.025/3)) * SE


# Diagnostics 

## Pearson residuals
residuals(m1, "pearson")

## Deviance residuals
residuals(m1)

## not the same but close
cbind(residuals(m1, "pearson"), residuals(m1))

## directionally the same
#as.vector(rowSums(apply(cbind(residuals(m1, "pearson"), residuals(m1)), 2, sign))/ 2)


## make dataframe and ggplot to display residuals
dat = data.frame(theta = as.numeric(M %*% betahat), 
                 mu = exp(as.numeric(M %*% betahat)), 
                 deviance_residuals = residuals(m1), 
                 pearson_residuals = residuals(m1, "pearson")) %>%
  pivot_longer(., cols = deviance_residuals:pearson_residuals, 
               names_to = "residuals")
dat$residuals = 
  factor(dat$residuals, 
         levels = c("deviance_residuals", "pearson_residuals"), 
         labels = c("Deviance", "Pearson"))

## on canonical scale
ggplot(dat) + 
  aes(x = theta, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

## on mean-value scale
ggplot(dat) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

## hard to tell (add smoother)
ggplot(dat) + 
  aes(x = theta, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  facet_wrap(~residuals) + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

ggplot(dat) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

halfnorm(residuals(m1), pch = 19)

## compare with saturated model
pchisq(m1$deviance, m1$df.residual, lower = FALSE)

dat = data.frame(observed = gala$Species, 
                 estimated = predict(m1, type = "response"))
dat %>% arrange(desc(observed))

## variance vs mean plot
plot(log(fitted(m1)),log((gala$Species-fitted(m1))^2), 
     xlab= expression(hat(mu)),ylab=expression((y-hat(mu))^2), 
     pch = 19, main = "variance vs mean (on log scale)")
abline(0,1)

# overdispersion
n = nrow(gala)
p = length(betahat)
y = gala$Species

## estimate dispersion directly
fits = predict(m1, type = "response")
dp = sum((y - fits)^2/fits) / (n - p)
summary(m1, dispersion = dp)

## fit the model with dispersion
m2 = glm(Species ~ Elevation + Nearest + Scruz + Adjacent + Size, 
         family = "quasipoisson", data = gala, x = TRUE)
summary(m2)

## dispersion and sqrt(dispersion)
c(dp, sqrt(dp))

## basic model outputs
## coefficient estimates are the same; standard errors are different
se = function(model) sqrt(diag(vcov(model)))
round(data.frame(coef.m1=coef(m1), 
                 coef.m2=coef(m2), 
                 se.m1=se(m1), 
                 se.m2=se(m2), 
                 ratio=se(m2)/se(m1)), 4)

## revisit our test between levels (with overdispersion)
summary(m2)
comp = c(0,0,0,0,0,-1,1)
betahat = m2$coefficients
grad = exp(betahat) * comp
est = sum(grad)
est

InvFish = vcov(m2) # this will be different
asympVar = as.numeric(t(grad) %*% InvFish %*% grad)
asympVar
SE = sqrt(asympVar)
SE

est/SE
est + qnorm(c(0.025,0.975)) * SE

## Bonferroni correction
est + qnorm(c(0.025/3, 1-0.025/3)) * SE

## is overdispersion present?
library(AER)
dispersiontest(m1, trafo = 1)


# START HERE 


# negative binomial regression
library(faraway)
library(tidyverse)
?solder
head(solder)
solder %>% arrange(desc(skips))

## fit with Poisson
modp = glm(skips ~., data = solder, family = "poisson")
summary(modp)
pchisq(deviance(modp), df.residual(modp), lower = FALSE)


## fit with Poisson quadratic
n = nrow(solder)
modp2 = glm(skips ~.^2, data = solder, family = "poisson") 
summary(modp2) 
# why?
# https://en.wikipedia.org/wiki/One_in_ten_rule
n / length(coef(modp2)) # not quite 1 in 10
# Other studies, however, show that the one in ten rule may be 
# too conservative as a general recommendation and that five 
# to nine events per predictor can be enough, depending on 
# the research question.
anova(modp, modp2)
pchisq(deviance(modp2), df.residual(modp2), lower = FALSE)

## check for outliers and overdispersion

### half normal plot
halfnorm(modp2) # that's annoying

### build it by hand
n = nrow(solder)
dat = data.frame(
  resid = sort(residuals(modp2)), 
  q = qnorm( (n + 1:n) / (2*n + 1))
)

ggplot(dat) + 
  aes(x = q, y = resid) + 
  geom_point() + 
  labs(title = "Half-normal plot") + 
  theme_minimal()

### overdispersion?
plot(predict(modp2, type = "response"), (solder$skips - fitted(modp2))^2, 
     xlab= expression(hat(theta)),ylab=expression((y-hat(mu))^2), 
     pch = 19)
lines(sort(predict(modp2, type = "response")), 
      sort(fitted(modp2)), lty="dashed", 
      col = "red")
library(AER)
dispersiontest(modp, trafo=1)
dispersiontest(modp2, trafo=1)

## try negative binomial regression
library(MASS)
modn = glm(skips ~ ., negative.binomial(1), solder)
summary(modn)
pchisq(modn$deviance, modn$df.residual, lower = FALSE)

## get optimal k
?glm.nb
modnk = glm.nb(skips ~ ., solder)
summary(modnk)

## look at coefficients from Poisson and neg Binom fits
sort(coef(modp2), decreasing = TRUE)[1:5]
sort(coef(modn), decreasing = TRUE)[1:5]
sort(coef(modnk), decreasing = TRUE)[1:5]

## make dataframe and ggplot to display residuals
dat = data.frame(mu =  predict(modn, type = "response"), 
                 deviance_residuals = residuals(modn), 
                 pearson_residuals = residuals(modn, "pearson")) %>%
  pivot_longer(., cols = deviance_residuals:pearson_residuals, 
               names_to = "residuals")
dat$residuals = 
  factor(dat$residuals, 
         levels = c("deviance_residuals", "pearson_residuals"), 
         labels = c("Deviance", "Pearson"))

## residuals
ggplot(dat) + 
  aes(x = mu, y = value) + 
  labs(y = "residuals") + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~residuals) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_minimal() 

## bad match between estimates and observations 
## when the number of skips is large
solder %>% 
  mutate(preds = predict(modn, type = "response")) %>% 
  arrange(desc(preds)) %>% 
  head()

summary(modn)

## Guess: add Opening:Mask interaction
modn2 = glm(skips ~ . + I(Opening:Mask), negative.binomial(1), solder)
summary(modn2)
anova(modn, modn2, test = "LRT")

## make dataframe and ggplot to display residuals
dat2 = data.frame(mu =  predict(modn2, type = "response"), 
                 deviance_residuals = residuals(modn2), 
                 pearson_residuals = residuals(modn2, "pearson")) %>%
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

## closer match between estimates and observations 
## when the number of skips is large
solder %>% 
  mutate(preds = predict(modn2, type = "response")) %>% 
  arrange(desc(preds)) %>% 
  head()


# zero-inflated regression
library(pscl)
?bioChemists

## Poisson fit
modp = glm(art ~ ., data=bioChemists, family=poisson)

## test against saturated
pchisq(deviance(modp), df.residual(modp), lower = FALSE)

## look at data
table(bioChemists$art)
ocount = table(bioChemists$art)[1:8]
pcount = colSums(predprob(modp)[,1:8])
?predprob

plot(pcount, ocount, type="n", xlab="Predicted", ylab="Observed", 
     ylim = c(0, 300), axes = FALSE)
axis(side = 1)
axis(side = 2)
text(pcount,ocount, 0:7)

## on probability scale
probs = dpois(0:7, mean(bioChemists$art))
obs_freq = table(bioChemists$art)[1:8]/sum(table(bioChemists$art)[1:8])
plot(probs, obs_freq, 
     type="n", xlab="Predicted", ylab="Observed")
axis(side = 1)
axis(side = 2)
text(probs, obs_freq, 0:7)

## zero-inflated model
modz = zeroinfl(art ~ ., data=bioChemists)
summary(modz)

## smaller model
modz2 = zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists)

## summary table for smaller model
summary(modz2)

## test of nested models
anova(modz2, modz, test = "LRT") 
# didn't work; package authors didn't write an anova method for 
# an object of class zeroinf; you could do it and potentially 
# be an author of pscl

## do by hand
pchisq(-2*(modz2$loglik - modz$loglik), 6, lower = FALSE)

## get estimates of mean-value parameters by hand
modz2 = zeroinfl(art ~ fem+kid5+ment | ment, data=bioChemists, 
                 x = TRUE)

### get betas for binomial and Poisson components
M = modz2$x
betahat = coef(modz2)
betahatp = betahat[1:4]
betahatb = betahat[5:6]
Mp = M[[1]]
Mb = M[[2]]

### get theta for binomial and Poisson components
thetap = as.numeric(Mp %*% betahatp)
thetab = as.numeric(Mb %*% betahatb)

### get mus for binomial and Poisson components
mup = exp(thetap)
mub = 1/(1 + exp(-thetab))

### get mu for 0-inflated Poisson
muZIP = (1 - mub) * mup

### compare with software
preds = as.numeric(predict(modz2))
all.equal((1 - mub) * mup, preds)




