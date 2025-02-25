
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

## summary table

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

# test against a smaller model with Elevation removed

## AIC/BIC

# test levels of the Size variable

## Delta method

## effect size and CI


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


