
library(tidyverse)

# Agresti example setup and summary
x = (1:9 * 10)[-5]
y = c(0,0,0,0,1,1,1,1)
plot(x, y, pch = 19)

## separation vector
b = c(-50, 1) 

## model matrix
M = cbind(1, x)

## check condition
cbind(M %*% b, y)

## model fit
m1 = glm(y ~ x, family = "binomial")

## summary table
summary(m1)

## submodel canonical parameter estimates
betahat = m1$coefficients
betahat

## saturated model canonical parameter estimates
thetahat = as.numeric(M %*% betahat)
thetahat

## saturated model mean value parameter estimates
phat = predict(m1, type = "response")
phat
1/(1 + exp(-thetahat))

## variance matrices
vcov(m1)
eigen(vcov(m1))$val
eigen(solve(vcov(m1)))$val

## LRT
anova(m1, test = "LRT")

m2 = glm(y ~ x, family = "binomial", 
         control = list(maxit = 1e4, epsilon = 1e-100))
summary(m2)

## submodel canonical parameter estimates
betahat2 = m2$coefficients
betahat2

## saturated model canonical parameter estimates
thetahat2 = as.numeric(M %*% betahat2)
thetahat2

## saturated model mean value parameter estimates
phat2 = predict(m2, type = "response")
phat2
1/(1 + exp(-thetahat2))

vcov(m2)
eigen(vcov(m2))$val
eigen(solve(vcov(m2)))$val

cbind(betahat, betahat2)
cbind(thetahat, thetahat2)
cbind(1/(1 + exp(-thetahat)), 1/(1 + exp(-thetahat2)))


## asymptote of log likelihood
asymptote = t(sapply(1:30, function(iter){
  m1 = glm(y ~ x, family = "binomial", 
           control = list(maxit = iter, epsilon = 1e-20))
  c(sqrt(log(crossprod(coef(m1)))), logLik(m1))
}))
asymptote = as.data.frame(asymptote)

library(ggplot2)
ggplot(asymptote) + 
  ggtitle('asymptote of the log likelihood') + 
  labs(x= expression(log(~"||"~beta~"||")), y= "log likelihood") + 
  geom_line(aes(x = V1, y = V2), color = "black") + 
  geom_abline(intercept = 0, slope = 0, lty = 2, color = "red") + 
  theme(legend.position="bottom", panel.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line("lightgrey", size = 0.15), 
        panel.grid.major.y = element_line("lightgrey", size = 0.15), 
        panel.grid.minor.x = element_line("lightgrey", size = 0.07), 
        panel.grid.minor.y = element_line("lightgrey", size = 0.07))

ggplot(asymptote[25:30, ]) + 
  ggtitle('zoomed in') + 
  labs(x= expression(log(~"||"~beta~"||")), y = "") + 
  geom_line(aes(x = V1, y = V2), color = "black") + 
  geom_abline(intercept = 0, slope = 0, lty = 2, color = "red") + 
  theme(legend.position="bottom", panel.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line("lightgrey", size = 0.15), 
        panel.grid.major.y = element_line("lightgrey", size = 0.15), 
        panel.grid.minor.x = element_line("lightgrey", size = 0.07), 
        panel.grid.minor.y = element_line("lightgrey", size = 0.07))


invFI = vcov(m1)
FI = solve(invFI)
eigen(FI)
M %*% eigen(FI)$vec


#devtools::install_github(repo = "cjgeyer/glmdr/package/glmdr")
library(glmdr)
eigen(FI)

m_glmdr = glmdr(y ~ x, family = "binomial")

## summary information
summary(m_glmdr)

## one-sided CIs
CIs = inference(m_glmdr)
cbind(CIs, y, x)

bounds.lower.p = CIs$lower
bounds.upper.p = CIs$upper
par(mar = c(4, 4, 0, 0) + 0.1)
plot(x, y, axes = FALSE, type = "n",
     xlab = expression(x), ylab = expression(mu(x)))
segments(x, bounds.lower.p, x, bounds.upper.p, lwd = 2)
box()
axis(side = 1)
axis(side = 2)
points(x, y, pch = 21, bg = "white")

#### Pick up here from last time

# demo on warning message
z = c((1:8)*10, 1e3)
w = c(0,0,0,1,0,1,1,1,1)
m = glm(w ~ z, family = "binomial")
predict(m, type = "response")

# Agresti example 2
x = c((1:9 * 10)[-5], 50, 50)
y = c(0,0,0,0,1,1,1,1,0,1)
plot(x, y, pch = 19)

## separation vector
b = c(-50, 1) 

## model matrix
M = cbind(1, x)

## check separation condition
cbind(M %*% b, y)

## model fit
m1 = glm(y ~ x, family = "binomial")

## summary table
summary(m1)
1.963 * 50

## submodel canonical parameter estimates
betahat = m1$coefficients
betahat

## saturated model canonical parameter estimates
thetahat = as.numeric(M %*% betahat)
thetahat

## saturated model mean value parameter estimates
phat = predict(m1, type = "response")
phat
1/(1 + exp(-thetahat))

## variance matrices
vcov(m1)
eigen(vcov(m1))$val
eigen(solve(vcov(m1)))$val

eigen(solve(vcov(m1)))
null_vec = eigen(solve(vcov(m1)))$vec[, 2]

## check condition
M %*% null_vec

## LCM
dat = data.frame(y = y, x = x)
m1_LCM = glm(y ~ x, family = "binomial", 
             data = dat, 
             subset = abs(M %*% null_vec) < 1e-6)
summary(m1_LCM)
predict(m1_LCM, type = "response")


## glmdr
library(glmdr)
m1_glmdr = glmdr(y ~ x, data = dat, family = "binomial")
summary(m1_glmdr)

m2_LCM = glm(y ~ x, family = "binomial", 
             data = dat, 
             subset = m1_glmdr$linearity)
summary(m2_LCM)


## inference
CIs = inference(m1_glmdr)

bounds.lower.p = CIs$lower
bounds.upper.p = CIs$upper
thetahat = predict(m2_LCM, se.fit = TRUE)
bounds.0.5 = 1/(1+exp(-qnorm(c(0.025,0.975))*thetahat$se.fit[1]))
bounds.lower.0.5 = bounds.0.5[1]
bounds.upper.0.5 = bounds.0.5[2]

par(mar = c(4, 4, 0, 0) + 0.1)
plot(x, y, axes = FALSE, type = "n",
     xlab = expression(x), ylab = expression(mu(x)))
segments(x[x != 50], bounds.lower.p, 
         x[x != 50], bounds.upper.p, lwd = 2)
segments(50, bounds.lower.0.5, 
         50, bounds.upper.0.5, lwd = 2)
box()
axis(side = 1)
axis(side = 2)
points(x, y, pch = 21, bg = "white")


loglike = function(s) {
  p = 1/(1 + exp(-s * M%*%null_vec))
  p
  #y*log(p) + (1-y)*log(1-p)
}
loglike(1)
loglike(4)
loglike(200)


## Histology example 

library(enrichwith)

?endometrial
data(endometrial)
head(endometrial)

## basic model
m = glm(HG ~ ., data = endometrial, family = "binomial", 
        x = TRUE, y = TRUE)
summary(m)

## FI degeneracy
solve(vcov(m))
eigen(solve(vcov(m)))

## likelihood asymptote
asymptote = t(sapply(1:30, function(iter){
  m_test = glm(HG ~ ., family = "binomial", data = endometrial,
                control = list(maxit = iter, epsilon = 1e-20)) 
  c(sqrt(log(crossprod(coef(m_test)))), logLik(m_test), coef(m_test))
}))
asymptote = as.data.frame(asymptote)
asymptote


## quasi-complete separation in NV
library(tidyverse)
b = c(0,1,0,0)
foo = as.data.frame(cbind(m$y, m$x %*% b))
colnames(foo) = c("y", "NV_observed")
foo %>% 
  group_by(y) %>% 
  reframe(
    NV = names(table(NV_observed)),
    count = table(NV_observed))

## glmdr fit
m_glmdr = glmdr(HG ~ ., data = endometrial, family = "binomial")
m_summary = summary(m_glmdr)
names(m_summary)

m2 = update(m, subset = m_glmdr$linearity)
summary(m2)

## get estimates of mean-value parameters in the LCM
preds = predict(m2, se.fit = TRUE, type = "response")
head(cbind(preds$fit, preds$se.fit))

## get one-sided CIs for constrained responses
preds_constrained = inference(m_glmdr)
cbind(endometrial[!m_glmdr$linearity, ], preds_constrained)

## testing/model comparison
m_small = glm(HG ~ PI + EH, data = endometrial, family = "binomial", 
              x = TRUE, y = TRUE)
anova(m_small, m, test = "LRT")
AIC(m); AIC(m_small)


## Bayesian methods debate

library(arm) # for bayesglm
library(brglm2) # for brglm2


# bayesglm (Gelman)

## fit a few candidate models
bayes_mod1 = bayesglm(HG~.,data=endometrial,family="binomial",
                      prior.scale = 1)
bayes_mod = bayesglm(HG~.,data=endometrial,family="binomial")
bayes_mod5 = bayesglm(HG~.,data=endometrial,family="binomial",
                      prior.scale = 5)
bayes_mod10 = bayesglm(HG~.,data=endometrial,family="binomial",
                       prior.scale = 10)

AIC(bayes_mod); AIC(bayes_mod10)

bayes_mod1e6 = bayesglm(HG~.,data=endometrial,family="binomial",
                        prior.scale = 1e6)

AIC(bayes_mod); AIC(bayes_mod10); AIC(bayes_mod1e6)

## p-value of NV coefficient
c(summary(bayes_mod1)$coef[2,4], 
  summary(bayes_mod)$coef[2,4],
  summary(bayes_mod5)$coef[2,4],
  summary(bayes_mod10)$coef[2,4])

xx = seq(from = 1, to = 5, length = 1e3)
foo = unlist(lapply(xx, function(j){
  summary(bayesglm(HG~.,data=endometrial,family="binomial",
                   prior.scale = j))$coef[2,4]
}))

plot.new()
plot.window(xlim = c(1,5), ylim = c(0.025, 0.0725))
title("Neovasculization p-value vs prior scale")
lines(xx, foo)
axis(1)
axis(2)
abline(h = 0.05, col = "red", lty = 2)
abline(v = 2.5, col = "blue", lty = 1)
mtext("prior scale", side = 1, line = 2.5)
mtext("p-value", side = 2, line = 2.5)


## investigate NV
bayes_mod_small = bayesglm(HG ~ PI + EH, 
                           data=endometrial, family="binomial")
AIC(bayes_mod_small)
AIC(bayes_mod)
summary(bayes_mod_small)
summary(bayes_mod)
anova(bayes_mod_small, bayes_mod, test = "LRT")

## investigate PI
bayes_mod_small = bayesglm(HG ~ NV + EH, 
                           data=endometrial, family="binomial")
AIC(bayes_mod)
AIC(bayes_mod_small)
anova(bayes_mod_small, bayes_mod, test = "LRT")
summary(bayes_mod_small)


## investigate NV when prior scale = 10
bayes_mod10_small = bayesglm(HG ~ PI + EH, 
                             data=endometrial, family="binomial", 
                             prior.scale = 10)
summary(bayes_mod10)
AIC(bayes_mod10_small)
AIC(bayes_mod10)
anova(bayes_mod10_small, bayes_mod10, test = "LRT")

## investigate PI when prior scale = 10
bayes_mod10_small = bayesglm(HG ~ NV + EH, 
                             data=endometrial, family="binomial", 
                             prior.scale = 10)
AIC(bayes_mod10)
AIC(bayes_mod10_small)
anova(bayes_mod10_small, bayes_mod10, test = "LRT")
summary(bayes_mod10_small)



# brglm2 (Firth and Kosmidis)

#brglm2
brglm_mod = glm(HG~.,data=endometrial,family = "binomial",
                method = "brglm_fit", type = "MPL_Jeffreys")
brglm_mod_AS_mean = glm(HG~.,data=endometrial,family = "binomial",
                        method = "brglm_fit", type = "AS_mean")
brglm_mod_AS_median = glm(HG~.,data=endometrial,family = "binomial",
                          method = "brglm_fit", type = "AS_median")
brglm_mod_AS_mixed = glm(HG~.,data=endometrial,family = "binomial",
                         method = "brglm_fit", type = "AS_mixed")
#brglm_mod_AS_correction = glm(HG~.,data=endometrial,family = "binomial",
#                           method = "brglm_fit", type = "correction")


summary(brglm_mod)$coef[2,4]
summary(brglm_mod_AS_mean)$coef[2,4]
summary(brglm_mod_AS_median)$coef[2,4]
summary(brglm_mod_AS_mixed)$coef[2,4]


## investigate NV
summary(brglm_mod)
brglm_mod_small = glm(HG~PI + EH, data=endometrial, 
  family = "binomial",
  method = "brglm_fit", type = "MPL_Jeffreys")
AIC(brglm_mod)
AIC(brglm_mod_small)
anova(brglm_mod_small, brglm_mod, test = "LRT")


