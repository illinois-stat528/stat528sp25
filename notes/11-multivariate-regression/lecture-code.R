
data(mtcars)
head(mtcars)
?mtcars

# multivariate regression model
mtcars$cyl <- factor(mtcars$cyl)
Y <- as.matrix(mtcars[,c("mpg","disp","hp","wt")])
m <- lm(Y ~ cyl + am + carb, data=mtcars, x = TRUE)

## estimate of beta' from lm
betahat = coef(m)
betahat

## estimate of beta by hand (compare with above)
X = m$x
head(X)
betahat_direct = t(Y) %*% X %*% solve(crossprod(X))
all.equal(betahat_direct, t(betahat))


## separate regressions
m_mpg = lm(mpg ~ cyl + am + carb, data=mtcars)
m_disp = lm(disp ~ cyl + am + carb, data=mtcars)
m_hp = lm(hp ~ cyl + am + carb, data=mtcars)
m_wt = lm(wt ~ cyl + am + carb, data=mtcars)
betahat_sep = rbind(coef(m_mpg), 
      coef(m_disp),
      coef(m_hp),
      coef(m_wt))
betahat_sep
betahat_sep - t(betahat)
all.equal(betahat_sep,t(betahat))
all.equal(unname(betahat_sep), unname(t(betahat)))

## fitted values Yhat
head(X)
all.equal(X %*% betahat, m$fitted.values)


## estimates of Sigma
SSE = crossprod(Y - m$fitted.values)
n = nrow(Y)
p = nrow(coef(m)) # includes an intercept
SigmaMLE = SSE / n
SigmaMLE
Sigmahat = SSE / (n - p)
Sigmahat


## var(vec(beta'))
?vcov
dim(vcov(m))
dim(betahat)
vcov(m)
unique(round(vcov(m) - kronecker(Sigmahat, solve(crossprod(X))), 10))



## summary table from lm
msum = summary(m)

## summary table for first component of Y: mpg
head(Y)
msum[[1]]
betahat[, 1]

## summary table for second component of Y: disp
head(Y)
msum[[2]]
betahat[, 2]

### summary table for mpg from theory (they are the same)
msum2 = cbind(coef(m)[, 1], sqrt(diag( kronecker(Sigmahat, solve(crossprod(X))) ))[1:5])
msum2 = cbind(msum2, msum2[, 1] / msum2[, 2])
msum2 = cbind(msum2, sapply(msum2[, 3], function(x) pt(abs(x), df = n - p, lower = FALSE)*2 ))
msum2
msum[[1]]$coef


## different multivariate tests
m0 = lm(Y ~ am + carb, data=mtcars)
anova(m0, m, test="Wilks")
anova(m0, m, test="Pillai")

Etilde = n * SigmaMLE
SigmaTilde1 = crossprod(Y - m0$fitted.values) / n
Htilde = n * (SigmaTilde1 - SigmaMLE)
HEi = Htilde %*% solve(Etilde)
HEi.values = eigen(HEi)$values
c(Wilks = prod(1 / (1 + HEi.values)), Pillai = sum(HEi.values / (1 + HEi.values)))


## confidence point-estimate
newdata = data.frame(cyl=factor(6, levels=c(4,6,8)), am=1, carb=4)
predict(m, newdata, interval="confidence")

## prediction point-estimate
newdata = data.frame(cyl=factor(6, levels=c(4,6,8)), am=1, carb=4)
predict(m, newdata, interval="prediction")

## interval estimation from Nate Helwig
pred.mlm = function(object, newdata, level=0.95,
                    interval = c("confidence", "prediction")){
  form = as.formula(paste("~",as.character(formula(object))[3]))
  xnew = model.matrix(form, newdata)
  fit = predict(object, newdata)
  Y = model.frame(object)[,1]
  X = model.matrix(object)
  n = nrow(Y)
  r = ncol(Y)
  p = ncol(X) - 1
  sigmas = colSums((Y - object$fitted.values)^2) / (n - p - 1)
  fit.var = diag(xnew %*% tcrossprod(solve(crossprod(X)), xnew))
  if(interval[1]=="prediction") fit.var = fit.var + 1
  const = qf(level, df1=r, df2=n-p-r) * r * (n - p - 1) / (n - p - r)
  vmat = (n/(n-p-1)) * outer(fit.var, sigmas)
  lwr = fit - sqrt(const) * sqrt(vmat)
  upr = fit + sqrt(const) * sqrt(vmat)
  if(nrow(xnew)==1L){
    ci = rbind(fit, lwr, upr)
    rownames(ci) = c("fit", "lwr", "upr")
  } else {
    ci = array(0, dim=c(nrow(xnew), r, 3))
    dimnames(ci) = list(1:nrow(xnew), colnames(Y), c("fit", "lwr", "upr") )
    ci[,,1] = fit
    ci[,,2] = lwr
    ci[,,3] = upr
  }
  ci
}


## confidence interval
newdata = data.frame(cyl=factor(6, levels=c(4,6,8)), am=1, carb=4)
pred.mlm(m, newdata)

## prediction interval
newdata = data.frame(cyl=factor(6, levels=c(4,6,8)), am=1, carb=4)
pred.mlm(m, newdata, interval="prediction")



# envelope modeling 

library(Renvlp)
library(tidyverse)
library(ggplot2)
library(reshape2)
data(wheatprotein)

dat = data.frame(Y1 = wheatprotein[, 1] - mean( wheatprotein[, 1]), 
                 Y2 = wheatprotein[, 2] - mean( wheatprotein[, 2]),
                 X  = wheatprotein[, 8])
dat$X = as.factor(dat$X)
foo = unlist(lapply(split(dat, f = dat$X), function(xx) colMeans(xx[, 1:2])))
dat_means = data.frame(Y1 = foo[c(1,3)], Y2 = foo[c(2,4)])

## plot data
ggplot(dat) + aes(x = Y1, y = Y2, color = X) + 
  geom_point() + 
  theme_minimal() + 
  geom_point(data=dat_means,  mapping=aes(x = Y1, y = Y2), col="black") + 
  stat_ellipse(geom = "polygon", aes(fill = X), alpha = 0.20)


## which dimension?
u.env(X = as.numeric(dat$X), Y = dat[, 1:2])

## ratios at u = 1
env_mod = env(X = as.numeric(dat$X), Y = dat[, 1:2], u = 1)
env_mod$ratio

env_mod$beta
env(X = as.numeric(dat$X), Y = dat[, 1:2], u = 2)$beta


## add envelope subspace
dat_means2 = data.frame(
  Y1 = c(env_mod$mu[1] + 4*env_mod$beta[1], 
         env_mod$mu[1] - 1.5*env_mod$beta[1]), 
  Y2 = c(env_mod$mu[2] + 4*env_mod$beta[2], 
         env_mod$mu[2] - 1.5*env_mod$beta[2]))
ggplot(dat) + aes(x = Y1, y = Y2, color = X) + 
  geom_point() + 
  theme_minimal() + 
  geom_line(data = dat_means2, 
            mapping = aes(x = Y1, y = Y2), 
            col="black") + 
  stat_ellipse(geom = "polygon", 
               aes(fill = X), 
               alpha = 0.20)



## weighted envelope estimation
set.seed(13)
wtenv = weighted.env(X = as.numeric(dat$X), Y = dat[, 1:2], bstrpNum = 1e3)

## ratios wrt to weighted envelope estimator after bootstrapping
wtenv$ratios

## ratios conditional on u = 1
env_mod$ratio

## number of times each dimension is selected
wtenv$bic_select
