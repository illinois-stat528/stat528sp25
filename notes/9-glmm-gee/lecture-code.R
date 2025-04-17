
library(faraway)
library(tidyverse)

# Binary response example

?ctsib
data(ctsib)

# For the purposes of this analysis, we will reduce the response to a two-point scale: 
# whether the subject was judged completely stable (=1) or not (=0).
ctsib$stable = ifelse(ctsib$CTSIB==1,1,0)
head(ctsib)

xtabs(stable ~ Surface + Vision, ctsib)/80

subsum = ctsib %>% 
  group_by(Subject) %>% 
  summarise(Height=Height[1], 
            Weight=Weight[1], 
            stable=mean(stable), 
            Age=Age[1], 
            Sex=Sex[1])

ggplot(subsum, aes(x=Height,y=stable)) + theme_minimal() + geom_point()
ggplot(subsum, aes(x=Weight,y=stable)) + theme_minimal() + geom_point()
ggplot(subsum, aes(x=Age,y=stable)) + theme_minimal() + geom_point()
ggplot(subsum, aes(x=Sex,y=stable)) + theme_minimal() + geom_boxplot()

## logistic regression; what is the problem here?
gf = glm(stable ~ Sex + Age + Height + Weight + Surface + Vision, 
         family = "binomial",
         data = ctsib)
summary(gf)



## glmer's convergence is quite finicky, best to rescale predictors up front
ctsib = ctsib %>% 
  mutate(Age = scale(Age), 
         Height = scale(Height), 
         Weight = scale(Weight))

# PQL
library(MASS)
modpql = glmmPQL(stable ~ Sex + Age + Height + Weight + Surface + Vision, 
                 random=~1|Subject, 
                 family=binomial, 
                 data=ctsib)
summary(modpql)


# Numerical integration
library(lme4)

## Laplace Approximation
system.time({
  modlap = glmer(stable ~ Sex + Age + Height + Weight + 
                   Surface + Vision + (1|Subject), family=binomial, 
                 data=ctsib)
})
summary(modlap)

## Gauss-Hermite quadrature
system.time({
  modgh = glmer(stable ~ Sex + Age + Height + Weight + Surface + 
                  Vision + (1|Subject), nAGQ=25, family=binomial, data=ctsib, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
})
summary(modgh)

modgh2 = glmer(stable ~ Surface + Vision + (1|Subject), nAGQ=25, 
               family=binomial, data=ctsib)
anova(modgh2, modgh)


## qq plots for residuals broken up by Surface/Vision combinations 

## This is because these were universally unstable conditions and have 
## been predicted as such by the model. In the most stable, normal and 
## open condition, larger positive residuals are not seen because there 
## is no headroom for such cases. It would be a mistake to view this 
## plot as indicating heteroscedascity as we have seen there are more 
## convincing explanations for the differences in spread.

dd = fortify.merMod(modgh2)
ggplot(dd, aes(sample=.resid)) + 
  stat_qq() + 
  theme_minimal() + 
  facet_grid(Surface~Vision)


# INLA

# model fit
library(INLA)
formula = stable ~ Surface + Vision + f(Subject, model="iid")
system.time({
  result = inla(formula, family="binomial", data=ctsib)  
})


## converts the posterior of precision into a posterior 
## for the standard deviation
sigmaalpha = inla.tmarginal(function(x) 1/sqrt(x), 
  result$marginals.hyperpar$"Precision for Subject")

## plot the posterior distribution for SD
x = seq(0,7,length.out = 100)
sdf = data.frame(
  effect = x, 
  density=inla.dmarginal(x, sigmaalpha)
)
ggplot(sdf,aes(x=effect,y=density)) + 
  ggtitle("Posterior distribution for SD") + 
  geom_line() + 
  theme_minimal()

## features of posterior distribution for some variables
restab = sapply(result$marginals.fixed, 
  function(x) inla.zmarginal(x, silent=TRUE))
restab = cbind(restab, inla.zmarginal(sigmaalpha,silent=TRUE))
colnames(restab) = c("intercept","norm","dome","open","SD")
data.frame(restab)

## posterior distribution for fixed effects
x = seq(-2,11,length.out = 100)
rden = sapply(result$marginals.fixed, 
              function(y) inla.dmarginal(x, y))[,-1]
ddf = data.frame(effect=rep(x,3), density=as.vector(rden), 
                 treat=gl(3,100, labels=c("norm","dome","open")))
ggplot(ddf, aes(x=effect, y=density, linetype=treat)) + 
  ggtitle("Posterior densities of the fixed effects") + 
  theme_minimal() + 
  geom_line()



library(glmm)
# subject needs to be a factor
ctsib$SubjectF = as.factor(ctsib$Subject)

set.seed(13)
clust = makeCluster(detectCores() - 2)
system.time({
  m1 = glmm(stable ~ Sex + Age + Height + 
              # random effect distribution centered at 0; no reference group
              Weight + Surface + Vision, random = list(~0+SubjectF), 
            family.glmm=bernoulli.glmm, m = 2e4, 
            varcomps.names = c("Subject"), cluster = clust, 
            data=ctsib)
})

summary(m1)

# standard error
se_glmm = se(m1)
se_glmm

# Monte Carlo standard error
MCse_glmm = mcse(m1)
MCse_glmm

se_glmm / MCse_glmm


# Count response example

data(epilepsy)
?epilepsy

epilepsy$period = rep(0:4, 59)
epilepsy$drug = factor(c("placebo","treatment")[epilepsy$treat+1])
epilepsy$phase = factor(c("baseline","experiment")[epilepsy$expind +1])
epilepsy[epilepsy$id < 2.5,]

epilepsy %>% 
  group_by(drug, phase) %>% 
  summarise(rate=mean(seizures/timeadj)) %>%
  xtabs(formula=rate ~ phase + drug)

ggplot(epilepsy, aes(x=period, y=seizures, linetype=drug, group=id)) + 
  geom_line() + 
  xlim(1,4) + 
  scale_y_sqrt(breaks=(0:10)^2) + 
  theme(legend.position = "top", legend.direction = "horizontal") + 
  theme_minimal()


## We now compare the average seizure rate to the baseline for the two 
## groups. The square-root transform is used to stabilize the variance
ratesum = epilepsy %>%
  group_by(id, phase, drug) %>%
  summarise(rate=mean(seizures/timeadj))
comsum = spread(ratesum, phase, rate)
ggplot(comsum, aes(x=baseline, y=experiment, shape=drug)) + 
  geom_point() + 
  scale_x_sqrt() + 
  scale_y_sqrt() + 
  geom_abline(intercept=0, slope=1) + 
  theme(legend.position = "top", legend.direction = "horizontal") + 
  theme_minimal()

epilo = filter(epilepsy, id != 49)


## GLM

## The interaction term is the primary parameter of interest. 
## All the subjects were untreated in the baseline, even the ones 
## who were subsequently treated.

## offset used to convert to a rate (recall different time windows for data 
## collection periods)
modglm = glm(seizures ~offset(log(timeadj)) + expind + treat + 
               I(expind*treat), family=poisson, data=epilo)
summary(modglm)


## PQL 
modpql = glmmPQL(seizures ~offset(log(timeadj)) + expind + treat + 
  I(expind*treat), random = ~1|id, family=poisson, data=epilo)
summary(modpql)


## numerical integration 
modgh = glmer(seizures ~offset(log(timeadj)) + expind + treat + 
  I(expind*treat)+ (1|id), nAGQ=25, family=poisson, data=epilo)
summary(modgh)

## seizure reduction rate
exp(-0.302)

## However, the subject SD is more than twice the drug effect of -0.3 at 0.718.

## INLA
formula = seizures ~ offset(log(timeadj)) + expind + treat + 
  I(expind*treat) + f(id,model="iid")
result = inla(formula, family="poisson", data = epilo)


sigmaalpha = inla.tmarginal(function(x) 1/sqrt(x), 
  result$marginals.hyperpar$"Precision for id")
restab = sapply(result$marginals.fixed, 
  function(x) inla.zmarginal(x, silent=TRUE))
restab = cbind(restab, 
  inla.zmarginal(sigmaalpha, silent=TRUE))
colnames(restab) = c("intercept","expind","treat",
                     "interaction","SD")
data.frame(restab)


x = seq(-0.75,0.75,length.out = 100)
rden = sapply(result$marginals.fixed,function(y) inla.dmarginal(x, y))[,-1]
ddf = data.frame(domain=rep(x,3), 
                 density=as.vector(rden), 
                 treat=gl(3,100, labels=c("expind","treat","interaction")))
ggplot(ddf, aes(x=domain, y=density, linetype=treat)) + 
  geom_line() + 
  theme_minimal()


# GEE
library(geepack)

## Binary response
modgeep = geeglm(stable ~ Sex + Age + Height + Weight + Surface + Vision, 
                 id=Subject, corstr="exchangeable", scale.fix=TRUE, 
                 data=ctsib, family=binomial)

## We can see that the estimated correlation between observations on the 
## same subject is 0.22 with a standard error of 0.04.
summary(modgeep)

## exchangeable correlation
modgeep2 = geeglm(stable ~ Sex + Age + Height + Weight + Surface,
                  id =Subject, corstr="exchangeable", scale.fix=TRUE, data=ctsib, 
                  family=binomial)
anova(modgeep2, modgeep)


## Count response 

## autoregressive correlation
modgeep = geeglm(seizures ~offset(log(timeadj)) + expind + treat + 
                   I(expind*treat), id=id, family=poisson, corstr="ar1", 
                 data=epilepsy, 
                 subset=(id!=49))
summary(modgeep)





