library(tidyverse)
library(lme4)
library(ggplot2)
library(faraway)


# ANOVA and REML
data(pulp)
?pulp
## note that we are changing the contrasts to sum contrasts
op = options(contrasts = c("contr.sum", "contr.poly"))
## aov is a wrapper for lm; results more appropriate for ANOVA models
lmod = aov(bright ~ operator, pulp)
summary(lmod)
coef(lmod)

## (MSA - MSE) / n 
pulp %>% group_by(operator) %>%  summarise(n = n())
sigma.sq_alpha = (0.4467 - 0.1062) / 5
sigma.sq_alpha

mmod = lmer(bright ~ 1 + (1|operator), pulp, REML = TRUE)
summary(mmod)


# Parametric bootstrap

## null model fit
nullmod = lm(bright ~ 1, pulp) 

# alternative model fit
smod = lmer(bright ~ 1 + (1|operator), pulp, REML = FALSE)

LRT = as.numeric(2*(logLik(smod) - logLik(nullmod)))
LRT
pchisq(LRT, df = 1, lower = FALSE)

set.seed(13)
B = 1000
lrtstat = numeric(B)
system.time(for(b in 1:B){
  y = unlist(simulate(nullmod))
  bnull = lm(y ~ 1)
  balt = lmer(y ~ 1 + (1|operator), pulp, REML = FALSE)
  lrtstat[b] = as.numeric(2*(logLik(balt) - logLik(bnull)))
})

mean(lrtstat < 1e-5)

## p-value
pval = mean(lrtstat > 2.568371)
pval

## simple standard error of the above
sqrt(pval*(1-pval)/B)



# Predicting random effects

## operator fixed effects from linear model
(cc = model.tables(lmod))

pulp %>% group_by(operator) %>% 
  summarise(bright = mean(bright)) %>% 
  select(bright) - mean(pulp$bright)

## operator random effects
ranef(mmod)$operator

## estimated fixed effects divided by predicted random effects
## shrinkage
cc[[1]]$operator / ranef(mmod)$operator

## 95% CI for predicted random effects
?ranef
library(lattice)
dotplot(ranef(mmod, condVar=TRUE))

## total operator effect
fixef(mmod)
fixef(mmod) + ranef(mmod)$operator


## get a 95% CI for a new observation (operator unspecified)
group.sd = as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd = as.data.frame(VarCorr(mmod))$sdcor[2]
?simulate.merMod
B = 1000
pv = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u = TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pv[i] = predict(bmod, re.form=~0)[1] + 
    rnorm(n=1,sd=group.sd) +  
    rnorm(n=1,sd=resid.sd)
})
quantile(pv, c(0.025, 0.975))


## get a 95% CI for a new observation for operator a
?simulate.merMod
B = 1000
pva = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u=TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pva[i] = predict(bmod, 
                  newdata=data.frame(operator="a")) + 
    rnorm(n=1,sd=resid.sd)
})
quantile(pva, c(0.025, 0.975))



## get a 95% CI for a new observation for operator b
group.sd = as.data.frame(VarCorr(mmod))$sdcor[1]
resid.sd = as.data.frame(VarCorr(mmod))$sdcor[2]
B = 1000
pvb = numeric(B)
system.time(for(i in 1:B){
  y = unlist(simulate(mmod, use.u=TRUE))
  bmod = suppressWarnings(refit(mmod, y))
  pvb[i] = predict(bmod, 
                  newdata=data.frame(operator="b")) + 
    rnorm(n=1,sd=resid.sd)
})

quantile(pvb, c(0.025, 0.975))
fixef(mmod)

data.frame(
  operator = rep(c("u","a","b"), each=1e3),
  bright = c(pv,pva,pvb)
) %>% 
  ggplot() + 
  aes(x = bright, color = operator) + 
  geom_density() + 
  theme_minimal() + 
  geom_vline(aes(xintercept = 60.4))
  

# split plot analysis (irrigation)

## "follow the randomness"
?irrigation
data(irrigation)

## data summary
summary(irrigation)

## data visualization
ggplot(irrigation, aes(y=yield, x=field, shape=irrigation, color= variety)) +
  geom_point(size = 2) +
  theme_minimal()

lmod = lmer(yield ~ irrigation * variety + (1|field) + (1|field:variety),
            data = irrigation)

lmod = lmer(yield ~ irrigation * variety + (1|field),
            data = irrigation)
sumary(lmod)

## Kenward-Roger approximate F-test
library(pbkrtest)
lmoda = lmer(yield ~ irrigation + variety + (1|field), data=irrigation)
?KRmodcomp
KRmodcomp(lmoda, lmod)

lmodi = lmer(yield ~ irrigation + (1|field), irrigation)
KRmodcomp(lmodi, lmoda)

lmodv = lmer(yield ~ variety + (1|field), irrigation)
KRmodcomp(lmodv, lmoda)

par(mfrow = c(1,2))
plot(fitted(lmod), residuals(lmod), xlab="Fitted", ylab="Residuals", pch = 19)
qqnorm(residuals(lmod), main="", pch = 19)



# Soybean Photoprotection Dynamics under Field Conditions
## might have to change path to soybean_course.csv
dat = read_csv("soybean_course.csv") %>% 
  select(-mAqI, -mtqE, -mAqM, -mtqM, -maxNPQ, -Ta, -VPD, -Precip, 
         -Fsd, -VPD_7day)
dat$ID = as.factor(dat$ID)
head(dat)

## weather variables importance
pairs(dat %>% select(mAqE, Ta_7day, Fsd_7day, Precip_7day, Precip_cum))


## modeling
### full data
m1_mAqE = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
                 (1|plot_number_year), 
               data = dat, REML = FALSE)
m1_mAqE_full = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
                      Fsd_7day + Ta_7day + Precip_7day + Precip_cum + 
                      (1|plot_number_year), 
                    data = dat, REML = FALSE)

### year 2021 data
m2_mAqE = lmer(mAqE ~ ID + Date_num + I(Date_num^2) +  
                 (1|plot_number), 
               data = dat %>% filter(year == 2021), REML = FALSE)
m2_mAqE_full = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
                      Fsd_7day + Ta_7day + Precip_7day + Precip_cum +  
                      (1|plot_number), 
                    data = dat %>% filter(year == 2021), REML = FALSE)

### year 2022 data
m3_mAqE = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
                 (1|plot_number), 
               data = dat %>% filter(year == 2022), REML = FALSE)
m3_mAqE_full = lmer(mAqE ~ ID + Date_num + I(Date_num^2) + 
                      Fsd_7day + Ta_7day + Precip_7day + Precip_cum + 
                      (1|plot_number), 
                    data = dat %>% filter(year == 2022), REML = FALSE)

## LRT

### takes too long, commented out

### setup
#set.seed(13)
#B = 1000
#lrtstat = numeric(B)

### parametric bootstrap
#system.time(for(b in 1:B){
#  y = unlist(simulate(m1_mAqE))
#  bnull = lm(y ~ ID + Date_num + I(Date_num^2), data = dat)
#  balt = lmer(y ~ ID + Date_num + I(Date_num^2) + 
#                (1|plot_number_year),
#              data = dat, REML = FALSE, 
#              control = lmerControl(optimizer ="Nelder_Mead"))
#  lrtstat[b] = as.numeric(2*(logLik(balt) - logLik(bnull)))
#})
#mean(lrtstat < 1e-5)

#pval = mean(lrtstat > 
#              as.numeric(2*(logLik(m1_mAqE_full) - logLik(m1_mAqE))))
#pval

## AIC
AIC(m1_mAqE, m1_mAqE_full)
AIC(m2_mAqE, m2_mAqE_full) 
AIC(m3_mAqE, m3_mAqE_full)

## BIC
BIC(m1_mAqE, m1_mAqE_full)
BIC(m2_mAqE, m2_mAqE_full) 
BIC(m3_mAqE, m3_mAqE_full)


## modeling assumptions

### model m1_mAqE
par(mfrow = c(1,2))
plot(fitted(m1_mAqE), residuals(m1_mAqE),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m1_mAqE), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m1_mAqE))

## model m1_mAqE_full
par(mfrow = c(1,2))
plot(fitted(m1_mAqE_full), residuals(m1_mAqE_full),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m1_mAqE_full), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m1_mAqE_full))

## model m2_mAqE
par(mfrow = c(1,2))
plot(fitted(m2_mAqE), residuals(m2_mAqE),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m2_mAqE), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m2_mAqE))

## model m2_mAqE_full
par(mfrow = c(1,2))
plot(fitted(m2_mAqE_full), residuals(m2_mAqE_full),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m2_mAqE_full), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m2_mAqE_full))

## model m3_mAqE
par(mfrow = c(1,2))
plot(fitted(m3_mAqE), residuals(m3_mAqE),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m3_mAqE), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m3_mAqE))

## model m3_mAqE_full
par(mfrow = c(1,2))
plot(fitted(m3_mAqE_full), residuals(m3_mAqE_full),
     xlab="Fitted", ylab="Residuals", pch = 19,
     col = rgb(0,0,0,alpha=0.2))
a = qqnorm(residuals(m3_mAqE_full), main="", pch = 19,
           col = rgb(0,0,0,alpha=0.2))
qqline(residuals(m3_mAqE_full))

## set up model matrices for all models
M = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2), data = dat5)
M_full = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2) + 
                        Fsd_7day + Ta_7day + Precip_7day + Precip_cum,
                      data = dat5)

M2021 = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2),
                     data = dat5 %>% filter(year == 2021))
M2021_full = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2) + 
                            Fsd_7day + Ta_7day + Precip_7day + Precip_cum,
                          data = dat5 %>% filter(year == 2021))

M2022 = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2),
                     data = dat5 %>% filter(year == 2022))
M2022_full = model.matrix(mAqE ~ ID + Date_num + I(Date_num^2) + 
                            Fsd_7day + Ta_7day + Precip_7day + Precip_cum,
                          data = dat5 %>% filter(year == 2022))

## AIC for each ID variable from full AqE fixed-effects model
library(parallel)
ncores = detectCores() - 2
AIC_IDs_mAqE = do.call(rbind, mclapply(
  grep("ID", colnames(M)), function(j){
    M1 = M[, -j]
    foo = lmer(mAqE ~ -1 + M1 + (1|plot_number_year),
               data = dat, REML = FALSE)
    M1_full = M_full[, -j]
    foo_full = lmer(mAqE ~ -1 + M1_full + (1|plot_number_year),
                    data = dat, REML = FALSE)
    M12021 = M2021[, -j]
    bar = lmer(mAqE ~ -1 + M12021 + (1|plot_number),
               data = dat %>% filter(year == 2021), REML = FALSE)
    M12021_full = M2021_full[, -j]
    bar_full = lmer(mAqE ~ -1 + M12021_full + (1|plot_number),
                    data = dat %>% filter(year == 2021), REML = FALSE)
    M12022 = M2022[, -j]
    baz = lmer(mAqE ~ -1 + M12022 + (1|plot_number),
               data = dat %>% filter(year == 2022), REML = FALSE)
    M12022_full = M2022_full[, -j]
    baz_full = lmer(mAqE ~ -1 + M12022_full + (1|plot_number),
                    data = dat %>% filter(year == 2022), REML = FALSE)
    
    c(AIC(m1_mAqE) - AIC(foo),
      AIC(m1_mAqE_full) - AIC(foo_full), 
      AIC(m2_mAqE) - AIC(bar), 
      AIC(m2_mAqE_full) - AIC(bar_full),
      AIC(m3_mAqE) - AIC(baz),
      AIC(m3_mAqE_full) - AIC(baz_full))
  }, mc.cores = ncores))
rownames(AIC_IDs_mAqE) = colnames(M)[grep("ID", colnames(M))] 
sign(AIC_IDs_mAqE < 0)

## regression coefficients
coefs_mAqE = round(cbind(summary(m1_mAqE)$coefficients[2:41, 1],
                         summary(m1_mAqE_full)$coefficients[2:41, 1],
                         summary(m2_mAqE)$coefficients[2:41, 1],
                         summary(m2_mAqE_full)$coefficients[2:41, 1],
                         summary(m3_mAqE)$coefficients[2:41, 1], 
                         summary(m3_mAqE_full)$coefficients[2:41, 1]), 3)
coefs_mAqE
sign(coefs_mAqE > 0)

## coefficients and AIC
inference_mAqE = sign(coefs_mAqE) * sign(AIC_IDs_mAqE < 0)
inference_mAqE

## note that these selected genotypes are different than those in the 
## reference material because of updates to lme4 (updated on March 26, 2025)
rownames(coefs_mAqE)[abs(rowSums(inference_mAqE)) == 6]
coefs_mAqE[abs(rowSums(inference_mAqE)) == 6, ]



# Nested random effects via an example

## "follow the randomness"
?eggs
data(eggs)
summary(eggs)

ggplot(eggs, aes(y=Fat, x=Lab, color=Technician, shape=Sample)) +
  geom_point(size = 2, 
             position = position_jitter(width=0.1, height=0.0)) +
  scale_color_grey() +
  theme_minimal()

cmod = lmer(Fat ~ 1 + (1|Lab) + (1|Lab:Technician) + (1|Lab:Technician:Sample), data=eggs)
sumary(cmod)
confint(cmod, method="boot")

cmod2 = lmer(Fat ~ 1 + 
               (1|Lab) + 
               (1|Lab:Technician), data=eggs)
as.matrix(model.matrix(cmod2, type = "random"))
confint(cmod2, method="boot")

cmod3 = lmer(Fat ~ 1 + (1|Lab), data=eggs)
confint(cmod3, method="boot")


cmod = lmer(Fat ~ 1 + 
              (1|Lab) + 
              (1|Lab:Technician) + 
              (1|Lab:Technician:Sample), data=eggs, 
            REML = FALSE)
cmod2 = lmer(Fat ~ 1 + 
               (1|Lab) + 
               (1|Lab:Technician), data=eggs, 
             REML = FALSE)
cmod3 = lmer(Fat ~ 1 + (1|Lab), data=eggs, REML = FALSE)

AIC(cmod)
AIC(cmod2)
AIC(cmod3)

confint(cmod, method="boot")
confint(cmod2, method="boot")
confint(cmod3, method="boot")


# Panel Study of Income Dynamics
library(faraway)
library(lme4)
library(tidyverse)
data(psid)
head(psid)
?psid

## income of first 20 individuals
psid20 = filter(psid, person <= 20)
ggplot(psid20, aes(x=year, y=income)) +
  geom_line() +
  facet_wrap(~ person) +
  theme_minimal()

## income of individuals broken up by sex
ggplot(psid20, aes(x=year, y=income+100, group=person)) +
  geom_line() +
  facet_wrap(~ sex) +
  scale_y_log10() +
  theme_minimal()

## intercept at 1978 instead of 1900
lmod = lm(log(income) ~ I(year-78), subset=(person==1), psid)
coef(lmod)

## response feature analysis. It requires choosing an important characteristic. 
### We have chosen two here: the slope and the intercept (income on year by sex)
ml = lmList(log(income) ~ I(year-78) | person, psid)
intercepts = sapply(ml,coef)[1,]
slopes = sapply(ml,coef)[2,]

plot(intercepts,slopes, xlab="Intercept", ylab="Slope")
psex = psid$sex[match(1:85, psid$person)]

boxplot(split(intercepts,psex))
boxplot(split(slopes,psex))

### strong statistical significance in intercepts
t.test(intercepts[psex=="M"], intercepts[psex=="F"])

### weak statistical significance in slopes
t.test(slopes[psex=="M"], slopes[psex=="F"])

## repeated measures with LMM 
foo = psid
foo$cyear = psid$year - 78

mmod = lmer(log(income) ~ cyear*sex + age + educ + (cyear|person), foo, 
            ## this set of controls fixes the mismatch between
            ## compiled markdown and an active R session
            control = lmerControl(
              optimizer = "bobyqa", 
              calc.derivs = TRUE, 
              optCtrl = list(maxfun = 1e5)))

summary(mmod)
sumary(mmod, digits=3)




# LMM coding 

## construction of model matrix
library(Matrix)

## first argument is the number of levels
## second argument is the number of replications
(f = gl(3,2))
(Ji = t(as(f, Class = "sparseMatrix")))

## treatment/control study 
(Xi = cbind(1,rep.int(c(-1,1), 3L)))

(Zi = t(KhatriRao(t(Ji), t(Xi))))

