
## load software
library(VGAM)  # has model-fitting functions
library(tidyverse)

## load data
bball = read.csv("stat528sp24/notes/6-multinomial-regression/bball.csv")
bball$events = as.factor(bball$events)
head(bball)

## summaries of launch angle, launch speed, spray angle

## box plot of quantitative variables by events

## fit two models
mod1 = vglm(events ~ launch_speed + launch_angle + spray_angle, 
            family=multinomial, data=bball)

mod2 = vglm(events ~ launch_speed + launch_angle + spray_angle + 
              I(launch_angle^2), 
            family=multinomial, data=bball)

## LRT
llrts = deviance(mod1) - deviance(mod2)
llrts.df = df.residual(mod1) - df.residual(mod2)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## AIC
AIC(mod1); AIC(mod2)

## 6th order polynomial in spray angle
system.time({mod3 = vglm(events ~ launch_speed + 
                           launch_angle + spray_angle + I(launch_angle^2) + 
                           I(spray_angle^2) + I(spray_angle^3) + 
                           I(spray_angle^4) + I(spray_angle^5) + 
                           I(spray_angle^6), family=multinomial, data=bball)})


llrts = deviance(mod2) - deviance(mod3)
llrts.df = df.residual(mod2) - df.residual(mod3)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## AIC
AIC(mod1); AIC(mod2); AIC(mod3)


## add interaction terms
system.time({mod4 = vglm(events ~ launch_speed + 
                           launch_angle + spray_angle + I(launch_angle^2) + 
                           I(spray_angle^2) + I(spray_angle^3) + I(spray_angle^4) + 
                           I(spray_angle^5) + I(spray_angle^6) + 
                           I(spray_angle*launch_angle) + I(spray_angle*launch_speed) + 
                           I(launch_angle*launch_speed), 
                         family=multinomial, data=bball)})

llrts = deviance(mod3) - deviance(mod4)
llrts.df = df.residual(mod3) - df.residual(mod4)
llrts; llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

AIC(mod1); AIC(mod2); AIC(mod3); AIC(mod4)
## summary table takes a long time to load
#s4 = summary(mod4)
#s4

## predictions 

### median launch angle and launch speed
summary(bball$launch_angle)
summary(bball$launch_speed)

new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 90.30, 
                      launch_angle = 12)

# type = response for category probabilities
pred = predict(mod4, newdata = new_data, type = "response")

par(mfrow = c(2,2), oma = c(4,4,0,0), mar = c(1,2,1,1))
plot.new()
title("single")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 1]), max(pred[, 1])))
points(new_data$spray_angle, pred[, 1], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(2)

plot.new()
title("double")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 2]), max(pred[, 2])))
points(new_data$spray_angle, pred[, 2], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(2)

plot.new()
title("triple")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 3]), max(pred[, 3])))
points(new_data$spray_angle, pred[, 3], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(1)
axis(2)

plot.new()
title("home run")
plot.window(xlim = c(-55,55), ylim = c(min(pred[, 4]), max(pred[, 4])))
points(new_data$spray_angle, pred[, 4], pch = 19, col = rgb(0,0,0,alpha=0.2))
axis(1)
axis(2)


### well hit balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 100, 
                      launch_angle = 20)

### should-be home runs
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 110, 
                      launch_angle = 29)

### bad balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 70, 
                      launch_angle = 0)

### bad balls
new_data = data.frame(spray_angle = seq(-55,55, by = 0.1), 
                      launch_speed = 80, 
                      launch_angle = 60)


## grouped data

### Example 1
library(tidyverse)
library(nnet)

set.seed(13)
dat = data.frame(A = rep(c(rep(1, 5), rep(2, 5), rep(3, 5)),2), 
                 B = c(rep(1, 15), rep(2, 15)))
Y = rpois(n = nrow(dat), lambda = exp(1/2*dat$A + 1/2*dat$B))
dat$Y = Y
dat$A = as.factor(dat$A)
dat$B = as.factor(dat$B)
dat

m1 = glm(Y ~ -1 + A:B, data = dat, family = "poisson")
beta = coef(m1)
exp(beta)
predict(m1, type = "response")
unique(predict(m1, type = "response"))

pred_probs_pois = data.frame(
  levels = names(beta),
  preds_pois = exp(beta)/sum(exp(beta))
)
pred_probs_pois

dat2 = dat %>% 
  mutate(factor = paste(A,B,sep = "")) %>% 
  group_by(factor) %>% 
  summarise(total = sum(Y))
dat2

## takes first term in alpha-numeric order as baseline variable
m2 = multinom(factor ~ 1, data = dat2, weight = total)
summary(m2)
pred_probs = data.frame(
  levels = c("A1:B1","A1:B2","A2:B1","A2:B2","A3:B1","A3:B2"),
  preds_multi = c(1,exp(coefficients(m2))) / (1 + sum(exp(coefficients(m2))))
)
pred_probs
pred_probs %>% left_join(pred_probs_pois)


#### simpler approach
cell_means = dat %>% 
  group_by(A,B) %>% 
  summarise(mu = mean(Y)) %>% 
  ungroup() %>% 
  mutate(level = paste("A",A,":","B",B, sep = "")) %>% 
  dplyr::select(level, mu)
exp(beta)
cell_means
cell_probs = cell_means$mu/sum(cell_means$mu)

cbind(cell_means, cell_probs)


### Example 2
### Adapted from https://sakai.unc.edu/access/content/group/2842013b-58f5-4453-aa8d-3e01bacbfc3d/public/Ecol562_Spring2012/docs/lectures/lecture38.htm
# y is the multinomial response (realized class label)
dat = data.frame(
  y = rep(1:3, 12),
  x = c(rep("A",12), rep("B",12), rep("A",12)),
  z = c(rep("a",24), rep("b", 12))
)
dat$x = as.factor(dat$x)
dat$y = as.factor(dat$y)
dat$z = as.factor(dat$z)

# generate frequencies
lambda = 
  as.numeric(model.matrix(~ -1 + x:y + z:y, data = dat) %*% 
               c(10,40,5,50,10,60,20,25,30))
lambda

set.seed(13)
dat$freq = rpois(nrow(dat), lambda = lambda) 
dat


mod1_multi = multinom(y~1, weight=freq, data = dat)
## see Agresti for reasons for Poisson formula
mod1_pois = glm(freq~x+z+x:z+y, family=poisson, data = dat)

mod2_multi = multinom(y~x, weight=freq, data = dat)
## see Agresti for reasons for Poisson formula
mod2_pois = glm(freq~x+z+x:z+y+y:x, family=poisson, data = dat)

## Chisq in anova.nnet is the same as LRT in anova.glm
anova(mod1_multi, mod2_multi, test = "Chisq")
anova(mod1_pois, mod2_pois, test = "LRT")



### Proportional odds model example
# 1 if very happy 
# 2 if pretty happy
# 3 if not too happy
happiness = read.table("stat528sp24/notes/6-multinomial-regression/happiness.txt", header=TRUE)
colnames(happiness)[1] = c("control")
happiness

library(VGAM)
mod = vglm(happy ~ trauma + control, family=propodds(reverse=FALSE),
           data=happiness)

summary(mod)
alpha = c(-0.5181, 3.4006)
beta = c(-0.4056, -2.0361)

## display predicted probabilities 
## across control and trauma
cbind(happiness[,-3], predict(mod, type = "response"))

modred = vglm(happy~trauma, family=propodds(reverse=FALSE),
              data=happiness)
llrts = deviance(modred) - deviance(mod)
llrts.df = df.residual(modred) - df.residual(mod)
llrts
llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## test proportional odds assumption
?propodds
modnotprop = vglm(happy~trauma+control, 
                  family=cumulative(parallel=FALSE),
                 data=happiness)
summary(modnotprop)

llrts = deviance(mod) - deviance(modnotprop)
llrts.df = df.residual(mod) - df.residual(modnotprop)
llrts
llrts.df
pchisq(llrts, llrts.df, lower = FALSE)

## polr fit
library(MASS)
mod.logit2 = polr(factor(happy) ~ trauma + control, 
                  data=happiness)
summary(mod.logit2)
?polr
