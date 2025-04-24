
# aster example

## aster software
library(aster)

## echinacea data
# estimate surrogate of Darwinian fitness for different 
# remnant populations
data(echinacea)
?echinacea
head(echinacea)
names(echinacea)


## nodes in graphs
vars = c("ld02", "ld03", "ld04", "fl02", "fl03", 
         "fl04", "hdct02", "hdct03", "hdct04")

## predecessors 
pred = c(0, 1, 2, 1, 2, 3, 4, 5, 6)

foo = rbind(vars, c("initial", vars)[pred + 1]) 
rownames(foo) = c("successor", "predecessor")
foo

## aster familes
# 1. Bernoulli
# 3. Zero-truncated Poisson
fam = c(1, 1, 1, 1, 1, 1, 3, 3, 3)
rbind(vars, fam)

## put data in long format
# add root node
redata = reshape(echinacea, varying = list(vars), 
                 direction = "long", timevar = "varb", 
                 times = as.factor(vars), v.names = "resp")
redata = data.frame(redata, root = 1)

## display reshaped data
# varb is the name of the node
# resp is the observed value of the node for each individual
head(redata)

## display names of nodes
levels(redata$varb)

## display names of populations
levels(redata$pop)

## extract "layers" 
layer = gsub("[0-9]", "", as.character(redata$varb))
unique(layer)
redata = data.frame(redata, layer = layer)
with(redata, class(layer))

## designate special name for fitness nodes
fit = as.numeric(layer == "hdct") 
redata = data.frame(redata, fit = fit)
with(redata, class(fit))

## fit aster model 
# need to include intercepts for nodes
# allow for components of fitness to vary with location
# estimate different fitnesses across populations
# specify pred, fam, varb, id, root
aout = aster(resp ~ varb + layer : (nsloc + ewloc) + 
               fit : pop, pred, fam, varb, id, root, data = redata)
summary(aout)


## can do LRTs as before
aout.smaller = aster(resp ~ varb + 
                       fit : (nsloc + ewloc + pop), 
                     pred, fam, varb, id, root, data = redata)
aout.bigger = aster(resp ~ varb + 
                      layer : (nsloc + ewloc + pop), 
                    pred, fam, varb, id, root, data = redata)
anova(aout.smaller, aout, aout.bigger)

## pick middle model. Why? 
### see end of slide deck: https://www.stat.umn.edu/geyer/8931aster/slides/s4.pdf

## Also, estimates of fitness are not that different

## we will go over this later
pop = levels(redata$pop)
nind = length(unique(redata$id))
nnode = nlevels(redata$varb)
npop = length(pop)
amat = array(0, c(nind, nnode, npop))
amat.ind = array(as.character(redata$pop),
                 c(nind, nnode, npop))
amat.node = array(as.character(redata$varb),
                  c(nind, nnode, npop))
amat.fit = grepl("hdct", amat.node)
amat.fit = array(amat.fit,
                 c(nind, nnode, npop))
amat.pop = array(pop, c(npop, nnode, nind))
amat.pop = aperm(amat.pop)
amat[amat.pop == amat.ind & amat.fit] = 1
pout = predict(aout,  varvar = varb, idvar = id,
               root = root, se.fit = TRUE, amat = amat)
pout.bigger = predict(aout.bigger, varvar = varb,
                      idvar = id, root = root, se.fit = TRUE, amat = amat)
pout$fit
pout.bigger$fit
all.equal(pout$fit, pout.bigger$fit)

pout$se.fit
pout.bigger$se.fit


# estimate expected surrogate of Darwinian fitness

## get mean-value parameter estimates
pout = predict(aout, se.fit = TRUE)
low = pout$fit - qnorm(0.975) * pout$se.fit
hig = pout$fit + qnorm(0.975) * pout$se.fit

## what we want is estimated expected surrogate of 
## Darwinian fitness for typical individuals 
nlevels(redata$pop)
dat = data.frame(
  nsloc = 0, ewloc = 0, pop = levels(redata$pop), 
  root = 1, ld02 = 1, ld03 = 1, ld04 = 1, fl02 = 1, fl03 = 1, 
  fl04 = 1, hdct02 = 1, hdct03 = 1, hdct04 = 1)
dat

## reshape data so that it can be analyzed by aster software
renewdata = reshape(dat, varying = list(vars), 
                    direction = "long", timevar = "varb", times = as.factor(vars), 
                    v.names = "resp")
layer = gsub("[0-9]", "", as.character(renewdata$varb))
renewdata = data.frame(renewdata, layer = layer)
fit = as.numeric(layer == "hdct")
renewdata = data.frame(renewdata, fit = fit)
renewdata
names(renewdata)

## get estimated mean-value parameters for typical individuals
pout = predict(aout, newdata = renewdata, varvar = varb, 
               idvar = id, root = root, se.fit = TRUE)
sapply(pout, length)

## organize estimated mean-value parameters by individual and 
## by node
nnode = length(vars)
preds = matrix(pout$fit, ncol = nnode) 
dim(preds)
rownames(preds) = unique(as.character(renewdata$pop))
colnames(preds) = unique(as.character(renewdata$varb))
preds

## surrogate of fitness is sum of head count nodes
preds_hdct = preds[ , grepl("hdct", colnames(preds))]
rowSums(preds_hdct)


## now get standard errors
# aster software can estimate standard errors for 
# linear functions of mean-value parameters
#
# need to supply array to get standard errors with three 
# dimensions supplied:
# 1. The first dimension is the number of individuals to make a 
#    a prediction for
# 2. number of nodes in graph
# 3. number of parameters we want point estimates and standard errors for.
npop = nrow(dat) 
nnode = length(vars)
amat = array(0, c(npop, nnode, npop))
dim(amat)

foo = grepl("hdct", vars)
for (k in 1:npop) amat[k, foo, k] = 1
amat

pout.amat = predict(aout, newdata = renewdata, varvar = varb, 
  idvar = id, root = root, se.fit = TRUE, amat = amat)

## predict.aster
pout.amat$fit

## computation by hand
rowSums(preds_hdct)

## they are the same!
cbind(pout.amat$fit, rowSums(preds_hdct))

## predictions + standard errors
foo = cbind(pout.amat$fit, pout.amat$se.fit)
rownames(foo) = unique(as.character(renewdata$pop))
colnames(foo) = c("estimates", "std. err.")
round(foo, 3)
