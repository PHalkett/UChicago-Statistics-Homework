##############################################################
# STAT 226 HW8 Lab
# Contents: All R code appearing in the lab assignment
##############################################################


## ----load-data-----------------------------------------------------------
wheat = read.csv("http://statistics.uchicago.edu/~collins/data/s226/wheat.csv")

## ----structure-data, message=FALSE, warning=FALSE------------------------
library(tidyverse)
glimpse(wheat)

## ----head-tail-data------------------------------------------------------
head(wheat)
tail(wheat)

## ----summary-wheat-------------------------------------------------------
summary(wheat)

## ----table-wheat, message=FALSE, warning=FALSE---------------------------
library(mosaic)
tally(~ type, data = wheat)

## ----table-wheat-type----------------------------------------------------
tally(~ class, data = wheat)
tally(type ~ class, data = wheat)

## ----bwplot-density, message=FALSE, warning=FALSE------------------------
library(ggformula)
gf_boxplot(density ~ type, data = wheat) %>%
  gf_labs(x = "Wheat Kernel Condition", y = "Kernel Density (g/cm^3)")

## ----bwplot-density-horizontal-------------------------------------------
gf_boxplot(density ~ type, data = wheat) %>%
  gf_labs(x = "Wheat Kernel Condition", y = "Kernel Density (g/cm^3)") + 
  coord_flip()

## ----hist-density, fig.height=4------------------------------------------
gf_histogram(~ density | type, data = wheat, color="white") %>%
  gf_labs(x = "Wheat Kernel Density (g/cm^3)", y = "Number of Wheat Kernels")

## ----hist-density-stacked, fig.height=4----------------------------------
gf_histogram(~ density | type, data = wheat, color="white") %>%
  gf_labs(x = "Wheat Kernel Density (g/cm^3)", y = "Number of Wheat Kernels") + 
  facet_grid(type ~ .)

## ----scatter-density-weight,  fig.height=4.5-----------------------------
gf_point(density ~ weight, color = ~ type, data = wheat)

## ----density-grp---------------------------------------------------------
wheat.grp = mutate(wheat, 
  density.grp = cut(density, breaks= c(0.7, 0.9, 1, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, Inf)))
type.count = tally(density.grp ~ type, data = wheat.grp)
addmargins(type.count, 2, FUN = list(Total = sum))

## ----density-grp-prop----------------------------------------------------
type.prop = prop.table( tally(density.grp ~ type, data = wheat.grp), 1)
round( addmargins(type.prop, 2, FUN = list(Total = sum)), 3)

## ----density-grpmean-----------------------------------------------------
density.grpmean = mean(density ~ density.grp, data = wheat.grp);
density.grpmean

## ----density-type-prop-plot, fig.height=4.5, fig.width=5-----------------
plot(density.grpmean, type.prop[,1], type="b", xlim=c(0.7,1.65),ylim=c(0,1),
     ylab="Estimated Probabilities", xlab="Density (g/cm^3)")
points(density.grpmean, type.prop[,2], pch=2)
points(density.grpmean, type.prop[,3], pch=3)
lines(density.grpmean, type.prop[,2], lty=2)
lines(density.grpmean, type.prop[,3], lty=3)
legend("left",c("Healthy","Scab","Sprout"),pch=1:3, lty=1:3)

## ----VGAM-install, eval=FALSE--------------------------------------------
## # Run this line only if you have never installed `VGAM`
## # Yes, you will need to run this once on rstudio.uchicago.edu
install.packages("VGAM")

## ----VGAM-load, message=FALSE, warning=FALSE-----------------------------
library(VGAM)  

## ----fit1----------------------------------------------------------------
fit1 =  vglm(type ~ density, data = wheat, family = multinomial)

## ----fit1-summary--------------------------------------------------------
summary(fit1)

## ----fit1-show-----------------------------------------------------------
show(fit1)

## ----fit1-nice-summary---------------------------------------------------
coef(fit1)  # parameter estimates (coefficients)
confint(fit1, level = 0.95)  # Wald-type CI for parameters
deviance(fit1)  # Deviance
df.residual(fit1)  # degrees of freedom 
nparam(fit1)  # number of parameters
AIC(fit1)  # AIC
logLik(fit1)  # Log-likelihood
head(resid(fit1, type = "pearson"))  # Pearson residuals
head(resid(fit1, type = "deviance"))  # deviance residuals

## ----type-levels---------------------------------------------------------
levels(wheat$type)

## ----fit1-coef-----------------------------------------------------------
coef(fit1)
a.h = coef(fit1)[1]
a.s = coef(fit1)[2]
b.h = coef(fit1)[3]
b.s = coef(fit1)[4]
data.frame(a.h, a.s, b.h, b.s)

## ----density-type-curve, fig.height=4.5, fig.width=5---------------------
plot(density.grpmean, type.prop[,1], type="p", xlim=c(0.7, 1.65), ylim=c(0, 1),
     ylab="Estimated Probabilities", xlab="Density (g/cm^3)")
points(density.grpmean, type.prop[,2], pch=2)
points(density.grpmean, type.prop[,3], pch=3)
legend("left", c("Healthy", "Scab", "Sprout"), pch = 1:3, lty = 1:3)

curve(exp(a.h + b.h*x) / (1 + exp(a.h + b.h*x) + exp(a.s + b.s*x)), lty = 1, add = T)  # P(Healthy)
curve(exp(a.s + b.s*x) / (1 + exp(a.h + b.h*x) + exp(a.s + b.s*x)), lty = 3, add = T)  # P(Scab)
curve(1 / (1 + exp(a.h + b.h*x) + exp(a.s + b.s*x)), lty = 2, add = T)                 # P(Sprout)

## ----fit2----------------------------------------------------------------
fit2 =  vglm(type ~ density + weight + hardness + size + moisture + class, data = wheat, family = multinomial)
show(fit2)

## ----lrt-fit2-hardness---------------------------------------------------
lrtest(fit2, "hardness")

## ----lrt-fit2-hardness-alt-----------------------------------------------
lrtest(fit2, 3)

## ----lrt-fit2-size-class-------------------------------------------------
lrtest(fit2, c("size","class"))
lrtest(fit2, c(4,6))         

