rat<-read.table("/Users/yangwenxiao/Desktop/STAT/STAT 425/Week 5 R Resources-20210926/rat.txt")
rat
pairs(rat)
names(rat)=c("body", "liver", "dose", "Y")
fit=lm(Y~., data=rat);
summary(fit)
par(mfrow=c(2,3)); 
plot(fit)
lev=influence(fit)$hat
library(faraway)
halfnorm(lev, 4, ylab="Leverages")
library(lmtest)
bptest(fit)
rat.lm<-lm(Y~body+liver+dose, data = rat)
summary(rat.lm)
rat.lm_body = update(rat.lm, ~ liver+dose)
summary(rat.lm_body)
rat.lmb<-lm(Y~liver+dose, data = rat)
summary(rat.lmb)
library(MASS)
min(rat$Y)
rat.transformation = boxcox(rat.lm, lambda=seq(-2, 2, length=400))
dim(rat.transformation)
boxcox(rat.lm, plotit=T, lambda=seq(0.1,1,by=0.000001))
x=model.matrix(fit)[,-1]
apply(x,2,mean)
apply(x,2,sd)
apply(x,2,var)
16.49029486^2
