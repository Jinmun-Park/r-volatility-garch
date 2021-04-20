### Setting
library(quantmod)
library(fBasics)
library(stats)
library(rugarch)
library(fpp)

setwd("C:/Users/supgyu/Desktop")
corn = read.delim("corn.txt", header = TRUE)
palm = read.delim("palm.txt", header = TRUE)
rap = read.delim("rapeseed.txt", header = TRUE)
soy = read.delim("soybean.txt", header = TRUE)

### (a) 
## Transform to log returns
log.corn = diff(log(corn[,-1]))*100 
log.palm = diff(log(palm[,-1]))*100
log.rap = diff(log(rap[,-1]))*100
log.soy = diff(log(soy[,-1]))*100

log.corn = ts(log.corn)
log.palm = ts(log.palm)
log.rap = ts(log.rap)
log.soy = ts(log.soy)

## Empirical density plot
d1 = density(log.corn)
d2 = density(log.palm)
d3 = density(log.rap)
d4 = density(log.soy)

plot(d1$x, d1$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Corn)", las=1)
plot(d2$x, d2$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Palm)", las=1)
plot(d3$x, d3$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Rapeseed)", las=1)
plot(d4$x, d4$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Soybean)", las=1)

## The output variables

# Corn
basicStats(log.corn)
mu1 = mean(log.corn)
sd1 = sd(log.corn)

x1 = seq(-15,15,0.1)
y1 = dnorm(x1, mean = mu1, sd = sd1)

# Palm
basicStats(log.palm)
mu2 = mean(log.palm)
sd2 = sd(log.palm)

x2 = seq(-16,12,0.1)
y2 = dnorm(x2, mean = mu2, sd = sd2)

# Rapeseed
basicStats(log.rap)
mu3 = mean(log.rap)
sd3 = sd(log.rap)

x3 = seq(-13,11,0.1)
y3 = dnorm(x3, mean = mu3, sd = sd3)

# Soybean
basicStats(log.soy)
mu4 = mean(log.soy)
sd4 = sd(log.soy)

x4 = seq(-18,10,0.1)
y4 = dnorm(x4, mean = mu4, sd = sd4)

## Empirical density plot with normal density 
plot(d1$x, d1$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Corn)", las=1)
lines(x1, y1, lty=2, col=2)
legend("topleft", lty=c(1,2), col=c(1,2), legend = c("empirical density","theoretical normal density"))

plot(d2$x, d2$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Palm)", las=1)
lines(x2, y2, lty=2, col=2)
legend("topleft", lty=c(1,2), col=c(1,2), legend = c("empirical density","theoretical normal density"))

plot(d3$x, d3$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Rapeseed)", las=1)
lines(x3, y3, lty=2, col=2)
legend("topleft", lty=c(1,2), col=c(1,2), legend = c("empirical density","theoretical normal density"))

plot(d4$x, d4$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (Soybean)", las=1)
lines(x4, y4, lty=2, col=2)
legend("topleft", lty=c(1,2), col=c(1,2), legend = c("empirical density","theoretical normal density"))

## Normality test
# 1. QQ plot

qqnorm(log.corn, main="Normal Q-Q Plot of log returns of corn", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(log.corn, col=2, lty=5, lwd=2) # far from normal

qqnorm(log.palm, main="Normal Q-Q Plot of log returns of palm", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(log.corn, col=2, lty=5, lwd=2) # far from normal

qqnorm(log.rap, main="Normal Q-Q Plot of log returns of rapeseed", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(log.corn, col=2, lty=5, lwd=2) # far from normal

qqnorm(log.soy, main="Normal Q-Q Plot of log returns of soybean", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(log.corn, col=2, lty=5, lwd=2) # far from normal

# 2. Jarque-Bera test 

normalTest(log.corn, method = "jb", na.rm = TRUE) # not normally distributed
normalTest(log.palm, method = "jb", na.rm = TRUE) # not normally distributed
normalTest(log.rap, method = "jb", na.rm = TRUE)  # not normally distributed
normalTest(log.soy, method = "jb", na.rm = TRUE)  # not normally distributed

### (b)
## (i) 
t.test(log.corn) # t = 0.371 < 1.96, Do not reject the null 
t.test(log.palm) # t = 0.817 < 1.96, Do not reject the null
t.test(log.rap)  # t = 0.618 < 1.96, Do not reject the null
t.test(log.soy)  # t = 0.616 < 1.96, Do not reject the null

## (ii)
basicStats(log.corn) # mean = 0.011117, SE Mean = 0.029983 
basicStats(log.palm) # mean = 0.022404, SE Mean = 0.027416
basicStats(log.rap)  # mean = 0.015015, SE Mean = 0.024298
basicStats(log.soy)  # mean = 0.016117, SE Mean = 0.026183

## (iii)
skewness(log.corn) # skewness = -0.1142468
t1 = skewness(log.corn)/sqrt(6/length(log.corn)) 
t1 # test-stat = -2.994

skewness(log.palm) # skewness = -0.1611146
t2 = skewness(log.palm)/sqrt(6/length(log.palm))
t2 # test-stat = -4.086

skewness(log.rap) # skewness = -0.06692508
t3 = skewness(log.rap)/sqrt(6/length(log.rap))
t3 # test-stat = -1.697

skewness(log.soy) # skewness = -0.8067807
t4 = skewness(log.soy)/sqrt(6/length(log.soy))
t4 # test-stat = -21.141

## (iv)
k1 = kurtosis(log.corn)/sqrt(24/length(log.corn))
k1 # test-stat = 39.210

k2 = kurtosis(log.palm)/sqrt(24/length(log.palm))
k2 # test-stat = 71.506

k3 = kurtosis(log.rap)/sqrt(24/length(log.rap))
k3 # test-stat = 93.505

k4 = kurtosis(log.soy)/sqrt(24/length(log.soy))
k4 # test-stat = 88.169


### (c) Ljung Box test
Box.test(log.corn, lag=10, type=c("Ljung-Box"),fitdf=0) # 0.0874     #DNR
Box.test(log.palm, lag=10, type=c("Ljung-Box"),fitdf=0) # 4.619e-06  #Reject
Box.test(log.rap, lag=10, type=c("Ljung-Box"),fitdf=0)  # 3.331e-16  #Reject
Box.test(log.soy, lag=10, type=c("Ljung-Box"),fitdf=0)  # 0.01069    #Reject  

### (d) (1)
# GARCH (1,1) norm (normal distribution)
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "norm") # Model specification
Gn.corn <- ugarchfit(log.corn, spec = spec1, solver = "gosolnp") # Model estimation 
Gn.palm <- ugarchfit(log.palm, spec = spec1, solver = "gosolnp") # Model estimation 
Gn.rap <- ugarchfit(log.rap, spec = spec1, solver = "gosolnp") # Model estimation 
Gn.soy <- ugarchfit(log.soy, spec = spec1, solver = "gosolnp") # Model estimation 

# GARCH (1,1) sstd (Skew-normal distribution)
spec1.2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "sstd") # Model specification
Gs.corn <- ugarchfit(log.corn, spec = spec1.2, solver = "gosolnp") # Model estimation 
Gs.palm <- ugarchfit(log.palm, spec = spec1.2, solver = "gosolnp") # Model estimation 
Gs.rap <- ugarchfit(log.rap, spec = spec1.2, solver = "gosolnp") # Model estimation 
Gs.soy <- ugarchfit(log.soy, spec = spec1.2, solver = "gosolnp") # Model estimation 

# GARCH (1,1) std (student t)
spec1.3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "std") # Model specification
Gt.corn <- ugarchfit(log.corn, spec = spec1.3, solver = "gosolnp") # Model estimation 
Gt.palm <- ugarchfit(log.palm, spec = spec1.3, solver = "gosolnp") # Model estimation 
Gt.rap <- ugarchfit(log.rap, spec = spec1.3, solver = "gosolnp") # Model estimation 
Gt.soy <- ugarchfit(log.soy, spec = spec1.3, solver = "gosolnp") # Model estimation 

### (d) (2)
# EGARCH (1,1) norm (normal distribution)
spec2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "norm") # Model specification
EGn.corn <- ugarchfit(log.corn, spec = spec2, solver = "gosolnp") # Model estimation 
EGn.palm <- ugarchfit(log.palm, spec = spec2, solver = "gosolnp") # Model estimation 
EGn.rap <- ugarchfit(log.rap, spec = spec2, solver = "gosolnp") # Model estimation 
EGn.soy <- ugarchfit(log.soy, spec = spec2, solver = "gosolnp") # Model estimation 

# EGARCH (1,1) sstd (Skew-normal distribution)
spec2.2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "sstd") # Model specification
EGs.corn <- ugarchfit(log.corn, spec = spec2.2, solver = "gosolnp") # Model estimation 
EGs.palm <- ugarchfit(log.palm, spec = spec2.2, solver = "gosolnp") # Model estimation 
EGs.rap <- ugarchfit(log.rap, spec = spec2.2, solver = "gosolnp") # Model estimation 
EGs.soy <- ugarchfit(log.soy, spec = spec2.2, solver = "gosolnp") # Model estimation 

# EGARCH (1,1) std (student t)
spec2.3 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "std") # Model specification
EGt.corn <- ugarchfit(log.corn, spec = spec2.3, solver = "gosolnp") # Model estimation 
EGt.palm <- ugarchfit(log.palm, spec = spec2.3, solver = "gosolnp") # Model estimation 
EGt.rap <- ugarchfit(log.rap, spec = spec2.3, solver = "gosolnp") # Model estimation 
EGt.soy <- ugarchfit(log.soy, spec = spec2.3, solver = "gosolnp") # Model estimation 

### (d) (3)
# GJR-GARCH (1,1) norm (normal distribution)
spec3 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "norm") # Model specification
GJRGn.corn <- ugarchfit(log.corn, spec = spec3, solver = "gosolnp") # Model estimation 
GJRGn.palm <- ugarchfit(log.palm, spec = spec3, solver = "gosolnp") # Model estimation 
GJRGn.rap <- ugarchfit(log.rap, spec = spec3, solver = "gosolnp") # Model estimation 
GJRGn.soy <- ugarchfit(log.soy, spec = spec3, solver = "gosolnp") # Model estimation 

# GJR-GARCH (1,1) sstd (Skew-normal distribution)
spec3.2 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "sstd") # Model specification
GJRGs.corn <- ugarchfit(log.corn, spec = spec3.2, solver = "gosolnp") # Model estimation 
GJRGs.palm <- ugarchfit(log.palm, spec = spec3.2, solver = "gosolnp") # Model estimation 
GJRGs.rap <- ugarchfit(log.rap, spec = spec3.2, solver = "gosolnp") # Model estimation 
GJRGs.soy <- ugarchfit(log.soy, spec = spec3.2, solver = "gosolnp") # Model estimation 

# GJR-GARCH (1,1) std (student t)
spec3.3 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), arfima = FALSE, include.mean = TRUE), distribution.model = "std") # Model specification
GJRGt.corn <- ugarchfit(log.corn, spec = spec3.3, solver = "gosolnp") # Model estimation 
GJRGt.palm <- ugarchfit(log.palm, spec = spec3.3, solver = "gosolnp") # Model estimation 
GJRGt.rap <- ugarchfit(log.rap, spec = spec3.3, solver = "gosolnp") # Model estimation 
GJRGt.soy <- ugarchfit(log.soy, spec = spec3.3, solver = "gosolnp") # Model estimation 

#Corn
print(Gn.corn) #4.0184
print(Gs.corn) #3.9525
print(Gt.corn) #3.9521
print(EGn.corn) #4.0161
print(EGs.corn) #3.9492
print(EGt.corn) #3.9488 **BEST**
print(GJRGn.corn) #4.0187
print(GJRGs.corn) #4.019
print(GJRGt.corn) #3.9527

#Palm
print(Gn.palm) #3.6796
print(Gs.palm) #3.5820 
print(Gt.palm) #3.5822
print(EGn.palm) #3.6951
print(EGs.palm) #3.5833
print(EGt.palm) #3.5836
print(GJRGn.palm) #3.6790
print(GJRGs.palm) #3.5819 **BEST**
print(GJRGt.palm) #3.5821 

#Rap
print(Gn.rap) #3.4130
print(Gs.rap) #3.2131
print(Gt.rap) #3.2126
print(EGn.rap) #3.4514
print(EGs.rap) #3.1938
print(EGt.rap) #3.1935 **BEST**
print(GJRGn.rap) #3.4123
print(GJRGs.rap) #3.2132
print(GJRGt.rap) #3.2127

#Soy
print(Gn.soy) #3.7134
print(Gs.soy) #3.6242
print(Gt.soy) #3.6242
print(EGn.soy) #3.7098
print(EGs.soy) #3.6216 **BEST**
print(EGt.soy) #3.6217 
print(GJRGn.soy) #3.7106
print(GJRGs.soy) #3.6231
print(GJRGt.soy) #3.6230

#######################Obtaining the standardized residuals###################
# (E) (1) GARCH (Corn)

#NORMAL
Gn.corn.st.resid = residuals(Gn.corn, standardize=TRUE)
Gn.corn.st.resid.SQ = (Gn.corn.st.resid^2)
Gn.corn.st.resid.ts = ts(Gn.corn.st.resid) 
Gn.corn.st.resid.SQ.ts =ts(Gn.corn.st.resid.SQ)

#SKEW-T
Gs.corn.st.resid = residuals(Gs.corn, standardize=TRUE)
Gs.corn.st.resid.SQ = (Gs.corn.st.resid^2) 
Gs.corn.st.resid.ts = ts(Gs.corn.st.resid) 
Gs.corn.st.resid.SQ.ts =ts(Gs.corn.st.resid.SQ) 

#STUDENT-T
Gt.corn.st.resid = residuals(Gt.corn, standardize=TRUE) 
Gt.corn.st.resid.SQ = (Gt.corn.st.resid^2) 
Gt.corn.st.resid.ts = ts(Gt.corn.st.resid) 
Gt.corn.st.resid.SQ.ts =ts(Gt.corn.st.resid.SQ)

# (E) (2) EGARCH (corn)

#NORMAL
EGn.corn.st.resid = residuals(EGn.corn, standardize=TRUE)
EGn.corn.st.resid.SQ = (EGn.corn.st.resid^2)
EGn.corn.st.resid.ts = ts(EGn.corn.st.resid)
EGn.corn.st.resid.SQ.ts =ts(EGn.corn.st.resid.SQ) 

#SKEW-T
EGs.corn.st.resid = residuals(EGs.corn, standardize=TRUE) 
EGs.corn.st.resid.SQ = (EGs.corn.st.resid^2)
EGs.corn.st.resid.ts = ts(EGs.corn.st.resid)
EGs.corn.st.resid.SQ.ts =ts(EGs.corn.st.resid.SQ) 

#STUDENT-T
EGt.corn.st.resid = residuals(EGt.corn, standardize=TRUE)
EGt.corn.st.resid.SQ = (EGt.corn.st.resid^2)
EGt.corn.st.resid.ts = ts(EGt.corn.st.resid) 
EGt.corn.st.resid.SQ.ts =ts(EGt.corn.st.resid.SQ) 

# (E) (3) GJRGARCH (corn)

#NORMAL
GJRGn.corn.st.resid = residuals(GJRGn.corn, standardize=TRUE)
GJRGn.corn.st.resid.SQ = (GJRGn.corn.st.resid^2) 
GJRGn.corn.st.resid.ts = ts(GJRGn.corn.st.resid) 
GJRGn.corn.st.resid.SQ.ts =ts(GJRGn.corn.st.resid.SQ) 

#SKEW-T
GJRGs.corn.st.resid = residuals(GJRGs.corn, standardize=TRUE) 
GJRGs.corn.st.resid.SQ = (GJRGs.corn.st.resid^2) 
GJRGs.corn.st.resid.ts = ts(GJRGs.corn.st.resid)
GJRGs.corn.st.resid.SQ.ts =ts(GJRGs.corn.st.resid.SQ) 

#STUDENT-T
GJRGt.corn.st.resid = residuals(GJRGt.corn, standardize=TRUE)
GJRGt.corn.st.resid.SQ = (GJRGt.corn.st.resid^2)
GJRGt.corn.st.resid.ts = ts(GJRGt.corn.st.resid) 
GJRGt.corn.st.resid.SQ.ts =ts(GJRGt.corn.st.resid.SQ) 

# (E) (1) GARCH (Palm)

#NORMAL
Gn.palm.st.resid = residuals(Gn.palm, standardize=TRUE)
Gn.palm.st.resid.SQ = (Gn.palm.st.resid^2)
Gn.palm.st.resid.ts = ts(Gn.palm.st.resid) 
Gn.palm.st.resid.SQ.ts =ts(Gn.palm.st.resid.SQ)

#SKEW-T
Gs.palm.st.resid = residuals(Gs.palm, standardize=TRUE)
Gs.palm.st.resid.SQ = (Gs.palm.st.resid^2) 
Gs.palm.st.resid.ts = ts(Gs.palm.st.resid) 
Gs.palm.st.resid.SQ.ts =ts(Gs.palm.st.resid.SQ) 

#STUDENT-T
Gt.palm.st.resid = residuals(Gt.palm, standardize=TRUE) 
Gt.palm.st.resid.SQ = (Gt.palm.st.resid^2) 
Gt.palm.st.resid.ts = ts(Gt.palm.st.resid) 
Gt.palm.st.resid.SQ.ts =ts(Gt.palm.st.resid.SQ)

# (E) (2) EGARCH (palm)

#NORMAL
EGn.palm.st.resid = residuals(EGn.palm, standardize=TRUE)
EGn.palm.st.resid.SQ = (EGn.palm.st.resid^2)
EGn.palm.st.resid.ts = ts(EGn.palm.st.resid)
EGn.palm.st.resid.SQ.ts =ts(EGn.palm.st.resid.SQ) 

#SKEW-T
EGs.palm.st.resid = residuals(EGs.palm, standardize=TRUE) 
EGs.palm.st.resid.SQ = (EGs.palm.st.resid^2)
EGs.palm.st.resid.ts = ts(EGs.palm.st.resid)
EGs.palm.st.resid.SQ.ts =ts(EGs.palm.st.resid.SQ) 

#STUDENT-T
EGt.palm.st.resid = residuals(EGt.palm, standardize=TRUE)
EGt.palm.st.resid.SQ = (EGt.palm.st.resid^2)
EGt.palm.st.resid.ts = ts(EGt.palm.st.resid) 
EGt.palm.st.resid.SQ.ts =ts(EGt.palm.st.resid.SQ) 

# (E) (3) GJRGARCH (palm)

#NORMAL
GJRGn.palm.st.resid = residuals(GJRGn.palm, standardize=TRUE)
GJRGn.palm.st.resid.SQ = (GJRGn.palm.st.resid^2) 
GJRGn.palm.st.resid.ts = ts(GJRGn.palm.st.resid) 
GJRGn.palm.st.resid.SQ.ts =ts(GJRGn.palm.st.resid.SQ) 

#SKEW-T
GJRGs.palm.st.resid = residuals(GJRGs.palm, standardize=TRUE) 
GJRGs.palm.st.resid.SQ = (GJRGs.palm.st.resid^2) 
GJRGs.palm.st.resid.ts = ts(GJRGs.palm.st.resid)
GJRGs.palm.st.resid.SQ.ts =ts(GJRGs.palm.st.resid.SQ) 

#STUDENT-T
GJRGt.palm.st.resid = residuals(GJRGt.palm, standardize=TRUE)
GJRGt.palm.st.resid.SQ = (GJRGt.palm.st.resid^2)
GJRGt.palm.st.resid.ts = ts(GJRGt.palm.st.resid) 
GJRGt.palm.st.resid.SQ.ts =ts(GJRGt.palm.st.resid.SQ) 

# (E) (1) GARCH (Rap)

#NORMAL
Gn.rap.st.resid = residuals(Gn.rap, standardize=TRUE)
Gn.rap.st.resid.SQ = (Gn.rap.st.resid^2)
Gn.rap.st.resid.ts = ts(Gn.rap.st.resid) 
Gn.rap.st.resid.SQ.ts =ts(Gn.rap.st.resid.SQ)

#SKEW-T
Gs.rap.st.resid = residuals(Gs.rap, standardize=TRUE)
Gs.rap.st.resid.SQ = (Gs.rap.st.resid^2) 
Gs.rap.st.resid.ts = ts(Gs.rap.st.resid) 
Gs.rap.st.resid.SQ.ts =ts(Gs.rap.st.resid.SQ) 

#STUDENT-T
Gt.rap.st.resid = residuals(Gt.rap, standardize=TRUE) 
Gt.rap.st.resid.SQ = (Gt.rap.st.resid^2) 
Gt.rap.st.resid.ts = ts(Gt.rap.st.resid) 
Gt.rap.st.resid.SQ.ts =ts(Gt.rap.st.resid.SQ)

# (E) (2) EGARCH (rap)

#NORMAL
EGn.rap.st.resid = residuals(EGn.rap, standardize=TRUE)
EGn.rap.st.resid.SQ = (EGn.rap.st.resid^2)
EGn.rap.st.resid.ts = ts(EGn.rap.st.resid)
EGn.rap.st.resid.SQ.ts =ts(EGn.rap.st.resid.SQ) 

#SKEW-T
EGs.rap.st.resid = residuals(EGs.rap, standardize=TRUE) 
EGs.rap.st.resid.SQ = (EGs.rap.st.resid^2)
EGs.rap.st.resid.ts = ts(EGs.rap.st.resid)
EGs.rap.st.resid.SQ.ts =ts(EGs.rap.st.resid.SQ) 

#STUDENT-T
EGt.rap.st.resid = residuals(EGt.rap, standardize=TRUE)
EGt.rap.st.resid.SQ = (EGt.rap.st.resid^2)
EGt.rap.st.resid.ts = ts(EGt.rap.st.resid) 
EGt.rap.st.resid.SQ.ts =ts(EGt.rap.st.resid.SQ) 

# (E) (3) GJRGARCH (rap)

#NORMAL
GJRGn.rap.st.resid = residuals(GJRGn.rap, standardize=TRUE)
GJRGn.rap.st.resid.SQ = (GJRGn.rap.st.resid^2) 
GJRGn.rap.st.resid.ts = ts(GJRGn.rap.st.resid) 
GJRGn.rap.st.resid.SQ.ts =ts(GJRGn.rap.st.resid.SQ) 

#SKEW-T
GJRGs.rap.st.resid = residuals(GJRGs.rap, standardize=TRUE) 
GJRGs.rap.st.resid.SQ = (GJRGs.rap.st.resid^2) 
GJRGs.rap.st.resid.ts = ts(GJRGs.rap.st.resid)
GJRGs.rap.st.resid.SQ.ts =ts(GJRGs.rap.st.resid.SQ) 

#STUDENT-T
GJRGt.rap.st.resid = residuals(GJRGt.rap, standardize=TRUE)
GJRGt.rap.st.resid.SQ = (GJRGt.rap.st.resid^2)
GJRGt.rap.st.resid.ts = ts(GJRGt.rap.st.resid) 
GJRGt.rap.st.resid.SQ.ts =ts(GJRGt.rap.st.resid.SQ) 

# (E) (1) GARCH (Soy)

#NORMAL
Gn.soy.st.resid = residuals(Gn.soy, standardize=TRUE)
Gn.soy.st.resid.SQ = (Gn.soy.st.resid^2)
Gn.soy.st.resid.ts = ts(Gn.soy.st.resid) 
Gn.soy.st.resid.SQ.ts =ts(Gn.soy.st.resid.SQ)

#SKEW-T
Gs.soy.st.resid = residuals(Gs.soy, standardize=TRUE)
Gs.soy.st.resid.SQ = (Gs.soy.st.resid^2) 
Gs.soy.st.resid.ts = ts(Gs.soy.st.resid) 
Gs.soy.st.resid.SQ.ts =ts(Gs.soy.st.resid.SQ) 

#STUDENT-T
Gt.soy.st.resid = residuals(Gt.soy, standardize=TRUE) 
Gt.soy.st.resid.SQ = (Gt.soy.st.resid^2) 
Gt.soy.st.resid.ts = ts(Gt.soy.st.resid) 
Gt.soy.st.resid.SQ.ts =ts(Gt.soy.st.resid.SQ)

# (E) (2) EGARCH (soy)

#NORMAL
EGn.soy.st.resid = residuals(EGn.soy, standardize=TRUE)
EGn.soy.st.resid.SQ = (EGn.soy.st.resid^2)
EGn.soy.st.resid.ts = ts(EGn.soy.st.resid)
EGn.soy.st.resid.SQ.ts =ts(EGn.soy.st.resid.SQ) 

#SKEW-T
EGs.soy.st.resid = residuals(EGs.soy, standardize=TRUE) 
EGs.soy.st.resid.SQ = (EGs.soy.st.resid^2)
EGs.soy.st.resid.ts = ts(EGs.soy.st.resid)
EGs.soy.st.resid.SQ.ts =ts(EGs.soy.st.resid.SQ) 

#STUDENT-T
EGt.soy.st.resid = residuals(EGt.soy, standardize=TRUE)
EGt.soy.st.resid.SQ = (EGt.soy.st.resid^2)
EGt.soy.st.resid.ts = ts(EGt.soy.st.resid) 
EGt.soy.st.resid.SQ.ts =ts(EGt.soy.st.resid.SQ) 

# (E) (3) GJRGARCH (soy)

#NORMAL
GJRGn.soy.st.resid = residuals(GJRGn.soy, standardize=TRUE)
GJRGn.soy.st.resid.SQ = (GJRGn.soy.st.resid^2) 
GJRGn.soy.st.resid.ts = ts(GJRGn.soy.st.resid) 
GJRGn.soy.st.resid.SQ.ts =ts(GJRGn.soy.st.resid.SQ) 

#SKEW-T
GJRGs.soy.st.resid = residuals(GJRGs.soy, standardize=TRUE) 
GJRGs.soy.st.resid.SQ = (GJRGs.soy.st.resid^2) 
GJRGs.soy.st.resid.ts = ts(GJRGs.soy.st.resid)
GJRGs.soy.st.resid.SQ.ts =ts(GJRGs.soy.st.resid.SQ) 

#STUDENT-T
GJRGt.soy.st.resid = residuals(GJRGt.soy, standardize=TRUE)
GJRGt.soy.st.resid.SQ = (GJRGt.soy.st.resid^2)
GJRGt.soy.st.resid.ts = ts(GJRGt.soy.st.resid) 
GJRGt.soy.st.resid.SQ.ts =ts(GJRGt.soy.st.resid.SQ) 
#########################Q-Q Plot(Best fitted model)########################
# Corn
qqnorm(EGt.corn.st.resid, main = "Q-Q Plot of standardized residuals (corn)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(EGt.corn.st.resid, col="red", lty=5, lwd=2)

# Palm
qqnorm(GJRGs.palm.st.resid, main = "Q-Q Plot of standardized residuals (palm)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(GJRGs.palm.st.resid, col="red", lty=5, lwd=2)

# Rap
qqnorm(EGt.rap.st.resid, main = "Q-Q Plot of standardized residuals (rap)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(EGt.rap.st.resid, col="red", lty=5, lwd=2)

# Soy
qqnorm(EGs.soy.st.resid, main = "Q-Q Plot of standardized residuals (soy)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col="darkblue", las=1)
qqline(EGs.soy.st.resid, col="red", lty=5, lwd=2)

#########################Ljung-Box TEST###############################

# (E) (1) GARCH (Corn)

#NORMAL
Box.test(Gn.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gn.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gn.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gn.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gn.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gn.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(Gs.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gs.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gs.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gs.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gs.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gs.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(Gt.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gt.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gt.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gt.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gt.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gt.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (2) EGARCH (corn)

#NORMAL
Box.test(EGn.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGn.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGn.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGn.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGn.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGn.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(EGs.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGs.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGs.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGs.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGs.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGs.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(EGt.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGt.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGt.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGt.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGt.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGt.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (3) GJRGARCH (corn)

#NORMAL
Box.test(GJRGn.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGn.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGn.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGn.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGn.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGn.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(GJRGs.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGs.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGs.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGs.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGs.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGs.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(GJRGt.corn.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGt.corn.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGt.corn.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGt.corn.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGt.corn.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGt.corn.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (1) GARCH (Palm)

#NORMAL
Box.test(Gn.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gn.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gn.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gn.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gn.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gn.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(Gs.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gs.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gs.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gs.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gs.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gs.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(Gt.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gt.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gt.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gt.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gt.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gt.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (2) EGARCH (palm)

#NORMAL
Box.test(EGn.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGn.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGn.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGn.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGn.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGn.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(EGs.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGs.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGs.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGs.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGs.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGs.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(EGt.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGt.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGt.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGt.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGt.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGt.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (3) GJRGARCH (palm)

#NORMAL
Box.test(GJRGn.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGn.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGn.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGn.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGn.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGn.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(GJRGs.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGs.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGs.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGs.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGs.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGs.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(GJRGt.palm.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGt.palm.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGt.palm.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGt.palm.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGt.palm.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGt.palm.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (1) GARCH (Rap)

#NORMAL
Box.test(Gn.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gn.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gn.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gn.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gn.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gn.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(Gs.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gs.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gs.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gs.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gs.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gs.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(Gt.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gt.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gt.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gt.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gt.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gt.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (2) EGARCH (rap)

#NORMAL
Box.test(EGn.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGn.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGn.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGn.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGn.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGn.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(EGs.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGs.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGs.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGs.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGs.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGs.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(EGt.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGt.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGt.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGt.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGt.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGt.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (3) GJRGARCH (rap)

#NORMAL
Box.test(GJRGn.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGn.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGn.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGn.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGn.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGn.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(GJRGs.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGs.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGs.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGs.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGs.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGs.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(GJRGt.rap.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGt.rap.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGt.rap.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGt.rap.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGt.rap.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGt.rap.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (1) GARCH (Soy)

#NORMAL
Box.test(Gn.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gn.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gn.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gn.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gn.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gn.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(Gs.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gs.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gs.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gs.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gs.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gs.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(Gt.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(Gt.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(Gt.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(Gt.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(Gt.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(Gt.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (2) EGARCH (soy)

#NORMAL
Box.test(EGn.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGn.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGn.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGn.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGn.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGn.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(EGs.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGs.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGs.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGs.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGs.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGs.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(EGt.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(EGt.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(EGt.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(EGt.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(EGt.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(EGt.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

# (E) (3) GJRGARCH (soy)

#NORMAL
Box.test(GJRGn.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGn.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGn.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGn.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGn.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGn.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#SKEW-T
Box.test(GJRGs.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGs.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGs.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGs.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGs.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGs.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

#STUDENT-T
Box.test(GJRGt.soy.st.resid, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Mean)
Box.test(GJRGt.soy.st.resid, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Mean)
Box.test(GJRGt.soy.st.resid, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Mean)
Box.test(GJRGt.soy.st.resid.SQ, lag = 5, type = "Ljung") # Ljung-Box test of lag 5 (Variance)
Box.test(GJRGt.soy.st.resid.SQ, lag = 10, type = "Ljung") # Ljung-Box test of lag 10 (Variance)
Box.test(GJRGt.soy.st.resid.SQ, lag = 20, type = "Ljung") # Ljung-Box test of lag 20 (Variance)

###############News impact curve#################

# Corn
ni.Gn.corn=newsimpact(z = NULL, Gn.corn)  ## chanege the ugarchfit object
ni.EGn.corn=newsimpact(z = NULL, EGn.corn)
plot(ni.Gn.corn$zx, ni.Gn.corn$zy, ylab=ni.Gn.corn$yexpr, xlab=ni.Gn.corn$xexpr, type="l", 
     main = "News Impact Curve for GARCH(1,1) and EGARCH(1,1) of corn")
lines(ni.EGn.corn$zx, ni.EGn.corn$zy, lty=2, col=2)
legend("bottomright", lty=c(1,2), col=c(1,2), legend = c("GARCH","EGARCH"), cex = 0.75)

# Palm
ni.Gn.palm=newsimpact(z = NULL, Gn.palm)  ## chanege the ugarchfit object
ni.EGn.palm=newsimpact(z = NULL, EGn.palm)
plot(ni.Gn.palm$zx, ni.Gn.palm$zy, ylab=ni.Gn.palm$yexpr, xlab=ni.Gn.palm$xexpr, type="l", 
     main = "News Impact Curve for GARCH(1,1) and EGARCH(1,1) of palm")
lines(ni.EGt.palm$zx, ni.EGt.palm$zy, lty=2, col=2)
legend("bottomright", lty=c(1,2), col=c(1,2), legend = c("GARCH","EGARCH"), cex = 0.70)

# Rap
ni.Gn.rap=newsimpact(z = NULL, Gn.rap)  ## chanege the ugarchfit object
ni.EGn.rap=newsimpact(z = NULL, EGn.rap)
plot(ni.Gn.rap$zx, ni.Gn.rap$zy, ylab=ni.Gn.rap$yexpr, xlab=ni.Gn.rap$xexpr, type="l", 
     main = "News Impact Curve for GARCH(1,1) and EGARCH(1,1) of rap")
lines(ni.EGn.rap$zx, ni.EGn.rap$zy, lty=2, col=2)
legend("bottomright", lty=c(1,2), col=c(1,2), legend = c("GARCH","EGARCH"), cex = 0.75)

# Soy
ni.Gn.soy=newsimpact(z = NULL, Gn.soy)  ## chanege the ugarchfit object
ni.EGn.soy=newsimpact(z = NULL, EGn.soy)
plot(ni.Gn.soy$zx, ni.Gn.soy$zy, ylab=ni.Gn.soy$yexpr, xlab=ni.Gn.soy$xexpr, type="l", 
     main = "News Impact Curve for GARCH(1,1) and EGARCH(1,1) of soy")
lines(ni.EGn.soy$zx, ni.EGn.soy$zy, lty=2, col=2)
legend("bottomright", lty=c(1,2), col=c(1,2), legend = c("GARCH","EGARCH"), cex = 0.70)


### (f)
## obtain mean and volatility forecasts and RMSE & MAE 

library(fpp)

### <corn>
## 1-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.corn.out.1 <- ugarchfit(log.corn, spec = spec1, solver = "gosolnp", out.sample = 1) 
f.Gn.corn.out.1 <- ugarchforecast(Gn.corn.out.1, n.ahead = 1)

vol.Gn.corn.out.1 <- sigma(f.Gn.corn.out.1)
vol.Gn.corn.out.1 <- ts(vol.Gn.corn.out.1)
vol.Gn.corn.out.sq.1 <- vol.Gn.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.Gn.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.corn.out.1 <- ugarchfit(log.corn, spec = spec1.2, solver = "gosolnp", out.sample = 1) 

f.Gs.corn.out.1 <- ugarchforecast(Gs.corn.out.1, n.ahead = 1)

vol.Gs.corn.out.1 <- sigma(f.Gs.corn.out.1)
vol.Gs.corn.out.1 <- ts(vol.Gs.corn.out.1)
vol.Gs.corn.out.sq.1 <- vol.Gs.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.Gs.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.corn.out.1 <- ugarchfit(log.corn, spec = spec1.3, solver = "gosolnp", out.sample = 1) 

f.Gt.corn.out.1 <- ugarchforecast(Gt.corn.out.1, n.ahead = 1)

vol.Gt.corn.out.1 <- sigma(f.Gt.corn.out.1)
vol.Gt.corn.out.1 <- ts(vol.Gt.corn.out.1)
vol.Gt.corn.out.sq.1 <- vol.Gt.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.Gt.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.corn.out.1 <- ugarchfit(log.corn, spec = spec2, solver = "gosolnp", out.sample = 1) 
f.EGn.corn.out.1 <- ugarchforecast(EGn.corn.out.1, n.ahead = 1)

vol.EGn.corn.out.1 <- sigma(f.EGn.corn.out.1)
vol.EGn.corn.out.1 <- ts(vol.EGn.corn.out.1)
vol.EGn.corn.out.sq.1 <- vol.EGn.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.EGn.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.corn.out.1 <- ugarchfit(log.corn, spec = spec2.2, solver = "gosolnp", out.sample = 1) 

f.EGs.corn.out.1 <- ugarchforecast(EGs.corn.out.1, n.ahead = 1)

vol.EGs.corn.out.1 <- sigma(f.EGs.corn.out.1)
vol.EGs.corn.out.1 <- ts(vol.EGs.corn.out.1)
vol.EGs.corn.out.sq.1 <- vol.EGs.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.EGs.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.corn.out.1 <- ugarchfit(log.corn, spec = spec2.3, solver = "gosolnp", out.sample = 1) 

f.EGt.corn.out.1 <- ugarchforecast(EGt.corn.out.1, n.ahead = 1)

vol.EGt.corn.out.1 <- sigma(f.EGt.corn.out.1)
vol.EGt.corn.out.1 <- ts(vol.EGt.corn.out.1)
vol.EGt.corn.out.sq.1 <- vol.EGt.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.EGt.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.corn.out.1 <- ugarchfit(log.corn, spec = spec3, solver = "gosolnp", out.sample = 1) 

f.GJRGn.corn.out.1 <- ugarchforecast(GJRGn.corn.out.1, n.ahead = 1)

vol.GJRGn.corn.out.1 <- sigma(f.GJRGn.corn.out.1)
vol.GJRGn.corn.out.1 <- ts(vol.GJRGn.corn.out.1)
vol.GJRGn.corn.out.sq.1 <- vol.GJRGn.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGn.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.corn.out.1 <- ugarchfit(log.corn, spec = spec3.2, solver = "gosolnp", out.sample = 1) 

f.GJRGs.corn.out.1 <- ugarchforecast(GJRGs.corn.out.1, n.ahead = 1)

vol.GJRGs.corn.out.1 <- sigma(f.GJRGs.corn.out.1)
vol.GJRGs.corn.out.1 <- ts(vol.GJRGs.corn.out.1)
vol.GJRGs.corn.out.sq.1 <- vol.GJRGs.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGs.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.corn.out.1 <- ugarchfit(log.corn, spec = spec3.3, solver = "gosolnp", out.sample = 1) 

f.GJRGt.corn.out.1 <- ugarchforecast(GJRGt.corn.out.1, n.ahead = 1)

vol.GJRGt.corn.out.1 <- sigma(f.GJRGt.corn.out.1)
vol.GJRGt.corn.out.1 <- ts(vol.GJRGt.corn.out.1)
vol.GJRGt.corn.out.sq.1 <- vol.GJRGt.corn.out.1^2

act.ret.corn <- (tail(log.corn,1))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGt.corn.out.sq.1, x=act.ret.corn, test=NULL, d=NULL, D=NULL)

## 5-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.corn.out.5 <- ugarchfit(log.corn, spec = spec1, solver = "gosolnp", out.sample = 5) 
f.Gn.corn.out.5 <- ugarchforecast(Gn.corn.out.5, n.ahead = 5)

vol.Gn.corn.out.5 <- sigma(f.Gn.corn.out.5)
vol.Gn.corn.out.5 <- ts(vol.Gn.corn.out.5)
vol.Gn.corn.out.sq.5 <- vol.Gn.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.Gn.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.corn.out.5 <- ugarchfit(log.corn, spec = spec1.2, solver = "gosolnp", out.sample = 5) 

f.Gs.corn.out.5 <- ugarchforecast(Gs.corn.out.5, n.ahead = 5)

vol.Gs.corn.out.5 <- sigma(f.Gs.corn.out.5)
vol.Gs.corn.out.5 <- ts(vol.Gs.corn.out.5)
vol.Gs.corn.out.sq.5 <- vol.Gs.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.Gs.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.corn.out.5 <- ugarchfit(log.corn, spec = spec1.3, solver = "gosolnp", out.sample = 5) 

f.Gt.corn.out.5 <- ugarchforecast(Gt.corn.out.5, n.ahead = 5)

vol.Gt.corn.out.5 <- sigma(f.Gt.corn.out.5)
vol.Gt.corn.out.5 <- ts(vol.Gt.corn.out.5)
vol.Gt.corn.out.sq.5 <- vol.Gt.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.Gt.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.corn.out.5 <- ugarchfit(log.corn, spec = spec2, solver = "gosolnp", out.sample = 5) 
f.EGn.corn.out.5 <- ugarchforecast(EGn.corn.out.5, n.ahead = 5)

vol.EGn.corn.out.5 <- sigma(f.EGn.corn.out.5)
vol.EGn.corn.out.5 <- ts(vol.EGn.corn.out.5)
vol.EGn.corn.out.sq.5 <- vol.EGn.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.EGn.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.corn.out.5 <- ugarchfit(log.corn, spec = spec2.2, solver = "gosolnp", out.sample = 5) 

f.EGs.corn.out.5 <- ugarchforecast(EGs.corn.out.5, n.ahead = 5)

vol.EGs.corn.out.5 <- sigma(f.EGs.corn.out.5)
vol.EGs.corn.out.5 <- ts(vol.EGs.corn.out.5)
vol.EGs.corn.out.sq.5 <- vol.EGs.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.EGs.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.corn.out.5 <- ugarchfit(log.corn, spec = spec2.3, solver = "gosolnp", out.sample = 5) 

f.EGt.corn.out.5 <- ugarchforecast(EGt.corn.out.5, n.ahead = 5)

vol.EGt.corn.out.5 <- sigma(f.EGt.corn.out.5)
vol.EGt.corn.out.5 <- ts(vol.EGt.corn.out.5)
vol.EGt.corn.out.sq.5 <- vol.EGt.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.EGt.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.corn.out.5 <- ugarchfit(log.corn, spec = spec3, solver = "gosolnp", out.sample = 5) 

f.GJRGn.corn.out.5 <- ugarchforecast(GJRGn.corn.out.5, n.ahead = 5)

vol.GJRGn.corn.out.5 <- sigma(f.GJRGn.corn.out.5)
vol.GJRGn.corn.out.5 <- ts(vol.GJRGn.corn.out.5)
vol.GJRGn.corn.out.sq.5 <- vol.GJRGn.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGn.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.corn.out.5 <- ugarchfit(log.corn, spec = spec3.2, solver = "gosolnp", out.sample = 5) 

f.GJRGs.corn.out.5 <- ugarchforecast(GJRGs.corn.out.5, n.ahead = 5)

vol.GJRGs.corn.out.5 <- sigma(f.GJRGs.corn.out.5)
vol.GJRGs.corn.out.5 <- ts(vol.GJRGs.corn.out.5)
vol.GJRGs.corn.out.sq.5 <- vol.GJRGs.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGs.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.corn.out.5 <- ugarchfit(log.corn, spec = spec3.3, solver = "gosolnp", out.sample = 5) 

f.GJRGt.corn.out.5 <- ugarchforecast(GJRGt.corn.out.5, n.ahead = 5)

vol.GJRGt.corn.out.5 <- sigma(f.GJRGt.corn.out.5)
vol.GJRGt.corn.out.5 <- ts(vol.GJRGt.corn.out.5)
vol.GJRGt.corn.out.sq.5 <- vol.GJRGt.corn.out.5^2

act.ret.corn.5 <- (tail(log.corn,5))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGt.corn.out.sq.5, x=act.ret.corn.5, test=NULL, d=NULL, D=NULL)

## 20-step ahead

# GARCH(1,1)-norm
spec1 
Gn.corn.out.20 <- ugarchfit(log.corn, spec = spec1, solver = "gosolnp", out.sample = 20) 
f.Gn.corn.out.20 <- ugarchforecast(Gn.corn.out.20, n.ahead = 20)

vol.Gn.corn.out.20 <- sigma(f.Gn.corn.out.20)
vol.Gn.corn.out.20 <- ts(vol.Gn.corn.out.20)
vol.Gn.corn.out.sq.20 <- vol.Gn.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.Gn.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.corn.out.20 <- ugarchfit(log.corn, spec = spec1.2, solver = "gosolnp", out.sample = 20) 

f.Gs.corn.out.20 <- ugarchforecast(Gs.corn.out.20, n.ahead = 20)

vol.Gs.corn.out.20 <- sigma(f.Gs.corn.out.20)
vol.Gs.corn.out.20 <- ts(vol.Gs.corn.out.20)
vol.Gs.corn.out.sq.20 <- vol.Gs.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.Gs.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.corn.out.20 <- ugarchfit(log.corn, spec = spec1.3, solver = "gosolnp", out.sample = 20) 

f.Gt.corn.out.20 <- ugarchforecast(Gt.corn.out.20, n.ahead = 20)

vol.Gt.corn.out.20 <- sigma(f.Gt.corn.out.20)
vol.Gt.corn.out.20 <- ts(vol.Gt.corn.out.20)
vol.Gt.corn.out.sq.20 <- vol.Gt.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.Gt.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.corn.out.20 <- ugarchfit(log.corn, spec = spec2, solver = "gosolnp", out.sample = 20) 
f.EGn.corn.out.20 <- ugarchforecast(EGn.corn.out.20, n.ahead = 20)

vol.EGn.corn.out.20 <- sigma(f.EGn.corn.out.20)
vol.EGn.corn.out.20 <- ts(vol.EGn.corn.out.20)
vol.EGn.corn.out.sq.20 <- vol.EGn.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.EGn.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.corn.out.20 <- ugarchfit(log.corn, spec = spec2.2, solver = "gosolnp", out.sample = 20) 

f.EGs.corn.out.20 <- ugarchforecast(EGs.corn.out.20, n.ahead = 20)

vol.EGs.corn.out.20 <- sigma(f.EGs.corn.out.20)
vol.EGs.corn.out.20 <- ts(vol.EGs.corn.out.20)
vol.EGs.corn.out.sq.20 <- vol.EGs.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.EGs.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.corn.out.20 <- ugarchfit(log.corn, spec = spec2.3, solver = "gosolnp", out.sample = 20) 

f.EGt.corn.out.20 <- ugarchforecast(EGt.corn.out.20, n.ahead = 20)

vol.EGt.corn.out.20 <- sigma(f.EGt.corn.out.20)
vol.EGt.corn.out.20 <- ts(vol.EGt.corn.out.20)
vol.EGt.corn.out.sq.20 <- vol.EGt.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.EGt.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.corn.out.20 <- ugarchfit(log.corn, spec = spec3, solver = "gosolnp", out.sample = 20) 

f.GJRGn.corn.out.20 <- ugarchforecast(GJRGn.corn.out.20, n.ahead = 20)

vol.GJRGn.corn.out.20 <- sigma(f.GJRGn.corn.out.20)
vol.GJRGn.corn.out.20 <- ts(vol.GJRGn.corn.out.20)
vol.GJRGn.corn.out.sq.20 <- vol.GJRGn.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGn.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.corn.out.20 <- ugarchfit(log.corn, spec = spec3.2, solver = "gosolnp", out.sample = 20) 

f.GJRGs.corn.out.20 <- ugarchforecast(GJRGs.corn.out.20, n.ahead = 20)

vol.GJRGs.corn.out.20 <- sigma(f.GJRGs.corn.out.20)
vol.GJRGs.corn.out.20 <- ts(vol.GJRGs.corn.out.20)
vol.GJRGs.corn.out.sq.20 <- vol.GJRGs.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGs.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.corn.out.20 <- ugarchfit(log.corn, spec = spec3.3, solver = "gosolnp", out.sample = 20) 

f.GJRGt.corn.out.20 <- ugarchforecast(GJRGt.corn.out.20, n.ahead = 20)

vol.GJRGt.corn.out.20 <- sigma(f.GJRGt.corn.out.20)
vol.GJRGt.corn.out.20 <- ts(vol.GJRGt.corn.out.20)
vol.GJRGt.corn.out.sq.20 <- vol.GJRGt.corn.out.20^2

act.ret.corn.20 <- (tail(log.corn,20))^2 ## squared actual returns of corn 

accuracy(f=vol.GJRGt.corn.out.sq.20, x=act.ret.corn.20, test=NULL, d=NULL, D=NULL)


### <palm>
## 1-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.palm.out.1 <- ugarchfit(log.palm, spec = spec1, solver = "gosolnp", out.sample = 1) 
f.Gn.palm.out.1 <- ugarchforecast(Gn.palm.out.1, n.ahead = 1)

vol.Gn.palm.out.1 <- sigma(f.Gn.palm.out.1)
vol.Gn.palm.out.1 <- ts(vol.Gn.palm.out.1)
vol.Gn.palm.out.sq.1 <- vol.Gn.palm.out.1^2

act.ret.palm <- (tail(log.palm[1:3859],1))^2 ## squared actual returns of palm 

accuracy(f=vol.Gn.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.palm.out.1 <- ugarchfit(log.palm, spec = spec1.2, solver = "gosolnp", out.sample = 1) 

f.Gs.palm.out.1 <- ugarchforecast(Gs.palm.out.1, n.ahead = 1)

vol.Gs.palm.out.1 <- sigma(f.Gs.palm.out.1)
vol.Gs.palm.out.1 <- ts(vol.Gs.palm.out.1)
vol.Gs.palm.out.sq.1 <- vol.Gs.palm.out.1^2

accuracy(f=vol.Gs.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.palm.out.1 <- ugarchfit(log.palm, spec = spec1.3, solver = "gosolnp", out.sample = 1) 

f.Gt.palm.out.1 <- ugarchforecast(Gt.palm.out.1, n.ahead = 1)

vol.Gt.palm.out.1 <- sigma(f.Gt.palm.out.1)
vol.Gt.palm.out.1 <- ts(vol.Gt.palm.out.1)
vol.Gt.palm.out.sq.1 <- vol.Gt.palm.out.1^2

accuracy(f=vol.Gt.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.palm.out.1 <- ugarchfit(log.palm, spec = spec2, solver = "gosolnp", out.sample = 1) 
f.EGn.palm.out.1 <- ugarchforecast(EGn.palm.out.1, n.ahead = 1)

vol.EGn.palm.out.1 <- sigma(f.EGn.palm.out.1)
vol.EGn.palm.out.1 <- ts(vol.EGn.palm.out.1)
vol.EGn.palm.out.sq.1 <- vol.EGn.palm.out.1^2

accuracy(f=vol.EGn.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.palm.out.1 <- ugarchfit(log.palm, spec = spec2.2, solver = "gosolnp", out.sample = 1) 

f.EGs.palm.out.1 <- ugarchforecast(EGs.palm.out.1, n.ahead = 1)

vol.EGs.palm.out.1 <- sigma(f.EGs.palm.out.1)
vol.EGs.palm.out.1 <- ts(vol.EGs.palm.out.1)
vol.EGs.palm.out.sq.1 <- vol.EGs.palm.out.1^2

accuracy(f=vol.EGs.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.palm.out.1 <- ugarchfit(log.palm, spec = spec2.3, solver = "gosolnp", out.sample = 1) 

f.EGt.palm.out.1 <- ugarchforecast(EGt.palm.out.1, n.ahead = 1)

vol.EGt.palm.out.1 <- sigma(f.EGt.palm.out.1)
vol.EGt.palm.out.1 <- ts(vol.EGt.palm.out.1)
vol.EGt.palm.out.sq.1 <- vol.EGt.palm.out.1^2

accuracy(f=vol.EGt.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.palm.out.1 <- ugarchfit(log.palm, spec = spec3, solver = "gosolnp", out.sample = 1) 

f.GJRGn.palm.out.1 <- ugarchforecast(GJRGn.palm.out.1, n.ahead = 1)

vol.GJRGn.palm.out.1 <- sigma(f.GJRGn.palm.out.1)
vol.GJRGn.palm.out.1 <- ts(vol.GJRGn.palm.out.1)
vol.GJRGn.palm.out.sq.1 <- vol.GJRGn.palm.out.1^2

accuracy(f=vol.GJRGn.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.palm.out.1 <- ugarchfit(log.palm, spec = spec3.2, solver = "gosolnp", out.sample = 1) 

f.GJRGs.palm.out.1 <- ugarchforecast(GJRGs.palm.out.1, n.ahead = 1)

vol.GJRGs.palm.out.1 <- sigma(f.GJRGs.palm.out.1)
vol.GJRGs.palm.out.1 <- ts(vol.GJRGs.palm.out.1)
vol.GJRGs.palm.out.sq.1 <- vol.GJRGs.palm.out.1^2

accuracy(f=vol.GJRGs.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.palm.out.1 <- ugarchfit(log.palm, spec = spec3.3, solver = "gosolnp", out.sample = 1) 

f.GJRGt.palm.out.1 <- ugarchforecast(GJRGt.palm.out.1, n.ahead = 1)

vol.GJRGt.palm.out.1 <- sigma(f.GJRGt.palm.out.1)
vol.GJRGt.palm.out.1 <- ts(vol.GJRGt.palm.out.1)
vol.GJRGt.palm.out.sq.1 <- vol.GJRGt.palm.out.1^2

accuracy(f=vol.GJRGt.palm.out.sq.1, x=act.ret.palm, test=NULL, d=NULL, D=NULL)


## 5-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.palm.out.5 <- ugarchfit(log.palm, spec = spec1, solver = "gosolnp", out.sample = 5) 
f.Gn.palm.out.5 <- ugarchforecast(Gn.palm.out.5, n.ahead = 5)

vol.Gn.palm.out.5 <- sigma(f.Gn.palm.out.5)
vol.Gn.palm.out.5 <- ts(vol.Gn.palm.out.5)
vol.Gn.palm.out.sq.5 <- vol.Gn.palm.out.5^2

act.ret.palm.5 <- (tail(log.palm[1:3859],5))^2 ## squared actual returns of palm 

accuracy(f=vol.Gn.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.palm.out.5 <- ugarchfit(log.palm, spec = spec1.2, solver = "gosolnp", out.sample = 5) 

f.Gs.palm.out.5 <- ugarchforecast(Gs.palm.out.5, n.ahead = 5)

vol.Gs.palm.out.5 <- sigma(f.Gs.palm.out.5)
vol.Gs.palm.out.5 <- ts(vol.Gs.palm.out.5)
vol.Gs.palm.out.sq.5 <- vol.Gs.palm.out.5^2

accuracy(f=vol.Gs.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.palm.out.5 <- ugarchfit(log.palm, spec = spec1.3, solver = "gosolnp", out.sample = 5) 

f.Gt.palm.out.5 <- ugarchforecast(Gt.palm.out.5, n.ahead = 5)

vol.Gt.palm.out.5 <- sigma(f.Gt.palm.out.5)
vol.Gt.palm.out.5 <- ts(vol.Gt.palm.out.5)
vol.Gt.palm.out.sq.5 <- vol.Gt.palm.out.5^2

accuracy(f=vol.Gt.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.palm.out.5 <- ugarchfit(log.palm, spec = spec2, solver = "gosolnp", out.sample = 5) 
f.EGn.palm.out.5 <- ugarchforecast(EGn.palm.out.5, n.ahead = 5)

vol.EGn.palm.out.5 <- sigma(f.EGn.palm.out.5)
vol.EGn.palm.out.5 <- ts(vol.EGn.palm.out.5)
vol.EGn.palm.out.sq.5 <- vol.EGn.palm.out.5^2

accuracy(f=vol.EGn.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.palm.out.5 <- ugarchfit(log.palm, spec = spec2.2, solver = "gosolnp", out.sample = 5) 

f.EGs.palm.out.5 <- ugarchforecast(EGs.palm.out.5, n.ahead = 5)

vol.EGs.palm.out.5 <- sigma(f.EGs.palm.out.5)
vol.EGs.palm.out.5 <- ts(vol.EGs.palm.out.5)
vol.EGs.palm.out.sq.5 <- vol.EGs.palm.out.5^2

accuracy(f=vol.EGs.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.palm.out.5 <- ugarchfit(log.palm, spec = spec2.3, solver = "gosolnp", out.sample = 5) 

f.EGt.palm.out.5 <- ugarchforecast(EGt.palm.out.5, n.ahead = 5)

vol.EGt.palm.out.5 <- sigma(f.EGt.palm.out.5)
vol.EGt.palm.out.5 <- ts(vol.EGt.palm.out.5)
vol.EGt.palm.out.sq.5 <- vol.EGt.palm.out.5^2

accuracy(f=vol.EGt.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.palm.out.5 <- ugarchfit(log.palm, spec = spec3, solver = "gosolnp", out.sample = 5) 

f.GJRGn.palm.out.5 <- ugarchforecast(GJRGn.palm.out.5, n.ahead = 5)

vol.GJRGn.palm.out.5 <- sigma(f.GJRGn.palm.out.5)
vol.GJRGn.palm.out.5 <- ts(vol.GJRGn.palm.out.5)
vol.GJRGn.palm.out.sq.5 <- vol.GJRGn.palm.out.5^2

accuracy(f=vol.GJRGn.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.palm.out.5 <- ugarchfit(log.palm, spec = spec3.2, solver = "gosolnp", out.sample = 5) 

f.GJRGs.palm.out.5 <- ugarchforecast(GJRGs.palm.out.5, n.ahead = 5)

vol.GJRGs.palm.out.5 <- sigma(f.GJRGs.palm.out.5)
vol.GJRGs.palm.out.5 <- ts(vol.GJRGs.palm.out.5)
vol.GJRGs.palm.out.sq.5 <- vol.GJRGs.palm.out.5^2

accuracy(f=vol.GJRGs.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.palm.out.5 <- ugarchfit(log.palm, spec = spec3.3, solver = "gosolnp", out.sample = 5) 

f.GJRGt.palm.out.5 <- ugarchforecast(GJRGt.palm.out.5, n.ahead = 5)

vol.GJRGt.palm.out.5 <- sigma(f.GJRGt.palm.out.5)
vol.GJRGt.palm.out.5 <- ts(vol.GJRGt.palm.out.5)
vol.GJRGt.palm.out.sq.5 <- vol.GJRGt.palm.out.5^2

accuracy(f=vol.GJRGt.palm.out.sq.5, x=act.ret.palm.5, test=NULL, d=NULL, D=NULL)


## 20-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.palm.out.20 <- ugarchfit(log.palm, spec = spec1, solver = "gosolnp", out.sample = 20) 
f.Gn.palm.out.20 <- ugarchforecast(Gn.palm.out.20, n.ahead = 20)

vol.Gn.palm.out.20 <- sigma(f.Gn.palm.out.20)
vol.Gn.palm.out.20 <- ts(vol.Gn.palm.out.20)
vol.Gn.palm.out.sq.20 <- vol.Gn.palm.out.20^2

act.ret.palm.20 <- (tail(log.palm[1:3859],20))^2 ## squared actual returns of palm 

accuracy(f=vol.Gn.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.palm.out.20 <- ugarchfit(log.palm, spec = spec1.2, solver = "gosolnp", out.sample = 20) 

f.Gs.palm.out.20 <- ugarchforecast(Gs.palm.out.20, n.ahead = 20)

vol.Gs.palm.out.20 <- sigma(f.Gs.palm.out.20)
vol.Gs.palm.out.20 <- ts(vol.Gs.palm.out.20)
vol.Gs.palm.out.sq.20 <- vol.Gs.palm.out.20^2

accuracy(f=vol.Gs.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.palm.out.20 <- ugarchfit(log.palm, spec = spec1.3, solver = "gosolnp", out.sample = 20) 

f.Gt.palm.out.20 <- ugarchforecast(Gt.palm.out.20, n.ahead = 20)

vol.Gt.palm.out.20 <- sigma(f.Gt.palm.out.20)
vol.Gt.palm.out.20 <- ts(vol.Gt.palm.out.20)
vol.Gt.palm.out.sq.20 <- vol.Gt.palm.out.20^2

accuracy(f=vol.Gt.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-norm
spec2 
EGn.palm.out.20 <- ugarchfit(log.palm, spec = spec2, solver = "gosolnp", out.sample = 20) 
f.EGn.palm.out.20 <- ugarchforecast(EGn.palm.out.20, n.ahead = 20)

vol.EGn.palm.out.20 <- sigma(f.EGn.palm.out.20)
vol.EGn.palm.out.20 <- ts(vol.EGn.palm.out.20)
vol.EGn.palm.out.sq.20 <- vol.EGn.palm.out.20^2

accuracy(f=vol.EGn.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.palm.out.20 <- ugarchfit(log.palm, spec = spec2.2, solver = "gosolnp", out.sample = 20) 

f.EGs.palm.out.20 <- ugarchforecast(EGs.palm.out.20, n.ahead = 20)

vol.EGs.palm.out.20 <- sigma(f.EGs.palm.out.20)
vol.EGs.palm.out.20 <- ts(vol.EGs.palm.out.20)
vol.EGs.palm.out.sq.20 <- vol.EGs.palm.out.20^2

accuracy(f=vol.EGs.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.palm.out.20 <- ugarchfit(log.palm, spec = spec2.3, solver = "gosolnp", out.sample = 20) 

f.EGt.palm.out.20 <- ugarchforecast(EGt.palm.out.20, n.ahead = 20)

vol.EGt.palm.out.20 <- sigma(f.EGt.palm.out.20)
vol.EGt.palm.out.20 <- ts(vol.EGt.palm.out.20)
vol.EGt.palm.out.sq.20 <- vol.EGt.palm.out.20^2

accuracy(f=vol.EGt.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-norm
spec3
GJRGn.palm.out.20 <- ugarchfit(log.palm, spec = spec3, solver = "gosolnp", out.sample = 20) 

f.GJRGn.palm.out.20 <- ugarchforecast(GJRGn.palm.out.20, n.ahead = 20)

vol.GJRGn.palm.out.20 <- sigma(f.GJRGn.palm.out.20)
vol.GJRGn.palm.out.20 <- ts(vol.GJRGn.palm.out.20)
vol.GJRGn.palm.out.sq.20 <- vol.GJRGn.palm.out.20^2

accuracy(f=vol.GJRGn.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-sstd
spec3.2
GJRGs.palm.out.20 <- ugarchfit(log.palm, spec = spec3.2, solver = "gosolnp", out.sample = 20) 

f.GJRGs.palm.out.20 <- ugarchforecast(GJRGs.palm.out.20, n.ahead = 20)

vol.GJRGs.palm.out.20 <- sigma(f.GJRGs.palm.out.20)
vol.GJRGs.palm.out.20 <- ts(vol.GJRGs.palm.out.20)
vol.GJRGs.palm.out.sq.20 <- vol.GJRGs.palm.out.20^2

accuracy(f=vol.GJRGs.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)

# GJR-GARCH(1,1)-std
spec3.3
GJRGt.palm.out.20 <- ugarchfit(log.palm, spec = spec3.3, solver = "gosolnp", out.sample = 20) 

f.GJRGt.palm.out.20 <- ugarchforecast(GJRGt.palm.out.20, n.ahead = 20)

vol.GJRGt.palm.out.20 <- sigma(f.GJRGt.palm.out.20)
vol.GJRGt.palm.out.20 <- ts(vol.GJRGt.palm.out.20)
vol.GJRGt.palm.out.sq.20 <- vol.GJRGt.palm.out.20^2

accuracy(f=vol.GJRGt.palm.out.sq.20, x=act.ret.palm.20, test=NULL, d=NULL, D=NULL)


### <rap>
## 1-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.rap.out.1 <- ugarchfit(log.rap, spec = spec1, solver = "gosolnp", out.sample = 1) 
f.Gn.rap.out.1 <- ugarchforecast(Gn.rap.out.1, n.ahead = 1)

vol.Gn.rap.out.1 <- sigma(f.Gn.rap.out.1)
vol.Gn.rap.out.1 <- ts(vol.Gn.rap.out.1)
vol.Gn.rap.out.sq.1 <- vol.Gn.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.Gn.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.rap.out.1 <- ugarchfit(log.rap, spec = spec1.2, solver = "gosolnp", out.sample = 1) 

f.Gs.rap.out.1 <- ugarchforecast(Gs.rap.out.1, n.ahead = 1)

vol.Gs.rap.out.1 <- sigma(f.Gs.rap.out.1)
vol.Gs.rap.out.1 <- ts(vol.Gs.rap.out.1)
vol.Gs.rap.out.sq.1 <- vol.Gs.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.Gs.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.rap.out.1 <- ugarchfit(log.rap, spec = spec1.3, solver = "gosolnp", out.sample = 1) 

f.Gt.rap.out.1 <- ugarchforecast(Gt.rap.out.1, n.ahead = 1)

vol.Gt.rap.out.1 <- sigma(f.Gt.rap.out.1)
vol.Gt.rap.out.1 <- ts(vol.Gt.rap.out.1)
vol.Gt.rap.out.sq.1 <- vol.Gt.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.Gt.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.rap.out.1 <- ugarchfit(log.rap, spec = spec2.2, solver = "gosolnp", out.sample = 1) 

f.EGs.rap.out.1 <- ugarchforecast(EGs.rap.out.1, n.ahead = 1)

vol.EGs.rap.out.1 <- sigma(f.EGs.rap.out.1)
vol.EGs.rap.out.1 <- ts(vol.EGs.rap.out.1)
vol.EGs.rap.out.sq.1 <- vol.EGs.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.EGs.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.rap.out.1 <- ugarchfit(log.rap, spec = spec2.3, solver = "gosolnp", out.sample = 1) 

f.EGt.rap.out.1 <- ugarchforecast(EGt.rap.out.1, n.ahead = 1)

vol.EGt.rap.out.1 <- sigma(f.EGt.rap.out.1)
vol.EGt.rap.out.1 <- ts(vol.EGt.rap.out.1)
vol.EGt.rap.out.sq.1 <- vol.EGt.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.EGt.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-norm
spec3
GJRGn.rap.out.1 <- ugarchfit(log.rap, spec = spec3, solver = "gosolnp", out.sample = 1) 
f.GJRGn.rap.out.1 <- ugarchforecast(GJRGn.rap.out.1, n.ahead = 1)

vol.GJRGn.rap.out.1 <- sigma(f.GJRGn.rap.out.1)
vol.GJRGn.rap.out.1 <- ts(vol.GJRGn.rap.out.1)
vol.GJRGn.rap.out.sq.1 <- vol.GJRGn.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGn.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-sstd
spec3.2 
GJRGs.rap.out.1 <- ugarchfit(log.rap, spec = spec3.2, solver = "gosolnp", out.sample = 1) 

f.GJRGs.rap.out.1 <- ugarchforecast(GJRGs.rap.out.1, n.ahead = 1)

vol.GJRGs.rap.out.1 <- sigma(f.GJRGs.rap.out.1)
vol.GJRGs.rap.out.1 <- ts(vol.GJRGs.rap.out.1)
vol.GJRGs.rap.out.sq.1 <- vol.GJRGs.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGs.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-std
spec3.3 
GJRGt.rap.out.1 <- ugarchfit(log.rap, spec = spec3.3, solver = "gosolnp", out.sample = 1) 

f.GJRGt.rap.out.1 <- ugarchforecast(GJRGt.rap.out.1, n.ahead = 1)

vol.GJRGt.rap.out.1 <- sigma(f.GJRGt.rap.out.1)
vol.GJRGt.rap.out.1 <- ts(vol.GJRGt.rap.out.1)
vol.GJRGt.rap.out.sq.1 <- vol.GJRGt.rap.out.1^2

act.ret.rap <- (tail(log.rap,1))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGt.rap.out.sq.1, x=act.ret.rap, test=NULL, d=NULL, D=NULL)


## 5-step ahead forecasts 

# GARCH(1,1)-norm
Gn.rap.out.5 <- ugarchfit(log.rap, spec = spec1, solver = "gosolnp", out.sample = 5) 
f.Gn.rap.out.5 <- ugarchforecast(Gn.rap.out.5, n.ahead = 5)

vol.Gn.rap.out.5 <- sigma(f.Gn.rap.out.5)
vol.Gn.rap.out.5 <- ts(vol.Gn.rap.out.5)
vol.Gn.rap.out.sq.5 <- vol.Gn.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.Gn.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
Gs.rap.out.5 <- ugarchfit(log.rap, spec = spec1.2, solver = "gosolnp", out.sample = 5) 

f.Gs.rap.out.5 <- ugarchforecast(Gs.rap.out.5, n.ahead = 5)

vol.Gs.rap.out.5 <- sigma(f.Gs.rap.out.5)
vol.Gs.rap.out.5 <- ts(vol.Gs.rap.out.5)
vol.Gs.rap.out.sq.5 <- vol.Gs.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.Gs.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
Gt.rap.out.5 <- ugarchfit(log.rap, spec = spec1.3, solver = "gosolnp", out.sample = 5) 

f.Gt.rap.out.5 <- ugarchforecast(Gt.rap.out.5, n.ahead = 5)

vol.Gt.rap.out.5 <- sigma(f.Gt.rap.out.5)
vol.Gt.rap.out.5 <- ts(vol.Gt.rap.out.5)
vol.Gt.rap.out.sq.5 <- vol.Gt.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.Gt.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.rap.out.5 <- ugarchfit(log.rap, spec = spec2.2, solver = "gosolnp", out.sample = 5) 

f.EGs.rap.out.5 <- ugarchforecast(EGs.rap.out.5, n.ahead = 5)

vol.EGs.rap.out.5 <- sigma(f.EGs.rap.out.5)
vol.EGs.rap.out.5 <- ts(vol.EGs.rap.out.5)
vol.EGs.rap.out.sq.5 <- vol.EGs.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.EGs.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.rap.out.5 <- ugarchfit(log.rap, spec = spec2.3, solver = "gosolnp", out.sample = 5) 

f.EGt.rap.out.5 <- ugarchforecast(EGt.rap.out.5, n.ahead = 5)

vol.EGt.rap.out.5 <- sigma(f.EGt.rap.out.5)
vol.EGt.rap.out.5 <- ts(vol.EGt.rap.out.5)
vol.EGt.rap.out.sq.5 <- vol.EGt.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.EGt.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-norm
spec3
GJRGn.rap.out.5 <- ugarchfit(log.rap, spec = spec3, solver = "gosolnp", out.sample = 5) 
f.GJRGn.rap.out.5 <- ugarchforecast(GJRGn.rap.out.5, n.ahead = 5)

vol.GJRGn.rap.out.5 <- sigma(f.GJRGn.rap.out.5)
vol.GJRGn.rap.out.5 <- ts(vol.GJRGn.rap.out.5)
vol.GJRGn.rap.out.sq.5 <- vol.GJRGn.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGn.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-sstd
spec3.2 
GJRGs.rap.out.5 <- ugarchfit(log.rap, spec = spec3.2, solver = "gosolnp", out.sample = 5) 

f.GJRGs.rap.out.5 <- ugarchforecast(GJRGs.rap.out.5, n.ahead = 5)

vol.GJRGs.rap.out.5 <- sigma(f.GJRGs.rap.out.5)
vol.GJRGs.rap.out.5 <- ts(vol.GJRGs.rap.out.5)
vol.GJRGs.rap.out.sq.5 <- vol.GJRGs.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGs.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-std
spec3.3 
GJRGt.rap.out.5 <- ugarchfit(log.rap, spec = spec3.3, solver = "gosolnp", out.sample = 5) 

f.GJRGt.rap.out.5 <- ugarchforecast(GJRGt.rap.out.5, n.ahead = 5)

vol.GJRGt.rap.out.5 <- sigma(f.GJRGt.rap.out.5)
vol.GJRGt.rap.out.5 <- ts(vol.GJRGt.rap.out.5)
vol.GJRGt.rap.out.sq.5 <- vol.GJRGt.rap.out.5^2

act.ret.rap <- (tail(log.rap,5))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGt.rap.out.sq.5, x=act.ret.rap, test=NULL, d=NULL, D=NULL)


## 20-step ahead forecasts 

# GARCH(1,1)-norm
Gn.rap.out.20 <- ugarchfit(log.rap, spec = spec1, solver = "gosolnp", out.sample = 20) 
f.Gn.rap.out.20 <- ugarchforecast(Gn.rap.out.20, n.ahead = 20)

vol.Gn.rap.out.20 <- sigma(f.Gn.rap.out.20)
vol.Gn.rap.out.20 <- ts(vol.Gn.rap.out.20)
vol.Gn.rap.out.sq.20 <- vol.Gn.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.Gn.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
Gs.rap.out.20 <- ugarchfit(log.rap, spec = spec1.2, solver = "gosolnp", out.sample = 20) 

f.Gs.rap.out.20 <- ugarchforecast(Gs.rap.out.20, n.ahead = 20)

vol.Gs.rap.out.20 <- sigma(f.Gs.rap.out.20)
vol.Gs.rap.out.20 <- ts(vol.Gs.rap.out.20)
vol.Gs.rap.out.sq.20 <- vol.Gs.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.Gs.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
Gt.rap.out.20 <- ugarchfit(log.rap, spec = spec1.3, solver = "gosolnp", out.sample = 20) 

f.Gt.rap.out.20 <- ugarchforecast(Gt.rap.out.20, n.ahead = 20)

vol.Gt.rap.out.20 <- sigma(f.Gt.rap.out.20)
vol.Gt.rap.out.20 <- ts(vol.Gt.rap.out.20)
vol.Gt.rap.out.sq.20 <- vol.Gt.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.Gt.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-sstd
spec2.2 
EGs.rap.out.20 <- ugarchfit(log.rap, spec = spec2.2, solver = "gosolnp", out.sample = 20) 

f.EGs.rap.out.20 <- ugarchforecast(EGs.rap.out.20, n.ahead = 20)

vol.EGs.rap.out.20 <- sigma(f.EGs.rap.out.20)
vol.EGs.rap.out.20 <- ts(vol.EGs.rap.out.20)
vol.EGs.rap.out.sq.20 <- vol.EGs.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.EGs.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# EGARCH(1,1)-std
spec2.3 
EGt.rap.out.20 <- ugarchfit(log.rap, spec = spec2.3, solver = "gosolnp", out.sample = 20) 

f.EGt.rap.out.20 <- ugarchforecast(EGt.rap.out.20, n.ahead = 20)

vol.EGt.rap.out.20 <- sigma(f.EGt.rap.out.20)
vol.EGt.rap.out.20 <- ts(vol.EGt.rap.out.20)
vol.EGt.rap.out.sq.20 <- vol.EGt.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.EGt.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-norm
spec3
GJRGn.rap.out.20 <- ugarchfit(log.rap, spec = spec3, solver = "gosolnp", out.sample = 20) 
f.GJRGn.rap.out.20 <- ugarchforecast(GJRGn.rap.out.20, n.ahead = 20)

vol.GJRGn.rap.out.20 <- sigma(f.GJRGn.rap.out.20)
vol.GJRGn.rap.out.20 <- ts(vol.GJRGn.rap.out.20)
vol.GJRGn.rap.out.sq.20 <- vol.GJRGn.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGn.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-sstd
spec3.2 
GJRGs.rap.out.20 <- ugarchfit(log.rap, spec = spec3.2, solver = "gosolnp", out.sample = 20) 

f.GJRGs.rap.out.20 <- ugarchforecast(GJRGs.rap.out.20, n.ahead = 20)

vol.GJRGs.rap.out.20 <- sigma(f.GJRGs.rap.out.20)
vol.GJRGs.rap.out.20 <- ts(vol.GJRGs.rap.out.20)
vol.GJRGs.rap.out.sq.20 <- vol.GJRGs.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGs.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)

# GJRGARCH(1,1)-std
spec3.3 
GJRGt.rap.out.20 <- ugarchfit(log.rap, spec = spec3.3, solver = "gosolnp", out.sample = 20) 

f.GJRGt.rap.out.20 <- ugarchforecast(GJRGt.rap.out.20, n.ahead = 20)

vol.GJRGt.rap.out.20 <- sigma(f.GJRGt.rap.out.20)
vol.GJRGt.rap.out.20 <- ts(vol.GJRGt.rap.out.20)
vol.GJRGt.rap.out.sq.20 <- vol.GJRGt.rap.out.20^2

act.ret.rap <- (tail(log.rap,20))^2 ## squared actual returns of rap 

accuracy(f=vol.GJRGt.rap.out.sq.20, x=act.ret.rap, test=NULL, d=NULL, D=NULL)


### <soy>
## 1-step ahead forecasts 

# GARCH(1,1)-norm
spec1 
Gn.soy.out.1 <- ugarchfit(log.soy, spec = spec1, solver = "gosolnp", out.sample = 1) 
f.Gn.soy.out.1 <- ugarchforecast(Gn.soy.out.1, n.ahead = 1)

vol.Gn.soy.out.1 <- sigma(f.Gn.soy.out.1)
vol.Gn.soy.out.1 <- ts(vol.Gn.soy.out.1)
vol.Gn.soy.out.sq.1 <- vol.Gn.soy.out.1^2

act.ret.soy <- (tail(log.soy,1))^2 ## squared actual returns of soy 

accuracy(f=vol.Gn.soy.out.sq.1, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.soy.out.1 <- ugarchfit(log.soy, spec = spec1.2, solver = "gosolnp", out.sample = 1) 

f.Gs.soy.out.1 <- ugarchforecast(Gs.soy.out.1, n.ahead = 1)

vol.Gs.soy.out.1 <- sigma(f.Gs.soy.out.1)
vol.Gs.soy.out.1 <- ts(vol.Gs.soy.out.1)
vol.Gs.soy.out.sq.1 <- vol.Gs.soy.out.1^2

act.ret.soy <- (tail(log.soy,1))^2 ## squared actual returns of soy 

accuracy(f=vol.Gs.soy.out.sq.1, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.soy.out.1 <- ugarchfit(log.soy, spec = spec1.3, solver = "gosolnp", out.sample = 1) 

f.Gt.soy.out.1 <- ugarchforecast(Gt.soy.out.1, n.ahead = 1)

vol.Gt.soy.out.1 <- sigma(f.Gt.soy.out.1)
vol.Gt.soy.out.1 <- ts(vol.Gt.soy.out.1)
vol.Gt.soy.out.sq.1 <- vol.Gt.soy.out.1^2

act.ret.soy <- (tail(log.soy,1))^2 ## squared actual returns of soy 

accuracy(f=vol.Gt.soy.out.sq.1, x=act.ret.soy, test=NULL, d=NULL, D=NULL)


## 5-step ahead forecasts 

# GARCH(1,1)-norm
Gn.soy.out.5 <- ugarchfit(log.soy, spec = spec1, solver = "gosolnp", out.sample = 5) 
f.Gn.soy.out.5 <- ugarchforecast(Gn.soy.out.5, n.ahead = 5)

vol.Gn.soy.out.5 <- sigma(f.Gn.soy.out.5)
vol.Gn.soy.out.5 <- ts(vol.Gn.soy.out.5)
vol.Gn.soy.out.sq.5 <- vol.Gn.soy.out.5^2

act.ret.soy <- (tail(log.soy,5))^2 ## squared actual returns of soy 

accuracy(f=vol.Gn.soy.out.sq.5, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.soy.out.5 <- ugarchfit(log.soy, spec = spec1.2, solver = "gosolnp", out.sample = 5) 

f.Gs.soy.out.5 <- ugarchforecast(Gs.soy.out.5, n.ahead = 5)

vol.Gs.soy.out.5 <- sigma(f.Gs.soy.out.5)
vol.Gs.soy.out.5 <- ts(vol.Gs.soy.out.5)
vol.Gs.soy.out.sq.5 <- vol.Gs.soy.out.5^2

act.ret.soy <- (tail(log.soy,5))^2 ## squared actual returns of soy 

accuracy(f=vol.Gs.soy.out.sq.5, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.soy.out.5 <- ugarchfit(log.soy, spec = spec1.3, solver = "gosolnp", out.sample = 5) 

f.Gt.soy.out.5 <- ugarchforecast(Gt.soy.out.5, n.ahead = 5)

vol.Gt.soy.out.5 <- sigma(f.Gt.soy.out.5)
vol.Gt.soy.out.5 <- ts(vol.Gt.soy.out.5)
vol.Gt.soy.out.sq.5 <- vol.Gt.soy.out.5^2

act.ret.soy <- (tail(log.soy,5))^2 ## squared actual returns of soy 

accuracy(f=vol.Gt.soy.out.sq.5, x=act.ret.soy, test=NULL, d=NULL, D=NULL)


## 20-step ahead forecasts 

# GARCH(1,1)-norm
Gn.soy.out.20 <- ugarchfit(log.soy, spec = spec1, solver = "gosolnp", out.sample = 20) 
f.Gn.soy.out.20 <- ugarchforecast(Gn.soy.out.20, n.ahead = 20)

vol.Gn.soy.out.20 <- sigma(f.Gn.soy.out.20)
vol.Gn.soy.out.20 <- ts(vol.Gn.soy.out.20)
vol.Gn.soy.out.sq.20 <- vol.Gn.soy.out.20^2

act.ret.soy <- (tail(log.soy,20))^2 ## squared actual returns of soy 

accuracy(f=vol.Gn.soy.out.sq.20, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-sstd
spec1.2 
Gs.soy.out.20 <- ugarchfit(log.soy, spec = spec1.2, solver = "gosolnp", out.sample = 20) 

f.Gs.soy.out.20 <- ugarchforecast(Gs.soy.out.20, n.ahead = 20)

vol.Gs.soy.out.20 <- sigma(f.Gs.soy.out.20)
vol.Gs.soy.out.20 <- ts(vol.Gs.soy.out.20)
vol.Gs.soy.out.sq.20 <- vol.Gs.soy.out.20^2

act.ret.soy <- (tail(log.soy,20))^2 ## squared actual returns of soy 

accuracy(f=vol.Gs.soy.out.sq.20, x=act.ret.soy, test=NULL, d=NULL, D=NULL)

# GARCH(1,1)-std
spec1.3 
Gt.soy.out.20 <- ugarchfit(log.soy, spec = spec1.3, solver = "gosolnp", out.sample = 20) 

f.Gt.soy.out.20 <- ugarchforecast(Gt.soy.out.20, n.ahead = 20)

vol.Gt.soy.out.20 <- sigma(f.Gt.soy.out.20)
vol.Gt.soy.out.20 <- ts(vol.Gt.soy.out.20)
vol.Gt.soy.out.sq.20 <- vol.Gt.soy.out.20^2

act.ret.soy <- (tail(log.soy,20))^2 ## squared actual returns of soy 

accuracy(f=vol.Gt.soy.out.sq.20, x=act.ret.soy, test=NULL, d=NULL, D=NULL)
