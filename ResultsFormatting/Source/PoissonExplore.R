#+++++++++++++++
# Front Matter +
#+++++++++++++++

##Activate Packages
#In case of count data we can use goodfit() included in vcd package
library(vcd) ## loading vcd package

#_____________________________________________________________________

shapiro.test(totalPtsDual$totalPts) # Significant therfore reject null - distribution NOT normal
qqnorm(totalPtsDual$totalPts)

hist(totalPtsDual$totalPts,main="Poisson distribution")

var.est <- var(totalPtsDual$totalPts)
lambda.est <- mean(totalPtsDual$totalPts) ## estimate of parameter lambda
(tab.os<-table(totalPtsDual$totalPts)) ## table with empirical frequencies


freq.os<-vector()
for(i in 1: length(tab.os)) freq.os[i]<-tab.os[[i]]  ## vector of emprical frequencies

freq.ex<-(dpois(0:max(totalPtsDual$totalPts),lambda=lambda.est)*200) ## vector of fitted (expected) frequencies

acc <- mean(abs(freq.os-trunc(freq.ex))) ## absolute goodness of fit index acc
acc/mean(freq.os)*100 ## relative (percent) goodness of fit index

h <- hist(totalPtsDual$totalPts ,breaks=length(tab.os))
xhist <- c(min(h$breaks),h$breaks)
yhist <- c(0,h$density,0)
xfit <- min(totalPtsDual$totalPts):max(totalPtsDual$totalPts)
yfit <- dpois(xfit,lambda=lambda.est)
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)), main="Poisson density and histogram")
lines(xfit,yfit, col="red")

#Perform the chi-square goodness of fit test 
gf <- goodfit(totalPtsDual$totalPts,type= "poisson",method= "MinChisq")
summary(gf) # Significant; Data NOT Poisson
plot(gf,main="Count data vs Poisson distribution")

fitdistr()
