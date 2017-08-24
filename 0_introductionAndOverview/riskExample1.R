n  = 10
Y  = rnorm(n)

plot(density(Y)$x,density(Y)$y,
     type='l',col='red',
     ylim=c(0,dnorm(0,sd=1/sqrt(n))),
     xlab='Y',ylab='Density')
abline(v=mean(Y),col='red')
yGrid = seq(min(Y),max(Y),length=1000)


nSamples = 1000
YmeanVec = rep(0,nSamples)
for(iter in 1:nSamples){
  Y = rnorm(n)
  YmeanVec[iter] = mean(Y)
}

lines(density(YmeanVec)$x,density(YmeanVec)$y,
      col='blue',lty=2)
abline(v=mean(YmeanVec),col='blue')
lines(yGrid,dnorm(yGrid,sd=1/sqrt(n)),col='black',lty=2)
