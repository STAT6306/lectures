#monte carlo
n = 100
sampleF = function(distn,n,size=10,prob=0.5,lambda=5){
  if(distn == 'normal'){
    return(rnorm(n))
  }
  if(distn == 'bernoulli'){
    return(rbinom(n,size=size,prob=prob))
  }
  if(distn == 'poisson'){
    return(rpois(n,lambda=lambda))
  }
}
distn  = 'poisson'
lambda = 50
mu     = lambda
sd     = sqrt(lambda)
Y      = sampleF(distn,n,lambda=lambda)

plot(density(Y)$x,density(Y)$y,
     type='l',col='red',
     ylim=c(0,dnorm(0,sd=sd/sqrt(n))),
     xlab='Y',ylab='Density')
abline(v=mean(Y),col='red')
yGrid = seq(min(Y),max(Y),length=1000)


nSamples = 1000
YmeanVec = rep(0,nSamples)
for(iter in 1:nSamples){
  Y = sampleF(distn,n,lambda=lambda)
  YmeanVec[iter] = mean(Y)
}

lines(density(YmeanVec)$x,density(YmeanVec)$y,
      col='blue',lty=2)
abline(v=mean(YmeanVec),col='blue')
lines(yGrid,dnorm(yGrid,mean=mu,sd=sd/sqrt(n)),col='black',lty=2)



###
# Some extras: ggplot2 is a much richer/expressive plotting 
#              implementation.  We will be using it more along with
#              other parts of the "tidyverse" semester/ in data science 2
###
if(!require(ggplot2)){install.packages('ggplot2');require(ggplot2)}

ggplot()+
  geom_line(data = data.frame(randomVariable = density(YmeanVec)$x,
                              density        = density(YmeanVec)$y),
            aes(randomVariable, density),
            colour = 'red') + 
  geom_line(data = data.frame(randomVariable = density(Y)$x,
                              density        = density(Y)$y),
            aes(randomVariable, density),
            colour = 'blue') + 
  geom_line(data = data.frame(randomVariable = yGrid,
                              density        = dnorm(yGrid,mean=mu,sd=sd/sqrt(n))),
            aes(randomVariable, density),
            colour = 'black') 
