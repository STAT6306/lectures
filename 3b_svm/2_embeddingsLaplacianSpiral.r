library(scatterplot3d)
library(mvtnorm)

n = 100
sigma = matrix(c(1,.5,.5,.5),nrow=2,byrow=T)
X = rmvnorm(n,c(0,0),sigma)
x = X[,1]
x_order = order(x)
x = sort(x)
y = X[x_order,2]
#t = X[,3]
nCols   = 10
rainVec = rainbow(nCols)
color   = 
c(
rep(rainVec[1],n/nCols),rep(rainVec[2],n/nCols),rep(rainVec[3],n/nCols),
rep(rainVec[4],n/nCols),rep(rainVec[5],n/nCols),rep(rainVec[6],n/nCols),
rep(rainVec[7],n/nCols),rep(rainVec[8],n/nCols),rep(rainVec[9],n/nCols),
rep(rainVec[10],n/nCols))

#PCA works fine
plot(x,y,col=color,xlab='X1',ylab='X2',xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)


X = cbind(x,y)
pca.out = svd(scale(X))
pca.scores = pca.out$u %*% diag(pca.out$d)
#plot(pca.scores,col=color,xlab='X1',ylab='X2',,xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)
pca.scores.red  = cbind(sort(pca.scores[,1]),rep(0,n))
pca.scores.can  = pca.out$v %*% t(pca.scores.red)
mean.vec        = apply(X,2,mean)
sd.vec          = apply(X,2,sd)
pca.scores.plot = t( (pca.scores.can+mean.vec)*sd.vec )

#Plot first PC scores in original axes
plot(pca.scores.plot,col=color,xlab='X1',ylab='X2',xlim=c(-3,3),ylim=c(-3,3),asp=1,pch=19)

n = 100
t = seq(0,4*pi,length=n)
X = scale(cbind(3*t/2*sin(t),t/2*cos(t)),scale=FALSE)

nCols   = 10
rainVec = rainbow(nCols)
color   = 
c(
rep(rainVec[1],n/nCols),rep(rainVec[2],n/nCols),rep(rainVec[3],n/nCols),
rep(rainVec[4],n/nCols),rep(rainVec[5],n/nCols),rep(rainVec[6],n/nCols),
rep(rainVec[7],n/nCols),rep(rainVec[8],n/nCols),rep(rainVec[9],n/nCols),
rep(rainVec[10],n/nCols))
lims = c(-4.7*pi,3.5*pi)

#PCA bad
plot(X,col=color,xlab='X1',ylab='X2',xlim=lims,ylim=lims,asp=1,pch=19)


pca.out = svd(scale(X,scale=FALSE))
pca.scores = pca.out$u %*% diag(pca.out$d)
pca.scores.red  = cbind(pca.scores[,1],rep(0,n))
pca.scores.can  = pca.out$v %*% t(pca.scores.red)
pca.scores.plot = t( (pca.scores.can) ) 


plot(X,col=color,xlab='X1',ylab='X2',xlim=lims,ylim=lims,asp=1,pch=19)
points(pca.scores.plot,col=color,pch=19)


###########
# Nonlinear embeddings
#  Note: This is a method known as a "diffusion map".  The details are
#        very similar to kernel PCA, but I just wanted to show you a different
#        method.  
###########
nonLinear = function(gamma){
	Wgamma = exp(-Delta/gamma)

	P = apply(Wgamma,1,sum)
	Pinv = P^(-1)
#	R = diag(rep(1,n)) - diag(Pinv)%*% Wgamma
	R = diag(Pinv)%*% Wgamma #this works the same, with reversed evals
	eig = eigen(R)
	evecs = eig$vectors
	return(evecs)
}

Delta = as.matrix(dist(X,diag=TRUE,upper=TRUE))

gammaStar = .2
Delta = as.matrix(dist(X,diag=TRUE,upper=TRUE))
evecs = nonLinear(gammaStar)
plot(evecs[,1],rep(0,n),xlab='eigenvector 1',ylab='',pch=19,col=color)
plot(evecs[,2],rep(0,n),xlab='eigenvector 2',ylab='',pch=19,col=color)

plot(evecs[,1],evecs[,2],xlab='eigenvector 1',ylab='eigenvector 2',pch=19,col=color)

