n = 30
p = 2
X = rep(0,n*p)
X = matrix(X,nrow=n,ncol=p)
set.seed(15)
x_1   = rnorm(n,0,1)
x_2   = rnorm(n,0,.2)

X[,1] = x_1
X[,2] = x_2
lims = c(-2, 2)

### Aligned example
plot(X,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))

plot(X,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))

plot(X,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))
points(rep(0,n),X[,2],col='blue')

plot(X,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))
points(X[,1],rep(0,n),col='red')
points(rep(0,n),X[,2],col='blue')
legend(x = 1,col=c('black','red','blue'),pch=1,legend=c('Data','Only x_1','Only x_2'))

### Unaligned example
#rotate data
theta = -pi/5
rot_mat   = matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2,byrow=T)
X_rot     = X%*%rot_mat

plot(X_rot,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))

plot(X_rot,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))
points(X_rot[,1],rep(0,n),col='red')

plot(X_rot,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))
points(rep(0,n),X_rot[,2],col='blue')

plot(X_rot,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]))
points(X_rot[,1],rep(0,n),col='red')
points(rep(0,n),X_rot[,2],col='blue')
legend(x = 1.25,y=-1,col=c('black','red','blue'),pch=1,legend=c('Data','Only x_1','Only x_2'))

Xscale   = scale(X_rot)
svd.out  = svd(X_rot)#No mean subtraction as data is already centered
V        = svd.out$v
PCcoords = svd.out$u %*% diag(svd.out$d)
PCvecs1  = PCcoords[,1] %*% t(V[,1])
PCvecs2  = PCcoords[,2] %*% t(V[,2])
lims = c(min(X_rot), max(X_rot))

plot(X_rot,xlim=lims,ylim=lims,xlab=expression(x[1]),ylab=expression(x[2]),asp=1)
points(PCvecs1,col='red')
points(PCvecs2,col='blue')
legend(x = .5,y=-1.5,col=c('black','red','blue'),pch=1,legend=c('Data','PC1 only','PC2 only'))

library(rgl)
#Example: PCR aligned with axis
n = 30
p = 2
X = rep(0,n*p)
X = matrix(X,nrow=n,ncol=p)
X[,1] = rnorm(n,0,1)
X[,2] = rnorm(n,0,.3)
Y     = X%*% c(1,1) + rnorm(n,0,.05)
allData = as.data.frame(cbind(X,Y))
names(allData) = c(expression(x[1]),expression(x[2]),'Y')
lims = c(min(allData), max(allData))
plot3d(allData, type = "s",xlim = lims, ylim = lims, zlim = lims)
allDataNoX2 = as.data.frame(cbind(X[,1],rep(0,n),Y))
names(allDataNoX2) = c(expression(x[1]),expression(x[2]),'Y')
plot3d(allDataNoX2, type = "s",xlim = lims, ylim = lims,zlim = lims,col='red',add=TRUE)

#Example: PCR not aligned with axis
n = 30
p = 2
X = rep(0,n*p)
X = matrix(X,nrow=n,ncol=p)
X[,1] = rnorm(n,0,1)
X[,2] = 1*X[,1] + rnorm(n,0,.3)
Y     = X%*% c(1,1) + rnorm(n,0,.05)
allData = as.data.frame(cbind(X,Y))
names(allData) = c(expression(x[1]),expression(x[2]),'Y')
lims = c(min(allData), max(allData))
plot3d(allData, type = "s",xlim = lims, ylim = lims, zlim = lims)
allDataNoX2 = as.data.frame(cbind(X[,1],rep(0,n),Y))
names(allDataNoX2) = c(expression(x[1]),expression(x[2]),'Y')
plot3d(allDataNoX2, type = "s",xlim = lims, ylim = lims,zlim = lims,col='red',add=TRUE)

PCA.out = prcomp(X,center=T,scale=T)






