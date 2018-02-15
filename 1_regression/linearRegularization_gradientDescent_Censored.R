##############################
# Install/load packages
##############################
packages = c('plot3D','scatterplot3d')
for(package in packages){
  if(!require(package,character.only=TRUE,quietly=TRUE)){
    install.packages(package,repos='http://cran.us.r-project.org')
    require(package,character.only=TRUE,quietly=TRUE)
  }
}

##############################
##############################
##############################
set.seed(1)
p = 2
n = 100
X = matrix(rnorm(n*p),nrow=n)
b = 1:p
Y = X%*%b + rnorm(n)


fF = function(X,b){
  return( X %*% b)
}
ellF = function(Y,f){
  return(...)
}
rF = function(ell){
  return( mean( ... ))
}

#Step 0
bHatInitial   = rnorm(p)
bHat          = bHatInitial
nSweep        = 100
learnRate     = ...

bHatSweep      = matrix(0,nrow=nSweep,ncol=p)

#Step 1: Now, keep iterating over the features 1 up to p:
 
#Step 1a: Iterate some number of times (note: in practice you want to iterate until some criterion is met)
for(sweep in 1:nSweep){
  #Step 1b: Iterate from 1 up to p
  for(j in 1:p){
    #Step 2a:Forward
    f   = fF(X,bHat)
    ell = ellF(Y,f)
    r   = rF(ell)
    #Step 2b:Backward
    dell_df = ...
    df_dbj  = ...
    dR_dbj  = ...
    #Step 3: Update
    bHat[j]  = bHat[j] - learnRate * dR_dbj
  }
  bHatSweep[sweep,] = bHat
}


bHat_LS = lm(Y~X-1)$coef
print(bHat_LS)
print(bHatSweep[nSweep,])

plotLoss = TRUE
if(p == 2){
plot(bHatSweep[,1:2],col=rainbow(nSweep),xlab='beta1',ylab='beta2',
     xlim = c(-2,2),ylim=c(-4,2))
points(bHat_LS[1],bHat_LS[2],col='black',pch=17)
points(bHatInitial[1],bHatInitial[2],col='green',pch='+')
}

if(plotLoss & p == 2){
  rssF = function(b){
    return(mean( (Y - X %*% b)**2 )+1.5)
  }
  lower = -4
  upper = 4
  nGrid  = 50
  grid.out = expand.grid(seq(lower,upper,length=nGrid),seq(lower,upper,length=nGrid))
  mesh.out = mesh(seq(lower,upper,length=nGrid),seq(lower,upper,length=nGrid))
  x=mesh.out$x
  y=mesh.out$y
  z=matrix(apply(grid.out,1,rssF),nrow=nGrid)
  library(plot3D)
  pdf('linearRegularization_perspPlot.pdf')
  persp3D(x=seq(lower,upper,length=nGrid),y=seq(lower,upper,length=nGrid),z=z,
          theta=50,phi=30,plot=TRUE,
          xlab='beta1',ylab='beta2',zlab='RSS',
          bty='f',alpha=.35,resfac = 1,ticktype = "detailed",zlim=c(0,20),
          contour = list(z = rssF(bHat_LS),nlevels=15))
  scatter3D(bHatSweep[,1], bHatSweep[,2], rep(0,nSweep), add = TRUE, colvar = nSweep:1, 
           colkey = FALSE,pch='o')
  points3D(bHat_LS[1], bHat_LS[2], 0, add = TRUE, colvar = 0, 
           colkey = FALSE, pch = 'X', cex = 1)
  points3D(bHatInitial[1], bHatInitial[2], 0, add = TRUE, colvar = 0, 
           colkey = FALSE, pch = '+', cex = 1)
dev.off()
}

if(p >= 3){
out.plot = scatterplot3d(bHatSweep[,c(2,1,3)],color=rainbow(nSweep),
                         xlab='beta1',ylab='beta2',zlab='beta3')
out.plot$points3d(bHatInitial[1],bHatInitial[2],bHatInitial[3],col='green',pch='+')
out.plot$points3d(bHat_LS[1],bHat_LS[2],bHat_LS[3],col='black',pch=2)
}
