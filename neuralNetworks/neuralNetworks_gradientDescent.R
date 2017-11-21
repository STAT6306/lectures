##############################
##############################
##############################
set.seed(1)
p = 2
n = 100
X = matrix(rnorm(n*p),nrow=n)
b = 1:p
Y = X%*%b + rnorm(n)


#Stochastic Gradient Descent
miniBatchParm = n

fF = function(X,b){
  return( X %*% b)
}
ellF = function(Y,f){
  return((Y - f)**2)
}
rF = function(ell){
  return( mean( ell ))
}

#Step 0
bHat          = c(-2,-4)#rnorm(p)
nIter         = 100
learnRate     = .1

bHatIter      = matrix(0,nrow=nIter,ncol=p)
#Step 1
for(iter in 1:nIter){
  batch = sample(1:n,miniBatchParm,replace=FALSE)
  for(j in 1:p){
    #Step 2a:Forward
    f  = fF(X[batch,],bHat)
    ell = ellF(Y[batch],f)
    r   = rF(ell)
    #Step 2b:Backward
    dell_df = -2*(Y[batch] - f)
    df_dbj  = X[batch,j]
    dR_dbj   = rF( dell_df*df_dbj )
    #Step 3: Update
    bHat[j]  = bHat[j] - learnRate * dR_dbj
  }
  bHatIter[iter,] = bHat
}


bHat_LS = lm(Y~X-1)$coef
print(bHat_LS)
print(bHatIter[nIter,])

plotLoss = TRUE
if(p == 2){
plot(bHatIter[,1:2],col=rainbow(nIter),xlab='beta1',ylab='beta2')
points(bHat_LS[1],bHat_LS[2],col='black',pch=2)
}

if(plotLoss & p == 2){
  library(plot3D)
  rssF = function(b){
    return(mean( (Y - X %*% b)**2 )+5)
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
  pdf('_perspPlot.pdf')
  persp3D(x=seq(lower,upper,length=nGrid),y=seq(lower,upper,length=nGrid),z=z,
          theta=50,phi=30,plot=TRUE,
          xlab='beta1',ylab='beta2',zlab='RSS',
          bty='f',alpha=.35,resfac = 1,ticktype = "detailed",zlim=c(0,20),
          contour = list(z = rssF(bHat_LS),nlevels=15))
  scatter3D(bHatIter[,1], bHatIter[,2], rep(0,nIter), add = TRUE, colvar = nIter:1, 
           colkey = FALSE,pch='o')
  points3D(bHat_LS[1], bHat_LS[2], 0, add = TRUE, colvar = 0, 
           colkey = FALSE, pch = 'X', cex = 1)
dev.off()
}

if(p >= 3){
require(scatterplot3d)
out.plot = scatterplot3d(bHatIter[,1:3],color=rainbow(nIter),
                         xlab='beta1',ylab='beta2',zlab='beta3')
out.plot$points3d(bHat_LS[1],bHat_LS[2],bHat_LS[3],col='black',pch=2)
}
