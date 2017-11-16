##############################
##############################
##############################
set.seed(1)
p = 3
n = 100
X = matrix(rnorm(n*p),nrow=n)
b = 1:p
Y = X%*%b + rnorm(n)


#Stochastic Gradient Descent
miniBatchParm = n/2

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
bHat          = rnorm(p)
nIter         = 1000
learnRate     = .01

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

plot(bHatIter[,1:2],col=rainbow(nIter),xlab='beta1',ylab='beta2')
points(bHat_LS[1],bHat_LS[2],col='black',pch=2)

require(scatterplot3d)
out.plot = scatterplot3d(bHatIter[,1:3],color=rainbow(nIter),
                         xlab='beta1',ylab='beta2',zlab='beta3')
                        
out.plot$points3d(bHat_LS[1],bHat_LS[2],bHat_LS[3],col='black',pch=2)
