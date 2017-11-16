##############################
##############################
##############################
set.seed(1)
p = 2
n = 10
X = matrix(rnorm(n*p),nrow=10)
b = c(10,1)
Y = X%*%b + rnorm(n)


#Stochastic Gradient Descent
miniBatchParm = 5

muF = function(X,b){
  return( X %*% b)
}
ellF = function(Y,mu){
  return((Y - mu)**2)
}
rF = function(ell){
  return( mean( ell ))
}

#Step 0
bHat          = rnorm(p)
nIter         = 1000
learnRate     = .01

bHatIter      = matrix(0,nrow=nIter,ncol=2)
#Step 1
for(iter in 1:nIter){
  batch = sample(1:n,miniBatchParm,replace=FALSE)
  for(j in 1:p){
    #Step 2a:Forward
    mu  = muF(X[batch,],bHat)
    ell = ellF(Y[batch],mu)
    r   = rF(ell)
    #Step 2b:Backward
    dell_dmu = -2*(Y[batch] - mu)
    dmu_dbj  = X[batch,j]
    dR_dbj   = rF( dell_dmu*dmu_dbj )
    #Step 3: Update
    bHat[j]  = bHat[j] - learnRate * dR_dbj
  }
  bHatIter[iter,] = bHat
}


bHat_LS = lm(Y~X-1)$coef
print(bHat_LS)

plot(bHatIter,col=rainbow(nIter))
points(bHat_LS[1],bHat_LS[2],col='black',pch=2)