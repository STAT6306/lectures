load("hiv.rda")

X = hiv.train$x
Y = hiv.train$y
X_0 = hiv.test$x
Y_0 = hiv.test$y

(n = nrow(X))
(p = ncol(X))
b = rep(0,p)
b[1:3] = 100
set.seed(2)
X = matrix(rbinom(n*p,1,prob=.2),nrow=n,ncol=p)#Try prob= 0.001 to generate coef issue
table(X)
Y = X %*% b + rnorm(n)

require(leaps)
outForward      = regsubsets(x=X,y=Y,nvmax=p,method='forward')

sumForward      = summary(outForward)
model.forward   = sumForward$which[which.min(sumForward$bic),]
S.forward       = model.forward[-1]
which(S.forward)#selected model
lm.forward      = lm(Y~X[,S.forward])
betaHat.forward = coef(lm.forward)
betaHat.forward

#or, using the coef. regsubsets function
betaHat.forward = coef(outForward,id=which.min(sumForward$bic))
betaHat.forward

Yfor_coef = X_0[,S.forward]%*%betaHat.forward[-1] + betaHat.forward[1]
Yfor_regsub = X_0[,S.forward]%*%betaHat.forward[-1] + betaHat.forward[1]
mean((Yfor_regsub - Y_0)**2)
mean((Yfor_coef - Y_0)**2)

## Using Step
X.df = data.frame(X)
upper = lm(Y~.,data=X.df)
lower = lm(Y~1,data=X.df)
lm.a = step(lower,scope=list(lower=lower,upper=upper),direction='forward',trace=FALSE,k=log(n))
summary(lm.a)
