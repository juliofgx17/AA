## TRAIN


digitosTrain <- read.csv("optdigits_tra.csv", header=F, sep=",") # 1. lectura zip del train

X <- as.matrix(digitosTrain[-65])
X <- cbind(rep(1,nrow(X)),X)

Y <- as.matrix(digitosTrain[,65])
Yorigins = Y


Ymatrix = NaN
for( i in 0:9 )
{
  for( j in 1:length(Y))
  {
    if( i == 0 && j == 1 )
    {
      Ymatrix = 0
      if( Y[j] == i ) Ymatrix = 1
    }
     
    else
    {
      if( Y[j] == i ) Ymatrix = c(Ymatrix,1)
      else Ymatrix = c(Ymatrix,0)
    }
  }
}

Ymatrix = matrix(Ymatrix,ncol=10,byrow=F)





sigmoid <- function(x)
{
  1/(1+exp(-x))
}


hx <- function(x,theta)
{
  1 / (1+exp(-t(theta)%*%x))
}


J <- function(theta)
{
  cost = 0
  for( i in 1:length(y))
  {
    cost = cost + y[i]*log(hx(x[i,],theta)) + (1-y[i])*log(1-hx(x[i,],theta))
  }
  cost = -cost/length(y)
  
  sumsquares = 0
  for( i in 1:length(theta) )
  {
    sumsquares = sumsquares + theta[i]*theta[i]
  }
  cost = cost + (lambda/(2*length(y)))*sumsquares
}



dJ <- function(theta,j)
{
  cost = 0
  for( i in 1:length(y) )
  {
    cost = cost + (hx(x[i,],theta)-y[i])*x[i,j]
  }
  
  cost = cost/length(y)
  
  if( j != 1 ) cost = cost - (lambda*theta[j])/length(y)
  
  cost
}



GD <- function()
{
  
  temp = theta
  
  for( i in 1:1000 )
  {
    print(i)
    for( j in 1:length(theta) )
    {
      temp[j] = temp[j] - lr*dJ(theta,j)
    }
    
    theta = temp
    
  }
  
  
  theta
}

prob <- function( Xi,theta )
{
  sigmoid(t(theta)%*%Xi)
}





x = X
lambda = 0.1
lr = 0.1
thetaMatrix = matrix(rep(0,length(c(X))),ncol=(ncol(X)))

for( i in 0:9 )
{
  y = Ymatrix[,i+1]
  theta = rep(0,ncol(X))
  #theta = optim(par = theta,fn = J,gr = GD)
  #theta = theta$par
  theta = GD()
  thetaMatrix[i+1,] = theta
  
  
  
}

Ypredicted = rep(0,length(Y))
Yprobs = rep(0,length(Y))

for( i in 1:10 )
{
  for( j in 1:length(Y) )
  {
    if( prob(X[j,],thetaMatrix[i,]) > Yprobs[j]  )
    {
      Yprobs[j] = prob(X[j,],thetaMatrix[i,])
      Ypredicted[j] = i-1
    }
  }
  
}


fallos = 0
for( i in 1:length(Y) )
{
  if( Ypredicted[i] != Yorigins[i] ) fallos = fallos + 1
}




## TEST




digitosTest <- read.csv("optdigits_tes.csv", header=F, sep=",") # 1. lectura zip del Test

X <- as.matrix(digitosTest[-65])
X <- cbind(rep(1,nrow(X)),X)

Y <- as.matrix(digitosTest[,65])
Yorigins = Y


Ymatrix = NaN
for( i in 0:9 )
{
  for( j in 1:length(Y))
  {
    if( i == 0 && j == 1 )
    {
      Ymatrix = 0
      if( Y[j] == i ) Ymatrix = 1
    }
    
    else
    {
      if( Y[j] == i ) Ymatrix = c(Ymatrix,1)
      else Ymatrix = c(Ymatrix,0)
    }
  }
}

Ymatrix = matrix(Ymatrix,ncol=10,byrow=F)

x = X

Ypredicted = rep(0,length(Y))
Yprobs = rep(0,length(Y))

for( i in 1:10 )
{
  for( j in 1:length(Y) )
  {
    if( prob(X[j,],thetaMatrix[i,]) > Yprobs[j]  )
    {
      Yprobs[j] = prob(X[j,],thetaMatrix[i,])
      Ypredicted[j] = i-1
    }
  }
  
}


fallos = 0
for( i in 1:length(Y) )
{
  if( Ypredicted[i] != Yorigins[i] ) fallos = fallos + 1
}


