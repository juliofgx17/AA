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
  Ymatrix = cbind(Ymatrix,Yorigins)
  
  
  
  
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
  
  
  
  dJ <- function(x,y,theta,j,lambda)
  {
    
    cost = 0
    for( i in 1:length(y) )
    {
      cost = cost + (    1 / (1+exp(-t(theta)%*%x[i,]))    -y[i])*x[i,j]
    }
    
    cost = cost/length(y)
    
    if( j != 1 ) cost = cost - (lambda*theta[j])/length(y)
  
    cost
  }
  
  
  
  GD <- function(x,y,lambda,lr=0.1)
  {
    
    temp = theta
    
    for( i in 1:100 )
    {
      print(i)
      for( j in 1:65 )
      {
        temp[j] = temp[j] - lr*dJ(x,y,theta,j,lambda)
      }
      
      
      
      theta = temp
  
    }
    
    
    theta
  }
  
  prob <- function( Xi,theta )
  {
    sigmoid(t(theta)%*%Xi)
  }
  
  
  
  
  
  ## CROSS VALIDATION
  
  
  lr = 0.1
  
  x1 = X[1:764,]
  x2 = X[765:1529,]
  x3 = X[1530:2293,]
  x4 = X[2294:3058,]
  x5 = X[3058:3823,]
  
  y1 = Ymatrix[1:764,]
  y2 = Ymatrix[765:1529,]
  y3 = Ymatrix[1530:2293,]
  y4 = Ymatrix[2294:3058,]
  y5 = Ymatrix[3058:3823,]
  
  
  
  
  for( k in 1:5 )
  {
    if( k == 1 ) lambda = 0.001
    if( k == 2 ) lambda = 0.01
    if( k == 3 ) lambda = 0.1
    if( k == 4 ) lambda = 0.5
    if( k == 5 ) lambda = 1
    
    fallos = 0
    
    for( l in 1:5 )
    {
      
      if( l == 1 )
      {
        xtest = x1
        ytest = y1
        
        x = rbind(x2,x3,x4,x5)
        y = rbind(y2,y3,y4,y5)
      
      }
      if( l == 2 )
      {
        xtest = x2
        ytest = y2
        
        
        x = rbind(x1,x3,x4,x5)
        y = rbind(y1,y3,y4,y5)
      
      }
      if( l == 3 )
      {
        xtest = x3
        ytest = y3
       
        
        x = rbind(x2,x1,x4,x5)
        y = rbind(y2,y1,y4,y5)
      
      }
      if( l == 4 )
      {
        xtest = x4
        ytest = y4
      
        x = rbind(x2,x3,x1,x5)
        y = rbind(y2,y3,y1,y5)
  
      }
      if( l == 5 )
      {
        xtest = x5
        ytest = y5
  
        
        x = rbind(x2,x3,x4,x1)
        y = rbind(y2,y3,y4,y1)
  
      }
      
      
      thetaMatrix = matrix(rep(0,650),ncol=(ncol(X)))
      
      for( i in 1:10 )
      {
        theta = rep(0,ncol(X))
        #theta = optim(par = theta,fn = J,gr = GD)
        #theta = theta$par
        theta = GD(x,y[,i],lambda)
        thetaMatrix[i,] = theta
      }
      
      
      
      
      Ypredicted = rep(0,length(ytest[,1]))
      Yprobs = rep(0,length(ytest[,1]))
      
      for( i in 1:10 )
      {
        for( j in 1:length(ytest[,i]) )
        {
          if( prob(xtest[j,],thetaMatrix[i,]) > Yprobs[j]  )
          {
            Yprobs[j] = prob(xtest[j,],thetaMatrix[i,])
            Ypredicted[j] = i-1
          }
        }
        
      }
      
      
      for( i in 1:length(Ypredicted) )
      {
        if( Ypredicted[i] != ytest[i,11] ) fallos = fallos + 1
      }
      
    }
    
    fallos = fallos/5
    
    if( k==1 ) fallos1 = fallos
    if( k==2 ) fallos2 = fallos
    if( k==3 ) fallos3 = fallos
    if( k==4 ) fallos4 = fallos
    if( k==5 ) fallos5 = fallos
    
   
    
    
    
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


