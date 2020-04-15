library(ggplot2)
library(gridExtra)
library(forecast)

# Définition de la densité PI(X)
pi_density <- function(x,lambda,x_etoiles){
  return(exp(-lambda*sum(x != x_etoiles)))
}

# Fonction qui permet d'inverser deux éléments d'une permutation
inverse_deux_elements <- function(X){
  indices = sample(1:length(X),2)
  temp = X[indices[1]]
  X[indices[1]] <- X[indices[2]]
  X[indices[2]] <- temp
  return(X)
}


pi_density_MCMC <- function(numSim, lambda, x_etoiles,y, m,n){
  X0 <- sample(1:m,m,replace=FALSE)
  X <-matrix(rep(X0,numSim),numSim,m,byrow = T)
  for (t in (1:(numSim-1))){
    Xprop=inverse_deux_elements(X[t,])
    if(runif(1) < min(1,pi_density(Xprop[1:n],lambda,x_etoiles)/pi_density(X[t,1:n],lambda,x_etoiles))){
      X[t+1,]=Xprop
    }
    else{
      X[t+1,]=X[t,]
    }
  }
  return(X[,1:n])
}


tracer_Trace <- function(nSim,lambda,x_etoiles,y,m,n,coord){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,y,m,n)
  ggplot(as.data.frame(out))+
    geom_line(aes(x = (1:nSim),y=out[,1]))+
    labs(title=paste('Trace plot'),x='',y='')
}

tracer_ACF <- function(nSim,lambda,x_etoiles,y,m,n,coord,burn_in){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,y,m,n)
  ggAcf(out[burn_in:dim(out)[1],coord])+
    labs(title=paste('ACF'))
}

m <- 40
n <- 30
nSim=1000+10*10*31
y <- sample(1:m,m,replace=FALSE)
x_etoiles = sample(1:m,m)
lambda=1

tracer_Trace(nSim,lambda,x_etoiles,y,m,n,2)
tracer_ACF(nSim,lambda,x_etoiles,y,m,n,2,1000)
