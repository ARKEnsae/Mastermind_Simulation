library(ggplot2)
library(mvtnorm)
library(gridExtra)
library(forecast)

x1 <- matrix(c(1,2,3,4),nrow= 1, ncol = 4)
x2 <- matrix(c(1,3,2,4),nrow= 1, ncol = 4)

# DEFINITION DE LA DENSITE PI(X°)
pi_density <- function(x,lambda,x_etoiles){
  return(exp(-lambda*sum(x != x_etoiles)))
}

# FONCTION INVERSE DEUX ELEMENTS
inverse_deux_elements <- function(X){
  indices = sample(1:length(X),2)
  temp = X[indices[1]]
  X[indices[1]] <- X[indices[2]]
  X[indices[2]] <- temp
  return(X)
}

# METROPOLIS HASTINGS
pi_density_MCMC <- function(numSim, lambda, x_etoiles){
  X <-matrix(rep(x_etoiles,numSim),numSim,length(x_etoiles),byrow = T)
  for (t in (1:(numSim-1))){
    Xprop=inverse_deux_elements(X[t,])
    
    if(runif(1) < min(1,pi_density(Xprop,lambda,x_etoiles)/pi_density(X[t,],lambda,x_etoiles))){
      X[t+1,]=Xprop
    }
    else{
      X[t+1,]=X[t,]
    }
  }
  return(X)
}

# TESTS
nSim=5000
x_etoiles = x1 # 1 2 3 4
lambda=c(0.001,0.1,0.5,1)
out <- pi_density_MCMC(nSim,lambda,x_etoiles)

# PLOT ACF / TRACE

plotTrace<-lapply(lambda,function(lambda){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles);
  ggplot(as.data.frame(out))+
    geom_line(aes(x = (1:nSim),y=out[,1]))+
    labs(title=paste('trace plot for X1\n', 'lambda=',lambda),x='',y='')
}
)
out[,1]

plotACF<-lapply(lambda,function(lambda){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles);
  ggAcf(out[,1])+
    labs(title=paste('ACF for X1\n','lambda=',lambda))
}
)

grid.arrange(grobs=plotTrace)
grid.arrange(grobs=plotACF)
