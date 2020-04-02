library(ggplot2)
library(mvtnorm)
library(gridExtra)
library(forecast)

m <- 4
n <- 4

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
  X <-matrix(rep(sample(1:m,n),numSim),numSim,n,byrow = T)
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

# ENLEVER BURN-IN ET TRAITER AUTO_CORR

modif_metro <- function(x){
  x_temp <- x[1000:dim(x)[1],]
  resultat <- acf(x_temp,plot=F)
  indice <- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
  x_temp_acf <- x_temp[seq(1,dim(x_temp)[1],indice),]
  res <- vector("list",dim(x_temp_acf)[1])
  for(i in 1:dim(x_temp_acf)[1]){
    val <- c()
    for(j in 1:dim(x_temp_acf)[2]){
      val <- c(val,x_temp_acf[i,j])
    }
    res[[i]] <- val
  }
  res
}

# TESTS
nSim=10000
x_etoiles = sample(1:m,n)
lambda=c(0.001,0.01,0.1)
out <- pi_density_MCMC(nSim,lambda,x_etoiles)

# PLOT ACF / TRACE

plotTrace<-lapply(lambda,function(lambda){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles);
  ggplot(as.data.frame(out))+
    geom_line(aes(x = (1:nSim),y=out[,1]))+
    labs(title=paste('trace plot for X1\n', 'lambda=',lambda),x='',y='')
}
)
result_acf <- acf(out[,1])

plotACF<-lapply(lambda,function(lambda){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles);
  ggAcf(out[,1])+
    labs(title=paste('ACF for X1\n','lambda=',lambda))
}
)

grid.arrange(grobs=plotTrace)
grid.arrange(grobs=plotACF)

test <- modif_metro(out)
test
