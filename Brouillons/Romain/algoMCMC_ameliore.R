library(ggplot2)
library(mvtnorm)
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

# Fonction pour appliquer l'algo de Metropolis Hastings
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

# Traiter le burn-in et les auto-corrélations (éventuellement à modif pour burn-in)

modif_metro <- function(x){
  x_temp <- x[1000:dim(x)[1],]
  resultat <- acf(x_temp,plot=F)
  indice_lag <<- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
  x_temp_acf <<- x_temp[seq(1,dim(x_temp)[1],indice_lag),]
  res <- vector("list",dim(x_temp_acf)[1])
  for(i in 1:dim(x_temp_acf)[1]){
    val <- c()
    for(j in 1:dim(x_temp_acf)[2]){
      val <- c(val,x_temp_acf[i,j])
    }
    res[[i]] <- val
  }
  return(res)
}

# Fonction à utiliser qui renvoie un échantillon indépendant de N X_i qui suivent la loi PI(X) pour lambda et x_etoiles
# donnés

simul_permutations <- function(N, lambda, x_etoiles, numSim = 10000){
  out <- pi_density_MCMC(numSim, lambda, x_etoiles)
  out_traite <- modif_metro(out)
  indices <- sample(1:length(out_traite),N)
  return(out_traite[indices])
}

#################
# DEMANDE ALAIN #
#################

# Fonction pour REPRENDRE l'algo de Metropolis Hastings
pi_density_MCMC_continue <- function(numSim, lambda, x_etoiles,X0){
  X <-matrix(rep(X0,numSim),numSim,n,byrow = T)
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

simul_permutations_best <- function(N,lambda, x_etoiles){
  num <- 1000 + N*100
  out <- pi_density_MCMC(num, lambda, x_etoiles)
  out_traite <- modif_metro(out)
  taille <- length(out_traite)
  while(taille<10*N){
    out <- rbind(out,pi_density_MCMC_continue(indice_lag*(10*N - taille), lambda, x_etoiles,out_traite[[taille]]))
    out_traite <- modif_metro(out)
    taille <- length(out_traite)
  }
  indices <- sample(1:length(out_traite),N)
  return(out_traite[indices])
}

#########
# TESTS #
#########

m <- 5
n <- 5
nSim=1000
x_etoiles = sample(1:m,n)
lambda=0.9
out_hamming <- simul_permutations(5*n*m,lambda,x_etoiles,nSim)
# On a une erreur car pas assez de simulations

out_hamming_best <- simul_permutations_best(5*n*m,lambda,x_etoiles)
length(out_hamming_best)
out_hamming_best

####################
# PLOT ACF / TRACE #
####################


plotACF<-lapply(lambda,function(lambda){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles);
  ggAcf(x_temp_acf[,1])+
    labs(title=paste('ACF for X1\n','lambda=',lambda))
}
)
grid.arrange(grobs=plotACF)


# plotTrace<-lapply(lambda,function(lambda){
#   out<-pi_density_MCMC(nSim,lambda,x_etoiles);
#   ggplot(as.data.frame(out))+
#     geom_line(aes(x = (1:nSim),y=out[,1]))+
#     labs(title=paste('trace plot for X1\n', 'lambda=',lambda),x='',y='')
# }
# )
# grid.arrange(grobs=plotTrace)

