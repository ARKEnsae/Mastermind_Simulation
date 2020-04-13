library(mvtnorm)
library(clue)

#source("ShinyApp/fonctions_initialisation.R", encoding = "UTF-8")

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
# On utilise le score du x*
pi_density_MCMC <- function(numSim, lambda, x_etoiles,y, m,n){
  nb <- nb_boules_noires(x_etoiles,y)
  ech1 <- sample(x_etoiles,nb)
  ech2 <- NULL
  for(i in 1:m){
    if(!(i %in% ech1)){
      ech2 <- c(ech2,i)
    }
  }
  ech2 <- sample(ech2,n-nb)
  ech <- c(ech1,ech2)
  X <-matrix(rep(ech,numSim),numSim,n,byrow = T)
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
# modif_metro <- function(x){
#   # Burn-in : commence à 1000
#   x_temp <- x[1000:dim(x)[1],]
#   resultat <- acf(x_temp,plot=F)
#   # On récupère le lag min pour être OK
#   indice <- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
#   x_temp_acf <- x_temp[seq(1,dim(x_temp)[1],indice),]
#   return(x_temp_acf)
# }

# Amélioré
modif_metro <- function(x){
  # Burn-in : commence à 1000
  x_temp <- x[1000:dim(x)[1],]
  resultat <- acf(x_temp,plot=F)
  indice_lag <- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
  x_temp_acf <- x_temp[seq(1,(dim(x_temp)[1]),indice_lag),]
  return(list(acf=x_temp_acf,indice_lag=indice_lag))
}

modif_metro_am <- function(x){
  x_temp <- x
  resultat <- acf(x_temp,plot=F)
  indice_lag <- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
  x_temp_acf <- x_temp[seq(1,(dim(x_temp)[1]),max(1,indice_lag)),]
  return(list(acf=x_temp_acf,indice_lag=indice_lag))
}

pi_density_MCMC_continue <- function(numSim, lambda, x_etoiles,X0,n){
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

# Fonction à utiliser qui renvoie un échantillon indépendant de N X_i qui suivent la loi PI(X) pour lambda et x_etoiles donnés
# Param un liste avec lambda et x_star
# simul_permutation <- function(N, param, numSim = 10000,y,m,n){
#   out <- pi_density_MCMC(numSim, param$lambda, param$x_star,y,m,n)
#   out_traite <- modif_metro(out)
#   indices <- sample.int(nrow(out_traite), size =  N, replace = FALSE)
#   return(out_traite[indices,])
# }

simul_permutation <- function(N, param,y,m,n){
  num <- 1000 + N*10
  out <- pi_density_MCMC(num, param$lambda, param$x_star,y,m,n)
  modif <- modif_metro(out)
  out_traite <- modif$acf
  indice_lag <- modif$indice_lag
  taille <- dim(out_traite)[1]
  while(taille<N){
    out <- rbind(out,pi_density_MCMC_continue(indice_lag*(N - taille), param$lambda, param$x_star,out_traite[[taille]],n))
    modif <- modif_metro_am(out)
    out_traite <- modif$acf
    indice_lag <- modif$indice_lag
    taille <- dim(out_traite)[1]
  }
  indices <- sample(1:dim(out_traite)[1],N)
  return(out_traite[indices,])
}


# Détermination du x* par l'algorithme hongrois #
creer_matriceF <- function(X_top,n,m){
  matriceF <- matrix(rep(0,n*m),nrow = n, ncol = m)
  for(i in 1:dim(X_top)[1]){
    x <- X_top[i,]
    for(i in 1:length(x)){
      matriceF[i,x[i]] <- matriceF[i,x[i]] + 1
    }
  }
  return(matriceF)
}


lancer_algorithme_hamming <- function(y, n, m, N = C * (n + 1), maxIters = 100,rho = 0.1, alpha = 0.7, poids_blanc = 1, poids_noir = 2, C = 5, d = 10, stop_d = TRUE){
  
  duree = Sys.time()
  duree_totale = NULL
  duree_arret = NULL
  duree_conv = NULL
  
  if(m<n){
    stop()
  }
  
  # Création des paramètres initiaux
  param_liste <- list()
  P_hat_tilde <- matrix(nrow = n, ncol = m)
  param_liste <- list()
  param_liste[[1]] <- list (lambda = 1,
                            x_star = initialisation_sample(m = m, n = n, N = 1,
                                                           avec_remise = FALSE))
  # Listes à agrémenter
  gammas_hat = c()
  s_max = c()
  indice_arret = NULL
  indice_conv = NULL
  ###### Algo
  
  #### début du try
  iter <- 0
  critere_arret <- TRUE
  #ceils = rounds each element of X to the nearest integer greater than or equal to that element.
  eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
  while(critere_arret & (iter+1)<= maxIters){
     iter <- iter + 1

    # X <- simul_permutation(N = N, param = param_liste[[iter]],numSim = 100000, y,m,n)
    X <- simul_permutation(N = N, param = param_liste[[iter]], y,m,n)
 
    #### Calcul du score
    
    scores <- apply(X, 1, score,
                    y = y, poids_noir = poids_noir, poids_blanc = poids_blanc)
    
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    gamma = scores_tries[eidx]
    s = scores_tries[N]
    X_top = X[scores>=gamma,]
    
    # Détermination du x* par l'algorithme hongrois #
    matriceF <- creer_matriceF(X_top,n,m)
    hongarian <- solve_LSAP(matriceF,maximum=TRUE)
    res <- cbind(seq_along(hongarian), hongarian)
    x_star <- 1:n
    for(i in 1:n){
      x_star[i] <- as.numeric(res[i,"hongarian"])
    }
    if(score(param_liste[[iter]]$x_star,y) >score(x_star,y)){
      x_star = param_liste[[iter]]$x_star
    }
    
    min_loss <- sum(apply(X_top,1, function(x) sum(x != x_star)))
    
    # Pour lambda, on le fait peu à peu tendre vers 0
    lambda <- param_liste[[iter]]$lambda - param_liste[[1]]$lambda/(maxIters+1)
    
    # print(sprintf("i %s - N_top %s - lambda %.3f - gamma %.3f - loss %.3f - prop %s",
    #               iter,
    #               nrow(X_top), lambda, gamma, min_loss, paste(x_star,collapse = " ")))
    
    gammas_hat[iter] = gamma
    s_max[iter] = s
    param_liste[[iter+1]] <- list(lambda = lambda,
                                  x_star = x_star)
    
    # Critère d'arrêt quand on trouve la bonne réponse
    if(isTRUE(all.equal(score(x = x_star,y = y, poids_noir = poids_noir, poids_blanc = poids_blanc), 1))){
      indice_arret <- iter+1 # différent de l'autre fonction attention
      duree_arret <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2) #NEW
      if(stop_d){
        critere_arret <- FALSE
      }
    }
    # Critère de convergence
    if(length(gammas_hat) > d & is.null(indice_arret)){
      gammas_d <- tail(gammas_hat,d)
      if(isTRUE(all.equal(tail(gammas_hat,1), 1))){
        indice_conv <- iter+1 # différent de l'autre fonction attention
        duree_conv <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2) #NEW
        if(stop_d){
          critere_arret <- FALSE
        }
      }
    }
  }
  
  ### fin de try
  duree_totale <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
  
  return(
    list(
      duree = list(
        duree_totale=duree_totale,
        duree_conv=duree_conv,
        duree_arret=duree_arret
      ),
      parametres=list(
        y=y,
        n=n,
        m=m,
        N=N,
        maxIters= maxIters,
        rho = rho,
        alpha = alpha,
        smoothing = FALSE,
        d=d,
        avec_remise = TRUE
      ),
      param_liste=param_liste,
      s_max=s_max,
      gammas_hat=gammas_hat,
      indices = list(
        indice_arret = indice_arret,
        indice_conv = indice_conv
      )
    )
    
  )
}

