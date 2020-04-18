
# Définition de la densité PI(X)
pi_density <- function(x,lambda,x_etoiles){
  return(exp(-lambda*sum(x != x_etoiles)))
}


# Fonction qui permet d'inverser deux éléments d'une permutation
inverse_deux_elements <- function(X, n){
  i1 <- sample(1:n, 1)
  i2 <- sample((1:length(X))[-i1],1)
  temp = X[i1]
  X[i1] <- X[i2]
  X[i2] <- temp
  return(X)
}


pi_density_MCMC <- function(numSim, lambda, x_etoiles, m,n){
  X0 <- sample(1:m,m,replace=FALSE)
  X <-matrix(rep(X0,numSim),numSim,m,byrow = T)
  for (t in (1:(numSim-1))){
    Xprop=inverse_deux_elements(X[t,], n)
    if(runif(1) < min(1,pi_density(Xprop[1:n],lambda,x_etoiles)/pi_density(X[t,1:n],lambda,x_etoiles))){
      X[t+1,]=Xprop
    }
    else{
      X[t+1,]=X[t,]
    }
  }
  return(X[,1:n])
}

# Traiter le burn-in et les auto-corrélations (éventuellement à modif pour burn-in)

# Tracer trace d'une coordonée pour voir burn-in
tracer_Trace <- function(nSim,lambda,x_etoiles,m,n,coord){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,m,n)
  ggplot(as.data.frame(out))+
    geom_line(aes(x = (1:nSim),y=out[,1]))+
    labs(title=paste('Trace plot'),x='',y='')
}

# Tracer ACF pour trouver lag
tracer_ACF <- function(nSim,lambda,x_etoiles,m,n,coord,burn_in){
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,m,n)
  library(forecast)
  ggAcf(out[burn_in:dim(out)[1],coord])+
    labs(title=paste('ACF'))
}



# Amélioré
modif_metro <- function(x, m, burn_in = TRUE, lag = 80){
  # Burn-in : commence à 1000
  if(burn_in){
    x <- x[(250*m):dim(x)[1],]
  }
  x <- x[seq(1, nrow(x), lag),]
  return(x)
}

simul_permutation <- function(N, param, m,n, lag = 80){
  num <- 250*m + N * lag
  out <- pi_density_MCMC(numSim = num, lambda =  param$lambda,
                         x_etoiles = param$x_star, m = m, n = n)
  out_traite <- modif_metro(x = out, m = m, burn_in = TRUE, lag = lag)
  return(out_traite)
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
    X <- simul_permutation(N = N, param = param_liste[[iter]],m = m,n = n)
 
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
    # lambda <- param_liste[[iter]]$lambda - param_liste[[1]]$lambda/(maxIters+1)
    # 
    # gradient <- function(lambda) {
    #   N_top = nrow(X_top)
    #   p1 <- N_top * m
    #   sum_exp <- sapply(seq(0,m),function(k){
    #     (exp(lambda) - 1)^k / factorial(k)
    #   })
    #   sum_exp_t <- sum(sum_exp)
    #   sum_exp_tm1 <- sum(sum_exp[-length(sum_exp)])
    #   (sum_exp_tm1 * exp(lambda) - m* sum_exp_t)/sum_exp_t + min_loss/N_top
    # }
    # lambda <- tryCatch(uniroot(gradient, c(0,4))$root, error = function(e) 1)
    # lambda <- alpha * lambda + (1-alpha)* param_liste[[iter]]$lambda
    lambda = 1
    print(sprintf("i %s - N_top %s - lambda %.3f - gamma %.3f - loss %.3f - prop %s",
                  iter,
                  nrow(X_top), lambda, gamma, min_loss, paste(x_star,collapse = " ")))
    
    gammas_hat[iter] = gamma
    s_max[iter] = s
    param_liste[[iter+1]] <- list(lambda = lambda,
                                  x_star = x_star)
    
    # Critère d'arrêt quand on trouve la bonne réponse
    if(isTRUE(all.equal(score(x = x_star,y = y, poids_noir = poids_noir, poids_blanc = poids_blanc), 1)) & is.null(indice_arret)){
      indice_arret <- iter+1 # différent de l'autre fonction attention
      duree_arret <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2) #NEW
      if(stop_d){
        critere_arret <- FALSE
      }
    }
    # Critère de convergence
    if(length(gammas_hat) > d & is.null(indice_conv)){
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

