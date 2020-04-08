library(mvtnorm)

source("ShinyApp/fonctions_initialisation.R", encoding = "UTF-8")
m=6
n=6
avec_remise = FALSE
N = 5 * n*m
maxIters = 20
C = 5
rho = 0.1
poids_blanc = 1
poids_noir = 2
smoothing = TRUE
d=5
stop_d = TRUE
set.seed(1)
y <- initialiser_y(m=m,n=n, avec_remise = avec_remise)
resultat <- lancer_algorithme_perm(y,n,m,N = N, stop_d = TRUE,maxIters = 200,d=10)
param_liste <- resultat$param_liste
x_star <- sapply(param_liste,function(x)x$x_star)
tail(t(x_star))
y
resultat$gammas_hat
lancer_algorithme_perm <- function(y, n, m, N = C * m * n, maxIters = 100,
                              rho = 0.1, alpha = 0.7,
                              poids_blanc = 1, poids_noir = 2,
                              smoothing = TRUE, C = 5, d = 10,
                              stop_d = FALSE){
  
  duree = Sys.time()
  
  # Création de la matrice P_hat initiale (n x m) 
  param_liste <- list()
  P_hat_tilde <- matrix(nrow = n, ncol = m)
  param_liste <- list()
  param_liste[[1]] <- list (lambda = 2,
                            x_star = initialisation_sample(m = m, n = n, N = 1,
                                                           avec_remise = FALSE))
  # Listes à agrémenter
  #meilleur_score = 0
  #meilleur_scores = c()
  gammas_hat = c()
  s_max = c()
  indice_stop = NULL
  
  ###### Algo
  
  
  #### début du try
  iter <- 0
  critere_arret <- TRUE
  #ceils = rounds each element of X to the nearest integer greater than or equal to that element.
  eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
  while(critere_arret & (iter+1)<= maxIters){
    iter <- iter + 1
    
    X <- simul_permutation(N = N, param = param_liste[[iter]],numSim = 100000)
    
    #### Calcul du score
    
    scores <- apply(X, 1, score,
                    y = y, poids_noir = poids_noir, poids_blanc = poids_blanc)
    
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    gamma = scores_tries[eidx]
    s = scores_tries[N]
    #  meilleur_score = max(meilleur_score,  scores_tries[N]) #garder une trace du meilleur résultat

    # meilleur_scores[iter] = meilleur_score
    
    X_top = X[scores>=gamma,]
    objectif <- function(x_star){
      sum(apply(X_top,1, function(x) sum(x != x_star)))
    }
    # Remplacer X_top par X ne change pas grand chose
    loss_tot <- apply(X_top,1,objectif)
    min_loss <- min(loss_tot)
    x_star <- which(loss_tot == min_loss)
    x_star <- sample(x_star,size = 1)
    x_star <- X_top[x_star,]
    
    
    
    permutations_top <- apply(X_top,1,paste0,collapse=",")
    comptage_permutations_top <- table(permutations_top)
    comptage_permutations_top[paste0(x_star,collapse=",")]
    x_star <- which(comptage_permutations_top == max(comptage_permutations_top))
    x_star <- names(x_star)[sample(length(x_star),1)]
    x_star <- as.numeric(strsplit(x_star,",")[[1]])
    min_loss <- sum(apply(X_top,1, function(x) sum(x != x_star)))
    
    # permutations_top <- apply(X_top,2, function(x){
    #   freq <- table(x)
    #   x_star_i <- which(freq == max(freq))
    #   x_star_i <- names(x_star_i)[sample(length(x_star_i),1)]
    #   as.numeric(x_star_i)
    # })
    # x_star <- permutations_top
    # min_loss <- sum(apply(X_top,1, function(x) sum(x != x_star)))
    
    # if(min_loss == 0){
    #   lambda <- lambda
    # }else{
    #   lambda <- nrow(X_top)/min_loss
    # }

    mle <- function(lambda) {   ## Rosenbrock Banana function
      N_top = nrow(X_top)
      p1 <- lambda * N_top * m
      sum_exp <- sum(sapply(seq(0,m),function(k){
        (exp(lambda) - 1)^k / factorial(k)
      }))
      p2 <- -N_top * log(sum_exp)
      p3 <- -lambda * min_loss
      p1+p2+p3
    }
    # Non utile ici
    # gradient <- function(lambda) {
    #   N_top = nrow(X_top)
    #   p1 <- N_top * m
    #   sum_exp <- sapply(seq(0,m),function(k){
    #     (exp(lambda) - 1)^k / factorial(k)
    #   })
    #   p2 <- -N_top*sum(sum_exp[-length(sum_exp)])/sum(sum_exp)
    #   p3 <- - min_loss
    #   p1+p2+p3
    # }

    max <- optimize(mle,c(0,2), maximum = TRUE)
    lambda <- max$maximum
    print(sprintf("iter %s - nrow(X_top) %s - lambda %.3f - gamma %.3f - loss %.3f",iter,
                  nrow(X_top), lambda, gamma,min_loss))
    
    gammas_hat[iter] = gamma
    s_max[iter] = s
    param_liste[[iter+1]] <- list(lambda = lambda,
                                   x_star = x_star)
    
    # Ou autre façon : 
    # permutations_top <- apply(X_top,1,paste0,collapse=",")
    # comptage_permutations_top <- table(permutations_top)
    # comptage_permutations_top[paste0(x_star,collapse=",")]
    # x_star <- which(comptage_permutations_top == max(comptage_permutations_top))
    # x_star <- names(x_star)[sample(length(x_star),1)]
    # x_star <- as.numeric(strsplit(x_star,",")[[1]])

    # lambda <- param_liste[[iter]]$lambda*0.87
                          
    
    # if(runif(1)> (score(x_star, y)/ score(param_liste[[iter]]$x_star,y)) ){
    #   if(iter>1){
    #     gammas_hat[iter] = gammas_hat[iter-1]
    #     s_max[iter] = s_max[iter-1]
    #   }
    #   param_liste[[iter+1]] <- param_liste[[iter]]
    # }else{
    #   gammas_hat[iter] = gamma
    #   s_max[iter] = s
    #   param_liste[[iter+1]] <- list (lambda = lambda,
    #                                  x_star = x_star)
    # }
    
    if(isTRUE(all.equal(score(x = x_star,y = y, poids_noir = poids_noir, poids_blanc = poids_blanc), 1))){
      indice_stop <- iter
      if(stop_d){
        critere_arret <- FALSE
      }
    }
    if(length(gammas_hat) > d & is.null(indice_stop)){
      gammas_d <- tail(gammas_hat,d)
      if(isTRUE(all.equal(tail(gammas_hat,1), 1))){
        indice_stop <- iter
        if(stop_d){
          critere_arret <- FALSE
        }
      }
    }
  }
  
  ### fin de try
  duree <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
  
  # # On enlève la dernière P_hat non utile
  # P_hat_liste <- P_hat_liste[-length(P_hat_liste)]
  
  return(
    list(
      duree = duree,
      parametres=list(
        n=n,
        m=m,
        N=N,
        maxIters= maxIters,
        rho = rho,
        alpha = alpha,
        smoothing = smoothing,
        d=d,
        avec_remise = avec_remise
        
      ),
      param_liste=param_liste,
      s_max=s_max,
      gammas_hat=gammas_hat,
      indice_stop=indice_stop
    )
    
  )
}
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
  indice <- which.max(as.integer(abs(resultat$acf)<=1.95/sqrt(resultat$n.used)))-1
  x_temp_acf <- x_temp[seq(1,dim(x_temp)[1],indice),]
  # res <- vector("list",dim(x_temp_acf)[1])
  # for(i in 1:dim(x_temp_acf)[1]){
  #   val <- c()
  #   for(j in 1:dim(x_temp_acf)[2]){
  #     val <- c(val,x_temp_acf[i,j])
  #   }
  #   res[[i]] <- val
  # }
  # res <- lapply(seq_len(dim(x_temp_acf)[1]),function(i){
  #   sapply(seq_len(dim(x_temp_acf)[2]), function(j) x_temp_acf[i,j])
  # })
  return(x_temp_acf)
}

# Fonction à utiliser qui renvoie un échantillon indépendant de N X_i qui suivent la loi PI(X) pour lambda et x_etoiles
# donnés
# Param un liste avec lambda et x_star
simul_permutation <- function(N, param, numSim = 10000){
  out <- pi_density_MCMC(numSim, param$lambda, param$x_star)
  out_traite <- modif_metro(out)
  indices <- sample.int(nrow(out_traite), size =  N, replace = FALSE)
  return(out_traite[indices,])
}

