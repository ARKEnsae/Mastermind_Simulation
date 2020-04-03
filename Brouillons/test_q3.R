library(mvtnorm)

source("ShinyApp/fonctions_initialisation.R", encoding = "UTF-8")
m=10
n=6
avec_remise = FALSE
N = m*n*6
maxIters = 20
C = 5
rho = 0.1
poids_blanc = 1
poids_noir = 2
smoothing = TRUE
d=10
stop_d = TRUE
set.seed(2)
y <- initialiser_y(m=m,n=n, avec_remise = avec_remise)
resultat <- lancer_algorithme_perm(y,n,m, stop_d = TRUE,maxIters = 100,d=10)
param_liste <- resultat$param_liste
x_star <- sapply(param_liste,function(x)x$x_star)
x_star
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
  param_liste[[1]] <- list (lambda = 1.5,
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
    print(iter)
    X <- simul_permutation(N = N, param = param_liste[[iter]],numSim = 30000)
    
    #### Calcul du score
    
    scores <- apply(X, 1, score,
                    y = y, poids_noir = poids_noir, poids_blanc = poids_blanc)
    
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    gamma = scores_tries[eidx]
    s = scores_tries[N]
    #  meilleur_score = max(meilleur_score,  scores_tries[N]) #garder une trace du meilleur résultat
    gammas_hat[iter] = gamma
    s_max[iter] = s
    # meilleur_scores[iter] = meilleur_score
    
    X_top = X[scores>=gamma,]
    objectif <- function(x_star){
      sum(apply(X_top,1, function(x) sum(x != x_star)))
    }
    # Remplacer X_top par X ne change pas grand chose
    loss_tot <- apply(X_top,1,objectif)
    x_star <- which(loss_tot == min(loss_tot))
    x_star <- sample(x_star,size = 1)
    x_star <- X_top[x_star,]
    
    # Ou de manière équivalente
    # permutations_top <- apply(X_top,1,paste0,collapse=",")
    # comptage_permutations_top <- table(permutations_top)
    # comptage_permutations_top[paste0(x_star,collapse=",")]
    # x_star <- sample(which(comptage_permutations_top == max(comptage_permutations_top)),1)
    # x_star <- names(comptage_permutations_top)[x_star]
    # x_star <- as.numeric(strsplit(x_star,",")[[1]])

    lambda <- param_liste[[iter]]$lambda  - param_liste[[1]]$lambda /(maxIters+1)
    
    
    param_liste[[iter+1]] <- list (lambda = lambda,
                              x_star = x_star)
    
    if(length(gammas_hat) > d & is.null(indice_stop)){
      gammas_d <- tail(gammas_hat,d)
      if(length(unique(gammas_d))==1){
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
