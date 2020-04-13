lancer_algorithme <- function(y, n, m, N = C * m * n, maxIters = 100,
                              rho = 0.1, alpha = 0.7,
                              poids_blanc = 1, poids_noir = 2,
                              smoothing = TRUE, C = 5, d = 5,
                              stop_d = FALSE, avec_remise = TRUE){
  
  if(!avec_remise & m<n){
    stop()
  }
  
  duree = Sys.time()
  duree_arret = NULL
  duree_conv = NULL
  duree_totale= NULL
  
  # Création de la matrice P_hat initiale (n x m) 
  P_hat_tilde <- matrix(nrow = n, ncol = m)
  P_hat_liste <- list()
  P_hat_liste[[1]] <- matrix(1/m,nrow = n, ncol = m)
  # Listes à agrémenter
  #meilleur_score = 0
  #meilleur_scores = c()
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
    
    X <- initialisation_sample(m = m, n = n, N = N,
                               P_hat = P_hat_liste[[iter]],
                               avec_remise = avec_remise)
    
    #### Calcul du score
    
    scores <- apply(X, 1, score,
                    y = y, poids_noir = poids_noir, poids_blanc = poids_blanc)
    
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    gamma = scores_tries[eidx]
    
    if(gamma==1 & is.null(duree_arret)){
      indice_arret <- iter
      duree_arret <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
    }
    
    s = scores_tries[N]
    #  meilleur_score = max(meilleur_score,  scores_tries[N]) #garder une trace du meilleur résultat
    gammas_hat[iter] = gamma
    s_max[iter] = s
    # meilleur_scores[iter] = meilleur_score
    
    for(i in 1:n){
      for(j in 1:m){
        P_hat_tilde[i,j]=sum(scores>=gamma & X[,i]==j)/sum(scores>=gamma)
      }
    }
    # Smoothing
    if(smoothing){
      #P_hat <- alpha * P_hat_tilde + (1-alpha)* P_hat_liste[[iter-1]]
      P_hat <- alpha * P_hat_tilde + (1-alpha)* P_hat_liste[[iter]] 
    } else{
      P_hat <- P_hat_tilde
    }
    
    P_hat_liste[[iter+1]] <- P_hat
    
    if(length(gammas_hat) > d & is.null(indice_conv)){
      gammas_d <- gammas_hat[(length(gammas_hat)-d):length(gammas_hat)]
      if(length(unique(gammas_d))==1){
        indice_conv <- iter
        duree_conv <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
        if(stop_d){
          critere_arret <- FALSE
        }
      }
    }
  }
 
  # On enlève la dernière P_hat non utile
  P_hat_liste <- P_hat_liste[-length(P_hat_liste)]
  
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
        smoothing = smoothing,
        d=d,
        avec_remise = avec_remise
        
      ),
      
      
      P_hat_liste=P_hat_liste,
      s_max=s_max,
      gammas_hat=gammas_hat,
      indices = list(
        indice_arret = indice_arret,
        indice_conv = indice_conv
      )
    )
    
  )
}