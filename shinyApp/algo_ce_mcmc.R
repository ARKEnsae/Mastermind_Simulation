proba_hamming <- function(n,x,x_star,lambda){
  d <- n - nb_fiches_noires(x,x_star)
  proba <- exp(-lambda*d)
  return(proba)
}


#### ébauche : ne fonctionne pas encore


 lancer_algorithme_hamming <- function(y, n, m, N = C*m*n, maxIters = 100,rho = 0.1, alpha = 0.7,poids_blanc = 1, poids_noir = 2, smoothing = TRUE, C=5, d=5, stop_d=FALSE, avec_remise=TRUE){
   
   duree = Sys.time()
   
   # Création des paramètres initiaux
   lambda_tilde = 1
   x_star_tilde = initialiser_y(m,n, avec_remise=FALSE)
   lambda_hat_liste <- c()
   x_star_hat_liste <- list()
   lambda_hat_liste <- c(lambda_hat_liste,lambda_tilde)
   x_star_hat_liste[[1]] <- x_star_tilde
   
   
   # Listes à agrémenter
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
     
     # Tirage de X1... XN par métropolis hastings
     # [TODO RORO] Pour le moment mise à jour "au hasard" de X
      X <- matrix(nrow = N, ncol = n) #Nxn
     for(i in 1:N){
       X[i,] <-initialiser_y(m,n, avec_remise=FALSE) # taille n
     }
     
     
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
     
     ##### mise à jour de x* et lambda (résoudre programme Max selon v de D_hat)
     ##### [TODO ALAIN] : pour le moment on met pas à jour comme ça
     lambda_tilde = 1 #TODO
     x_star_tilde = initialiser_y(m,n, avec_remise=FALSE)
     

      # Smoothing : à réfléchir si maintenir
     if(smoothing){
       #       lambda_hat <- alpha * lambda_tilde + (1-alpha)* lambda_hat[[iter-1]]
       #       x_star_hat <- alpha * x_star_tilde + (1-alpha)* x_star_hat[[iter-1]]
       lambda_hat <- lambda_tilde
       x_star_hat <- x_star_tilde
     } else{
       lambda_hat <- lambda_tilde
       x_star_hat <- x_star_tilde
     }
     
     lambda_hat_liste <- c(lambda_hat_liste,lambda_hat)
     x_star_hat_liste[[iter]] <- x_star_hat
     
     if(length(gammas_hat) > d & is.null(indice_stop)){
       gammas_d <- gammas_hat[(length(gammas_hat)-d):length(gammas_hat)]
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
   
   # On enlève les derniers paramètres non utiles
   lambda_hat_liste <- lambda_hat_liste[-length(lambda_hat_liste)]
  # x_star_hat_liste[[iter]] <- x_star_hat_liste[-length(x_star_hat_liste)]
   
   return(
     list(
       duree = duree,
       parametres=list(
         y=y,
         n=n,
         m=m,
         N=N,
         maxIters= maxIters,
         rho = rho,
         alpha = alpha,
         smoothing = smoothing,
         d=d
       ),
       
       x_star_hat_liste=x_star_hat_liste,
       lambda_hat_liste=lambda_hat_liste,
       s_max=s_max,
       gammas_hat=gammas_hat,
       indice_stop=indice_stop
     )
     
     
   )
 }