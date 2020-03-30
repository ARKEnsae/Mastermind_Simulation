proba_hamming <- function(n,x,x_star,lambda){
  d <- n - nb_fiches_noires(x,x_star)
  proba <- exp(-lambda*d)
  return(proba)
}


#### ébauche : ne fonctionne pas encore

# lancer_algorithme_hamming <- function(y, n, m, N = C*m*n, maxIters = 100,rho = 0.1, alpha = 0.7,poids_blanc = 1, poids_noir = 2, smoothing = TRUE, C=5, d=5, stop_d=FALSE){
#   
#   duree = Sys.time()
#   
#   possibilites <- permutations(n=m,r=n,v=1:m,repeats.allowed = FALSE)
#   
#   X <- matrix(nrow = N, ncol = n) #Nxn
#   for(i in 1:N){
#     X[i,] <-initialiser_y(m,n, avec_remise=FALSE) # taille n
#   }
#   
#   
#   # Création des paramètres initiaux
#   lambda_tilde = 1
#   x_star_tilde = initialiser_y(m,n, avec_remise=FALSE)
#   lambda_liste <- list()
#   x_star_liste <- list()
#   lambda_liste[[1]] <- lambda_tilde
#   x_star_liste[[1]] <- x_star_tilde
#   
#   
#   # Listes à agrémenter
#   gammas_hat = c()
#   s_max = c()
#   indice_stop = NULL
#   
#   ###### Algo
#   
#   #### début du try
#   
#   tryCatch({
#     
#     
#     for(iter in 2:maxIters){
#       
#       
#       if(iter>2){
#         
#         probabilites <- apply(possibilites,function(x_star){
#           return(proba_hamming(n,x,x_star,lambda))
#         })
#         
#         for(i in 1:N){
#           xi_ligne <- sample(1:length(possibilites), 1, replace = TRUE, prob=probabilites)
#           X[i,] <- possibilites[xi_ligne,] 
#           
#         }
#         
#         
#       }  
#       
#       
#       #### Calcul du score
#       
#       scores <- apply(X,1,function(ligne){score(ligne,y=y,poids_noir = poids_noir,poids_blanc=poids_blanc)})
#       
#       scores_tries <- sort(scores)
#       
#       ##### Mise à jour de Gamma 
#       eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
#       gamma = scores_tries[eidx]
#       s = scores_tries[N]
#       gammas_hat[iter] = gamma
#       s_max[iter] = s
# 
#       
#       ##### mise à jour de x* et lambda 
#       ##### TODO : (résoudre programme Max selon v de D_hat)
#       lambda_tilde = 1 #TODO 
#       x_star_tilde = 1:n #TODO
#       
#       # Smoothing
#       if(smoothing){
#        lambda_hat <- alpha * lambda_tilde + (1-alpha)* lambda_hat[[iter-1]]
#        x_star_hat <- alpha * x_star_tilde + (1-alpha)* x_star_hat[[iter-1]]
#       } else{
#         lambda_hat <- lambda_tilde
#         x_star_hat <- x_star_tilde
#       }
#       
#       lambda_hat_liste[[iter]] <- lambda_hat
#       x_star_liste[[iter]] <- x_star_hat
#       
#       if(length(gammas_hat)>d & is.null(indice_stop)){
#         gammas_d <- gammas_hat[(length(gammas_hat)-d):length(gammas_hat)]
#         if(length(unique(gammas_d))==1){
#           indice_stop <- iter
#           if(stop_d){
#             stop("Dernière itération : ",indice_stop)  
#           }
#         }
#       }
#       
#       
#     }
#     
#   },error=function(e){})
#   ### fin de try
#   
#   duree <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
#   
#   
#   return(
#     list(
#       duree = duree,
#       parametres=list(
#         n=n,
#         m=m,
#         N=N,
#         maxIters= maxIters,
#         rho = rho,
#         alpha = alpha,
#         smoothing = smoothing,
#         d=d
#       ),
#       
#       x_star_hat_liste=x_star_hat_liste
#       lambda_hat_liste=lambda_hat_liste,
#       s_max=s_max,
#       gammas_hat=gammas_hat,
#       indice_stop=indice_stop
#     )
#     
#   )
#   
#   
# }