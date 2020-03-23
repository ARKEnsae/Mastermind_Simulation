rm(list=ls())

#################################################################
##################### Paramètres du modèle  #####################
#################################################################

m = 6 #j (couleur)
n = 4 #i (bille)
N = 100
rho = 0.1 
maxIters=100
poids_noir = 2
poids_blanc = 1
set.seed(1) # la seed
smoothing = TRUE
alpha = 0.7

#################################################################
########################## Fonctions  ###########################
#################################################################

nb_fiches_noires <- function(x, y){
  sum(x == y)
}

nb_fiches_blanches <- function(x, y){
  # On enlève les bien placés
  sous_x <- x[x != y]
  sous_y <- y[x != y]
  if(length(sous_x) == 0)
    return(0)
  # Pour chaque couleur de sous_x, on regarde si elle est dans y 
  mal_places <- sapply(sous_x, function(x){
    length(grep(x,sous_y))>0
  })
  sum(mal_places)
}

score <- function(x, y){
  return(poids_noir* nb_fiches_noires(x,y) * poids_blanc + nb_fiches_blanches(x,y))
}
#score(X[2,],y)

#################################################################
###################### Etape d'initialisation ###################
#################################################################

# Création du vecteur y 
y <- sample(1:m, n, replace = TRUE)
y

# Creation des N vecteurs X  : Matrice X (Nxm)
X <- matrix(rep(sample(1:m, N, replace = TRUE),N),
            nrow = N, ncol = n, byrow=TRUE)
X

# Création de la matrice P_hat initiale (n x m) 
P_hat_tilde <- matrix(nrow = n, ncol = m)
P_hat_liste <- list()
P_hat_liste[[1]] <- matrix(1/m,nrow = n, ncol = m) # initialisation

# Listes à agrémenter
meilleur_score = 0
meilleur_scores = c()
gammas = c()

###### Algo

for(iter in 2:maxIters){

if(iter>2){
### Calcul des nouveau X
   for(i in 1:n){
    X[,i] <- sample(1:m, N, replace = TRUE, prob=P_hat[i,])
  }
}
  
#### Calcul du score

scores <- apply(X,1,function(ligne){score(ligne,y=y)})
scores_tries <- sort(scores)
scores_indices <- order(scores)

# Mise à jour de Gamma 
#ceils = rounds each element of X to the nearest integer greater than or equal to that element.
eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
gamma = scores_tries[eidx] ; 
meilleur_score = max(meilleur_score,  scores_tries[N]) #garder une trace du meilleur résultat
gammas[iter] = gamma 
meilleur_scores[iter] = meilleur_score


for(i in 1:n){
  for(j in 1:m){
    P_hat_tilde[i,j]=sum(scores>=gamma & X[,i]==j)/sum(scores>=gamma)
  }
}

# Smoothing
P_hat <- alpha * P_hat_tilde + (1-alpha)* P_hat_liste[[iter-1]]
P_hat_liste[[iter]] <- P_hat

}


###### Résultat de l'algorithme
# A trouver 
y
# Ce qu'on trouve 
matrice_ordre <- apply(P_hat_liste[[100]],1,rank)
apply(matrice_ordre,2,function(x){which(x==6)})

