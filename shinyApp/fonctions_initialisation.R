# Création du vecteur à deviner
initialiser_y <- function(m, n, avec_remise = TRUE){
  # Avec remise = Q1
  # Sans remise = Q2/3
  y <- sample(1:m, n, replace = avec_remise)
  return(y)
}

# Le score est calculé de façon suivante :
# poids_noir * nb_boules_noires + poids_noir * nb_boules_blanches
# Le score maximal (lorsqu'on a la bonne réponse) est égal à poids_noir * nb billes à deviner
# Pour faciliter les comparaisons, on normalise par défaut le score en le divisant par ce nombre
# Nombre de boules noires = nombre de boules bien placées
# Nombre de boules blanches = nombre de boules de la bonne couleur mais mal placées
score <- function(x, y,
                  poids_noir = 2, poids_blanc = 1,
                  normalisation = TRUE){
  score = poids_noir * nb_boules_noires(x, y) * poids_blanc + nb_boules_blanches(x, y)
  if(normalisation){
    score = score / (poids_noir * length(x))   
  }
  return(score)
}

nb_boules_noires <- function(x, y){
  sum(x == y)
}

nb_boules_blanches <- function(x, y){
  # On enlève les bien placés
  sous_x <- x[x != y]
  sous_y <- y[x != y]
  if(length(sous_x) == 0)
    return(0)
  # Pour chaque couleur de sous_x, on regarde si elle est dans y 
  mal_places <- sapply(sous_x, function(x){
    length(grep(x, sous_y))>0
  })
  sum(mal_places)
}

#score(X[2,],y)

# m = nb couleurs
# n = nb boules
# N = taille
initialisation_sample <- function(m, n, N, P_hat, avec_remise = TRUE){
  if(avec_remise){
    initialisation_sample_avec_remise(m, n, N, P_hat)
  } else{
    initialisation_sample_sans_remise(m, n, N, P_hat)
  }  
}
initialisation_sample_avec_remise <- function(m, n, N, P_hat){
  X <- matrix(nrow = N, ncol = n)
  for(i in 1:n){
    X[,i] <- sample(1:m, N, replace = TRUE, prob = P_hat[i,])
  }
  X
}
initialisation_sample_sans_remise <- function(m, n, N, P_hat){
  X <- matrix(nrow = N, ncol = n)
  for(it in 1:N){ #X : Nxn   Phat : nxm
    couleurs_restantes <- 1:m
    for(i in 1:n){
      X[it,i] <- sample(couleurs_restantes, 1,
                        prob = P_hat[i,couleurs_restantes])
      couleurs_restantes <- setdiff(couleurs_restantes,
                                    X[it,i])
    }
  }
  X
}