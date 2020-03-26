rm(list=ls())
library(plot3D)
library(shinyWidgets)

#################################################################
##################### Paramètres du globaux  #####################
#################################################################

#m = 6 #j (couleur)
#n = 4 #i (bille)
#N = 100
#rho = 0.1 
#maxIters=100
#poids_noir = 2
#poids_blanc = 1
#smoothing = TRUE
#alpha = 0.7

set.seed(1) # la seed

colors <- c("#0000FF","#00FF00","#FF3232","#FFFF00","#CF00CF","#FFCFFF","#00FFFF","#008F00","#FF4F4F","#000000") # 10 couleurs max

C = 5 #non paramétrable dans shiny

#################################################################
########################## Fonctions  ###########################
#################################################################

# Création du vecteur y 
initialiser_y <- function(m,n){
  y <- sample(1:m, n, replace = TRUE)
  return(y)
}

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

score <- function(x, y,poids_noir,poids_blanc){
  return(poids_noir* nb_fiches_noires(x,y) * poids_blanc + nb_fiches_blanches(x,y))
}
#score(X[2,],y)

#################################################################
###################### Etape d'initialisation ###################
#################################################################

# Création du vecteur y 
#y <- initialiser_y(m,n)


lancer_algorithme <- function(y, n, m, N = C*m*n, maxIters = 100,
                              rho = 0.1, alpha = 0.7,
                              poids_blanc = 1, poids_noir = 2,
                              smoothing = TRUE, C=5, d=5){
  
  
  # Creation des N vecteurs X  : Matrice X (Nxm)
  X <- matrix(rep(sample(1:m, N, replace = TRUE),N),
              nrow = N, ncol = n, byrow=TRUE)
  X
  
  # Création de la matrice P_hat initiale (n x m) 
  P_hat_tilde <- matrix(nrow = n, ncol = m)
  P_hat_liste <- list()
  P_hat_liste[[1]] <- matrix(1/m,nrow = n, ncol = m) # initialisation
  
  # Listes à agrémenter
  #meilleur_score = 0
  #meilleur_scores = c()
  gammas_hat = c()
  s_max = c()
  indice_stop = NULL
  
  ###### Algo
  
  for(iter in 2:maxIters){
    
    if(iter>2){
      ### Calcul des nouveau X
      for(i in 1:n){
        X[,i] <- sample(1:m, N, replace = TRUE, prob=P_hat[i,])
      }
    }
    
    #### Calcul du score
    
    scores <- apply(X,1,function(ligne){score(ligne,y=y,poids_noir = poids_noir,poids_blanc=poids_blanc)})
   
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    #ceils = rounds each element of X to the nearest integer greater than or equal to that element.
    eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
    gamma = scores_tries[eidx]
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
      P_hat <- alpha * P_hat_tilde + (1-alpha)* P_hat_liste[[iter-1]]
    } else{
      P_hat <- P_hat_tilde
    }
    
    P_hat_liste[[iter]] <- P_hat
    
    if(length(gammas_hat)>d & is.null(indice_stop)){
          gammas_d <- gammas_hat[(length(gammas_hat)-d):length(gammas_hat)]
      if(length(unique(gammas_d))==1){
        indice_stop <- iter
      }
    }
  
  }
  
  return(list(P_hat_liste=P_hat_liste,
              s_max=s_max,
              gammas_hat=gammas_hat,
              indice_stop=indice_stop))

  
}
modele <- lancer_algorithme(y=sample(1:5, 5, replace = TRUE), n=5, m=5,maxIters=40)

#matrice : n lignes (i) m colonnes (j)

p_min_max <- function(matrice){
  max_min <- max(apply(matrice,1,min))
  min_max <- min(apply(matrice,1,max))
  min <- min(matrice)
  max <- max(matrice)
  
  return(list(min = min,
              max = max,
              min_max = min_max,
              max_min = max_min))
}



matrice_to_proposition <- function(matrice){
  
  matrice_ordre <- apply(matrice,1,rank)  
  return(apply(matrice_ordre,2,function(x){which(x==max(x))})) 
}
#matrice_to_proposition(modele$P_hat_liste[[100]])

### Dessiner histogramme

dessiner_histo <- function(liste_matrice,indice,colors){
  matrice=liste_matrice[[indice]]
  n = c(1:nrow(matrice))
  m = c(1:ncol(matrice))
  couleurs_graphe <- t(matrix(rep(1:length(m),length(n)),nrow=length(n),ncol=length(m),byrow=TRUE))
  
  par(mar = c(0,0,0,0))
  hist3D(m, n, t(matrice), zlim=c(0,1), colvar = couleurs_graphe,
         col = colors[1:ncol(matrice)],theta=50, phi=40, axes=TRUE,label=TRUE, ticktype="detailed", space=0.5, lighting=TRUE, light="diffuse", shade=0.5, alpha=0.6, xlab="",ylab="billes",zlab="",colkey=list(plot=FALSE))
  
}


#################################################################
###################### Affichage des billes #####################
#################################################################

#https://www.html5canvastutorials.com/tutorials/html5-canvas-circles/
  
library(shinyjs)
canvas_width <- 25
canvas_height <- 25

# radio buttons pour les boules
guesscell1 <- 'guesscell1'
guesscell2 <- 'guesscell2'
guesscell3 <- 'guesscell3'
guesscell4 <- 'guesscell4'
guesscell5 <- 'guesscell5'
guesscell6 <- 'guesscell6'

itercell1 <- 'itercell1'
itercell2 <- 'itercell2'
itercell3 <- 'itercell3'
itercell4 <- 'itercell4'
itercell5 <- 'itercell5'
itercell6 <- 'itercell6'

jsDrawCircle <-
  "shinyjs.drawCircle = function(args){var id = args[0]; var code_color = args[1]; console.log(id); var canvas = document.getElementById(id); console.log(canvas); var ctx = canvas.getContext('2d'); ctx.beginPath(); ctx.arc(10, 10, 10, 0, Math.PI * 2, true); ctx.fillStyle = code_color; ctx.fill(); ctx.closePath(); ctx.stroke();}"


jsClearCircle <-
  "shinyjs.clearCircle = function(args){var id = args[0]; console.log(id); var canvas = document.getElementById(id); var ctx = canvas.getContext('2d'); ctx.beginPath(); ctx.clearRect(0, 0, canvas.width, canvas.height); ctx.closePath(); ctx.stroke();}"



               
