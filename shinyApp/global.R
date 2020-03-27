rm(list=ls())
library(plot3D)
library(shinyWidgets)
library(kableExtra)
library(shinyjs)

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
initialiser_y <- function(m,n, avec_remise=FALSE){
  if(avec_remise){
    y <- sample(1:m, n, replace = TRUE) # question 1
  } else{
    y <- sample(1:m, n, replace = FALSE) # question 2
  }
  
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

score <- function(x, y,poids_noir=2,poids_blanc=1,normalisation=TRUE){
  score = poids_noir* nb_fiches_noires(x,y) * poids_blanc + nb_fiches_blanches(x,y)
  if(normalisation){
    score = score / (poids_noir*length(x))   
  }
    return(score)
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
                              smoothing = TRUE, C=5, d=5, stop_d=FALSE, avec_remise=FALSE){
  
  duree = Sys.time()
  
  # Creation des N vecteurs X  : Matrice X (Nxn)
    # X <- matrix(rep(sample(1:m, N, replace = TRUE),N),
    #             nrow = N, ncol = n, byrow=TRUE)
  X <- matrix(nrow = N, ncol = n) #Nxn
  for(i in 1:N){
   X[i,] <-initialiser_y(m,n, avec_remise=avec_remise) # taille n
  }
  
 
  
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
  
  
  
  #### début du try
  
  tryCatch({
    
    for(iter in 2:maxIters){
      
      if(iter>2){
        ### Calcul des nouveau X > à déplacer à la fin de l'algo ?
       
       if(avec_remise){
         for(it in 1:N){ #X : Nxn   Phat : nxm
           X[it,] <- sample(1:m, n, replace = avec_remise, prob=P_hat[it,])
         }
       } else{
         for(it in 1:N){ #X : Nxn   Phat : nxm
           couleurs_restantes <- 1:m
           for(i in 1:n){
             X[it,i] <- sample(couleurs_restantes, 1,
                                 prob=P_hat[i,couleurs_restantes])
             couleurs_restantes <- setdiff(couleurs_restantes,
                                           X[it,i])
           }
           
         }
         
         
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
          if(stop_d){
            stop("Dernière itération : ",indice_stop)  
          }
        }
      }
      
    }
    
    
  },error=function(e){})
  ### fin de try
  
  duree <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2)
  
  
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
        d=d
        
      ),
      
      
      P_hat_liste=P_hat_liste,
      s_max=s_max,
      gammas_hat=gammas_hat,
      indice_stop=indice_stop
    )
  
  )

  
}

m=5
n=4
modele <- lancer_algorithme(y=sample(1:m, n, replace = TRUE), n=n, m=m,maxIters=10,N=10,stop_d=TRUE)


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

tableau_bilan <- function(modele){
  
  if(!is.null(modele$indice_stop)){
    i <- modele$indice_stop
  } else{
    i <- modele$parametres$maxIters
  }

  
  tableau <- data.frame(
    t = 1:i,
    s_max = round(modele$s_max[1:i],3),
    gammas_hat = round(modele$gammas_hat[1:i],3),
    min = round(unlist(sapply(modele$P_hat_liste,p_min_max)["min",1:i]),4),
    max_min =round(unlist(sapply(modele$P_hat_liste,p_min_max)["max_min",1:i]),4),
    min_max =round(unlist(sapply(modele$P_hat_liste,p_min_max)["min_max",1:i]),4),
    max = round(unlist(sapply(modele$P_hat_liste,p_min_max)["max",1:i]),4)
    
    
  )
   
  
  return(tableau)
  
}

#tab <- tableau_bilan(modele)

mise_en_forme_tableau <- function(modele){
  
  tableau <- tableau_bilan(modele)
  
  parametres <- paste0(#" : ",
                       "n = ", modele$parametres$n, " / ",
                       "m = ", modele$parametres$m, " / ",
                       "N = ", modele$parametres$N, " / ",
                       "rho = ", modele$parametres$rho, " / ",
                       "alpha = ", modele$parametres$alpha, " / ",
                       "smoothing = ", ifelse(modele$parametres$smoothing,"oui","non"), " / ",
                       "d = ", modele$parametres$d
  )
  
  tableau_joli <- kable(tableau, align = "c") %>%
    kable_styling(full_width = F) %>%
    footnote(general = paste0("Parametres : ",parametres,"\nTemps de calcul : ", modele$duree, " sec."),
             general_title = "\nNote",
             title_format = c("italic", "underline")
    )
  tableau_joli <- gsub('\\bNA\\b', '  ', tableau_joli) #remove NA
  
  return(tableau_joli)
  
}


#tab_joli <- mise_en_forme_tableau(tab, modele)



#################################################################
###################### Affichage des billes #####################
#################################################################

#https://www.html5canvastutorials.com/tutorials/html5-canvas-circles/
  
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



               
