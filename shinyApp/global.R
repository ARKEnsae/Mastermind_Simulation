rm(list=ls())
library(plot3D)
library(shinyWidgets)
library(kableExtra)
library(shinyjs)
library(gtools) #pour générer les permutations
library(mvtnorm)
library(clue)
library(V8)

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

colors <- c("#0000FF","#00FF00","#FF3232","#FFFF00","#CF00CF","#FFCFFF","#00FFFF","#008F00","#CECECE","#000000") # 10 couleurs max

# C est le paramètre multiplicateur utilisé pour calculer le nombre N
C = 5 #non paramétrable dans shiny
# Création du vecteur y 
#y <- initialiser_y(m,n)
#m=5
#n=4
#y=sample(1:m, n, replace = FALSE) #4 2 3 5
#y=c(4,2,3,5)
#modele <- lancer_algorithme(y, n=n, m=m,maxIters=10,N=10,stop_d=FALSE,avec_remise = TRUE)


#################################################################
########################## Fonctions  ###########################
#################################################################

#Fonctions d'initialisation
source("fonctions_initialisation.R", encoding = "UTF-8")
#Fonctions pour faire les algo de CE classique avec ou sans remise
source("algo_ce_classique.R", encoding = "UTF-8")
#Fonctions pour faire l'algo de la question 3
source("algo_ce_mcmc.R", encoding = "UTF-8")
#Fonctions pour faire graphiques et tableaux
source("fonctions_graphiques.R", encoding = "UTF-8")



# 
# 
# #matrice : n lignes (i) m colonnes (j)
# 
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



meilleure_proposition <- function(matrice){
  matrice_ordre <- apply(matrice,1,rank)  
  return(apply(matrice_ordre,2,function(x){which(x==max(x))[1]})) 
}
#meilleure_proposition(modele$P_hat_liste[[100]])


