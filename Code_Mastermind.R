# Packages nécessaires
# install.packages(c("clue", "kableExtra", "mvtnorm", "plot3D", "portes", "shiny", "shinyjs", "shinyWidgets", "V8"))

############################################################
########################## Packages ########################
############################################################

library(clue)
library(kableExtra)
library(mvtnorm)
library(plot3D)
library(portes)
library(shiny)
library(shinyjs)
library(shinyWidgets)


############################################################
############# Génération de codes et calcul du score #######
############################################################

# Création du vecteur à deviner
initialiser_y <- function(m, n, avec_remise = TRUE){
  # Avec remise = Q1
  # Sans remise = Q2/3
  if(!avec_remise & m<n){
    return(NULL)
    stop()
  }
  
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

############################################################
############# Mise en forme des résultats #######
############################################################

dessiner_histo <- function(liste_matrice,indice,colors){
  if(!is.null(indice)){
    matrice=liste_matrice[[indice]]
    n = c(1:nrow(matrice))
    m = c(1:ncol(matrice))
    couleurs_graphe <- t(matrix(rep(1:length(m),length(n)),nrow=length(n),ncol=length(m),byrow=TRUE))
    
    par(mar = c(0,0,0,0))
    hist3D(m, n, t(matrice), zlim=c(0,1), colvar = couleurs_graphe,
           col = colors[1:ncol(matrice)],theta=50, phi=40, axes=TRUE,label=TRUE, ticktype="detailed", space=0.5, lighting=TRUE, light="diffuse", shade=0.5, alpha=0.6, xlab="",ylab="billes",zlab="",colkey=list(plot=FALSE))
    
  }
}
tableau_bilan <- function(modele,matriciel=TRUE){
  
  if(!is.null(modele$indices$indice_arret) | !is.null(modele$indices$indice_conv)){
    i <- max(modele$indices$indice_arret,modele$indices$indice_conv)
  } else{
    i <- modele$parametres$maxIters
  }
  
  if(matriciel){
    tableau <- data.frame(
      t = 1:i,
      s_max = round(modele$s_max[1:i],3),
      gammas_hat = round(modele$gammas_hat[1:i],3),
      min = round(unlist(sapply(modele$P_hat_liste,p_min_max)["min",1:i]),4),
      max_min =round(unlist(sapply(modele$P_hat_liste,p_min_max)["max_min",1:i]),4),
      min_max =round(unlist(sapply(modele$P_hat_liste,p_min_max)["min_max",1:i]),4),
      max = round(unlist(sapply(modele$P_hat_liste,p_min_max)["max",1:i]),4)
    )
  }else{
    tableau <- data.frame(
      t = 1:i,
      s_max = round(modele$s_max[1:i],3),
      gammas_hat = round(modele$gammas_hat[1:i],3),
      lambda=round(unlist(lapply(1:i,function(k){((modele$param_liste)[[k]])$lambda})),2),
      score_x_star= unlist(lapply(1:i,function(k){score(((modele$param_liste)[[k]])$x_star, modele$parametres$y)})),
      x_star = unlist(lapply(1:i,function(k){paste(modele$param_liste[[k]]$x_star,collapse = "-")}))
    )
  }
  
  return(tableau)
  
}

mise_en_forme_tableau <- function(modele,matriciel=TRUE){
  
  tableau <- tableau_bilan(modele,matriciel)
  
  type_modele <- ifelse(!matriciel,"Loi avec distance de Hamming",ifelse(modele$parametres$avec_remise,"Tirage avec remise","Tirage sans remise"))
  
  parametres <- paste0(#" : ",
    "n = ", modele$parametres$n, " / ",
    "m = ", modele$parametres$m, " / ",
    "N = ", modele$parametres$N, " / ",
    "rho = ", modele$parametres$rho, " / ",
    "smoothing = ", ifelse(modele$parametres$smoothing,"oui","non"), " / ",
    ifelse(modele$parametres$smoothing, paste0("alpha = ", modele$parametres$alpha, " / "),""),
    "d = ", modele$parametres$d
  )
  
  
  
  convergence <- paste0("Convergence : ",
                        ifelse(!is.null(modele$indices$indice_conv),paste0("Etape n°", modele$indices$indice_conv, " (",modele$duree$duree_conv," sec.)"),"Non"),
                        " / ",
                        "Arrêt : ",
                        ifelse(!is.null(modele$indices$indice_arret),paste0("Etape n°", modele$indices$indice_arret, " (",modele$duree$duree_arret," sec.)"),"Non")
  )
  
  texty = NULL
  if(!matriciel){
    texty <- paste("\ny =",paste(modele$parametres$y,collapse = "-"))
  }
  tableau_joli <- kable(tableau, align = "c") %>%
    kable_styling(full_width = F) %>%
    footnote(general = paste0(type_modele,"\n",
                              "Parametres : ",parametres,"\n",
                              convergence,'\n',
                              "Temps de calcul total : ", modele$duree$duree_totale, " sec.",
                              texty),
             general_title = "\nNote",
             title_format = c("italic", "underline")
    )
  tableau_joli <- gsub('\\bNA\\b', '  ', tableau_joli) #remove NA
  
  return(tableau_joli)
  
}

############################################################
############# Questions 1 et 2 #######
############################################################

initialisation_sample <- function(m, n, N, P_hat = NULL, avec_remise = TRUE){
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
      if(length(couleurs_restantes)>1){
        X[it,i] <- sample(couleurs_restantes, 1,
                          prob = P_hat[i,couleurs_restantes])
      }else{
        X[it,i] <- couleurs_restantes
      }
      couleurs_restantes <- setdiff(couleurs_restantes,
                                    X[it,i])
    }
  }
  X
}

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

# Algorithme de *Cross-Entropy*
# Il faut utiliser l'option `avec_remise = FALSE` si on veut
# tester l'algorithme en ne générant que des permutations.

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
  # Listes de paramètres à agrémenter
  gammas_hat = c()
  s_max = c()
  
  indice_arret = NULL
  indice_conv = NULL
  
  ###### Algo
  
  #### début du try
  iter <- 0
  critere_arret <- TRUE
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
    gammas_hat[iter] = gamma
    s_max[iter] = s

    for(i in 1:n){
      for(j in 1:m){
        P_hat_tilde[i,j]=sum(scores>=gamma & X[,i]==j)/sum(scores>=gamma)
      }
    }
    
    # Smoothing
    if(smoothing){
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


############# Exemple d'application

#On fixe m et n et on génère un y aléatoirement.

m = 6
n = 4
set.seed(1)
y <- initialiser_y(m = m, n = n, avec_remise = TRUE) #
y

#On fait tourner l'algorithme et on affiche le tableau des résultats.


resultat <- lancer_algorithme(y = y, n = n, m = m,
                              avec_remise = TRUE,
      # TRUE si question 1, FALSE si question 2.
                              stop_d = TRUE
      #L'algorithme s'arrête si convergence
      ) 


tab1 <- tableau_bilan(resultat)
tab1
mise_en_forme_tableau(resultat)


# Histogramme de la matrice de probabilités à l'itération 4 (par exemple).

dessiner_histo(resultat$P_hat_liste, 
               4, #itération 4
               c("#0000FF","#00FF00","#FF3232",
                 "#FFFF00","#CF00CF","#FFCFFF"))

############################################################
############# Question 3 #######
############################################################



# Définition de la densité PI(X)
pi_density <- function(x,lambda,x_etoiles){
  return(exp(-lambda*sum(x != x_etoiles)))
}


# Fonction qui permet d'inverser deux éléments d'une permutation
inverse_deux_elements <- function(X, n){
  i1 <- sample(1:n, 1)
  i2 <- sample((1:length(X))[-i1],1)
  temp = X[i1]
  X[i1] <- X[i2]
  X[i2] <- temp
  return(X)
}


pi_density_MCMC <- function(numSim, lambda, x_etoiles, m,n){
  X0 <- sample(1:m,m,replace=FALSE)
  X <-matrix(rep(X0,numSim),numSim,m,byrow = T)
  for (t in (1:(numSim-1))){
    Xprop=inverse_deux_elements(X[t,], n)
    if(runif(1) < min(1,pi_density(Xprop[1:n],lambda,x_etoiles)/pi_density(X[t,1:n],lambda,x_etoiles))){
      X[t+1,]=Xprop
    }
    else{
      X[t+1,]=X[t,]
    }
  }
  return(X[,1:n])
}

modif_metro <- function(x, m, burn_in = TRUE, lag = 80){
  if(burn_in){
    x <- x[(250*m):dim(x)[1],]
  }
  x <- x[seq(1, nrow(x), lag),]
  return(x)
}

simul_permutation <- function(N, param, m,n, lag = 80){
  num <- 250*m + N * lag
  out <- pi_density_MCMC(numSim = num, lambda =  param$lambda,
                         x_etoiles = param$x_star, m = m, n = n)
  out_traite <- modif_metro(x = out, m = m, burn_in = TRUE, lag = lag)
  return(out_traite)
}


# Détermination du x* par l'algorithme hongrois #
creer_matriceF <- function(X_top,n,m){
  matriceF <- matrix(rep(0,n*m),nrow = n, ncol = m)
  for(i in 1:dim(X_top)[1]){
    x <- X_top[i,]
    for(i in 1:length(x)){
      matriceF[i,x[i]] <- matriceF[i,x[i]] + 1
    }
  }
  return(matriceF)
}

# Nouvel algorithme de *Cross-Entropy*
# L'option mle = TRUE peut-être utilisée si l'on souhaite estimer lambda par maximum de vraisemblance.

lancer_algorithme_hamming <- function(y, n, m, N = C * (n + 1), maxIters = 100,
                                      rho = 0.1, alpha = 0.7, poids_blanc = 1, 
                                      poids_noir = 2, C = 5, d = 10, stop_d = TRUE,
                                      mle = FALSE,
                                      meilleur_x_star = TRUE){
  
  duree = Sys.time()
  duree_totale = NULL
  duree_arret = NULL
  duree_conv = NULL
  
  if(m<n){
    stop()
  }
  
  # Création des paramètres initiaux
  param_liste <- list()
  P_hat_tilde <- matrix(nrow = n, ncol = m)
  param_liste <- list()
  param_liste[[1]] <- list (lambda = 1,
                            x_star = initialisation_sample(m = m, n = n, N = 1,
                                                           avec_remise = FALSE))
  # Listes à agrémenter
  gammas_hat = c()
  s_max = c()
  indice_arret = NULL
  indice_conv = NULL
  
  ###### Algo
  
  #### début du try
  iter <- 0
  critere_arret <- TRUE
  eidx = ceiling((1-rho)*N) #plus petit indice du meilleur Score.
  while(critere_arret & (iter+1)<= maxIters){
    iter <- iter + 1
    
    X <- simul_permutation(N = N, param = param_liste[[iter]],m = m,n = n)
    
    #### Calcul du score
    
    scores <- apply(X, 1, score,
                    y = y, poids_noir = poids_noir, poids_blanc = poids_blanc)
    
    scores_tries <- sort(scores)
    
    # Mise à jour de Gamma 
    gamma = scores_tries[eidx]
    s = scores_tries[N]
    X_top = X[scores>=gamma,]
    
    # Détermination du x* par l'algorithme hongrois #
    matriceF <- creer_matriceF(X_top,n,m)
    hongarian <- solve_LSAP(matriceF,maximum=TRUE)
    res <- cbind(seq_along(hongarian), hongarian)
    x_star <- 1:n
    for(i in 1:n){
      x_star[i] <- as.numeric(res[i,"hongarian"])
    }
    
    
    # Pour lambda, on le fait peu à peu tendre vers 0 (ancienne version)
    # lambda <- param_liste[[iter]]$lambda + 3*param_liste[[1]]$lambda/(maxIters+1)
    
    lambda = 1
    # Si on veut tester estimation par maximum de vraisemblance
    if(mle){
      min_loss <- sum(apply(X_top,1, function(x) sum(x != x_star)))
      gradient <- function(lambda) {
        N_top = nrow(X_top)
        p1 <- N_top * m
        sum_exp <- sapply(seq(0,m),function(k){
          (exp(lambda) - 1)^k / factorial(k)
        })
        sum_exp_t <- sum(sum_exp)
        sum_exp_tm1 <- sum(sum_exp[-length(sum_exp)])
        (sum_exp_tm1 * exp(lambda) - m* sum_exp_t)/sum_exp_t + min_loss/N_top
      }
      lambda <- tryCatch(uniroot(gradient, c(0,10))$root, error = function(e){
        print(paste0("Pour l'itération ",iter,", pas de solution pour lambda : on fixe lambda = 1"))
        1})
      
    }
    
    if(score(param_liste[[iter]]$x_star,y) >= score(x_star,y) & meilleur_x_star){
      x_star = param_liste[[iter]]$x_star
    }
    
    gammas_hat[iter] = gamma
    s_max[iter] = s
    param_liste[[iter+1]] <- list(lambda = lambda,
                                  x_star = x_star)
    
    # Critère d'arrêt quand on trouve la bonne réponse
    if(isTRUE(all.equal(score(x = x_star,y = y, poids_noir = poids_noir, poids_blanc = poids_blanc), 1)) & is.null(indice_arret)){
      indice_arret <- iter+1 # différent de l'autre fonction attention
      duree_arret <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2) 
      if(stop_d){
        critere_arret <- FALSE
      }
    }
    # Critère de convergence
    if(length(gammas_hat) > d & is.null(indice_conv)){
      gammas_d <- tail(gammas_hat,d)
      if(isTRUE(all.equal(tail(gammas_hat,1), 1))){
        indice_conv <- iter+1 # différent de l'autre fonction attention
        duree_conv <- round(as.numeric(difftime(Sys.time(), duree),units="secs"),2) 
        if(stop_d){
          critere_arret <- FALSE
        }
      }
    }
  }
  
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
        smoothing = FALSE,
        d=d,
        avec_remise = TRUE
      ),
      param_liste=param_liste,
      s_max=s_max,
      gammas_hat=gammas_hat,
      indices = list(
        indice_arret = indice_arret,
        indice_conv = indice_conv
      )
    )
    
  )
}

############# Exemple d'application

#On fixe m et n et on génère un y aléatoirement.

m = 6
n = 4
set.seed(1)
y <- initialiser_y(m = m, n = n, avec_remise = FALSE) 
y

#On fait tourner l'algorithme (version lambda = 1) et on affiche le tableau des résultats.

resultat <- lancer_algorithme_hamming(y = y, n = n, m = m,
                      # Pour s'arrêter si convergence :
                                      stop_d = TRUE)

tab2 <- tableau_bilan(resultat, matriciel = FALSE)
tab2

mise_en_forme_tableau(resultat, matriciel = FALSE)

#On fait tourner l'algorithme (version lambda = MLE) 
#et on affiche le tableau des résultats.

resultat_mle <- lancer_algorithme_hamming(y = y, n = n, m = m,
                                          stop_d = TRUE,
                                          mle = TRUE,
                 # Pour changer x* même si le score est moins bon :
                                          meilleur_x_star = FALSE)


tab3 <- tableau_bilan(resultat_mle, matriciel = FALSE)
tab3

mise_en_forme_tableau(resultat_mle, matriciel = FALSE)
