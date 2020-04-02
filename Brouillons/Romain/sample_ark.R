sample_ark <- function(x, n, probas, replace = TRUE){
  choix <- rep(0,10000*length(x))
  taille <- length(choix)
  compteur <- 1
  for(i in 1:length(x)){
    fin <- round(probas[i]*taille)
    for(j in compteur:(compteur+fin)){
      choix[j] <- x[i]
    }
    compteur <- compteur + fin
  }
  resultat = c()
  if(replace){
    for(k in 1:n){
      indice = as.integer(runif(1,1,taille+1))
      resultat = c(resultat,choix[indice])
    } 
  } else {
    sous_choix = choix
    for(k in 1:n){
      taille_sc = length(sous_choix)
      indice = as.integer(runif(1,1,taille_sc+1))
      resultat = c(resultat,sous_choix[indice])
      sous_choix = sous_choix[sous_choix != sous_choix[indice]]
    } 
  }
  resultat
}

for(i in 1:100){
  print(sample_ark(1:10,3,c(0.1111,0.1111,0.1111,0.1111,0.1111
                            ,0.1111,0.1111,0.1111,0.1111,1-0.9999),replace=FALSE))
}

for(i in 1:100){
  print(sample_ark(1:10,3,rep(0.1,10),replace=FALSE))
}
