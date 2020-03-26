library(plot3D)


matrice <- P_hat_liste
indice <- 100
colors <- c("#0000FF","#00FF00","#FF3232","#FFFF00","#CF00CF","#FFCFFF","#00FFFF","#008F00","#FF4F4F","#000000") 


dessiner_histo <- function(liste,indice,colors){
  matrice=liste_matrice[[indice]]
  n = c(1:nrow(matrice))
  m = c(1:ncol(matrice))
  #z = matrix(runif(length(x)*length(y),min=0,max=1), nrow=5, ncol=5, byrow=TRUE)
  hist3D(n, m, matrice, zlim=c(0,1), colvar = m, col = colors[ncol(matrice)],
         theta=50, phi=40, axes=TRUE,label=TRUE, nticks=5, ticktype="detailed", space=0.5, lighting=TRUE, light="diffuse", shade=0.5)
  
}

?hist3D
