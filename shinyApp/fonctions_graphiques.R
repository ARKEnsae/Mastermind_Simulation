### Dessiner histogramme

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
  
  if(!is.null(modele$indice_stop)){
    i <- modele$indice_stop
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
      lambda=unlist(lapply(1:i,function(k){modele$lambda_hat_liste[[k]]})),
      score_xstar= unlist(lapply(1:i,function(k){score(modele$x_star_hat_liste[[k]], modele$parametres$y)}))
    )
  }
  
  
  
  return(tableau)
  
}

#tab <- tableau_bilan(modele)

mise_en_forme_tableau <- function(modele,matriciel=TRUE){
  
  tableau <- tableau_bilan(modele,matriciel)
  
  parametres <- paste0(#" : ",
    "n = ", modele$parametres$n, " / ",
    "m = ", modele$parametres$m, " / ",
    "N = ", modele$parametres$N, " / ",
    "rho = ", modele$parametres$rho, " / ",
    "alpha = ", modele$parametres$alpha, " / ",
    "smoothing = ", ifelse(modele$parametres$smoothing,"oui","non"), " / ",
    "avec remise = ", ifelse(modele$parametres$avec_remise,"oui","non"), " / ",
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
guesscell7 <- 'guesscell7'
guesscell8 <- 'guesscell8'

itercell1 <- 'itercell1'
itercell2 <- 'itercell2'
itercell3 <- 'itercell3'
itercell4 <- 'itercell4'
itercell5 <- 'itercell5'
itercell6 <- 'itercell6'
itercell7 <- 'itercell7'
itercell8 <- 'itercell8'

xstarcell1 <- 'xstarcell1'
xstarcell2 <- 'xstarcell2'
xstarcell3 <- 'xstarcell3'
xstarcell4 <- 'xstarcell4'
xstarcell5 <- 'xstarcell5'
xstarcell6 <- 'xstarcell6'
xstarcell7 <- 'xstarcell7'
xstarcell8 <- 'xstarcell8'

jsDrawCircle <-
  "shinyjs.drawCircle = function(args){var id = args[0]; var code_color = args[1]; console.log(id); var canvas = document.getElementById(id); console.log(canvas); var ctx = canvas.getContext('2d'); ctx.beginPath(); ctx.arc(10, 10, 10, 0, Math.PI * 2, true); ctx.fillStyle = code_color; ctx.fill(); ctx.closePath(); ctx.stroke();}"


jsClearCircle <-
  "shinyjs.clearCircle = function(args){var id = args[0]; console.log(id); var canvas = document.getElementById(id); var ctx = canvas.getContext('2d'); ctx.beginPath(); ctx.clearRect(0, 0, canvas.width, canvas.height); ctx.closePath(); ctx.stroke();}"

