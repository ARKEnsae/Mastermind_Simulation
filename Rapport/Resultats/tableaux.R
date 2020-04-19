source("./shinyApp/global.R", encoding="UTF-8",chdir = TRUE)
set.seed(1)
m = 33
n = 33
liste_n = c(4, 6, 10, 15, 20, 30, 40)
liste_m = c(4, 6, 10, 15, 20, 30, 40)

all_models_names <- expand.grid(n = liste_n,
            m = liste_m)
m = n = 4
liste_seeds <- c(1:10)
all_models <- apply(all_models_names,1, function(x){
  n = x[1]
  m = x[2]
  print(sprintf("n = %i - m = %i", n, m))
  lapply(liste_seeds, function(seed){
    set.seed(seed)
    y <- initialiser_y(m=m,n=n, avec_remise = TRUE)
    lancer_algorithme(y = y, n = n, m = m, avec_remise = TRUE, stop_d = TRUE)
  })
})
names(all_models) <- sprintf("n = %i - m = %i", all_models_names[,1], all_models_names[,2])
all_models <- readRDS("q1.RDS")
#####
format_ind <- function(indicateur){
  indicateur <- data.frame(x = indicateur, n = all_models_names[,1], m = all_models_names[,2])
  indicateur <- reshape2::dcast(indicateur, n ~ m,value.var = "x")
  rownames(indicateur) <- indicateur[,1]
  indicateur[,-1]
}
xOrNull <- function(x){
  if(is.null(x)){
    NA
  }else{
    x
  }
}

it_conv_min <- format_ind(sapply(all_models, function(model_all_s){
  convs <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_conv))
  min(convs, na.rm = TRUE)
}))
it_conv_max <- format_ind(sapply(all_models, function(model_all_s){
  convs <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_conv))
  max(convs, na.rm = TRUE)
}))
it_conv_med <- format_ind(sapply(all_models, function(model_all_s){
  convs <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_conv))
  median(convs, na.rm = TRUE)
}))
it_arret_min <- format_ind(sapply(all_models, function(model_all_s){
  arrets <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_arret))
  min(arrets, na.rm = TRUE)
}))
it_arret_max <- format_ind(sapply(all_models, function(model_all_s){
  arrets <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_arret))
  max(arrets, na.rm = TRUE)
}))
it_arret_med <- format_ind(sapply(all_models, function(model_all_s){
  arrets <- sapply(model_all_s,function(x) xOrNull(x$indices$indice_arret))
  median(arrets, na.rm = TRUE)
}))
erreur_finale <- format_ind(sapply(all_models, function(model_all_s){
  erreur <- sapply(model_all_s,function(x) xOrNull(tail(x$gammas_hat,1)))
  mean(1-erreur)
}))
temps_arret <-format_ind(sapply(all_models, function(model_all_s){
  tps <- sapply(model_all_s,function(x) xOrNull(x$duree$duree_arret))
  mean(tps, na.rm = TRUE)
}))
temps_conv <-format_ind(sapply(all_models, function(model_all_s){
  tps <- sapply(model_all_s,function(x) xOrNull(x$duree$duree_conv))
  mean(tps, na.rm = TRUE)
}))
nb_non_conv <-format_ind(sapply(all_models, function(model_all_s){
  tps <- sapply(model_all_s,function(x) xOrNull(x$duree$duree_arret))
  sum(is.na(tps), na.rm = F)
}))
stats <- list(it_conv_min =it_conv_min,
              it_conv_max =it_conv_max,
              it_conv_med =it_conv_med,
              it_arret_min =it_arret_min,
              it_arret_max =it_arret_max,
              it_arret_med =it_arret_med,
              erreur_finale = erreur_finale,
              temps_arret = temps_arret,
              temps_conv = temps_conv,
              nb_non_conv = nb_non_conv)
saveRDS(stats,file = "statsq1.RDS")
saveRDS(all_models,file = "q1.RDS")
