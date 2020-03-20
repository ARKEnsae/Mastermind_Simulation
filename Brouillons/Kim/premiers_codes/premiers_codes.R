
##### Parametres
set.seed(1)
m= 6
n = 5

########### Fonctions
couleurs <- sprintf("Couleur%i", seq_len(m))
couleurs

# Retourne le nombre de fiches blanches et le nombre de fiches noires :
# Fiches noires = nombre de fiches bien placées
# Nombre de fiches de la bonne couleur mais mal placées
reponse <- function(proposition, jeu){
  c("Fiches noires" = nb_fiches_noires(proposition, jeu),
    "Fiches blanches" = nb_fiches_blanches(proposition, jeu))
}
nb_fiches_noires <- function(proposition, jeu){
  sum(proposition == jeu)
}
nb_fiches_blanches <- function(proposition, jeu){
  # On enlève les bien placés
  sous_prop <- proposition[proposition != jeu]
  sous_jeu <- jeu[proposition != jeu]
  if(length(sous_prop) == 0)
    return(0)
  # Pour chaque couleur de sous_prop, on regarde si elle est dans jeu
  mal_places <- sapply(sous_prop, function(x){
    length(grep(x,sous_jeu))>0
  })
  sum(mal_places)
}
# Fonction utilisée pour calculer la performance :
# une fiche noire compte double car c'est plus important
score <- function(proposition, jeu){
  resultat <- reponse(proposition, jeu)
  resultat["Fiches noires"] * 2 + resultat["Fiches blanches"]
}




####### Application fonctions

jeu <- sample(couleurs, n, replace = TRUE)
jeu

proposition <- sample(couleurs, n, replace = TRUE)
proposition

resultat <- reponse(proposition, jeu)
resultat
score(proposition, jeu)
