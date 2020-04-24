# Mastermind_Simulation  <img src="Rapport/img/LOGO-ENSAE.png" align="right" alt=""  width="100"/>

Projet ENSAE 2020

Cours "Simulation et Monte-Carlo"

Kim Antunez, Romain Lesauvage, Alain Quartier-la-Tente

- Application shiny : https://antuki.shinyapps.io/mastermind

- Rapport et code : https://arkensae.github.io/Mastermind_Simulation/Rapport/Rapport.html

- Slides : https://arkensae.github.io/Mastermind_Simulation/Rapport/KA_RL_AQLT_Mastermind_slides.pdf

- [Cliquer ici pour le télécharger une compilation du code](https://arkensae.github.io/Mastermind_Simulation/Code_Mastermind.R)

Pour lancer l'application depuis R :

```r
library(shiny)
runGitHub("Mastermind_Simulation", "ARKEnsae", subdir = "shinyApp")
```

## Sujet 

Le Mastermind est un jeu à deux joueurs, où le premier joueur choisit un “code” (une séquence de n fiches de couleur, parmi m couleurs possibles), et le second joueur doit deviner ce code en un minimum de coups. A chaque coup, le second joueur propose un code, et le premier joueur doit lui donner...  

a. le nombre de fiches bien placées (par ex. une fiche noire au troisième emplacement);
b. et le nombre de fiches de la bonne couleur, mais mal placées.

Ce qu'il faut faire : 

1. Mettre en oeuvre un algorithme basé sur la méthode CE (Cross-Entropy) pour retrouver en un faible nombre de coups le code choisi par le premier joueur. Bien détailler  

  a. la fonction score choisie;  
  b. la famille paramétrique choisie (pour simuler des codes);  
  c. la méthode pour simuler une loi de cette famille;  
  d. la méthode utilisée pour estimer le paramètre “optimal” à chaque étape. (On pourra essayer plusieurs valeurs pour m et n; les valeurs standards sont m = 6, et n = 4).

2. Supposons maintenant que le premier joueur doit forcément choisir comme code une permutation (i.e. chaque couleur ne peut apparaître qu’une seule fois; donc *m* ≥ *n*). On propose d’adapter l’approche de la question précédente de cette façon: On choisit aléatoirement la première fiche de la même façon que dans la question 2; puis on choisit la seconde fiche de la même façon, mais conditionellement au fait qu’elle a une couleur différente de la première; et ainsi de suite. Mettre en oeuvre l’algorithme précédent. Est-ce que la méthode d’estimation utilisée dans la question précédente est toujours valide? Commenter.


3. Considérons désormais la loi suivante sur l’ensemble des permutations:

*π*(*x*) *α* *e**x**p*( − λ d(x,x \* ))



où *λ* &gt; 0 , et d(x,x \* ) est la distance de Hamming (nombre de positions
où les deux permutations *x* et x \* diffèrent). Proposer un algorithme de MCMC pour simuler selon une telle loi. (Conseil: considérer un mécanisme de proposition simple de type: inverser deux éléments de la permutation). Utiliser cette algorithme au sein d’une approche CE, et comparer la performance de l’algorithme obtenu à l’algorithme proposé en question 1 (qui ne cherche pas à simuler des permutations). Pour l’estimation des paramètres *λ* et x\*, une méthode ad-hoc est tout à fait acceptable !

## Instructions 

- Présentation orale de 15 minutes devant les autres étudiants.
- Rendre le jour de la soutenance un document contenant certains graphiques et résultats et envoyer le programme au chargé de TD
- S'inspirer d'internet et de la littérature scientifique mais **citer ses sources**
- **Point Essentiel** : toujours évaluer (d’une façon ou d’une autre: intervalles de confiance, box-plots, etc.) l’erreur de Monte Carlo de vos résultats. Dans le cas du MCMC, pensez aussi aux ACF (graphe de la fonction d’autocorrélation pour chaque composantes) et aux “traces” (valeur de chaque composante en fonction du temps, notamment pour déterminer le burn-in). Faites preuve d’un esprit scientifique!
- Les questions bonus sont facultatives: elles sont réservées aux étudiants qui veulent s’investir plus dans leur projet. Leur bonne résolution sera récompensée par une meilleure note, mais uniquement si le reste du projet a été bien traité.



