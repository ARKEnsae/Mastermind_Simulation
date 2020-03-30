library(DEoptim)
library(optimization)
?optim_nm
# Dans notre cas, k=n+1
# fonction générique
somme_a_minimiser <- function(x, bdd){{
  lambda <- x[1]
  x_star <- x[-1]
  lambda * sum(sapply(bdd,hamming, x_star = x_star))
}}
f_a_minimiser <- function(x){
  somme_a_minimiser(x, bdd = bdd)
}
optim_nm(fun = hi, k = n+1, start = seq(0,n))
# En fait ne va pas marcher :
#https://dirkschumacher.github.io/rmpk/index.html
# library(rmpk)
# library(ROI.plugin.glpk)
# set.seed(42)
# solver <- ROI_solver("glpk")
# v <- rnorm(10)
# w <- rnorm(10)
# model <- MIPModel(solver)
# x <- model$add_variable(type = "binary", i = 1:10)
# model$set_objective(sum_expr(v[i] * x[i], i = 1:10), sense = "max")
# model$add_constraint(sum_expr(w[i] * x[i], i = 1:10) <= 10)
# model$optimize()
# model$get_variable_value(x[i])

#https://github.com/dirkschumacher/ompr

library(ompr)
library(magrittr)
# ?add_variable
# bdd : list avec toutes les échantillons
result <- MIPModel() %>%
  add_variable(x_star[i],
               i=1:n,
               type = "integer",lb = 1, ub = m) %>%
  add_variable(lambda, type = "continuous", lb = 0) %>%
  set_objective(sum_expr(lambda * hamming(bdd[[i]],x_star), i = 1:M), "max")
# n <- 5
# result <- MIPModel() %>%
#   add_variable(x_star[i], i = 1:n)
# mieux le faire pour ne pas rajouter des contraintes
compteur <- 0
for(i in seq_len(n-1)){
  for(j in seq(from = i+1, to = n, by = 1)){
    result <- result %>%
      add_constraint(((x_star[i] > x_star[j])|(x_star[i] < x_star[j]))>=1, i = 1, j = 2)
      
  }
}
result <- result %>% 
  solve_model(with_ROI(solver = "glpk")) 
get_solution(result, lambda)
get_solution(result, x_star[i])
