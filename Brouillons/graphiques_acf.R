source("./ShinyApp/global.R", encoding="UTF-8",chdir = TRUE)
set.seed(1)
m = 33
n = 33
y=sample(1:m, n, replace = FALSE)
y
modele <- lancer_algorithme_hamming(y=y, n=n, m=m,maxIters=100,stop_d=TRUE,
                                    poids_noir = 2.1,alpha = 0.7)
tableau2 <- mise_en_forme_tableau(modele,matriciel = FALSE)
tableau2

simul_permutation


sample2 <- function(x, size, prob, replace = TRUE){
  taille <- 10000*length(x)
  choix <- lapply(seq_along(x), function(i){
    rep(x[i], round(prob[i]*taille))
  })
  choix <- unlist(choix)
  if(replace){
    resultat = sapply(seq_len(size), function(i){
      indice = as.integer(runif(1,1,taille+1))
      choix[indice]
    })
    
  } else {
    resultat = vector(mode = mode(x),
                      length = size)
    sous_choix = choix
    for(k in 1:size){
      taille_sc = length(sous_choix)
      indice = as.integer(runif(1,1,taille_sc+1))
      resultat[k] = sous_choix[indice]
      sous_choix = sous_choix[sous_choix != sous_choix[indice]]
    } 
  }
  resultat
}
set.seed(1)
prob = c(1/4, 1/3, 5/12) # soit : 0.250, 0.333 et 0.417
# On retrouve les proportions :
table(sample2(x=1:3,size = 100000, prob = prob, replace = T))/100000

table(sample(x=1:3,size =100000, prob = c(1/4, 1/3, 5/12),replace = T))/100000
table(sample2(x=1:3,n =100000 , probas = c(1/4, 1/3, 5/12),replace = T))/100000

nSim= 4000
burn_in = 1000
m = 30
n = 30
lambda <- 1
coord <- 1
x_etoiles <- sample(1:m,size=n)
lag.max = 100
out<-pi_density_MCMC(nSim,lambda,x_etoiles,,m,n)
colnames(out) <- seq_len(ncol(out))
out <- out[-c(1:1000),]
portes::LjungBox(mat,lags=seq(1,30,1))
data <- t(sapply(1:1000,function(x){
  temp <- sample(1:20,size = 10)
  t <- c(sample(temp,size = 10),sample(temp,size = 10))
  t <- matrix(t,nrow =2,byrow = T)
  t
}))

colnames(data) <- 1:ncol(data)
portes::LjungBox(data)

head(data)

rep()

x <- cbind(rnorm(100),rnorm(100))
BoxPierce(out,lags=seq(5,100,5) )
VAR(out,lag.max=1)
acf <- apply(out,2,acf,plot=F, lag.max=lag.max)
signif <- 1.95/sqrt(acf[[1]]$n.used)
data_plot <- do.call(rbind,lapply(1:6,function(i){
  data.frame(acf = acf[[i]]$acf[-1],
             x = seq_len(lag.max),
             coordonnee = paste("Coordonnée",i))
}))
data_plot$coordonnee <- factor(data_plot$coordonnee)
range(data_plot$acf)
ggplot(data_plot, mapping = aes(x = x, y = acf))+
  geom_bar(stat= "identity",
           position = "identity",
           width = 0.7) +
  geom_hline(yintercept=signif, linetype = "dashed")+ 
  geom_hline(yintercept=-signif, linetype = "dashed")+
  facet_wrap(~coordonnee)
acf[[1]]$acf
acf
ggAcf(out[burn_in:dim(out)[1],2])+
  labs(title=paste('ACF'))
library(forecast)
ggplot(as.data.frame(out))+
  geom_line(aes(x = (1:nSim),y=out[,4]))+
  labs(title=paste('Trace plot'),x='',y='')
tracer_ACF <- function(nSim,lambda,x_etoiles,y,m,n,coord,burn_in){

}

?vec2var

vec2var(ca.jo(out),r=0)
mat <- matrix(rnorm(1000),ncol = 10)



          