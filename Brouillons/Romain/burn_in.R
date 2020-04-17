#source("./ShinyApp/global.R", encoding="UTF-8",chdir = TRUE)
library(ggplot2)
library(gridExtra)
set.seed(1)


pi_density_MCMC <- function(numSim, lambda, x_etoiles,y, m,n){
  X0 <- sample(1:m,m,replace=FALSE)
  X <-matrix(rep(X0,numSim),numSim,m,byrow = T)
  for (t in (1:(numSim-1))){
    Xprop=inverse_deux_elements(X[t,])
    if(runif(1) < min(1,pi_density(Xprop[1:n],lambda,x_etoiles)/pi_density(X[t,1:n],lambda,x_etoiles))){
      X[t+1,]=Xprop
    }
    else{
      X[t+1,]=X[t,]
    }
  }
  return(X[,1:n])
}

m = 4
n = 4
y=sample(1:m, n, replace = FALSE)
x_etoiles=sample(1:m, n, replace = FALSE)

tracer_Trace <- function(nSim,lambda,x_etoiles,y,m,n,min=1){
    out<-pi_density_MCMC(nSim,lambda,x_etoiles,y,m,n)
    plot1 <- ggplot(as.data.frame(out[-c(1:min),]))+
      geom_line(aes(x = ((min+1):nSim),y=out[-c(1:min),1]))+
      labs(title=paste('Trace 1ère coordonnée'),x='',y='')
    plot2 <- ggplot(as.data.frame(out[-c(1:min),]))+
      geom_line(aes(x = ((min+1):nSim),y=out[-c(1:min),2]))+
      labs(title=paste('Trace 2ème coordonnée'),x='',y='')
    plot3 <- ggplot(as.data.frame(out[-c(1:min),]))+
      geom_line(aes(x = ((min+1):nSim),y=out[-c(1:min),3]))+
      labs(title=paste('Trace 3ème coordonnée'),x='',y='')
    plot4 <- ggplot(as.data.frame(out[-c(1:min),]))+
      geom_line(aes(x = ((min+1):nSim),y=out[-c(1:min),4]))+
      labs(title=paste('Trace 4ème coordonnée'),x='',y='')
    
    grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow = 2)
}

tracer_Trace(250*m+100*(n+1),1,x_etoiles,y,m,n)
# 5/5 1000
# 20/20 5000
# 40/40 10000
# 60/60 15000
