
burn_in = 1000
all_models <- data.frame(n = c(4,4,10,10,20,40), m = c(4,6,20,40,40,40))
m = 40
n = 10
lambda <- 1
coord <- 1

acf_analyse <- apply(all_models,1,function(x){
  m = x[2]
  n = x[1]
  acf_all(n, m)
})
acf_analyse <- lapply(acf_analyse, function(x) x[[1]])
acf_analyse[[3]]

acf_analyse_cor <- apply(all_models,1,function(x){
  m = x[2]
  n = x[1]
  acf_s_corr(n, m, pas = 80)
})
acf_analyse_cor <- lapply(acf_analyse_cor, function(x) x[[1]])
data_corr <- cbind(acf_analyse_cor[[4]],acf_analyse_cor[[5]][,-1])
saveRDS(data_corr, file = "Resultats/Resultats/corr_n10m40n20m40.RDS")
data_corr <- readRDS("Resultats/Resultats/corr_n10m40n20m40.RDS")
note <- "(H0) : échantillons indépendants contre (H1) ils sont corrélés"
data_corr %>% 
  kable(caption = "Test de LjungBox en ne gardant qu'un échantillon sur 80") %>% 
kable_styling(full_width = F)%>%
  column_spec(1, bold = T) %>% 
  add_header_above(c(" " = 2,"n = 10, m = 40" = 3 ,"n = 20, m = 40" = 3)) %>% 
  footnote(general = note, general_title = "Note : ")

saveRDS(data_corr,file = "Resultats/Resultats/corr_n0m40")

acf_s_corr(4,6,80)[[1]]
acf_s_corr(10,20,80)[[1]]
acf_s_corr(10,40,80)[[1]]
acf_s_corr(20,40,80)[[1]]
acf_s_corr(40,40,200)[[1]]


acf_s_corr <- function(n, m, pas = 100){
  set.seed(1)
  nSim= 250* m + pas*1000
  x_etoiles <- sample(1:m,size=n)
  lag.max = 40
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,,m,n)
  colnames(out) <- seq_len(ncol(out))
  out <- out[-c(1:(250*m)),]
  out <- out[seq(1,nrow(out), pas),]

  acf <- apply(out,2,acf,plot=F, lag.max=lag.max)
  data_plot <- do.call(rbind,lapply(seq_len(4),function(i){
    data.frame(acf = acf[[i]]$acf[-1],
               x = seq_len(lag.max),
               coordonnee = paste("Composante",i))
  }))
  signif <- 1.95/sqrt(acf[[1]]$n.used)
  p <- ggplot(data_plot, mapping = aes(x = x, y = acf))+
    geom_bar(stat= "identity",
             position = "identity",
             width = 0.7) +
    geom_hline(yintercept=signif, linetype = "dashed")+ 
    geom_hline(yintercept=-signif, linetype = "dashed")+
    facet_wrap(~coordonnee) +
    labs(title = NULL,
         x = "Nombre de retards", y = "Autocorrélation")
  ggsave(sprintf("Resultats/img/acfn%im%icorr.png",n,m),
         width = 8, height = 5)
  plot(p)
  
  try(list(round(portes::LjungBox(out,lags=seq(1,20,1)),3)))
}
acf_all <- function(n, m){
  set.seed(1)
  nSim= 250* m + 5000
  x_etoiles <- sample(1:m,size=n)
  lag.max = 100
  out<-pi_density_MCMC(nSim,lambda,x_etoiles,,m,n)
  colnames(out) <- seq_len(ncol(out))
  out <- out[-c(1:(250*m)),]

  acf <- apply(out,2,acf,plot=F, lag.max=lag.max)
  data_plot <- do.call(rbind,lapply(seq_len(4),function(i){
    data.frame(acf = acf[[i]]$acf[-1],
               x = seq_len(lag.max),
               coordonnee = paste("Composante",i))
  }))
  signif <- 1.95/sqrt(acf[[1]]$n.used)
  p <- ggplot(data_plot, mapping = aes(x = x, y = acf))+
    geom_bar(stat= "identity",
             position = "identity",
             width = 0.7) +
    geom_hline(yintercept=signif, linetype = "dashed")+ 
    geom_hline(yintercept=-signif, linetype = "dashed")+
    facet_wrap(~coordonnee)  +
    labs(title = NULL,
         x = "Nombre de retards", y = "Autocorrélation")
  ggsave(sprintf("Resultats/img/acfn%im%i.png",n,m),
         width = 8, height = 5)
}

