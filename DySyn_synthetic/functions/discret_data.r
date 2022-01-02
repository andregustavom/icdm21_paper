#-----------------------------------------------------------------------------------------------------------------------------

discret_data_normalized <- function(x, inter){
  re <- rep((1/(length(inter)-1)),length(inter)-1)
  for(i in 2:length(inter)){
    re[i-1] <- (re[i-1] + length(which(x >= inter[i-1] & x < inter[i])))/(length(x)+1)
  }
  return(re)
  
}

#-----------------------------------------------------------------------------------------------------------------------------
discret_data <- function(x, inter){
  re <- NULL
  for(i in 2:length(inter)){
    re <- c(re, length(which(x >= inter[i-1] & x < inter[i]))/length(x) )
  }
  return(re)
  
}

#-----------------------------------------------------------------------------------------------------------------------------
getHist <- function(sc_1, sc_2, sc_t, measure, nbins){
  
  dist_corr <- c("harmonic_mean", "pearson", "neyman", "squared_chi", "prob_symm", "divergence", "clark", "additive_symm", "taneja", "kumar-johnson", 
                 "kullback-leibler", "jeffreys", "k_divergence", "topsoe", "jensen-shannon", "jensen_difference", "topsoe.D")
  
  breaks <- seq(0,1,length.out = nbins+1)
  breaks <- c(breaks[-length(breaks)], 1.1)
  
  Sty_1 <- discret_data(sc_1,breaks)
  Sty_2 <- discret_data(sc_2,breaks)
  Uy    <- discret_data(sc_t,breaks)
  
  if(length(which(dist_corr%in%measure))){
      Sty_1 <- discret_data_normalized(sc_1,breaks)
      Sty_2 <- discret_data_normalized(sc_2,breaks)
      Uy    <- discret_data_normalized(sc_t,breaks)
  }  
  
  return(list(Sty_1, Sty_2, Uy))
  
}
#-----------------------------------------------------------------------------------------------------------------------------
discret_data_percentil <- function(x, inter){
  
  re <- NULL
  
  re <- c(re, length(which(x <= inter[1]))/length(x))
  
  for(i in 2:length(inter)) re <- c(re, length(which(x > inter[i-1] & x <= inter[i]))/length(x) )
  
  re <- c(re, length(which(x > inter[length(inter)]))/length(x))
  return(re)
  
}
#-----------------------------------------------------------------------------------------------------------------------------
getHist_percentil <- function(sc_1, sc_2, sc_t, measure, nbins ){
  nbins <- nbins-1
  
  dist_corr <- c("harmonic_mean", "pearson", "neyman", "squared_chi", "prob_symm", "divergence", "clark", "additive_symm", "taneja", "kumar-johnson", 
                 "kullback-leibler", "jeffreys", "k_divergence", "topsoe", "jensen-shannon", "jensen_difference")
  
  sc_t    <- sc_t+t(rand(n=length(sc_t),m=1))*0.001
  
  breaks  <- as.numeric(quantile(sc_t, probs = seq(0,1,length.out = nbins)))
  
  #breaks  <- c(breaks[-length(breaks)],1.1)

  Sty_1   <- discret_data_percentil(sc_1,breaks)
  
  Sty_2   <- discret_data_percentil(sc_2,breaks)
  
  Uy      <- discret_data_percentil(sc_t,breaks)
  
  return(list(Sty_1, Sty_2, Uy))
}





