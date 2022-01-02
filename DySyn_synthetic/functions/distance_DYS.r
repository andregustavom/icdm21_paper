distance_DYS <- function (x, method= "hellinger", p = 0.5){ 
  
  if(method=="topsoe.D"){
    
    return(D.Topsoe(x[1,], x[2,]))
  }
  
  if (method == "ord") {
    x_dif <- x[1,]-x[2,]
    acum <- 0
    aux <- 0
    for(i in 1:length(x_dif)){
      aux <- x_dif[i]+aux
      acum <- acum+aux
    }
    return(abs(acum))
  }
  
  return(as.numeric(distance(x, method = method, p=p)))
  
}


