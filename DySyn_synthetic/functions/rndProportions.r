rndProportions <- function(bins, sample_size){
  
  if(bins < 2) return(sample_size)
  
  weigths <- round(runif(bins),2)
  #weigths <- sample(seq(0,1,0.01), bins)
  sum_weigths <- sum(weigths)
  quantities <- floor(sample_size*weigths/sum_weigths)
  total <- sum(quantities)
  
  lack <- sample_size - total
 
 
  lack <- round(lack,0)
  for(i in 1:lack){
    p <- sample(1:bins,1)
    quantities[p] <- quantities[p] + 1
  }
 
  
  return(quantities)
}