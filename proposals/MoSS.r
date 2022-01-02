MoSS <- function(n, alpha, m){
  
  p.score <- runif(n*alpha)**m
  n.score <- 1 - runif( round(n*(1-alpha), digits=0) )**m
  scores  <- cbind(c(p.score, n.score),c(rep(1,length(p.score)), rep(2,length(n.score))))
  scores  <- cbind(scores[,1], scores[,1], scores[,2])
  return(scores)
  
}