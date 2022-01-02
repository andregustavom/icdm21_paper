DySyn <- function(ts, measure){
  
  MF <- seq(0.1,0.9,0.2)
  result <- NULL
  rD <- NULL
  
  for(ti in 1:length(MF)){
    scores <- MoSS(1000, 0.5, MF[ti])
    test.p <- scores[scores[,3]==1,1]
    test.n <- scores[scores[,3]==2,1]
    
    ifelse(measure=="sord", rQnt <- DySyn_SORD(test.p, test.n, ts), rQnt <- DySyn_DyS(test.p, test.n, ts, measure))

    rD <- c(rD, rQnt[[2]])
    result <- c(result, rQnt[[1]][1])
    
  }
  result <- round(result[which.min(rD)],2)
  result <- c(result, 1-result)
  return(list(result, min(rD)))
}

DySyn_DyS <- function(p.score, n.score, test, measure="hellinger"){
  
  alpha <- seq(0,1,by=0.01)
  
  b_sizes <- c(seq(2,20,2),30)
  
  result <- NULL
  
  for(hi in 1:length(b_sizes)){
    
    Sty_1 <- getHist(p.score, b_sizes[hi])
    Sty_2 <- getHist(n.score, b_sizes[hi])
    Uy    <- getHist(test, b_sizes[hi])
    
    vDist <- NULL
    vDistAll <- NULL
    
    f <- function(x){
      return(DySyn_distance(rbind((Sty_1*x)+ (Sty_2*(1-x)), Uy), method = measure))
    }
    
    result <- c(result, TernarySearch(0, 1, f, 1e-2))
    vDistAll <- c(vDistAll, f(result))
  } 
  
  result <- median(result)
  result <- c(result, 1 - result)
  names(result) <- c("1", "2")
  return(list(round(result,2),vDistAll[order(vDistAll)[1]]))
  
}

PNTDiff <- function(pos, neg, test, pos_prop){
  p_w <- pos_prop / length(pos)
  n_w <- (1 - pos_prop) / length(neg)
  t_w <- -1 / length(test) # repare no -1 (menos um) !!
  
  
  
  # Cria listas com (valor da observacao), peso [fixo pra cada lista])
  #p <- list(map(lambda x: (x, p_w), pos))
  p <- cbind(pos, rep(p_w, length(pos)))
  #n <- list(map(lambda x: (x, n_w), neg))
  n <- cbind(neg, rep(n_w, length(neg)))
  #t <- list(map(lambda x: (x, t_w), test))
  t <- cbind(test, rep(t_w, length(test)))
  
  
  # Concatena numa lista soh e ordena pelos valores de obervacao
  #v <- sorted(p + n + t, key = lambda x: x[0])
  v <- rbind(p, n, t)
  v <- v[order(v[,1]),]
  
  
  
  #acc <- v[0][1] # inicializa acc com o peso do primeiro elemento
  acc <- v[1,2]
  total_cost <- 0
  
  # comeca a iterar no SEGUNDO elemento (python eh 0-indexado)
  for( i in 2:nrow(v)){
    cost_mul <- v[i,1] - v[i - 1, 1] # custo da movimentacao
    total_cost <- total_cost + abs(cost_mul * acc) #movimenta o que esta acumulado
    acc <- acc + v[i,2]
  }
  
  return(total_cost)
}


DySyn_SORD <- function(p.score, n.score, test){
  
  alpha <- seq(0,1,by=0.01)
  vDist <- NULL
  vDistAll <- NULL
  f <- function(x){
    return(PNTDiff(p.score, n.score, test, x))
  }
  
  result <- TernarySearch(0, 1, f, 1e-5)
  vDist  <- f(result)
  
  result <- c(result, 1 - result)
  names(result) <- c("1", "2")
  return(list(round(result,2), vDist))
  
}


