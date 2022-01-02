getScore_using_K_folds <- function(dts, alg, k){
  nn <- dimnames(dts)[[2]]
  id_c <- which(nn=="class")
  if(id_c != ncol(dts)){
    nn <- c(nn[-id_c], nn[id_c])
    dts <- as.data.frame(dts)
    dts <- cbind(dts[,-1], dts[,1])
    names(dts) <- nn
  }
  dts$class <- as.factor(dts$class)
  rownames(dts) <- 1:nrow(dts)
  if(length(which(table(dts$class) < 10))>0) return(getScore_using_LeaveOneOut(dts, alg))
  
  folds <- createFolds(dts$class, k=k)
  
  result <- matrix(nrow = nrow(dts), ncol = 3)
  for(i in 1:k){
    classifier <- construcClassifiers(dts[-folds[[i]],], alg)
    re <- applyModel(classifier, dts[folds[[i]],], prob=T)
    result[folds[[i]],] <- cbind(re[,order(as.numeric(colnames(re)))],dts$class[folds[[i]]])
  }
  
  return(result)
}

