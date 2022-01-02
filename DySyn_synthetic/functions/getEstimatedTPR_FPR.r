#------------------------------------------------------------------------------------------------------------------------------------------
getEstimatedTPR_FPR <- function(dts, alg, k){
  rownames(dts) <- 1:length(dts$class)
  
  if(length(which(table(dts$class) < 10))>0) k <- min(table(dts$class))
  
  folds <- createFolds(dts$class, k=k)
  vfpr <- NULL
  vtpr <- NULL
  
  for(i in 1:k){
    
    classifier <- construcClassifiers(dts[-folds[[i]],], alg)
    
    re <- applyModel(classifier, dts[folds[[i]],])
    
    cm <- confusionMatrix(as.factor(re), as.factor(dts$class[folds[[i]]]))
    
    fpr <- 1-cm[[4]][[2]]
    tpr <- cm[[4]][[1]]
    
    vfpr <- c(vfpr, fpr)
    vtpr <- c(vtpr, tpr)
    
  }
  TprFpr <- data.frame(tpr=mean(vtpr), fpr= mean(vfpr))
  #print(TprFpr)
  return(TprFpr)
}
