#------------------------------------------------------------------------------------------------------------------------------------------
# return the TPR and FPR estimated for a set of thresholds in a given validation set of scores
# thresholds vary from 0.01 to 0.99 with 0.01 step
getTPRandFPRbyThreshold <- function(validation_scores)
{
  
  unique_scores <- seq(0.01,0.99,0.01)
  arrayOfTPRandFPRByTr = NULL
  for (threshold in unique_scores)
  {
    total_positive = length(which(validation_scores[,3]==1))
    total_negative = length(which(validation_scores[,3]==2))
    fp = length(which(validation_scores[,1]>threshold & validation_scores[,3]==2)) 
    tp = length(which(validation_scores[,1]>threshold & validation_scores[,3]==1))
    tpr = tp/total_positive
    fpr = fp/total_negative
    
    threshold = format(round(threshold, 2), nsmall = 2)
    
    arrayOfTPRandFPRByTr = rbind(arrayOfTPRandFPRByTr,c(threshold,tpr,fpr))
  }
  
  colnames(arrayOfTPRandFPRByTr) <- c('tr','tpr','fpr')
  
  return (arrayOfTPRandFPRByTr)
}
