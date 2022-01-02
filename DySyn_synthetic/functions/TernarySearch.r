TernarySearch  <- function(left, right, f, eps=1e-4){
  while(TRUE){
    if (abs(left - right) < eps) return((left + right) / 2)
    
    leftThird  <- left + (right - left) / 3
    rightThird <- right - (right - left) / 3
    
    if (f(leftThird) > f(rightThird)) 
      left <- leftThird
    else
      right <- rightThird
  }
  
}
