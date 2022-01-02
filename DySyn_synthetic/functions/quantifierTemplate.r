apply.qntMethod <- function(qntMethod, 
                            p.score,
                            n.score,
                            test, 
                            TprFpr = NULL, 
                            thr = NULL,
                            measure="hellinger"){
  
  if(qntMethod == "CC")
    return(mlquantify::CC(test = test, thr = thr))
  
  if(qntMethod == "PCC")
    return(mlquantify::PCC(test))
  
  if(qntMethod == "ACC")
    return(mlquantify::ACC(test = test, TprFpr = TprFpr, thr = thr))
  
  if(qntMethod == "T50")
    return(mlquantify::T50(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "X")
    return(mlquantify::X(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "MAX")
    return(mlquantify::MAX(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "PACC")
    return(mlquantify::PACC(test = test, TprFpr = TprFpr, thr=thr))
  
  if(qntMethod == "HDy")
    return(Syn_HDy(p.score = p.score, n.score = n.score, test = test))
  
  if(qntMethod == "DySyn")
    return(DySyn(ts = test, measure))

  if(qntMethod == "HDy-LP")
    return(mlquantify::HDy_LP(p.score = p.score, n.score = n.score, test = test))
  
  if(qntMethod == "DyS")
    return(mlquantify::DyS(p.score = p.score, n.score = n.score, test = test, measure = measure))

  if(qntMethod == "SORD")
    return(mlquantify::SORD(p.score = p.score, n.score = n.score, test = test))
  
  if(qntMethod == "MS")
    return(mlquantify::MS(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "MS2")
    return(mlquantify::MS2(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "SMM")
    return(mlquantify::SMM(p.score = p.score, n.score = n.score, test = test))
  
  if(qntMethod == "SMM_MS_CO")
    return(SMM_MedianSweep_concat(p.score, n.score, test))
    
  print("ERROR - Quantification method was not applied!") 
  return(NULL)
}

#----------------------------------------------------------------------------------------------------------------------
