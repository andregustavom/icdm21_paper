apply.qntMethod <- function(qntMethod, 
                            p.score,
                            n.score,
                            test, 
                            TprFpr = NULL, 
                            thr = NULL,
                            measure="hellinger",
                            train = NULL,
                            dts_id,
                            db_te){
  
  if(qntMethod == "CC")
    return(mlquantify::CC(test))
  
  if(qntMethod == "PCC")
    return(mlquantify::PCC(test))
  
  if(qntMethod == "ACC")
    return(mlquantify::ACC(test, TprFpr))
  
  if(qntMethod == "PACC")
    return(mlquantify::PACC(test, TprFpr))
  
  if(qntMethod == "HDy-LP")
    return(mlquantify::HDy_LP(p.score, n.score, test))
  
  if(qntMethod == "DyS")
    return(mlquantify::DyS(p.score, n.score, test, measure = measure))
  
  if(qntMethod == "SORD")
    return(mlquantify::SORD(p.score, n.score, test))
  
  if(qntMethod == "DySyn")
    return(DySyn(test, measure)[[1]])
  
  if(qntMethod == "MS")
    return(mlquantify::MS(test, TprFpr))
  
  if(qntMethod == "MS2")
    return(mlquantify::MS2(test = test, TprFpr = TprFpr))
  
  if(qntMethod == "T50")
    return(mlquantify::T50(test, TprFpr))
  
  if(qntMethod == "X")
    return(mlquantify::X(test, TprFpr))
  
  if(qntMethod == "MAX")
    return(mlquantify::MAX(test, TprFpr))
  
  if(qntMethod == "EMQ"){
    test <- as.data.frame(cbind(test,1-test))
    names(test) <- c("1", "2")
    return(mlquantify::EMQ(train = train, test = test))
  }
  if(qntMethod == "SMM")
    return(mlquantify::SMM(p.score = p.score, n.score = n.score, test = test))
  
  if(qntMethod == "QT"){
    
    #db_te$class <- as.factor(db_te$class)
    nn <- names(db_te)
    nn <- c(nn[-1], nn[1])
    db_te <- cbind(db_te[,-1], db_te[,1])
    names(db_te) <- nn
    db_te$class <- factor(db_te$class, levels=sort(unique(train$class)))
    
    write.arff(db_te, paste0("./test_QT/test_set_", dts_id,".arff"))                  
    command <- paste0("java  -Xmx5G -cp quantify.jar:weka.jar:. weka.classifiers.trees.RandomForest -l ./models_train_test/",dts_id, "/", paste0("classifier_RF_QT"),  
                      " -T ", "./test_QT/test_set_", dts_id,".arff")          
    system(paste0(command," > ", "./test_QT/re_", dts_id,".txt"))          
    x <- read.delim(paste0("./test_QT/re_", dts_id,".txt"))      
    
    pos <- as.vector(na.omit(as.numeric(strsplit(as.character(x[4,])," ")[[1]])))[1]          
    pos <- pos + as.vector(na.omit(as.numeric(strsplit(as.character(x[5,])," ")[[1]])))[1]          
    pos <- pos/nrow(db_te)          
    freq_PRE <- as.data.frame(round(cbind(pos, 1-pos),2))         
    names(freq_PRE) <- c("1", "2")
    return(freq_PRE)
  }
  
  print("ERROR - Quantification method was not applied!") 
  return(NULL)
}

#----------------------------------------------------------------------------------------------------------------------
