
prepare_dataset <- function(exp_i){
  
  dts       <- list.files("./datasets/")[exp_i]
  db_dir    <- paste0("./models_train_test/", dts)
  dtsConf   <- datasetsConf(dts)
  
  print(paste0("Preparing dataset ----->>>> ",dts))
  
  dts       <- strsplit(dts, ".csv")[[1]][1]
  dbi       <- dtsConf[[1]]
  pos_class <- dtsConf[[2]]
  neg_class <- dtsConf[[3]]
  n_class <- unique(dbi$class)
  dbi     <- getBatch(dbi, n_class, c(0.33,0.33,0.33), min(table(dbi$class)))[[1]]
  
  if(!dir.exists("./models_train_test/"))
    dir.create(paste0("./models_train_test/"))
  
  if(!dir.exists(db_dir)){
    dir.create(paste0("./models_train_test/", dts))
  }else{
    print(paste0("The dataset ", dts," was already preprocessed and it is ready for the experiments."))
    return(-1)
  }
  
  idx <- sample(1:nrow(dbi), nrow(dbi))
  db <- dbi[idx,] #shuffle the dataset
  folds <- createFolds(db$class, k=2)
  
  saveRDS(db, paste0("./models_train_test/", dts,"/dataset.rds"))
  saveRDS(folds, paste0("./models_train_test/", dts,"/folds.rds"))
  saveRDS(idx, paste0("./models_train_test/", dts,"/idx.rds"))
  saveRDS(pos_class, paste0("./models_train_test/", dts,"/pos_class.rds"))
  saveRDS(neg_class, paste0("./models_train_test/", dts,"/neg_class.rds"))
  
  train <- db[folds[[1]],]
  test  <- db[as.vector(unlist(folds[-1])),]
  ifelse(min(table(train$class)) < 1000, train_size <- min(table(train$class)), train_size <- 1000)
  train.info <- getBatch(train, unique(train$class), rep(1, length(unique(train$class)))/length(unique(train$class)), train_size*length(unique(train$class)))
  
  train <- train.info[[1]]
  
  vc <- train$class
  pos <- which(vc==pos_class)
  vc[pos] <- 1
  vc[-pos] <- 2
  
  nn <- names(train)
  train.b <- cbind(vc, train[,-1])
  names(train.b) <- nn
  
  #train.b<- apply(train.b, 2,as.numeric)
  train.b <- as.data.frame(train.b)
  train.b$class <- as.factor(train.b$class)
  
  tryCatch(classifier <- randomForest(class~., data=train.b, ntree = 200), error = function(e) { print("ERROR 1 - Classifier error!")})
  tryCatch(scores     <- getScore_using_K_folds(train.b, 10), error = function(e) { print("ERROR 2 - Scores error!")})
  tryCatch(TprFpr     <- getTPRandFPRbyThreshold(scores), error = function(e) { print("ERROR 3 - TprFpr error!")})
  
  saveRDS(classifier, paste0("./models_train_test/", dts,"/classifier_RF.rds"))
  saveRDS(scores    , paste0("./models_train_test/", dts,"/scores.rds"))
  saveRDS(TprFpr    , paste0("./models_train_test/", dts,"/TprFpr.rds"))
  
  train.b$class <- as.factor(train.b$class)
  nn <- names(train.b)
  nn <- c(nn[-1], nn[1])
  train.b <- cbind(train.b[,-1], train.b[,1])
  names(train.b) <- nn
  write.arff(train.b, paste0("./models_train_test/", dts, "/train_arff.arff"))
  command <- paste0("java -Xmx6G -cp quantify.jar:weka.jar:. weka.classifiers.trees.RandomForest -I 500 -P 1 -t ", "./models_train_test/",dts,"/train_arff", 
                      ".arff", " -d ./models_train_test/",dts, "/", paste0("classifier_RF_QT_", dts))
  system(command)
  
  
  return("Finish")
}

