source("./load_libraries.r")
library(RWeka)

run_scorerEvaluation_real <- function(exp_i){

  if(!dir.exists("./icdm_submitted/"))
    dir.create("./icdm_submitted/")
  
  if(!dir.exists("./test_QT/"))
    dir.create("./test_QT/")
  
  
  dts       <- list.files("./models_train_test/")[exp_i]
  v_per     <- seq(0,1,0.05) #range for positive class variability
  v_per_n   <- seq(0,1,0.25) #range for subclasses variability
  te_size   <- 100 # size of each test set
  runs      <- 10 # replications
  
  # These distances are parameters for DyS framework. We used the topsoe, reported by the authors as the best one.
  vdist        <- c("topsoe", "jensen_difference", "prob_symm", "ord", "sord")
  names(vdist) <- c("TS", "JD", "PS", "ORD", "SORD")

  #Quantification algorithm that will run for each dataset
  counters     <- c("DySyn-TS", "DySyn-SORD", 
                    "HDy-LP", "DyS-TS", "MS", "T50", 
                    "MAX", "X", "CC", "PCC", "ACC", "PACC", "SORD", "EMQ", "SMM", "MS2", "QT")

  # We saved in a separated directory for each dataset the following itens:
  # The classifier (Random Forest) - classifier_RF.rds file
  # The QT classifier - classifier_RF_QT.rds file
  # The dataset file in rds format - dataset.rds file
  # Indexes that define training and test sets - folds.rds
  # neg_class.rds file, containing the negative classes (according to Table I)
  # pos_class.rds file, containing the positive classes (according to Table I)
  # TprFpr.rds file, containing the tpr and fpr for all threshold estimated using cross-validation from training partition
  # scores.rds file, containing scores estimated for each positive and negative classes estimated using cross-validation from training partition
  # train_arff.arff, it is the training partition required as an entry for QT .jar, provided by the QT's authors
  for(i in 1:length(dts)){
    print(paste0("Quantifying ----->>>> ",dts[i]))
    db_dir <- paste0("./models_train_test/", dts[i])
    dbi    <- readRDS(paste0("./models_train_test/", dts[i],"/dataset",".rds"))

    load_files <- T
    if(!dir.exists(db_dir)){
      print(paste0("Artefacts were not found! Please run this function: prepare_dataset(",i,")"))
      return(-1)
    }
    folds<- readRDS(paste0("./models_train_test/", dts[i],"/folds",".rds"))
    idx  <- readRDS(paste0("./models_train_test/", dts[i],"/idx",".rds"))
    pos  <- readRDS(paste0("./models_train_test/", dts[i],"/pos_class",".rds"))
    neg  <- readRDS(paste0("./models_train_test/", dts[i],"/neg_class",".rds"))
    db   <- dbi[idx,]
    db_te<- db[idx,]
    db_tr<- db[folds[[1]],]
    db_te<- db_te[folds[[2]],]
   
    classifier<- readRDS(paste0("./models_train_test/", dts[i],"/classifier_RF.rds"))
    scores    <- readRDS(paste0("./models_train_test/", dts[i],"/scores.rds"))
    TprFpr    <- readRDS(paste0("./models_train_test/", dts[i],"/TprFpr.rds"))
    TprFpr    <- apply(TprFpr, 2, as.numeric)
    colnames(TprFpr) <- c("thr", "tpr", "fpr")

    subClDist <- 1:length(unique(db_te$class))
    names(subClDist) <- unique(db_te$class)
    
    for(pp in v_per){
      results <- NULL
      test_sample_pos <- pp*te_size
      test_sample_neg <- te_size - test_sample_pos
      
      for(ps in v_per_n){
        print(paste0(pp," ----- ",ps))
        for(k in 1:runs){
          ppx <- sample(seq(0,1,0.1),1)
          
          if(length(pos)==1){
            tep <- getBatch(db_te, c(pos, pos), c(ppx,1-ppx), test_sample_pos)[[1]]
          }else{ tep <- getBatch(db_te, pos, c(ppx,1-ppx), test_sample_pos)[[1]]}
          
          if(length(neg) > 2){
              quantities  <- rndProportions(length(neg), test_sample_neg)
              ten <- getBatch(db_te, neg, quantities/sum(quantities), test_sample_neg)[[1]]  
          }else{
            if(length(neg)==1)
              ten <- getBatch(db_te, c(neg,neg), c(1-ps,ps), test_sample_neg)[[1]]   
            else ten <- getBatch(db_te, neg, c(1-ps,ps), test_sample_neg)[[1]]   
          }
          
          te    <- rbind(tep, ten)
          vc    <- te$class
          pi    <- which(vc%in%pos)
          vc[]  <- 2
          vc[pi]<- 1
          nn    <- names(te)
          te    <- cbind(vc, te[,-1])
          
          names(te) <- nn
          te$class  <- as.factor(te$class)
          te        <- as.data.frame(te)
          
          freq_REAL<- factor(te$class, levels = c(1,2))
          freq_REAL<- round(table(freq_REAL)/length(freq_REAL),2)
          
          sc_test <- predict(classifier, te, type = c("prob"))[,1]
    
          for(qi in counters){
            qntMethod <- qi
            if(qi!="HDy-LP"){
              ifelse(length(strsplit(qi, "-")[[1]]) > 1, qntMethod <- strsplit(qi, "-")[[1]][1], qntMethod <- qi)
              nk <- as.numeric(strsplit(qntMethod, "_")[[1]][1])
              if(is.na(nk)){
                nk <- 1
                }else{
                   nk <- nk
                   qntMethod <- "DySyn"
              }
            }
            freq_PRE  <- apply.qntMethod(qntMethod, 
                                          p.score = scores[scores[,3]==1,1],
                                          n.score = scores[scores[,3]==2,1], 
                                          test = sc_test, 
                                          TprFpr = apply(TprFpr,2,as.numeric),
                                          thr = 0.5,
                                          measure = vdist[strsplit(qi,"-")[[1]][2]],
                                          train = db_tr,
                                          dts[i],
                                         db_te = te)
            results  <- rbind(results, unlist(c(
                                                freq_REAL,
                                                freq_PRE,
                                                round(abs(freq_REAL[1]-freq_PRE[1]),2),
                                                nrow(te), 
                                                qi,
                                                ps,
                                                dts[i])))
          }
          
        }
      }
       results <- as.data.frame(results)
  	   names(results) <- c( 
                      "actual+", 
                      "actual-", 
                      "predict+",
                      "predict-", 
                      "error",
                      "testSize",
                      "quantifier",
                      "harderDistr",
                      "dataset")
    
      #Results will be saved for each dataset according to each positive class distribution
  		saveRDS(results, paste0("./icdm_submitted/icdm21_scorer_complexity",dts[i],"_",pp,".rds"))
    }
  }
  return(1)
}

#setwd(dirname(parent.frame(2)$ofile))
#%%%%%%%%%%%%%%%% IMPORTANT %%%%%%%%%%%%%%%

# Running this file will take a long time to get it concluded! Then you can only run the show_results.r file 
# for reploting or modified some anlaysis.

#for remote running, uncomment only the next line
#run_scorerEvaluation_real(dts_id) 

#uncomment all for running each dataset in your pc
#run_scorerEvaluation_real(1)
#run_scorerEvaluation_real(2)
#run_scorerEvaluation_real(3)
#run_scorerEvaluation_real(4)
#run_scorerEvaluation_real(5)
#run_scorerEvaluation_real(6)
#run_scorerEvaluation_real(7)
#run_scorerEvaluation_real(8)
#run_scorerEvaluation_real(9)
#run_scorerEvaluation_real(10)
#run_scorerEvaluation_real(11)
#run_scorerEvaluation_real(12)

#%%%%%%%%%%%%%%%% IMPORTANT %%%%%%%%%%%%%%%
setwd(dirname(parent.frame(2)$ofile))

#uncomment for formating the results
#source("./show_results_real_datasets.r")
#load_results()
#table_results()
#ranking_boxPlot()
#graph_analysis_real_1()
#statistical_comparison()




