source("./load_libraries.r")


exec_eval_complexity <- function(MFtr){
  
  if(!dir.exists("./synthetic/"))
    dir.create("./synthetic/")
  
  vdist <- c("topsoe", "jensen_difference", "prob_symm", "ord", "sord")
  names(vdist) <- c("TS", "JD", "PS", "ORD", "SORD")
  
  var_perc <- seq(0,1,0.05) # positive class distribution
  var_size <- 1000#test set size
  n_tests  <- 10# replication
  MF       <- seq(0.05,0.95,0.05) # m parameter for MoSS model
  qnt      <- c("HDy-LP", "DyS-TS","DyS-JD", "DyS-ORD", "MS", "T50", "MAX", "X", "CC", "PCC", "ACC", "PACC", "SORD", "MS2", "SMM")
  
  results <- data.frame(MFtr=NA, MFte=NA, R_1=NA, P_1=NA, MAE=NA,Distance=NA, Value.dist=NA, Qnt=NA)  
  for(mi in 1:length(MFtr)){
    scores  <- MoSS(2000, 0.5, MFtr[mi]) 
    TprFpr  <- apply(getTPRandFPRbyThreshold(scores), 2, as.numeric)
    colnames(TprFpr) <- c("thr", "tpr", "fpr")
    for(k in 1:length(var_size)){
      for(i in 1:length(var_perc)){
        print(paste0("Class distr. ", var_perc[i]))
        for(j in 1:n_tests){
          for(ti in 1:length(MF)){
            for(qi in qnt){
              test_set <- MoSS(var_size[k], var_perc[i], MF[ti])
              freq_REAL<- factor(test_set[,3], levels = c(1,2))
              freq_REAL<- table(freq_REAL)/length(freq_REAL)
              qntMethod<- qi
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
              qnt_re <- apply.qntMethod(qntMethod = qntMethod, 
                                        p.score = scores[scores[,3]==1,1], 
                                        n.score = scores[scores[,3]==2,1], 
                                        test = test_set[,1], 
                                        TprFpr = TprFpr, 
                                        thr = 0.5,
                                        measure = vdist[strsplit(qi,"-")[[1]][2]]
                                       )
              
              freq_PRE <- qnt_re[[1]]
              results  <- rbind(results, unlist(c(MFtr[mi],
                                                  MF[ti],
                                                  freq_REAL[1],
                                                  freq_PRE[1],
                                                  round(abs(freq_REAL[1]-freq_PRE[1]),2),
                                                  vdist[strsplit(qi,"-")[[1]][2]],
                                                  qnt_re[[2]],
                                                  qi)))
            }
          }
        }
      }
    }
    results <- results[-1,]
    #saveRDS(results, paste0("./synthetic/scorer_complexity_syn","_", MFtr[mi],".rds"))
  }
  return(1)
}


setwd(dirname(parent.frame(2)$ofile))
#%%%%%%%%%%%%%%%% IMPORTANT %%%%%%%%%%%%%%%

# Running this file will take a long time to get it concluded! Then you can only run the show_results_synthetic.r file 
# for reploting or modified some analaysis.

#uncomment for running the evaluation of each quantifier
m_Tr <- seq(0.05, 0.95, 0.05)
print("############ - It will take a couple of minutes! - ############")
for(i in m_Tr)
  exec_eval_complexity(i)

#%%%%%%%%%%%%%%%% IMPORTANT %%%%%%%%%%%%%%%
