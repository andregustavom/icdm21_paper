setwd(dirname(parent.frame(2)$ofile))
library(PMCMR)
library(ggplot2)
library(gridExtra)

load_results <- function(path= "./icdm_submitted/"){
  if(!dir.exists("./synthetic")){
    stop("The results folder was not found! The raw paper results are available at https://www.dropbox.com/sh/9kk77qd1nusoj4q/AACzD3vI_YKWv0sXoHChy-Zoa?dl=0")
  }
  
  vf <- list.files(path)
  all<- NULL
  for(i in vf){
    print(i)
    x <- readRDS(paste0(path,i))
    all <- rbind(all, x)
  }
  saveRDS(all, "./real_results_icdm21.rds")
  return(all)
}
#+++++++++++++++++++++++++++++++ Used to summarize the results in Table II ++++++++++++++++++++++++++++++++++
# Table II
table_results <- function(){
  
  #all <- load_results(path = "./ICDM_submitted/")
  all <- readRDS("./real_results_icdm21.rds")
  re  <- all[,c("quantifier","dataset","error", "harderDistr", "actual+")]
  
  aux<-aggregate(as.numeric(as.vector(re$error)), by=list(re$quantifier, re$dataset, re$harderDistr), FUN=mean)
  names(aux) <- c("Quantifier", "Dataset", "SubClass","MAE")
  
  dts <- sort(as.vector(unique(aux$Dataset)))
  res <- NULL
  for(i in dts){
    a1 <-  aux[aux$Dataset==i,]
    a2 <- aggregate(as.numeric(as.vector(a1$MAE)), by=list(a1$Quantifier), FUN=mean)
    res <- cbind(res, a2$x)
  }
  res <- as.data.frame(res)
  res <- round(res,3)
  res <- cbind(as.factor(a2$Group.1),res)
  names(res) <- c("Quantifier", as.vector(dts))
  
  x <- t(res)
  nn <- x[1,]
  x <- x[-1,]
  dts <- rownames(x)
  x <- as.data.frame(x)
  names(x) <- nn
  
  qn <- unique(re$quantifier)
  all<- NULL 
  for(di in dts){
    at <- NULL
    for(qi in qn){
      at <- cbind(at, as.numeric(as.vector(re[re$quantifier==qi & re$dataset==di,3])))
    }
    at <- cbind(as.numeric(as.vector(re[re$quantifier==qi & re$dataset==di,4])), at)
    at <- apply(at, 2, as.numeric)
    at <- as.matrix(at)
    colnames(at) <- c("m_Te",as.vector(qn))
    all <- rbind(all, at)
  }
  return(res)
  #return(list(res,x,at, all))
}

#+++++++++++++++++++++++++++++++ Plot the ranking of quantifier considering all datasets ++++++++++++++++++++++++++++++++++
# Figures 6 and 8
ranking_boxPlot <- function(drift=FALSE){
  #all <- load_results(path = "./ICDM_submitted/")
  all <- readRDS("./real_results_icdm21.rds")
  re <- all[,c("quantifier","dataset","error", "harderDistr", "actual+")]
  
  aux <- aggregate(as.numeric(as.vector(re$error)), by=list(re$harderDistr, re$dataset, re$quantifier), FUN=mean)
  names(aux) <- c("Subclass", "Dataset", "Quantifier", "MAE")
  
  dts <- unique(aux$Dataset)
  dts <- c("Chess_game", "Dermatology", "HAR", "Land-use", "Nursery", "Phishing_URL", "Avila", "Covertype", "MFeat", "Mosquitoes",
           "Satimage", "Walking")
  
  sbc <- unique(aux$Subclass)
  res <- NULL
  for(di in dts){
    for(si in sbc){
      x <- cbind(aux[aux$Dataset==di & aux$Subclass==si,],rank(aux[aux$Dataset==di & aux$Subclass==si,"MAE"]))    
      res <- rbind(res, x[,c(1,3,5)]) 
    }
  }
  names(res) <-  c("Subclass","Quantifier", "Rank")
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(drift){
    res <- res[res$Subclass%in%c(0,0.25,0.55, 0.75,1),]
  }
  else{
    res <- res[res$Subclass%in%c(0.5),]
  }
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  theme_set(theme_bw())
  v1 <- ggplot(res, aes(reorder(Quantifier, Rank), Rank, fill=Quantifier)) + 
    geom_boxplot(varwidth=F, alpha=0.3, size=1.5)+
    labs(title="",x="Quantification algorithms")+
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.background = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,text = element_text(size=50),
      strip.background=element_rect(fill="black"),
      legend.position="none",
      legend.background = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size=1),
      axis.ticks = element_line(size = 1)
    )
  print(v1)
}

#+++++++++++++++++++++++++++++++ Plot error varying the presence of harder subclass ++++++++++++++++++++++++++++++++++
# Figure 7
graph_analysis_real_1 <- function(){
  
  vcol <- c(
            "lightsalmon2",
            "lightsteelblue",
            "plum3",
            "dodgerblue2",
            "indianred",
            "olivedrab3",
            "navajowhite2",
            "pink3",
            "lightyellow4",
            "green3","#597DBE", "#C76E6E", "#76BF72", "#AF85BE",  2, 3, 4, 8
  )
  all <- readRDS("./real_results_icdm21.rds")
  re <- all[,c("quantifier","dataset","error", "harderDistr", "actual+")]
  
  aux<-aggregate(as.numeric(as.vector(re$error)), by=list(re$quantifier, re$dataset, re$harderDistr), FUN=mean)
  
  names(aux) <- c("Quantifier", "Dataset", "SubClass","MAE")
  aux <- aux[which(aux$Quantifier%in%c("DySyn-TS", "DyS-TS", "DySyn-SORD", "CC", "X", "MAX", "SORD", "HDy-LP", "ACC", "MS2")),]
  dts <- as.vector(unique(aux$Dataset))
  dts <- dts[-which(dts=="Mosquitoes")]
  dts <- c("Mosquitoes", dts)
  dtsNN <- dts
  dtsNN[which(dtsNN=="Chess_game")] <- "Chess game"
  dtsNN[which(dtsNN=="Phishing_URL")] <- "Phishing URL"
  
  x <- as.matrix(aux)
  v_alpha <- rep(0.6, 10)
  v_alpha[c(4,5)] <- 0.9
  
  for(di in 1:length(dts)){
    aux2 <- aux[aux$Dataset==dts[di],]
    aux2$SubClass <- as.numeric(as.vector(aux2$SubClass))
    vlegend <- c("top", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none")
    p1 <- ggplot(data = aux2, aes(x=SubClass,y=MAE, group= Quantifier, linetype=Quantifier, alpha=Quantifier))+
      
      geom_vline(xintercept = 0.5, linetype="longdash", 
                 color = "black", size=3.5) +
      geom_line(size=5, aes(color=Quantifier))+
      scale_linetype_manual(values=c("twodash", "dotted", "longdash", "solid", "solid", "dotdash", 
                                     "dashed", "twodash", "dotted", "dotdash","longdash"))+
      scale_alpha_manual(values=v_alpha)+
      geom_point(size=7, aes(shape=Quantifier, color=Quantifier), stroke=3, alpha=1)+
      scale_color_manual(values=vcol)+
      ylab("MAE") +
      xlab("Proportion of the first subclass") +
      guides(color = guide_legend(nrow = 2))+
      theme_bw()+
      theme(
        plot.background = element_blank()
        ,text = element_text(size=50),
        legend.position=vlegend[di]
        ,legend.background = element_blank()
        ,legend.title = element_blank()
        ,legend.key.width = unit(3,"cm")
      ) +
      ggtitle(paste0(dtsNN[di]))+
      #draws x and y axis line
      theme(axis.line = element_line(color = 'black'))
    
    
    print(p1)
    
  }
  
  return(as.data.frame(x))
}

#+++++++++++++++++++++++++++++++ Statistical Comparisons between quantifier for each dataset ++++++++++++++++++++++++++++++++++
# Used to get the statistical analysis showed in Table II
statistical_comparison <- function(){
  all <- readRDS("./real_results_icdm21.rds")
  dts <- sort(as.vector(unique(all$dataset)))
  for(di in dts){
    print(paste0("================ Comparisons for ", di, " dataset ================"))
    x <- all[all$dataset==di,]
    counters <- unique(x$quantifier)
    re <- NULL
    for(i in counters){
      aux <- as.numeric(as.vector(x[x$quantifier== i,"error"]))
      re <- cbind(re, aux)
    }
    re <- as.data.frame(re)
    names(re) <- counters
    re <- as.matrix(re)
    print(posthoc.friedman.nemenyi.test(re))
  }

}

#uncomment the next two lines for getting the result's table
#t_re <- table_results()
#grid.table(t_re)

#Ranking's plot including drift scenarios
#ranking_boxPlot(TRUE)

#Ranking's plot without drift scenarios
#ranking_boxPlot(FALSE)

#uncomment the next line to plot the datasets results
#graph_analysis_real_1()

#uncomment the next line to print in the shell the statistical analysis for each dataset
#statistical_comparison()

