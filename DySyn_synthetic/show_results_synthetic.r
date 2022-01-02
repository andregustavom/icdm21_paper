library(ggplot2)

load_results <- function(path= "./synthetic/"){
  
  if(!dir.exists("./synthetic")){
    stop("The results folder was not found! The raw paper results are available at https://www.dropbox.com/sh/xurs5z6z3uzj6vs/AAC8fzABOvkFaWF9sZFuS57Ua?dl=0")
  }
    
  vf <- list.files(path)
  all<- NULL
  for(i in vf){
    print(i)
    x <- readRDS(paste0(path,i))
    all <- rbind(all, x)
    
  }
  return(all)
}

#+++++++++++++++++++++++++++++++ Plot the error when MTe vary for a given MTr ++++++++++++++++++++++++++++++++++
# Used to get the plots of Figure 3
plot_synthetic <- function(MTr){
  
  all <- readRDS("./all_syn_results_icdm21.rds")
  #all <- load_results("./synthetic/")
  
  vcol <- c("dodgerblue2",
            "lightsalmon2",
            "lightsteelblue",
            "plum3",
            "pink3",
            "olivedrab3",
            "navajowhite2",
            "lightyellow4",
            "indianred",
            "green3","#597DBE", "#76BF72", "#AF85BE", "#C76E6E"
  )
  aux2 <- all[all$Qnt%in%c("MS", "DyS-TS", "PACC", "HDy-LP", "X", "MAX", "T50", "SORD", "ACC", "CC", "PCC", "MS2", "SMM"),]
  aux2 <- aux2[aux2$MFtr==MTr,]
  aux2 <- aggregate(as.numeric(as.vector(aux2$MAE)), by=list(aux2$Qnt, aux2$MFte), FUN=mean)
  
  names(aux2) <- c("Quantifier", "MFte", "MAE")
  a1 <- aux2[,-2]
  qn <- unique(a1$Quantifier)
  at <- NULL
  for(qi in qn){
    at <- cbind(at, a1[a1$Quantifier==qi,2])  
  }
  at <- apply(at, 2, as.numeric)
  at <- as.matrix(at)
  colnames(at) <- qn
  
  p1 <- ggplot(data = aux2, aes(x=MFte,y=MAE, group= Quantifier, linetype=Quantifier))+
    geom_vline(xintercept = MTr*20, linetype="dashed", 
               color = "black", size=3.5) +
    geom_line(size=5, aes(color=Quantifier), alpha=0.7)+
    geom_point(size=7, aes(shape=Quantifier, color=Quantifier), stroke=2)+
    scale_shape_manual(values=c(0:14))+
    scale_color_manual(values=vcol)+
    scale_x_discrete(breaks = seq(0.05,0.95, length.out = 10))+
    ylab("MAE") +
    xlab("MTe") +
    guides(col = guide_legend(nrow = 5, byrow = TRUE))+
    theme_bw()+
    theme(
      plot.background = element_blank()
      ,text = element_text(size=50)
      ,legend.position = "top"
      ,legend.title = element_blank()
      ,panel.border = element_rect(size = 1.5)
      ,axis.line.x = element_line(size=1)
      ,axis.ticks = element_line(size = 1),
      plot.margin = unit(c(.5,0.1,0.1,0.1), "cm")
    )
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black'))
  print(p1)
  return(p1)
}



setwd(dirname(parent.frame(2)$ofile))
#uncomment the next lines to get the plots presented in Figure 3
#plot_synthetic(0.05)
#plot_synthetic(0.25)
#plot_synthetic(0.5)
#plot_synthetic(0.75)


