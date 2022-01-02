
datasetsConf <- function(dts){
  
  
  if(dts=="Mfeat.csv"){
    #https://www.openml.org/d/18
    # Training - 50% and Test - 50%
    #db <- as.data.frame(fread(paste0("./process_real_results/datasets_multi/",dts)))
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    db$class <- as.numeric(db$class)
    #all_classes <- names(table(db$class))[which(table(db$class)>=100)]
    ncl <- as.vector(unique(db$class))
    #browser()
    db$class[which(db$class%in%c(10,9))] <- 2
    db$class[db$class%in%c(4,7,8)] <- 4
    db$class[which(db$class%in%c(5,3))] <- 5
    #browser()
    neg_class <- c(2, 4)
    pos_class <- c(5)
    
    
    #pos_class <- c(5)
    #neg_class <- all_classes[-which(all_classes==pos_class)]
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[c(3,5)], ncl[c(2,9,10)], ncl[c(4,7,8)]))
  }
  
  
  if(dts=="Avila.csv"){
    # https://archive.ics.uci.edu/ml/datasets/Avila
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    nn <- names(db)
    
    nn[length(nn)] <- "class"
    names(db) <- nn
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    db$class[db$class%in%c(2,3,9,4,8)] <- 2
    
    pos_class <- c(1)
    neg_class <- c(5,2)
    db <- db[db$class%in%c(pos_class, neg_class),]
    return(list(db,pos_class, neg_class,ncl[1], ncl[5], ncl[c(2,3,9,4,8)]))
  }
  
  
  if(dts=="Mosquitoes.csv"){
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    pos_class <- c(1)
    neg_class <- c(5,10)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    db <- db[,c(1,16,29:54)]
    
    return(list(db,pos_class, neg_class, 1, 5, 10))
  }
  
  
  if(dts=="Covertype.csv"){
    # https://www.openml.org/d/180
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    nn <- names(db)
    
    nn[length(nn)] <- "class"
    names(db) <- nn
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    pos_class <- c(2)
    neg_class <- c(5,1)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    return(list(db, pos_class, neg_class, ncl[2], ncl[5], ncl[1]))
  }
  
  
  if(dts=="Walking.csv"){
    # https://www.openml.org/d/1509
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"Class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    db$class[db$class%in%c(21,6,12)] <- 4
    
    pos_class <- 22
    neg_class <- c(13,4)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    return(list(db,pos_class, neg_class, 22, 13, 4,6,12,21))
  }
  

  if(dts=="Satimage.csv"){
    # https://www.openml.org/d/182
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    db$class[db$class%in%c(3,7)] <- 5
    pos_class <- 4
    neg_class <- c(2,5)
  
    db <- db[db$class%in%c(pos_class, neg_class),]
    return(list(db,pos_class, neg_class, 4, 2, c(3,5,7)))
  }
  
  
 if(dts=="Nursery.csv"){
    #https://www.openml.org/d/26
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    db$class[db$class%in%c(5)] <- 4
    pos_class <- 2#c(5)
    neg_class <- c(3,4)#c(2,4)
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[2], ncl[3], ncl[c(4,5)]))
  }
  
  if(dts=="URL.csv"){
    #https://www.openml.org/d/42670
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    nn <- names(db)
    idx<- which(nn%in%"URL_Type_obf_Type")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    #db$class[db$class%in%c(4)] <- 5
    #db <- db[db$class%in%c(1,2,5),]
    db$class[db$class%in%c(2,3,4)] <- 2
    pos_class <- 1#c(5)
    neg_class <- c(5,2)#c(2,4)
    
    #pos_class <- c(1)
    #neg_class <- c(7,2)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    names(db) <- c("class",paste0("V",2:length(names(db))))
    
    return(list(db, pos_class, neg_class, ncl[1], ncl[5], ncl[c(2,3,4)]))
  }
  
  #if(dts=="php88ZB4Q.csv"){
  if(dts=="HAR.csv"){
    #https://www.openml.org/d/1478
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"Class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    #db$class[db$class%in%c(4,5,6)] <- 2
    pos_class <- 1
    neg_class <- c(3,2)#(Easy, Hard)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[1], ncl[3], ncl[2]))
  }
  
  if(dts=="Fabert.csv"){
    #https://www.openml.org/d/41164
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- unique(db$class)
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
  
    #db$class[db$class%in%c(3,4)] <- 3
    pos_class <- 2
    neg_class <- c(1,5)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[2], ncl[1], ncl[5]))
  }
  
  if(dts=="Accidents.csv"){
    #https://www.openml.org/d/40672
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    names(db) <- c("class",paste0("V",2:length(names(db))))
    ncl <- unique(db$class)
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    pos_class <- 2#c(5)
    neg_class <- c(1,4)#c(2,4)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[2], ncl[1], ncl[6]))
  }
  
  #if(dts=="indian_pines.csv"){
  if(dts=="Land-use.csv"){
    #https://www.openml.org/d/41972
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    nn[length(nn)] <- 'Class'
    names(db) <- nn
    db <- db[-1,]
    
    nn <- names(db)
    idx<- which(nn%in%"Class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    pos_class <- 2#c(5)
    neg_class <- c(4,6)#(Easy, Hard)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[2], ncl[4], ncl[6]))
  }
  
  #if(dts=="dataset_188_kropt.csv"){
  if(dts=="Chess game.csv"){
    #https://www.openml.org/d/184
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    # nn <- names(db)
    # nn[length(nn)] <- 'class'
    # names(db) <- nn
    # db <- db[-1,]
    
    
    #db <- as.matrix(db)
    #db[db=="?"]<-0
    #db <- as.data.frame(db)
    
    nn <- names(db)
    idx<- which(nn%in%"game")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    #db$class[db$class%in%c(4)] <- 5
    
    #db <- db[db$class%in%c(1,2,5),]
    db$class[db$class%in%c(2:11)] <- 2
    pos_class <- 16#c(5)
    neg_class <- c(2,15)#c(2,4)
    
    #pos_class <- c(1)
    #neg_class <- c(7,2)
    
    db <- db[db$class%in%c(pos_class, neg_class),]

    return(list(db, pos_class, neg_class, ncl[16], ncl[c(2:11)], ncl[15]))
  }
  #sem uso
  if(dts=="Aggregation.csv"){
    #https://www.openml.org/d/41671
    # Training - 50% and Test - 50%
    db <- as.data.frame(fread(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- unique(db$class)
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    #db$class[db$class%in%c(1,5)] <- 1
    
    pos_class <- 2#c(1:19,21,23)#c(5)
    neg_class <- c(4,1)#c(2,4)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[2], ncl[4], ncl[1]))
  }
  
  #if(dts=="All_cleaned.csv"){Phishing URL
  if(dts=="Phishing URL.csv"){
    #https://www.openml.org/d/42641
    # Training - 50% and Test - 50%
    #db <- as.data.frame(fread(paste0("./process_real_results/datasets_multi/new/",dts)))
    db <- as.data.frame(read.csv(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    #db$class[db$class%in%c(1,2,5)] <- 2
    db$class[db$class%in%c(4)] <-3
    pos_class <- 1#2#c(1:19,21,23)#c(5)
    neg_class <- c(2,3)#c(2,4)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[1], ncl[2], ncl[c(3,4)]))
  }
  
  if(dts=="Dermatology.csv"){
    #https://www.openml.org/d/263
    # Training - 50% and Test - 50%
    db <- as.data.frame(read.csv(paste0("./datasets/",dts)))
    db <- db %>% mutate_if(is.character, as.factor)
    
    nn <- names(db)
    idx<- which(nn%in%"class")
    
    db <- cbind(db[,idx], db[,-idx])
    nn <- c("class", nn[-idx])
    names(db) <- nn
    
    ncl <- as.vector(unique(db$class))
    aux <- 1:nrow(db)
    for(i in 1:length(ncl)){
      po <- which(db$class==ncl[i])
      aux[po] <- i
      
    }
    nn <- names(db)
    db <- cbind(aux, db[,-1])
    names(db) <- nn
    
    db$class[db$class%in%c(1,2,5)] <- 1
    pos_class <- 6#c(1:19,21,23)#c(5)
    neg_class <- c(4,1)#c(2,4)
    
    db <- db[db$class%in%c(pos_class, neg_class),]
    
    return(list(db, pos_class, neg_class, ncl[6], ncl[4], ncl[c(1,2,5)]))
  }
  
}
  
 
