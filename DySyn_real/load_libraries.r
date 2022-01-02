#install.packages("remotes")
#remotes::install_github("andregustavom/mlquantify")

#setwd(dirname(parent.frame(2)$ofile))

library(data.table) #fread
library(caret)
library(dplyr)
library(randomForest)
library(mlquantify)
#library(philentropy)

source("./functions/rndProportions.r")
source("./functions/getBatch.r")
source("./functions/prepare_dataset.r")
source("./functions/datasetsConf.r")
source("./functions/getScore_using_K_folds.r")
source("./functions/getTPRandFPRbyThreshold.r")
source("./functions/quantifierTemplate.r")
source("../proposals/DySyn_method.r")
source("../proposals/MoSS.r")
source("../proposals/utils.r")



