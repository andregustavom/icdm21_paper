#Running the experiments we need to install the mlquantify package. Please uncomment the next lines to get it installed.
#install.packages("remotes")
#remotes::install_github("andregustavom/mlquantify")

setwd(dirname(parent.frame(2)$ofile))

library(data.table) #fread
library(caret)
library(mlquantify)

source("./functions/rndProportions.r")

source("./functions/getBatch.r")
source("./functions/getEstimatedTPR_FPR.r")
source("./functions/getScore_using_K_folds.r")
source("./functions/getTPRandFPRbyThreshold.r")
source("./functions/discret_data.r")
source("./functions/distance_DYS.r")
source("./functions/TernarySearch.r")
source("./functions/quantifierTemplate.r")
source("../proposals/DySyn_method.r")
source("../proposals/MoSS.r")
source("../proposals/utils.r")
