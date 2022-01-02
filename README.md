# Accurately Quantifying under Score Variability - Supplementary Material

This repository contains supplemental material for the paper. We divide it into two parts: i) Experiments with artificial scores (DySyn_synthetic folder) and ii) Experiments over real datasets (DySyn_real folder).


### Requirements:
  + R version 3.6.1 (2019-07-05) or newer
  + Packages:
    - [data.table](https://cran.r-project.org/web/packages/data.table/index.html)
    - [caret](https://cran.r-project.org/web/packages/caret/)
    - [mlquantify](https://gitlab.com/andregustavom/mlquantify.git) (devtools::install_git(url = "https://gitlab.com/andregustavom/mlquantify.git"))
    - [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
    - [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html)
    - [RWeka](https://cran.r-project.org/web/packages/RWeka/index.html)
    
    The following packages are required for plotting the results:
    - [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html)
    - [PMCMR](https://cran.r-project.org/web/packages/PMCMR/index.html)
    - [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
  
  + First experiment (synthetic scores) - DySyn_synthetic folder:
    - Run the file `runner_syn.r`. It will create a new folder named _synthetic_ containing the whole results. **Note:** it will take a couple of minutes. It will run several quantifiers.
    - Then to get results in `a data.frame`, please, run the file `show_results_syntheric.r`

  + Second experiment (real datasets) - DySyn_real folder:
    - Run the file `runner_real.r`. It will create a new folder named _real_ containing the whole results. **Note:** it will take a couple of minutes. It will run several quantifiers.
    - Then to get results in `a data.frame` and some plots, please, run the file `show_results_real_datasets.r`
    
> **Note:** Additional plots and figures are available on the paper website [link](https://quantification.shinyapps.io/dysyn/)
