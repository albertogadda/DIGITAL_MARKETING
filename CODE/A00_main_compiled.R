#### OPTIONS ####

options(scipen=999)
set.seed(12345)

#### LIBRARIES ####
lib_array = c("dplyr", "magrittr", "ggplot2", "forcats", "lubridate", "RQuantLib", "arules", "arulesViz", "e1071", "corrplot", "caret", "rpart", "rpart.plot", "ranger", "randomForest", "glmnet", "funModeling")

for (l in lib_array) {
  if (require(l, character.only = TRUE) == FALSE) {
    install.packages(l)
    library(l, character.only = TRUE)
  }
}

#### LIBRARY VERSIONS ####
sessionInfo()

#### DIRECTORIES ####
working_dir = r"{C:\Users\User\Documents\Uni\Web Marketing\Scripts}"
data_dir = r"{C:\Users\User\Documents\Uni\Web Marketing\Datasets}"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

 PIPELINE_scripts <- c(
    'B01_ingestion.R'
   , 'C01_preparation_df1.R'
   , 'C02_preparation_df2.R'
   , 'C03_preparation_df3.R'
   , 'C04_preparation_df4.R'
   , 'C05_preparation_df5.R'
   , 'C06_preparation_df6.R'
   , 'C07_preparation_df7.R'
   , 'D01_RFM.R'
   , 'D02_CHURN.R'
   , 'D03_MBA.R'
   )

 for(i in PIPELINE_scripts){
   source(i, echo = TRUE)
 }
