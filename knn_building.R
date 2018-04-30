# list all packages/libraries and assign them to "my_packages"-variable

# load libraries
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

pkg <- c("dplyr","nnet", "reshape","bigmemory","magrittr","doFuture","doParallel", "biganalytics","doMC", "arules", "caret", "corrplot", "ggplot2", "ggthemes", "tidyr", "readr" )

ipak(new.pkg)

sample_train <- read.csv("sample_train.csv", sep=",",na.strings = c("?", "NA"), stringsAsFactors = FALSE)
#------------------------------Preprocessing for model building---------------------------####

sample_train$BUILDINGID <- as.factor(sample_train$BUILDINGID)

set.seed(405)

trainSize <- round(nrow(sample_train) * 0.75)
testSize <- nrow(sample_train) - trainSize

training_indices <- sample(seq_len(nrow(sample_train)), size =trainSize)

trainSet <- sample_train[training_indices, ]
testSet <- sample_train[-training_indices, ]

#-----------------------------------Knn---------------------------------####

set.seed(5)

ctrl <- trainControl(method = "cv", # specifies repeated K-fold cross-validation 
                     number = 7)
                     #repeats = 3, # number of repititions
                     #classProbs = TRUE, # incudes calculations of measures specific to two-class problems (area under ROC curve, sensitivity/specificity)
                     #summaryFunction = twoClassSummary) # takes the observed and prdicted values to estimate measure of performance

# Accuracy and kappa
knnFit1_B <- train(BUILDINGID ~.
                 -LONGITUDE 
                 -FLOOR
                 -LATITUDE, 
                 data = trainingset, 
                 method = "knn",
                 #tuneLength = 10, # controls how many candidate sets of parameter values are evaluated
                 trControl = ctrl, 
                 metric = "RMSE") # specifies the criterion that should be optimized - takes tuning parameters with best result
                 #preProc = c("center","scale"))

knnFit1

saveRDS(knnFit1_B, file = "knnFit1_B.RDS")
#modelrf_B <- readRDS("modelrf_B.RDS")

plot(knnFit1)

predictors(knnFit1)

testPredknn1 <- predict(knnFit1, testingset) #type = "prob")

postResample(testPredknn1, testingset$LONGITUDE)