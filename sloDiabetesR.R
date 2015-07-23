library(ROCR)
library(pROC)
library(RWeka)
library(caret)
library (gdata) # za branje xls
data <- read.xls("sloDiabetes_NA_6,1.xls")
data <- read.xls("sloDiabetes_NA_6,9.xls")
data <- read.xls("sloDiabetes_brezNA_6,1.xls")
head(data)
saveRDS(data,"sloDiabetes_6.9.rds")
data <- readRDS("sloDiabetes_6.9.rds")
data <- readRDS("sloDiabetes_6.1.rds")

# odstranimo krvni sladkor
data <- data[,-which(names(data) == "KrvniSladkor")]
data <- data[,-which(names(data) == "KrvniSladkor.NA")]

# za FINDRISK


data$Class <- as.factor(data$Class) #kasneje rabimo v tej obliki

# 10 fold cross-validation ponovljena 5 krat
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(324)

# modeli
glmFit <- train(Class~., data = data, method = "glm", trControl = fitControl, metric="ROC")
glmnetFit <- train(Class~., data = data, method = "glmnet", trControl = fitControl, metric="ROC")
library(RWeka)
library(C50)
C5.0RulesFit <- train(Class~., data = data, method = "C5.0Rules", trControl = fitControl, metric="ROC")
OneRFit <- train(Class~., data = data, method = "OneR", trControl = fitControl, metric="ROC")
library(caTools)
library(pls)
plsFit <- train(Class~., data = data, method = "pls",trControl = fitControl, metric="ROC")
multinomFit <- train(Class~., data = data, method = "multinom",trControl = fitControl, metric="ROC")
pamFit <- train(Class~., data = data, method = "pam",trControl = fitControl, metric="ROC")
pda2Fit <- train(Class~., data = data, method = "pda2",trControl = fitControl, metric="ROC")
simplsFit <- train(Class~., data = data, method = "simpls",trControl = fitControl, metric="ROC")
sldaFit <- train(Class~., data = data, method = "slda",trControl = fitControl, metric="ROC")
fdaFit <- train(Class~., data = data, method = "fda",trControl = fitControl, metric="ROC")
nnetFit <- train(Class~., data = data, method = "nnet",trControl = fitControl, metric="ROC")
splsFit <- train(Class~., data = data, method = "spls",trControl = fitControl, metric="ROC")
stepLDAFit <- train(Class~., data = data, method = "stepLDA",trControl = fitControl, metric="ROC")
stepQDAFit <- train(Class~., data = data, method = "stepQDA",trControl = fitControl, metric="ROC")
svmLinearFit <- train(Class~., data = data, method = "svmLinear",trControl = fitControl, metric="ROC")
xyfFit <- train(Class~., data = data, method = "xyf",trControl = fitControl, metric="ROC")
knnFit <- train(Class~., data = data, method = "knn",trControl = fitControl, metric="ROC")
bdkFit <- train(Class~., data = data, method = "bdk",trControl = fitControl, metric="ROC")

#drevesni#
PARTFit <- train(Class~., data = data, method = "PART", trControl = fitControl, metric="ROC")
ctree2Fit <- train(Class~., data = data, method = "ctree2",trControl = fitControl, metric="ROC")
treebagFit <- train(Class~., data = data, method = "treebag",trControl = fitControl, metric="ROC")
C5.0Fit <- train(Class~., data = data, trials = 0.5, method = "C5.0",trControl = fitControl, metric="ROC")
LogitBoostFit <- train(Class~., data = data, method = "LogitBoost",trControl = fitControl, metric="ROC")
blackboostFit <- train(Class~., data = data, method = "blackboost",trControl = fitControl, metric="ROC")
J48Fit <- train(Class~., data = data, method = "J48", trControl = fitControl, metric="ROC")
CARTFit <- train(Class~., data = data, method = "rpart", trControl = fitControl, metric="ROC") 
C5.0TreeFit <- train(Class~., data = data, method = "C5.0Tree", trControl = fitControl, metric="ROC")

#izpis rezultatov
PARTFit
ctree2Fit
treebagFit
C5.0Fit
LogitBoostFit
blackboostFit
J48Fit
CARTFit
C5.0TreeFit

glmFit
glmnetFit
C5.0RulesFit
OneRFit
plsFit
multinomFit
pamFit
pda2Fit
simplsFit
sldaFit
fdaFit 
nnetFit
splsFit
stepLDAFit
stepQDAFit
svmLinearFit
xyfFit
knnFit
bdkFit

######  Izris primerjav metod  ###### 
models <- list(Fda = fdaFit)  
results <- resamples(models)
trellis.par.set(caretTheme())
bwplot(results,layout = c(3,1))

# Variable importance
Importance <- varImp(bdkFit, scale = TRUE)
plot(Importance)
Importance
