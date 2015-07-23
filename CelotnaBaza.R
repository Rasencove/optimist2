library(ROCR)
library(pROC)
library(caret)
library(RWeka)
getwd()
###### branje in predprirava podatkovne baze
data <- readRDS("diabetesP1.rds")

data <- readRDS("diabetesP2.rds")
data$ClassP1 <-data$ClassP2  
data$ClassP2 <- NULL
head(data)

data <- readRDS("PreDiabetes.rds")
head(data)
getwd()
###### Metoda, ki izloči spremenljivke z visoko korelacijo ######

library(corrplot)
korelacija <- function(data){
    ClassP1 <- data[,ncol(data)]
    data <- data[,-c(ncol(data))]#BREZ class
    correlations <- cor(data) 
    corrplot(correlations, order="hclust")
    highCorr <-findCorrelation(correlations, cutoff =.80)
    data <- data[,-highCorr]#odstranil element ki visoko korelira z drugimi
    data$ClassP1 <- ClassP1
    return (data)
}

# baza brez spremenljivk ki visoko korelirajo 
dataTest <- data[,c(1:16,ncol(data))] #brez NA spremenljivk
dataTest <- korelacija(dataTest) # data, cutoff
head(dataTest)


data <- data[,-1]
data <- data[,-c(5,21)] # diabetes1 : brez weight in weight.NA
head(data)


data$ClassP1 <- as.factor(data$ClassP1) #kasneje rabimo v tej obliki


# 10 fold cross-validation ponovljena 5 krat
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(123)

glmFit <- train(ClassP1~., data = data, method = "glm", trControl = fitControl, metric="ROC") #metric ="" ? verbose = FALSE
glmFit
glmnetFit <- train(ClassP1~., data = data, method = "glmnet", trControl = fitControl, metric="ROC")
glmnetFit

library(rpart)

CARTFit <- train(ClassP1~., data = data, method = "rpart", trControl = fitControl, metric="ROC") 
CARTFit
library(RWeka)

J48Fit <- train(ClassP1~., data = data, method = "J48", trControl = fitControl, metric="ROC")
J48Fit
## J50 drevo  dela tree size
library(C50)

C5.0TreeFit <- train(ClassP1~., data = data, method = "C5.0Tree", trControl = fitControl, metric="ROC")
C5.0TreeFit
C5.0RulesFit <- train(ClassP1~., data = data, method = "C5.0Rules", trControl = fitControl, metric="ROC")
C5.0RulesFit


PARTFit <- train(ClassP1~., data = data, method = "PART", trControl = fitControl, metric="ROC")
PARTFit

OneRFit <- train(ClassP1~., data = data, method = "OneR", trControl = fitControl, metric="ROC")
OneRFit


library(caTools)

LogitBoostFit <- train(ClassP1~., data = data, method = "LogitBoost",trControl = fitControl, metric="ROC")
LogitBoostFit

library(pls)

plsFit <- train(ClassP1~., data = data, method = "pls",trControl = fitControl, metric="ROC")
plsFit


library(qda)

qdaFit <- train(ClassP1~., data = data, method = "qda",trControl = fitControl, metric="ROC")
qdaFit

multinomFit <- train(ClassP1~., data = data, method = "multinom",trControl = fitControl, metric="ROC")
multinomFit
pamFit <- train(ClassP1~., data = data, method = "pam",trControl = fitControl, metric="ROC")
pamFit
mdaFit <- train(ClassP1~., data = data, method = "mda",trControl = fitControl, metric="ROC")
mdaFit
pda2Fit <- train(ClassP1~., data = data, method = "pda2",trControl = fitControl, metric="ROC")
pda2Fit
simplsFit <- train(ClassP1~., data = data, method = "simpls",trControl = fitControl, metric="ROC")
simplsFit
sldaFit <- train(ClassP1~., data = data, method = "slda",trControl = fitControl, metric="ROC")
sldaFit
fdaFit <- train(ClassP1~., data = data, method = "fda",trControl = fitControl, metric="ROC")
fdaFit 
nnetFit <- train(ClassP1~., data = data, method = "nnet",trControl = fitControl, metric="ROC")
nnetFit
splsFit <- train(ClassP1~., data = data, method = "spls",trControl = fitControl, metric="ROC")
splsFit

stepLDAFit <- train(ClassP1~., data = data, method = "stepLDA",trControl = fitControl, metric="ROC")
stepLDAFit


stepQDAFit <- train(ClassP1~., data = data, method = "stepQDA",trControl = fitControl, metric="ROC")
stepQDAFit


svmLinearFit <- train(ClassP1~., data = data, method = "svmLinear",trControl = fitControl, metric="ROC")
svmLinearFit


treebagFit <- train(ClassP1~., data = data, method = "treebag",trControl = fitControl, metric="ROC")
treebagFit
xyfFit <- train(ClassP1~., data = data, method = "xyf",trControl = fitControl, metric="ROC")
xyfFit


knnFit <- train(ClassP1~., data = data, method = "knn",trControl = fitControl, metric="ROC")
knnFit


ctree2Fit <- train(ClassP1~., data = data, method = "ctree2",trControl = fitControl, metric="ROC")
ctree2Fit


C5.0Fit <- train(ClassP1~., data = data, method = "C5.0",trControl = fitControl, metric="ROC")
C5.0Fit

blackboostFit <- train(ClassP1~., data = data, method = "blackboost",trControl = fitControl, metric="ROC")
blackboostFit 


bdkFit <- train(ClassP1~., data = data, method = "bdk",trControl = fitControl, metric="ROC")
bdkFit
# 11:10 -19:00
######  Izris primerjav metod  ###### 
models <- list(Fda = fdaFit, GLM = glmFit, Multinom = multinomFit, SVMLinear = svmLinearFit, C5.0 = C5.0Fit, BlackBoost= blackboostFit) # diabetes1 
models <- list(Fda = fdaFit, GLM = glmFit, Multinom = multinomFit, SVMLinear = svmLinearFit, C5.0 = C5.0Fit, BlackBoost= blackboostFit, Spls = splsFit) # diabetes2

models <- list(Fda = fdaFit, GLM = glmFit, Multinom = multinomFit, SVMLinear = svmLinearFit, C5.0 = C5.0Fit, BlackBoost= blackboostFit) # diabetes1 
results <- resamples(models)
trellis.par.set(caretTheme())
bwplot(results,layout = c(3,1))
splom(results)
ANFISFit
# Variable importance
Importance <- varImp(splsFit, scale = TRUE)
plot(Importance)
head(data)
