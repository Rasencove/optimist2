library (caret)
library (gdata) 

### VNOS PODATKOV ###
    # Starost
            Starost <- 20   #tu pride podatek za starost#
    
    # Indeks telesne mase
            ITM <- 35
    
    # Obseg pasu
            ObsegPasu <- 200
        
    # 30 MINUT TELESNO AKTIVNI
            V2 <- 1
        
    # Zdravila za nižanje krvnega tlaka
            V3 <- 2
        
    # Izmerili zvišano koncentracijo krvnega sladkorja 
            V4 <- 2
     
    # Zelenjava in sadje
            V5 <- 1
    
    # Sorodniki sladkorna bolezen 
            V6 <- 1
###!!VNOS PODATKOV!!###
    
# 0 - vsi podatki so vneseni
    Starost.NA <- 0
    ITM.NA <- 0
    ObsegPasu.NA <- 0
    V2.NA <- 0
    V3.NA <- 0
    V4.NA <- 0
    V5.NA <- 0
    V6.NA <- 0

# Seznam vnešenih podatkov
podatki <- list(Starost,
                ITM,
                ObsegPasu,
                V2,
                V3,
                V4,
                V5,
                V6,
                Starost.NA,
                ITM.NA,
                ObsegPasu.NA,
                V2.NA,
                V3.NA,
                V4.NA,
                V5.NA,
                V6.NA)

# branje baze, gradnja modela
    data <- read.xls("sloDiabetes_NA_6,1_FINDRISK.xls")
    data$Class <- as.factor(data$Class)
    fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary,classProbs = TRUE)
    set.seed(324)
    splsFit <- train(Class~., data = data, method = "spls",trControl = fitControl, metric="ROC")
   # glmnetFit <- train(Class~., data = data, method = "glmnet", alpha = 0.1, lambda = 0.1, trControl = fitControl, metric="ROC")

# metoda napovednega modela
    diabetesTest <- function(podatki){
        cont <- predict(splsFit, newdata=podatki, type="prob")
        #contSlovenski <- predict(glmFit, newdata=train, type="prob")
        #plot(contSlovenski[,2], ylab = "Tveganje (%)")
        #abline(h = 0.5, col ="red") #meja
        return (cont=cont[,2] ) # vrne % 
    }

# klicanje metode Test
    diabetesTest(podatki)
