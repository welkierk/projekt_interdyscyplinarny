#set path if needed
#setwd("C:/Users/koste/Studia/07/PI/github/projekt_interdyscyplinarny/model_test")
library(dplyr)
library(randomForest)
library(mlr)
library(ranger)
library(tuneRanger)
# accuracy, precision, etc.
library(ROCR)
library(MLmetrics)

# data for regression model
set.seed(1628)
df <- read.csv("../dochody_i_ludnosc_2.csv", encoding = "UTF-8") # 2521 gminas
modelResultsBin <- read.csv2("gminasDevClasByModel.csv") # 2046 gminas - developed (1) / undeveloped (0)
modelResultsCont <- read.csv2("gminasDevRegByModel.csv")

is_neg_inf_val <- (df == -Inf)
which_have_neg_infs <- which(apply(is_neg_inf_val, 1, any))
df <- df[-which_have_neg_infs,]
#nrow(df) # 2518

mergedBin <- modelResultsBin %>% left_join(df, by = "X")
mergedCont <- modelResultsCont %>% left_join(df, by = "X")
#colnames(mergedBin)
main_bin <- subset(mergedBin, select = -c(X, gmina, powiat, gminaType, Kod,
                                          longitude, latitude))
main_bin <- na.omit(main_bin)
main_cont <- subset(mergedCont, select = -c(X, gmina, powiat, gminaType, Kod,
                                            longitude, latitude))
main_cont <- na.omit(main_cont)
#nrow(main_bin) # 1680
#colnames(main_bin) # 30 feature columns, 1 target one (+Nazwa)

main_nrows <- nrow(main_bin)
train_set_size <- 0.75
trainIds <- sort(sample(1:main_nrows, main_nrows * train_set_size)) # 1260
testIds <- c(1:main_nrows)[!(c(1:main_nrows) %in% trainIds)] # 420

train <- main_bin[trainIds,]
train_for_modeling <- subset(train, select = -Nazwa)
row.names(train_for_modeling) <- NULL
test_bin <- main_bin[testIds,]
test_cont <- main_cont[testIds,]

# building a model
classif_task <- makeClassifTask(data = train_for_modeling, target = "score", positive = 1)
classif_lrn <- makeLearner("classif.ranger", par.vals = list( "num.trees" = 2500), predict.type = "prob")
res_ranger <- tuneRanger(classif_task, measure = list(gmean), num.threads = 6, num.trees = 2500)
# uwaga!!! To wy¿ej na wiekszym zbiorze dluzej sie mieli (~20 minut?)

row.names(test_bin) <- NULL
row.names(test_cont) <- NULL

gminy <- test_bin[,c("Nazwa")]
#gminy
test_for_predicting <- subset(test_bin, select = -Nazwa)
X_test <- subset(test_for_predicting, select = -score)
pred_ranger <- predict(res_ranger$model, newdata = X_test)
result <- as.data.frame(cbind(as.numeric(pred_ranger$data$prob.1), gminy))
colnames(result) = c("score", "name")
#print(result)
result$scoreBin <- ifelse(result$score <= 0.5, 0, 1)

yBin <- test_bin$score
yCont <- test_cont$score
predictedBin <- result$scoreBin
predictedCont <- as.numeric(result$score)
predBin <- prediction(predictedBin, yBin)

# BINARY
## 1. Accuracy
print(paste0("Accuracy: ", round(mean(yBin == predictedBin), 2))) # 0.86

## 2. ROC curve
ROC.perf <- performance(predBin, "tpr", "fpr")
plot (ROC.perf); # beautiful!!

## 3. ROC area under the curve
auc.tmp <- performance(predBin,"auc");
auc <- as.numeric(auc.tmp@y.values)
print(paste0("Area of place under the curve: ", round(auc, 2))) # 0.85

## 4. Precision
print(paste0("Precision: ", round(Precision(yBin, predictedBin), 2))) # 0.87

## 5. Recall
print(paste0("Recall: ", round(Recall(yBin, predictedBin), 2))) # 0.9

## 6. F1 score
print(paste0("F1 score: ", round(F1_Score(yBin, predictedBin), 2))) # 0.88

# CONTINUOUS
# 7. Correlation
cor(yCont, predictedCont) # 0.71
