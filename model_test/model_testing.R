#set path if needed
#setwd("C:/Users/koste/Studia/07/PI/github/projekt_interdyscyplinarny/model_test")
library(dplyr)
library(randomForest)
library(mlr)
library(dplyr)
library(ranger)
library(tuneRanger)
# accuracy, precision, etc.
library(ROCR)
library(MLmetrics)

# data for regression model
set.seed(420)
df <- read.csv("../dochody_i_ludnosc.csv", encoding = "UTF-8") # 2521 gminas
modelResults <- read.csv2("gminasDevByModel.csv") # 2046 gminas - developed (1) / undeveloped (0)

is_neg_inf_val <- (df == -Inf)
which_have_neg_infs <- which(apply(is_neg_inf_val, 1, any))
df <- df[-which_have_neg_infs,]
#nrow(df) # 2518

merged <- modelResults %>% left_join(df, by = "X")
#colnames(merged)
main_df <- subset(merged, select = -c(X, gmina, powiat, gminaType, Kod))
main_df <- na.omit(main_df)
#nrow(main_df) # 1680
#colnames(main_df) # 28 feature columns, 1 target one

main_nrows <- nrow(main_df)
train_set_size <- 0.75
trainIds <- sample(1:main_nrows, main_nrows * train_set_size) # 1260
testIds <- c(1:main_nrows)[!(c(1:main_nrows) %in% trainIds)] # 420

train <- main_df[trainIds,]
train_for_modeling <- subset(train, select = -Nazwa)
test <- main_df[testIds,]

# building a model
classif_task <- makeClassifTask(data = train_for_modeling, target = "score", positive = 1)
classif_lrn <- makeLearner("classif.ranger", par.vals = list( "num.trees" = 2500), predict.type = "prob")
res_ranger <- tuneRanger(classif_task, measure = list(gmean), num.threads = 6, num.trees = 2500)
# uwaga!!! To wy¿ej na wiekszym zbiorze dluzej sie mieli (~20 minut?)

gminy <- test[,c("Nazwa")]
test_for_predicting <- subset(test, select = -Nazwa)
X_test <- subset(test_for_predicting, select = -X)
pred_ranger <- predict(res_ranger$model, newdata = X_test)
result <- as.data.frame(cbind(as.numeric(pred_ranger$data$prob.1), gminy))
colnames(result) = c("score", "name")
#print(result)
result$score <- ifelse(result$score <= 0.5, 0, 1)

y <- test$score
predicted <- result$score
pred <- prediction(predicted, y)

# 1. Accuracy
print(paste0("Accuracy: ", round(mean(y == predicted), 2))) # 0.92!!

# 2. ROC curve
ROC.perf <- performance(pred, "tpr", "fpr")
plot (ROC.perf); # beautiful!!

# 3. ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
print(paste0("Area of place under the curve: ", round(auc, 2))) # 0.9!!

# 4. Precision
print(paste0("Precision: ", round(Precision(y, predicted), 2))) # 0.92!!

# 5. Recall
print(paste0("Precision: ", round(Recall(y, predicted), 2))) # 0.84!!

# 6. F1 score
print(paste0("F1 score: ", round(F1_Score(y, predicted), 2))) # 0.88!
      