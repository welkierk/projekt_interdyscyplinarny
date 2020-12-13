library(dplyr)
library(randomForest)
library(mlr)
library(ranger)
library(tuneRanger)

dt <- read.csv("dochody_i_ludnosc.csv", encoding = "UTF-8")
train <- read.csv("train.csv")
t2 <- train %>% left_join(dt, by=c("id" = "Kod"))
t3 <- subset(t2, select = -c(X.x, gmina, powiat, id, longitude, latitude, X.y, Nazwa))
t3 <- na.omit(t3)

classif_task_ <- makeClassifTask(data = t3, target = "wynik", positive = 1)
classif_lrn_4 <- makeLearner("classif.ranger", par.vals = list( "num.trees" = 2500), predict.type = "prob")
res_ranger <- tuneRanger(classif_task_, measure = list(gmean), num.threads = 6, num.trees = 2500)
dt_test <- na.omit(subset(dt, select = -c(X, Kod)))
gminy <- dt_test[,"Nazwa"]

dt_test <- subset(dt_test, select = -c(Nazwa))
pred_ranger <- predict(res_ranger$model, newdata = dt_test)

result <- as.data.frame(cbind(as.numeric(pred_ranger$data$prob.1),gminy))
head(result[order(result$V1, decreasing=TRUE),],20)
tail(result[order(result$V1, decreasing=TRUE),],20)
