#setwd("C:/Users/koste/Studia/07/PI/github/projekt_interdyscyplinarny/model_z_danymi_ze_zdj")
dt <- read.csv("dochody_i_ludnosc_2.csv", encoding = "Windows-1250")
dt <- dt[, -c(32:33)]
dt$Nazwa <- substr(dt$Nazwa,1,nchar(dt$Nazwa)-4)
library(stringi)
dt$Nazwa <- stri_trans_general(str = dt$Nazwa, id = "Latin-ASCII")
dt <- dt %>% distinct(Nazwa, .keep_all=TRUE)

train <- read.csv("new_train.csv")
train
train <- train[, -1]
#train

t2 <- train %>% left_join(select(dt, -c(X, water, vegetation)), by=c("id" = "Kod"))
colnames(t2)
t3 <- subset(t2, select = -c(X, gmina, powiat, id, longitude, latitude, Nazwa))
t3 <- na.omit(t3)
#colnames(t3)

rg <- ranger(wynik~., data=t3, importance =  'impurity', num.threads = 6, num.trees = 2500)

saveRDS(rg, "ranger.rds")