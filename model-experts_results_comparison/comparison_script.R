#set path if needed
#setwd("C:/Users/koste/Studia/07/PI/github/projekt_interdyscyplinarny/model-experts_results_comparison")
library(dplyr)
library(randomForest)
library(mlr)
library(dplyr)
library(ranger)
library(tuneRanger)
library(ggplot2)

# data for regression model
set.seed(1613)
df <- read.csv("../model_z_danymi_ze_zdj/dochody_i_ludnosc_2.csv", encoding = "UTF-8") # 2522 gminas
train <- read.csv("../model_z_danymi_ze_zdj/new_train.csv") # 72 gminas - developed (1) / undeveloped (0)
train <- train[, -1]

colnames(train)
t2 <- train %>% left_join(select(df, -c(X, water, vegetation)), by=c("id" = "Kod"))
colnames(t2)
t3 <- subset(t2, select = -c(X, gmina, powiat, id, longitude.x, latitude.x, longitude.y, latitude.y, Nazwa))
t3 <- na.omit(t3)
#colnames(t3)

# building a model
classif_task <- makeClassifTask(data = t3, target = "wynik", positive = 1)
classif_lrn <- makeLearner("classif.ranger", par.vals = list( "num.trees" = 2500), predict.type = "prob")
res_ranger <- tuneRanger(classif_task, measure = list(gmean), num.threads = 6, num.trees = 2500)
df_test <- na.omit(subset(df, select = -X))
gminy <- df_test[,c("Nazwa")]

df_test <- subset(df_test, select = -c(Nazwa, Kod))
pred_ranger <- predict(res_ranger$model, newdata = df_test)
result <- as.data.frame(cbind(as.numeric(pred_ranger$data$prob.1), gminy))
colnames(result) = c("score", "name")

#result %>% arrange(name)
# Attention please! Some gminas appear more than once with the same name in the country
# However, it's only a few percentages. We will simply not take them into account
# (without loss of generality)
print(paste0("Before deleting repeated onces: ", nrow(result)))
resultFiltered <- result %>%
  group_by(name) %>%
  filter(n() < 2) %>%
  as.data.frame()
print(paste0("After deleting repeated onces: ", nrow(resultFiltered)))
print(paste0("Percent of gminas deleted: ", round((nrow(result)-nrow(resultFiltered))/nrow(result)*100, 2),'%'))
# Yay! Shouldn't make a difference

exDir <- "../rankingi_ekspertow_PW" # expert data folder
cities <- read.csv(paste0(exDir, "/miastaNaPrawachPowiatu.tsv"), sep = '\t', encoding = 'UTF-8')
urban <- read.csv(paste0(exDir, "/gminyMiejskie.tsv"), sep = '\t', encoding = 'UTF-8')
urban_rural <- read.csv(paste0(exDir, "/gminyMiejskoWiejskie.tsv"), sep = '\t', encoding = 'UTF-8')
rural <- read.csv(paste0(exDir, "/gminyWiejskie.tsv"), sep = '\t', encoding = 'UTF-8')

all_gminas <- list(cities, urban, urban_rural, rural)
gminas_types <- c("cities", "urban", "urban_rural", "rural")

which <- 1
for(gmina in all_gminas){
  current_type <- gminas_types[which]
  print(paste0("Analyzing gminas type: ", current_type))
  gmina <- gmina %>%
    select(-c('Województwo', 'Powiat')) %>%
    right_join(resultFiltered, by = c("Gmina" = "name")) %>%
    na.omit()
  nOfRows <- nrow(gmina)
  gmina <- gmina %>%
    mutate(Miejsce.w.rankingu = 1:nOfRows,
           their = 1:nOfRows) %>%
    rename("our" = "Miejsce.w.rankingu", "gmina" = "Gmina")
  gmina <- gmina[order(gmina$score, decreasing=TRUE),] %>%
    mutate(our = 1:nOfRows,
           score = round(as.double(score), 5))
  #print(head(gmina, 10))
  
  corr <- cor(gmina$our, gmina$their)
  print(paste0("Correlation coefficient for ", current_type, ": ", corr))
  ggplot(gmina, aes(our, their)) +
    geom_point() +
    geom_smooth(method="lm") +
    theme_bw() +
    labs(title = paste0("Comparison of placing type: ", current_type),
         subtitle = paste0("cor: ", round(corr, 4)),
         x = "Our place in ranking",
         y = "WUT's experts' place in ranking")
  ggsave(paste0("result_", current_type, ".png"))
  
  which <- which + 1
}
