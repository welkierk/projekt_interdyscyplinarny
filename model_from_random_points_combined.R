
library('osmar')
library('osmdata')
library('geosphere')
library('sf')
library(stringr)
library(ggmap)
library(dplyr)
library(randomForest)
library(mlr)
library(ranger)
library(tuneRanger)



# FUNKCJA ZWRACAJACA WSPOLRZEDNE LOSOWYCH PUNKTOW LEZACYCH NA DROGACH
random_points <- function(xmin, ymin, xmax, ymax){
  
  # Wspolrzedne podane na wejsciu przez uzytkownika
  box <- c(xmin, ymin, xmax, ymax)
  
  important_values <- c('residential', 'service', 'track', 'unclassified', 'path', 'tertiary',
                        'secondary', 'primary', 'living_street', 'trunk', 'motorway')
  
  qBox <- box %>% 
    opq(timeout = 10000) %>%
    add_osm_feature(key = 'highway', value = important_values) %>%
    osmdata_sf()
  
  
  random_points_lat <- numeric(0)
  random_points_long <- numeric(0)
  
  for (i in 1:100) {
    random_way <- sample(qBox$osm_lines$geometry, size = 1)
    random_way <- as.vector(random_way[[1]])
    index <- sample(1:(length(random_way)/2), 1)
    random_lat <- random_way[index]
    random_long <- random_way[length(random_way)/2 + index]
    random_points_lat[i] <- random_lat
    random_points_long[i] <- random_long
  }
  
  df <- data.frame("Longtitude" = random_points_lat,
                   "Latitude" = random_points_long)
  
  return(df)
  
}

dane_ze_wspol <- function(wektor_longitude, wektor_latitude){
  stopifnot(length(wektor_longitude) == length(wektor_latitude))
  #mój klucz API - mam bezpłatny limit na dość sporą liczbę zapytań, ale jest to ograniczona
  #liczba, więc korzystajcie, ale nie róbcie bez potrzeby pętli z tysiącami adresów
  register_google(key = "AIzaSyDzpUawTQC4I_Sru1G0EkgcgbsJ9uKAt2I", write = TRUE)
  tb <- read.csv("kody.csv", header = TRUE, row.names=NULL, sep = ";", fileEncoding = "UTF-8")
  tb <- unique(tb[,c("KOD.POCZTOWY","POWIAT")])
  random_longitude <- wektor_longitude
  random_latitude <- wektor_latitude
  n <- length(wektor_longitude)
  #wektor na gminy
  gminy <- rep(0,n)
  #wektor na kody pocztowe
  postal_codes <- rep(0,n)
  #wektor na powiaty
  powiaty <- rep(0, n)
  longitude <- rep(0,n)
  latitude <- rep(0,n)
  #iterator po powyższych wektorach, bo nie wszystkie adresy są takie ładne,
  #więc je poniższa pętla pomija
  results_element_add <- 1
  for (i in 1:n){
    d1 <- random_longitude[i]
    d2 <- random_latitude[i]
    address <- revgeocode(c(d1, d2), output="all")
    df <- str_match(address$results, regex("[^\"]+ County"))
    element_gmina <- df[!is.na(df)]
    df2 <- str_match(address, regex("[0-9]{2}-[0-9]{3}"))
    element_kod <- df2[!is.na(df2)]
    element_powiat <- tb[tb$KOD.POCZTOWY == element_kod,"POWIAT"]
    if (!identical(element_gmina, character(0)) & !identical(element_kod, character(0)) & !identical(element_powiat, character(0))){
      gminy[results_element_add] <- paste0("", substr(element_gmina,1,nchar(element_gmina)-7))
      longitude[results_element_add] <- random_longitude[i]
      latitude[results_element_add] <- random_latitude[i]
      postal_codes[results_element_add] <- element_kod
      if (str_detect(element_powiat, regex("na prawach powiatu"))){
        powiaty[results_element_add] <- substr(element_powiat, 8, nchar(element_powiat)-19)
      }
      else{
        powiaty[results_element_add] <- substr(element_powiat, 8, nchar(element_powiat))
      }
      results_element_add <- results_element_add + 1
    }
  }
  cbind(gminy[1:results_element_add-1], powiaty[1:results_element_add-1], postal_codes[1:results_element_add-1], as.numeric(longitude[1:results_element_add-1]), as.numeric(latitude[1:results_element_add-1]))
}

#funkcja - n to liczba punktów do wylosowania, xmin,ymin,xmax,ymax wspolrzedne (x - dlugosc, y-szerokosc)
#data_frame_for_predictions <- function(xmin, ymin, xmax, ymax, n=100){


predict_from_area <- function(xmin, ymin, xmax, ymax, n=100){
  points <- random_points(xmin, ymin, xmax, ymax)
  points <- points[sample(1:nrow(points),n),]
  rownames(points) <- 1:nrow(points)
  lon <- points$Longtitude
  lat <- points$Latitude
  x <- dane_ze_wspol(lon, lat)
  colnames(x) <- c("gmina", "powiat", "kod_poczt", "długość", "szerokość")
  dt <- read.csv("dochody_i_ludnosc.csv", encoding = "UTF-8")
  dt$Nazwa <- substr(dt$Nazwa,1,nchar(dt$Nazwa)-4)
  dt <- dt %>% distinct(Nazwa, .keep_all=TRUE)
  x <- as.data.frame(x)
  wynik <- x %>% left_join(dt, by=c("gmina"="Nazwa"))
  wynik <- na.omit(wynik)
  ncol(wynik[, 8:ncol(wynik)])
  train <- read.csv("train.csv")
  t2 <- train %>% left_join(dt, by=c("id" = "Kod"))
  t3 <- subset(t2, select = -c(X.x, gmina, powiat, id, longitude, latitude, X.y, Nazwa))
  t3 <- na.omit(t3)
  

  classif_task_ <- makeClassifTask(data = t3, target = "wynik", positive = 1)
  classif_lrn_4 <- makeLearner("classif.ranger", par.vals = list( "num.trees" = 2500), predict.type = "prob")
  res_ranger <- tuneRanger(classif_task_, measure = list(gmean), num.threads = 6, num.trees = 2500)

  nazwy <- wynik[,c(1,4,5)]
  predictions <- predict(res_ranger$model, newdata=wynik[, 8:ncol(wynik)])
  result <- as.data.frame(cbind(as.numeric(predictions$data$prob.1), nazwy))
  return(result)}



##PREZENTACJA 
xmin <- 20.22
ymin <- 52.71
xmax <- 21.11
ymax <- 53.82
n <- 50
result <- predict_from_area(xmin,ymin,xmax,ymax,n)
colnames(result)[1]<-c("score")
result %>% distinct(gmina, .keep_all = TRUE) %>% arrange(desc(score) )
head(result[order(result$score, decreasing=TRUE),],5)
