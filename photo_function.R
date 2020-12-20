library(SoDA)
library(geodist)
library(getSpatialData)
library(water)
library(dplyr)
source("./random_points_on_roads_from_area.R")

area_around_point <- function(lat, lon, a = 10000) {
  # Znajduje dwa punkty na mapie wyznaczajace kwadrat o boku a,
  # gdzie punkt (lat, lon) jest srodkiem tego kwadrata.
  
  dist <- a*sqrt(2)/2
  x <- 0
  lat1 <- lat
  lon1 <- lon
  while (x < dist) {
    lat1 <- lat1 + 0.001
    lon1 <- lon1 - 0.001
    df <- geodist(data.frame(lat = c(lat, lat1), long = c(lon, lon1)))
    x <- df[1,2]
  }
  lat2 <- lat - (lat1 - lat)
  lon2 <- lon + (lon - lon1)
  points <- data.frame("Lattitude" = c(lat1, lat2),
                       "Longtitude" = c(lon1, lon2))
}

data_frame_from_photos <- function(lat, lon, a = 10000) {
  # Zwraca informacje o wodzie i roslinnosci na obszarze o polu a^2 w kolo punktu (lat, lon)
  
  df <- area_around_point(lat, lon, a)
  
  aoi <- createAoi(topleft = as.matrix(df[2,]), bottomright = as.matrix(df[1,]), EPSG=4326)
  set_aoi(aoi)
  
  # logowanie do serwisu (niestety narazie trzeba stworzyc konto i logowac sie, w przyszlosci do poprawy)
  time_range <-  c("2018-01-01", "2018-12-31")
  platform <- "Sentinel-2"
  login_CopHub(username = "jacekchess", password = "Jacekchess6")
  
  # odfiltrowanie zdjec ograniczonych do zakresu
  query <- getSentinel_records(time_range, platform)
  query <- query[query$level == "Level-2A", ]
  data_mod <- query %>%
    # group_by(tile_id) %>%
    summarise(water_mean = mean(water), vegetation_mean = mean(vegetation))
  data_mod <- data.frame(water = data_mod$water_mean, vegetation = data_mod$vegetation_mean)
}
