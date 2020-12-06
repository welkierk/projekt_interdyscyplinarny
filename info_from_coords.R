library(stringr)
library(ggmap)
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
      gminy[results_element_add] <- paste0("gmina ", substr(element_gmina,1,nchar(element_gmina)-7))
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
  cbind(gminy[1:results_element_add-1], powiaty[1:results_element_add-1], postal_codes[1:results_element_add-1])
}

#przykład
#longitude <- runif(20, 19, 23)
#latitude <- runif(20, 52, 54)
#dane_ze_wspol(longitude, latitude)
