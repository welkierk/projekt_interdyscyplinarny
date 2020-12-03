library(stringr)
library(ggmap)
#mogę podać mój kod API - mam bezpłatny limit na dość sporą liczbę zapytań,
#ale jest to ograniczona liczba, więc korzystajcie, ale nie róbcie bez potrzeby pętli z tysiącami adresów
register_google(key = "kod_API", write = TRUE)
random_longitude <- runif(11, 14, 24)
random_latitude <- runif(10,50,54)
#wektor na całe adresy
results <- rep(0,10)
#wektor na same kody pocztowe (chyba z nich najłatwiej będzie brać gminy)
postal_codes <- rep(0,10)
#iterator po powyższych wektorach, bo nie wszystkie adresy są takie ładne,
#więc je poniższa pętla pomija
results_element_add <- 0
for (i in 1:10){
  d1 <- random_longitude[i]
  d2 <- random_latitude[i]
  address <- revgeocode(c(d1, d2), output="address")
  if (!is.na(str_match(address, regex("[0-9]+-[0-9]+")))){
    results[results_element_add] <- address
    postal_codes[results_element_add] <- str_match(results[results_element_add], regex("[0-9]+-[0-9]+"))
    results_element_add <- results_element_add + 1
  }
}
results
postal_codes
?register_google
