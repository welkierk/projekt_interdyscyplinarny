source("./photo_function.R")

options(stringsAsFactors = F)
des_classes <- rep("numeric", 10)
names(des_classes) <- c("rok_2010", "rok_2011", "rok_2012", "rok_2013", "rok_2014", "rok_2015", "rok_2016", "rok_2017", "rok_2018", "rok_2019")
powierzchnia <- read.csv("powierzchnia.csv", fileEncoding='Windows-1250', sep = ";")
#dochody <- read.csv("dochody/dochody.csv", sep = ";")

dochody_dzialalnosc_gosp <- read.csv("dochody/dochody_dzialalnosc_gosp.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_inwestycyjne <- read.csv("dochody/dochody_inwestycyjne.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_majatek <- read.csv("dochody/dochody_majatek.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_najm_dierzawa <- read.csv("dochody/dochody_najm_dierzawa.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_nieruchomosci <- read.csv("dochody/dochody_nieruchomosci.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_podatek_lesny <- read.csv("dochody/dochody_podatek_lesny.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_podatek_rolny <- read.csv("dochody/dochody_podatek_rolny.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_srodki_transportowe <- read.csv("dochody/dochody_srodki_transportowe.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_total <- read.csv("dochody/dochody_total.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
dochody_uslugi <- read.csv("dochody/dochody_uslugi.csv", sep = ";", fileEncoding='Windows-1250', dec = ",")
ludnosc_po_prod <- read.csv("ludnosc/ludnosc_po_prod.csv", sep=";", fileEncoding='Windows-1250', dec = ",")
ludnosc_prod <- read.csv("ludnosc/ludnosc_prod.csv", sep=";", fileEncoding='Windows-1250', dec = ",")
ludnosc_prod_mobil <- read.csv("ludnosc/ludnosc_prod_mobil.csv", sep=";", fileEncoding='Windows-1250', dec = ",")
ludnosc_prod_niemobil <- read.csv("ludnosc/ludnosc_prod_niemobil.csv", sep=";", fileEncoding='Windows-1250', dec = ",")
ludnosc_przed_prod <- read.csv("ludnosc/ludnosc_przed_prod.csv", sep=";", fileEncoding='Windows-1250', dec = ",")

dochody_dzialalnosc_gosp[["dochody_dzialalnosc_gosp_wzrost_2018"]] <- (dochody_dzialalnosc_gosp[["rok_2018"]] - dochody_dzialalnosc_gosp[["rok_2019"]])/dochody_dzialalnosc_gosp[["rok_2019"]]
dochody_dzialalnosc_gosp[["dochody_dzialalnosc_gosp_wzrost_2019"]] <- (dochody_dzialalnosc_gosp[["rok_2019"]] - dochody_dzialalnosc_gosp[["rok_2018"]])/dochody_dzialalnosc_gosp[["rok_2018"]]

#dochody_inwestycyjne[["dochody_inwestycyjne_wzrost_2018"]] <- (dochody_inwestycyjne[["rok_2018"]] - dochody_inwestycyjne[["rok_2019"]])/dochody_inwestycyjne[["rok_2019"]]
#dochody_inwestycyjne[["dochody_inwestycyjne_wzrost_2019"]] <- (dochody_inwestycyjne[["rok_2019"]] - dochody_inwestycyjne[["rok_2018"]])/dochody_inwestycyjne[["rok_2018"]]

dochody_majatek[["dochody_majatek_wzrost_2018"]] <- (dochody_majatek[["rok_2018"]] - dochody_majatek[["rok_2019"]])/dochody_majatek[["rok_2019"]]
dochody_majatek[["dochody_majatek_wzrost_2019"]] <- (dochody_majatek[["rok_2019"]] - dochody_majatek[["rok_2018"]])/dochody_majatek[["rok_2018"]]

dochody_najm_dierzawa[["dochody_najm_dzierzawa_wzrost_2018"]] <- (dochody_najm_dierzawa[["rok_2018"]] - dochody_najm_dierzawa[["rok_2019"]])/dochody_najm_dierzawa[["rok_2019"]]
dochody_najm_dierzawa[["dochody_najm_dzierzawa_wzrost_2019"]] <- (dochody_najm_dierzawa[["rok_2019"]] - dochody_najm_dierzawa[["rok_2018"]])/dochody_najm_dierzawa[["rok_2018"]]

dochody_nieruchomosci[["dochody_nieruchomosci_wzrost_2018"]] <- (dochody_nieruchomosci[["rok_2018"]] - dochody_nieruchomosci[["rok_2019"]])/dochody_nieruchomosci[["rok_2019"]]
dochody_nieruchomosci[["dochody_nieruchomosci_wzrost_2019"]] <- (dochody_nieruchomosci[["rok_2019"]] - dochody_nieruchomosci[["rok_2018"]])/dochody_nieruchomosci[["rok_2018"]]

dochody_podatek_lesny[["dochody_podatek_lesny_wzrost_2018"]] <- (dochody_podatek_lesny[["rok_2018"]] - dochody_podatek_lesny[["rok_2019"]])/dochody_podatek_lesny[["rok_2019"]]
dochody_podatek_lesny[["dochody_podatek_lesny_wzrost_2019"]] <- (dochody_podatek_lesny[["rok_2019"]] - dochody_podatek_lesny[["rok_2018"]])/dochody_podatek_lesny[["rok_2018"]]

dochody_podatek_rolny[["dochody_podatek_rolny_wzrost_2018"]] <- (dochody_podatek_rolny[["rok_2018"]] - dochody_podatek_rolny[["rok_2019"]])/dochody_podatek_rolny[["rok_2019"]]
dochody_podatek_rolny[["dochody_podatek_rolny_wzrost_2019"]] <- (dochody_podatek_rolny[["rok_2019"]] - dochody_podatek_rolny[["rok_2018"]])/dochody_podatek_rolny[["rok_2018"]]

dochody_srodki_transportowe[["dochody_srodki_transportowe_wzrost_2018"]] <- (dochody_srodki_transportowe[["rok_2018"]] - dochody_srodki_transportowe[["rok_2019"]])/dochody_srodki_transportowe[["rok_2019"]]
dochody_srodki_transportowe[["dochody_srodki_transportowe_wzrost_2019"]] <- (dochody_srodki_transportowe[["rok_2019"]] - dochody_srodki_transportowe[["rok_2018"]])/dochody_srodki_transportowe[["rok_2018"]]

dochody_total[["dochody_total_wzrost_2018"]] <- (dochody_total[["rok_2018"]] - dochody_total[["rok_2019"]])/dochody_total[["rok_2019"]]
dochody_total[["dochody_total_wzrost_2019"]] <- (dochody_total[["rok_2019"]] - dochody_total[["rok_2018"]])/dochody_total[["rok_2018"]]

dochody_uslugi[["dochody_uslugi_wzrost_2018"]] <- (dochody_uslugi[["rok_2018"]] - dochody_uslugi[["rok_2019"]])/dochody_uslugi[["rok_2019"]]
dochody_uslugi[["dochody_uslugi_wzrost_2019"]] <- (dochody_uslugi[["rok_2019"]] - dochody_uslugi[["rok_2018"]])/dochody_uslugi[["rok_2018"]]

ludnosc_po_prod[["ludnosc_po_prod_wzrost_2018"]] <- (ludnosc_po_prod[["rok_2018"]] - ludnosc_po_prod[["rok_2019"]])/ludnosc_po_prod[["rok_2019"]]
ludnosc_po_prod[["ludnosc_po_prod_wzrost_2019"]] <- (ludnosc_po_prod[["rok_2019"]] - ludnosc_po_prod[["rok_2018"]])/ludnosc_po_prod[["rok_2018"]]

ludnosc_prod[["ludnosc_prod_wzrost_2018"]] <- (ludnosc_prod[["rok_2018"]] - ludnosc_prod[["rok_2019"]])/ludnosc_prod[["rok_2019"]]
ludnosc_prod[["ludnosc_prod_wzrost_2019"]] <- (ludnosc_prod[["rok_2019"]] - ludnosc_prod[["rok_2018"]])/ludnosc_prod[["rok_2018"]]

ludnosc_prod_mobil[["ludnosc_prod_mobil_wzrost_2018"]] <- (ludnosc_prod_mobil[["rok_2018"]] - ludnosc_prod_mobil[["rok_2019"]])/ludnosc_prod_mobil[["rok_2019"]]
ludnosc_prod_mobil[["ludnosc_prod_mobil_wzrost_2019"]] <- (ludnosc_prod_mobil[["rok_2019"]] - ludnosc_prod_mobil[["rok_2018"]])/ludnosc_prod_mobil[["rok_2018"]]

ludnosc_prod_niemobil[["ludnosc_prod_niemobil_wzrost_2018"]] <- (ludnosc_prod_niemobil[["rok_2018"]] - ludnosc_prod_niemobil[["rok_2019"]])/ludnosc_prod_niemobil[["rok_2019"]]
ludnosc_prod_niemobil[["ludnosc_prod_niemobil_wzrost_2019"]] <- (ludnosc_prod_niemobil[["rok_2019"]] - ludnosc_prod_niemobil[["rok_2018"]])/ludnosc_prod_niemobil[["rok_2018"]]

ludnosc_przed_prod[["ludnosc_przed_prod_wzrost_2018"]] <- (ludnosc_przed_prod[["rok_2018"]] - ludnosc_przed_prod[["rok_2019"]])/ludnosc_przed_prod[["rok_2019"]]
ludnosc_przed_prod[["ludnosc_przed_prod_wzrost_2019"]] <- (ludnosc_przed_prod[["rok_2019"]] - ludnosc_przed_prod[["rok_2018"]])/ludnosc_przed_prod[["rok_2018"]]

ludnosc_wzrost_merged <- merge.data.frame(ludnosc_przed_prod[,c("Kod", "Nazwa", "ludnosc_przed_prod_wzrost_2018", "ludnosc_przed_prod_wzrost_2019")], ludnosc_prod[,c("Kod", "Nazwa", "ludnosc_prod_wzrost_2018", "ludnosc_prod_wzrost_2019")], by = c("Kod", "Nazwa"))
ludnosc_wzrost_merged <- merge.data.frame(ludnosc_wzrost_merged, ludnosc_prod_mobil[,c("Kod", "Nazwa", "ludnosc_prod_mobil_wzrost_2018", "ludnosc_prod_mobil_wzrost_2019")], by = c("Kod", "Nazwa"))
ludnosc_wzrost_merged <- merge.data.frame(ludnosc_wzrost_merged, ludnosc_prod_niemobil[,c("Kod", "Nazwa", "ludnosc_prod_niemobil_wzrost_2018", "ludnosc_prod_niemobil_wzrost_2019")], by = c("Kod", "Nazwa"))
ludnosc_wzrost_merged <- merge.data.frame(ludnosc_wzrost_merged, ludnosc_po_prod[,c("Kod", "Nazwa", "ludnosc_po_prod_wzrost_2018", "ludnosc_po_prod_wzrost_2019")], by = c("Kod", "Nazwa"))

dochody_wzrost_merged <- merge.data.frame(dochody_dzialalnosc_gosp[,c("Kod", "Nazwa", "dochody_dzialalnosc_gosp_wzrost_2018", "dochody_dzialalnosc_gosp_wzrost_2019")], dochody_majatek[,c("Kod", "Nazwa", "dochody_majatek_wzrost_2018", "dochody_majatek_wzrost_2018")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_najm_dierzawa[,c("Kod", "Nazwa", "dochody_najm_dzierzawa_wzrost_2018", "dochody_najm_dzierzawa_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_nieruchomosci[,c("Kod", "Nazwa", "dochody_nieruchomosci_wzrost_2018", "dochody_nieruchomosci_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_podatek_lesny[,c("Kod", "Nazwa", "dochody_podatek_lesny_wzrost_2018", "dochody_podatek_lesny_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_podatek_rolny[,c("Kod", "Nazwa", "dochody_podatek_rolny_wzrost_2018", "dochody_podatek_rolny_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_srodki_transportowe[,c("Kod", "Nazwa", "dochody_srodki_transportowe_wzrost_2018", "dochody_srodki_transportowe_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_total[,c("Kod", "Nazwa", "dochody_total_wzrost_2018", "dochody_total_wzrost_2019")], by = c("Kod", "Nazwa"))
dochody_wzrost_merged <- merge.data.frame(dochody_wzrost_merged, dochody_uslugi[,c("Kod", "Nazwa", "dochody_uslugi_wzrost_2018", "dochody_uslugi_wzrost_2019")], by = c("Kod", "Nazwa"))


gminy_dane <- merge.data.frame(dochody_wzrost_merged, ludnosc_wzrost_merged[,-2], by = "Kod")
gminy_dane[gminy_dane==Inf]<- NA

gminy_dane
gminy_dane_stare <- read.csv("dochody_i_ludnosc_2.csv", fileEncoding='Windows-1250')
nrow(gminy_dane_stare)
gminy_dane <- merge.data.frame(gminy_dane, gminy_dane_stare[,c("Kod", "longitude", "latitude")], by="Kod")

water <- c()
vegetation <- c()
for (i in 1:nrow(gminy_dane)) {
  photo <- data_frame_from_photos(lat = gminy_dane$latitude[i], lon = gminy_dane$longitude[i])
  water[i] <- photo[1,1]
  vegetation[i] <- photo[1,2]
  print(i)
}

data <- cbind(gminy_dane, water, vegetation)

write.csv(data, "dochody_i_ludnosc_2017-2019.csv")
