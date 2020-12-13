
# LUDNOŚĆ

ludnosc <- read.csv2("./ludnosc/ludnosc.csv", encoding = "UTF-8", sep = ";")
indeks_nazwa <- ludnosc[,1:2]
ludnosc <- ludnosc[,-c(1,2,53)]

przed_prod <- cbind(indeks_nazwa,ludnosc[,1:10])
prod <- cbind(indeks_nazwa,ludnosc[,11:20])
prod_mobil <- cbind(indeks_nazwa,ludnosc[,21:30])
prod_niemobil <- cbind(indeks_nazwa,ludnosc[,31:40])
po_prod <- cbind(indeks_nazwa,ludnosc[,41:50])

lata <- paste0("rok_",2010:2019)
colnames(przed_prod)[3:12] <- lata
colnames(prod)[3:12] <- lata
colnames(prod_mobil)[3:12] <- lata
colnames(prod_niemobil)[3:12] <- lata
colnames(po_prod)[3:12] <- lata

write.csv2(przed_prod,"./ludnosc/ludnosc_przed_prod.csv", row.names = FALSE, na="")
write.csv2(prod,"./ludnosc/ludnosc_prod.csv", row.names = FALSE, na="")
write.csv2(prod_mobil,"./ludnosc/ludnosc_prod_mobil.csv", row.names = FALSE, na="")
write.csv2(prod_niemobil,"./ludnosc/ludnosc_prod_niemobil.csv", row.names = FALSE, na="")
write.csv2(po_prod,"./ludnosc/ludnosc_po_prod.csv", row.names = FALSE, na="")

# read.csv2("./ludnosc/ludnosc_przed_prod.csv")

# POWIERZCHNIA

powierzchnia <- read.csv2("powierzchnia.csv", encoding = "UTF-8")
powierzchnia <- powierzchnia[,-ncol(powierzchnia)]
colnames(powierzchnia)[3:12] <- lata
write.csv2(powierzchnia,"powierzchnia.csv", row.names = FALSE, na="")

# read.csv2("powierzchnia.csv") 

# DOCHODY

dochody <- read.csv2("./dochody/dochody.csv", encoding = "UTF-8")
indeks_nazwa <- dochody[,1:2]
dochody <- dochody[,-c(1,2)]

dochody_total <- cbind(indeks_nazwa,dochody[,1:10])
dochody_rolny <- cbind(indeks_nazwa,dochody[,11:20])
dochody_lesny <- cbind(indeks_nazwa,dochody[,21:30])
dochody_nieruch <- cbind(indeks_nazwa,dochody[,31:40])
dochody_trans <- cbind(indeks_nazwa,dochody[,41:50])
dochody_gosp <- cbind(indeks_nazwa,dochody[,51:60])
dochody_uslug <- cbind(indeks_nazwa,dochody[,61:70])
dochody_majatek <- cbind(indeks_nazwa,dochody[,71:80])
dochody_najm <- cbind(indeks_nazwa,dochody[,81:90])
dochody_inwest <- cbind(indeks_nazwa,dochody[,91:100])

colnames(dochody_total)[3:12] <- lata
colnames(dochody_rolny)[3:12] <- lata
colnames(dochody_lesny)[3:12] <- lata
colnames(dochody_nieruch)[3:12] <- lata
colnames(dochody_trans)[3:12] <- lata
colnames(dochody_gosp)[3:12] <- lata
colnames(dochody_uslug)[3:12] <- lata
colnames(dochody_majatek)[3:12] <- lata
colnames(dochody_najm)[3:12] <- lata
colnames(dochody_inwest)[3:12] <- lata


write.csv2(dochody_total,"./dochody/dochody_total.csv", row.names = FALSE, na="")
write.csv2(dochody_rolny,"./dochody/dochody_podatek_rolny.csv", row.names = FALSE, na="")
write.csv2(dochody_lesny,"./dochody/dochody_podatek_lesny.csv", row.names = FALSE, na="")
write.csv2(dochody_nieruch,"./dochody/dochody_nieruchomosci.csv", row.names = FALSE, na="")
write.csv2(dochody_trans,"./dochody/dochody_srodki_transportowe.csv", row.names = FALSE, na="")
write.csv2(dochody_gosp,"./dochody/dochody_dzialalnosc_gosp.csv", row.names = FALSE, na="")
write.csv2(dochody_uslug,"./dochody/dochody_uslugi.csv", row.names = FALSE, na="")
write.csv2(dochody_majatek,"./dochody/dochody_majatek.csv", row.names = FALSE, na="")
write.csv2(dochody_najm,"./dochody/dochody_najm_dierzawa.csv", row.names = FALSE, na="")
write.csv2(dochody_inwest,"./dochody/dochody_inwestycyjne.csv", row.names = FALSE, na="")

# read.csv2("./dochody/dochody_podatek_rolny.csv")
