library(raster)
library(stringr)
library(RStoolbox)
options(stringsAsFactors = F)


ref <- shapefile('../satellite_img/Gminy.shp') #pobrane z http://www.gugik.gov.pl/pzgik/dane-bez-oplat/dane-z-panstwowego-rejestru-granic-i-powierzchni-jednostek-podzialow-terytorialnych-kraju-prg

train_data <- read.csv('train.csv', fileEncoding='Windows-1250')

ref$JPT_NAZWA_

train_data$gmina <- str_remove(train_data$gmina, "gmina ")
ref_short <- ref[ref$JPT_KOD_JE %in% c(paste0(0,train_data$id), train_data$id), c('JPT_NAZWA_', 'Shape_Leng', 'Shape_Area')]
ref_short$class <- rep(0, nrow(ref_short@data))

gmina_poz<- train_data[train_data[['wynik']] == 1, 'id']

ref_short[ref_short$JPT_KOD_JE %in% c(gmina_poz, paste0(0,gmina_poz)), 'class'] <- 1

#okej, przygotowana ramka ref_short


?superClass


gminy_wwk <- c("Kowal","Brześć Kujawski", "Chodecz", "Lubień Kujawski", "Lubraniec", "Baruchowo", "Boniewo", "Choceń", "Fabianki", "Kowal", "Lubanie", "Włocławek", "Izbica Kujawska") 

ref_wwk <- ref[ref$JPT_NAZWA_ %in% gminy_wwk, c('JPT_NAZWA_', 'Shape_Leng', 'Shape_Area')]
ref_wwk$class <- c(1,0,0,0,0,0,0,0,1,0,0,1,1,0)

#pliki pobrane przy pomocy polecenia w teminalu: sudo python3 client.py -f -s Sentinel2 -l LEVEL1C -r 2 -c 3 -p 19.08640,52.64520 -t 2017-07-01 -e 2017-08-01
r1 <- raster("../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B05.jp2")
wwk_stack <- stack("../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B02.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B03.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B04.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B08.jp2")
plotRGB(wwk_stack, r=3, g=2, b=1, stretch='lin')

#problem: zdjecie jest w projekcji UTM (Mercator), a dane w longlat
library(rgdal)

wwk_stack_longlat <- projectRaster(wwk_stack, crs='+proj=longlat +ellps=GRS80 +no_defs')

powiat_wwk <- crop(wwk_stack_longlat, c(18.63, 19.53, 52.33, 52.86))

plotRGB(powiat_wwk, r=3, g=2, b=1, stretch='lin')
library(kernlab)
set.seed(1)
ref_wwk_train <- ref_wwk[1:12, ]
ref_wwk_val <- ref_wwk[13:14, ]
wwk_model <- RStoolbox::superClass(powiat_wwk, ref_wwk_train, ref_wwk_val, trainPartition = 0.9, responseCol = 'class', model='rf',
                      mode='classification', tuneLength = 3, kfold = 1, predict=F, verbose=T)

