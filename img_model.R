library(raster)
library(stringr)
options(stringsAsFactors = F)


ref <- shapefile('../satellite_img/Gminy.shp') #pobrane z http://www.gugik.gov.pl/pzgik/dane-bez-oplat/dane-z-panstwowego-rejestru-granic-i-powierzchni-jednostek-podzialow-terytorialnych-kraju-prg

train_data <- read.csv('train.csv', fileEncoding='Windows-1250')

ref$JPT_NAZWA_

train_data$gmina <- str_remove(train_data$gmina, "gmina ")
ref_short <- ref[ref$JPT_NAZWA_ %in% train_data$gmina, c('JPT_NAZWA_', 'Shape_Leng', 'Shape_Area')]
ref_short$class <- rep(0, nrow(ref_short@data))

gmina_poz<- train_data[train_data[['wynik']] == 1, 'gmina']

ref_short[ref_short$JPT_NAZWA_ %in% gmina_poz, 'class'] <- 1

#okej, przygotowana ramka ref_short


?superClass


gminy_wwk <- c("Kowal","Brześć Kujawski", "Chodecz", "Lubień Kujawski", "Lubraniec", "Baruchowo", "Boniewo", "Choceń", "Fabianki", "Kowal", "Lubanie", "Włocławek", "Izbica Kujawska") 

ref_wwk <- ref[ref$JPT_NAZWA_ %in% gminy_wwk, c('JPT_NAZWA_', 'Shape_Leng', 'Shape_Area')]
ref_wwk$class <- c(1,0,0,0,0,0,0,0,1,0,0,1,1,0)

r1 <- raster("../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B05.jp2")
wwk_stack <- stack("../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B02.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B03.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B04.jp2",
             "../satellite_img/CREODIAS_client/download/__Sentinel2_3_LEVEL1C_19.0864_52.6452_2020-12-06T22_34_10.332287/S2A_MSIL1C_20170730T100031_N0205_R122_T33UYU_20170730T100535.SAFE/GRANULE/L1C_T33UYU_A010987_20170730T100535/IMG_DATA/T33UYU_20170730T100031_B08.jp2")
plotRGB(wwk_stack, r=3, g=2, b=1, stretch='lin')

#problem: zdjecie jest w projekcji UTM (Mercator), a dane w longlat
library(rgdal)

projectRaster(wwk_stack, crs='+proj=longlat +ellps=GRS80 +no_defs')

utms <- SpatialPoints(wwk_stack[, c("long", "lat")], proj4string=CRS("+proj=utm +zone=10")) #create UTM matrix


superClass(wwk_stack, ref_wwk, set.seed(1), trainPartition = 0.7, responseCol = 'class', model='svmLinear', mode='classification', tuneLength = 10,
           kfold = 2)
