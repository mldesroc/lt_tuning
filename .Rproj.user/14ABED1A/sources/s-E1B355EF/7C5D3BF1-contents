library(raster)
library(sf)
library(fasterize)
library(here)

#load rasters and shapefiles
shp_HWF_boundary <- st_read("Shapefiles/HWF_Property_Boundary/Property_Boundary.shp")
shp_HWF_harvests <- st_read("Shapefiles/HWF_Recent_Harvests/HWF_Recent_Harvests.shp")
shp_UHW_boundary <- st_read("Shapefiles/UHW_Tracts/UHW_Tracts.shp")
shp_UHW_harvests <- st_read("Shapefiles/UHW_NY_UpperHudson_harvests_FW/NY_UpperHudson_harvests_FW.shp")

disturbances <- raster("rasters/lt_gee_nbr_greatest_tuning_modelproportion_1.tif")

#clip/mask rasters to extent of harvest polygons
UHW_clip <- crop(disturbances, shp_UHW_boundary, snap='out')
invisible(UHW_clip <- mask(UHW_clip, shp_UHW_boundary, snap ='out'))
HWF_clip <- crop(disturbances, shp_HWF_boundary, snap = 'out')
HWF_clip <- mask(HWF_clip, shp_HWF_boundary, snap = 'out')
UHW_clip[UHW_clip==2020] <- 0
UHW_clip[UHW_clip<2010] <- 0
UHW_clip[UHW_clip<1990] <- 0
HWF_clip[HWF_clip==2020] <- 0
HWF_clip[HWF_clip<1990] <- 0

#create other rasters 
UHW_harvests <- fasterize(shp_UHW_harvests, UHW_clip, field ="Year2")
UHW_harvest_ranges <- fasterize(shp_UHW_harvests, UHW_clip, field = "Range")
HWF_harvests <- fasterize(shp_HWF_harvests, HWF_clip, field = "Date")

#set no data values to zero
UHW_clip[is.na(UHW_clip)] <- 0
UHW_harvests[is.na(UHW_harvests)] <- 0
UHW_harvest_ranges[is.na(UHW_harvest_ranges)] <- 0
HWF_clip[is.na(HWF_clip)] <- 0
HWF_harvests[is.na(HWF_harvests)] <- 0

#UHW accuracy assessment calculations
UHW_accuracy_assessment <- UHW_clip
UHW_accuracy_assessment[!is.na(UHW_accuracy_assessment)] <- NA

UHW_accuracy_assessment[UHW_clip + UHW_harvests == 0] <- 1
UHW_accuracy_assessment[UHW_clip == 0 & UHW_harvests > 1] <- 2
UHW_accuracy_assessment[UHW_clip >0 & ((abs(UHW_clip - UHW_harvests)) <= (1+(UHW_harvest_ranges/2)))] <- 3
UHW_accuracy_assessment[UHW_clip >0 & ((abs(UHW_clip - UHW_harvests)) > (1+(UHW_harvest_ranges/2)))] <- 4

UHW_accuracy_assessment <- crop(UHW_accuracy_assessment, shp_UHW_boundary, snap = 'out')
UHW_accuracy_assessment <- mask(UHW_accuracy_assessment, shp_UHW_boundary, snap = 'out')

#HWF accuracy assessment
HWF_accuracy_assessment <- HWF_clip
HWF_accuracy_assessment[!is.na(HWF_accuracy_assessment)] <- NA

HWF_accuracy_assessment[HWF_clip + HWF_harvests ==0] <- 1
HWF_accuracy_assessment[HWF_clip==0 & HWF_harvests >1] <- 2
HWF_accuracy_assessment[HWF_clip>0 & ((abs(HWF_clip - HWF_harvests)) <= 1)] <- 3
HWF_accuracy_assessment[HWF_clip>0 & ((abs(HWF_clip - HWF_harvests)) > 1)] <- 4

HWF_accuracy_assesment <- crop(HWF_accuracy_assessment, shp_HWF_boundary, snap = "out")
HWF_accuracy_assesment <- mask(HWF_accuracy_assessment, shp_HWF_boundary, snap = "out")

UHW_TP <- UHW_clip
UHW_TP[!is.na(UHW_TP)] <- 0
UHW_TP[is.na(UHW_TP)] <- 0
UHW_TP[UHW_accuracy_assessment == 3] <- 3

HWF_TP <- HWF_clip
HWF_TP[!is.na(HWF_TP)] <- 0
HWF_TP[is.na(HWF_TP)] <- 0
HWF_TP[HWF_accuracy_assessment == 3] <- 3

#accuracy metrics
TN <- as.numeric(freq(HWF_accuracy_assesment, value = 1) + freq(UHW_accuracy_assessment, value = 1))
FN <- as.numeric(freq(HWF_accuracy_assesment, value = 2) + freq(UHW_accuracy_assessment, value = 2))
TP <- as.numeric(freq(HWF_accuracy_assesment, value = 3) + freq(UHW_accuracy_assessment, value = 3))
FP <- as.numeric(freq(HWF_accuracy_assesment, value = 4) + freq(UHW_accuracy_assessment, value = 4))
recall <- TP/(TP + FP)
precision <- TP/(TP + FN)
F1 <- 2 * (recall * precision)/(recall + precision)
accuracy <- (TP + TN)/(TN + FN + TP + FP)

#tuning_results <- read.csv("tuning_results.csv", header = TRUE)
tuning_results <- dplyr::add_row(tuning_results, 
                                 file_name = "lt_gee_nbr_greatest_tuning_modelproportion_5", 
                                 short_name ="modelproportion_0.5", 
                                 maxSegments = 10, 
                                 vertexCountOvershoot = 3, 
                                 recoveryThreshold = 0.25, 
                                 pvalThreshold = 0.2, 
                                 bestModelProportion = 0.5, 
                                 TN = TN, 
                                 FN = FN, 
                                 TP = TP, 
                                 FP = FP, 
                                 Recall = recall,
                                 Precision = precision, 
                                 F1 = F1, 
                                 Accuracy = accuracy)
write.csv(tuning_results, "tuning_results.csv")

#writeRaster(HWF_TP, "HWF_LT_newest_TP_test_2", format = 'GTiff')
#writeRaster(UHW_TP, "UHW_LT_newest_TP_test_2", format = 'GTiff')