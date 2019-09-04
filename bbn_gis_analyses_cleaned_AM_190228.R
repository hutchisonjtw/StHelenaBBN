library(sp)
library(raster)
library(rgdal)
library(bnspatial)
library(gRain)


setwd("D:/St Helena/BBN input layers")
#list.files(pattern = "\\.tif")

#import files as Raster file
alt <- raster("Altitude.tif")
asp <- raster("Aspect.tif")
dPath <- raster("DistancePath.tif")
dRoad <- raster("DistanceRoad.tif")
dSettle <- raster("DistanceSettlement.tif")
forest <- raster("ForestEstate.tif")
landCover <- raster("LandCoverType.tif")
biodiv <- raster("LandscapeBiodiversity.tif")
slope <- raster("Slope.tif")
soilC <- raster("SoilCarbon.tif")
soilPH <- raster("SoilpH.tif")
soilStab <- raster("SoilStability.tif")

layers <- list(alt, asp, dPath, dRoad, dSettle, forest, landCover, biodiv, slope, soilC, soilPH, soilStab)

## Fix resolution and extent
layersClean <- lapply(layers, function(x) {
    res(x) <- c(100,100)
    origin(x) <- c(0,0)
    return(x)
})

minExt <- extent(c(max(sapply(layersClean, xmin)),
                   min(sapply(layersClean, xmax)),
                   max(sapply(layersClean, ymin)),
                   min(sapply(layersClean, ymax))))

layersCrop <- lapply(layersClean, function(x) {
    crop(x, minExt)
})
        
checkStack <- stack(layersCrop)

#CHECK LEVELS
layersFactor <- lapply(layersCrop, as.factor)
sapply(layersFactor, levels)

layersDF <- as.data.frame(checkStack)

#IMPORT NETWORK
stH_network <- loadNetwork('StH_BBN_draft1r6_181220.net')
#stH_network
# networkscenario <- loadNetwork('StH_BBN_final_190228_scenarios.net')
# networkscenario

#using a lookup table where nodes names are without spaces
#lookup is a list which contains three columns that define the data states (classes), the numeric values and whether or not they are categorical
#the table links properly the nodes from the network to the spatial data in input
intervals_lk <- importClasses('lookup_table_short.txt')

layersDFClass <- layersDF

for(i in 1:ncol(layersDF)) {
    layersDFClass[ , i] <- intervals_lk[[names(layersDF)[i]]]$States[layersDF[ , i]]
}

#set the target
targetNodes <- list('FoodProvMeat',
                    'FoodProvVeg',
                    'CarbonSequestration',
                    'Coffee',
                    'Honey',
                    'Fuel',
                    'ConstructionMaterials',
                    'RecreationLocal',
                    'RecreationTourists',
                    'GeneticMedicalResources',
                    'ReductionDamageInfraProperty',
                    'WaterProvision',
                    'PrimProdInputs'
)


##Testing with grainquery:
setEvidence(stH_network, nodes = names(layersDFClass), states = layersDFClass[1, ])
querygrain(stH_network, nodes = unlist(targetNodes), evidence = setEvidence(stH_network, 
                                                                            
## See also:
testResults <- predict(object = stH_network, response = unlist(targetNodes), newdata = layersDFClass[complete.cases(layersDFClass),])

predToRas <- function(x){
    result <- as.factor(x) %>%
        as.numeric()
    resultRasVals <- rep(NA, nrow(layersDF))
    resultRasVals[complete.cases(layersDFClass)] <- result
    resultRas <- setValues(checkStack$Altitude, resultRasVals)
    return(resultRas)
}

resultsRaster <- lapply(testResults$pred, predToRas)
resultsStack <- stack(resultsRaster)
names(resultsStack) <- names(testResults$pred)

#run the bnspatial for class and entropy
bnt1 <- bnspatial(stH_network, targetNodes[[1]], layersFactor, intervals_lk, layersFactor[[1]])

bnt2 <- bnspatial(network, target2, spatialData, intervals_lk, msk=a_raster)

bnt3 <- bnspatial(network, target3, spatialData, intervals_lk, msk=a_raster)

bnt4 <- bnspatial(network, target4, spatialData, intervals_lk, msk=a_raster)

bnt5 <- bnspatial(network, target5, spatialData, intervals_lk, msk=a_raster)

bnt6 <- bnspatial(network, target6, spatialData, intervals_lk, msk=a_raster)

bnt7 <- bnspatial(network, target7, spatialData, intervals_lk, msk=a_raster)

bnt8 <- bnspatial(network, target8, spatialData, intervals_lk, msk=a_raster)

bnt9 <- bnspatial(network, target9, spatialData, intervals_lk, msk=a_raster)

bnt10 <- bnspatial(network, target10, spatialData, intervals_lk, msk=a_raster)

bnt11 <- bnspatial(network, target11, spatialData, intervals_lk, msk=a_raster)

bnt12 <- bnspatial(network, target12, spatialData, intervals_lk, msk=a_raster)

bnt13 <- bnspatial(network, target13, spatialData, intervals_lk, msk=a_raster)

writeRaster(bnt1$Class,"D:/St Helena/BBN output maps/FoodProvMeat_class.tif", overwrite=TRUE)
writeRaster(bnt1$Entropy,"D:/St Helena/BBN output maps/FoodProvMeat_entropy.tif", overwrite=TRUE)

writeRaster(bnt2$Class,"D:/St Helena/BBN output maps/FoodProvVeg_class.tif", overwrite=TRUE)
writeRaster(bnt2$Entropy,"D:/St Helena/BBN output maps/FoodProvVeg_entropy.tif", overwrite=TRUE)

writeRaster(bnt3$Class,"D:/St Helena/BBN output maps/CarbonSequestration_class.tif", overwrite=TRUE)
writeRaster(bnt3$Entropy,"D:/St Helena/BBN output maps/CarbonSequestration_entropy.tif", overwrite=TRUE)

writeRaster(bnt4$Class,"D:/St Helena/BBN output maps/Coffee_class.tif", overwrite=TRUE)
writeRaster(bnt4$Entropy,"D:/St Helena/BBN output maps/Coffee_entropy.tif", overwrite=TRUE)

writeRaster(bnt5$Class,"D:/St Helena/BBN output maps/Honey_class.tif", overwrite=TRUE)
writeRaster(bnt5$Entropy,"D:/St Helena/BBN output maps/Honey_entropy.tif", overwrite=TRUE)

writeRaster(bnt6$Class,"D:/St Helena/BBN output maps/Fuel_class.tif", overwrite=TRUE)
writeRaster(bnt6$Entropy,"D:/St Helena/BBN output maps/Fuel_entropy.tif", overwrite=TRUE)

writeRaster(bnt7$Class,"D:/St Helena/BBN output maps/ConstructionMaterials_class.tif", overwrite=TRUE)
writeRaster(bnt7$Entropy,"D:/St Helena/BBN output maps/ConstructionMaterials_entropy.tif", overwrite=TRUE)

writeRaster(bnt8$Class,"D:/St Helena/BBN output maps/RecreationLocal_class.tif", overwrite=TRUE)
writeRaster(bnt8$Entropy,"D:/St Helena/BBN output maps/RecreationLocal_entropy.tif", overwrite=TRUE)

writeRaster(bnt9$Class,"D:/St Helena/BBN output maps/RecreationTourists_class.tif", overwrite=TRUE)
writeRaster(bnt9$Entropy,"D:/St Helena/BBN output maps/RecreationTourists_entropy.tif", overwrite=TRUE)

writeRaster(bnt10$Class,"D:/St Helena/BBN output maps/GeneticMedicalResources_class.tif", overwrite=TRUE)
writeRaster(bnt10$Entropy,"D:/St Helena/BBN output maps/GeneticMedicalResources_entropy.tif", overwrite=TRUE)

writeRaster(bnt11$Class,"D:/St Helena/BBN output maps/ReductionDamageInfraProperty_class.tif", overwrite=TRUE)
writeRaster(bnt11$Entropy,"D:/St Helena/BBN output maps/ReductionDamageInfraProperty_entropy.tif", overwrite=TRUE)

writeRaster(bnt12$Class,"D:/St Helena/BBN output maps/WaterProvision_class.tif", overwrite=TRUE)
writeRaster(bnt12$Entropy,"D:/St Helena/BBN output maps/WaterProvision_entropy.tif", overwrite=TRUE)

writeRaster(bnt13$Class,"D:/St Helena/BBN output maps/PrimProd_class.tif", overwrite=TRUE)
writeRaster(bnt13$Entropy,"D:/St Helena/BBN output maps/PrimProd_entropy.tif", overwrite=TRUE)

#OUTPUT MAPS1
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(1,2))
plot(bnt8$Class, main='Most likely class')
plot(bnt8$Entropy, main='Uncertainty (Shannon index)')


#run the bnspatial for probability and target states
bn1 <- bnspatial(network, 'FoodProvMeat', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn1$Probability$High, main="Probability of High")
#plot(bn1$Probability$Medium, main="Probability of Medium")
#plot(bn1$Probability$Low, main="Probability of Low")
writeRaster(bn1$Probability$High,"D:/St Helena/BBN output maps/FoodProvMeat_high.tif", overwrite=TRUE)
writeRaster(bn1$Probability$Medium,"D:/St Helena/BBN output maps/FoodProvMeat_medium.tif", overwrite=TRUE)
writeRaster(bn1$Probability$Low,"D:/St Helena/BBN output maps/FoodProvMeat_low.tif", overwrite=TRUE)

bn2 <- bnspatial(network, 'FoodProvVeg', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn2$Probability$High, main="Probability of High")
#plot(bn2Probability$Medium, main="Probability of Medium")
#plot(bn2$Probability$Low, main="Probability of Low")
writeRaster(bn2$Probability$High,"D:/St Helena/BBN output maps/FoodProvVeg_high.tif", overwrite=TRUE)
writeRaster(bn2$Probability$Medium,"D:/St Helena/BBN output maps/FoodProvVeg_medium.tif", overwrite=TRUE)
writeRaster(bn2$Probability$Low,"D:/St Helena/BBN output maps/FoodProvVeg_low.tif", overwrite=TRUE)

bn3 <- bnspatial(network, 'CarbonSequestration', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn3$Probability$High, main="Probability of High")
#plot(bn3$Probability$Medium, main="Probability of Medium")

writeRaster(bn3$Probability$High,"D:/St Helena/BBN output maps/CarbonSequestration_high.tif", overwrite=TRUE)
writeRaster(bn3$Probability$Medium,"D:/St Helena/BBN output maps/CarbonSequestration_medium.tif", overwrite=TRUE)
writeRaster(bn3$Probability$Low,"D:/St Helena/BBN output maps/CarbonSequestration_low.tif", overwrite=TRUE)

bn4 <- bnspatial(network, 'Coffee', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

#par(mfrow=c(1,2))
#plot(bn4$Probability$High, main="Probability of High")
#plot(bn4$Probability$Medium, main="Probability of Medium")

writeRaster(bn4$Probability$High,"D:/St Helena/BBN output maps/Coffee_high.tif", overwrite=TRUE)
writeRaster(bn4$Probability$Medium,"D:/St Helena/BBN output maps/Coffee_medium.tif", overwrite=TRUE)
writeRaster(bn4$Probability$Low,"D:/St Helena/BBN output maps/Coffee_low.tif", overwrite=TRUE)

bn5 <- bnspatial(network, 'Honey', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn5$Probability$High, main="Probability of High")
#plot(bn5$Probability$Medium, main="Probability of Medium")

writeRaster(bn5$Probability$High,"D:/St Helena/BBN output maps/Honey_high.tif", overwrite=TRUE)
writeRaster(bn5$Probability$Medium,"D:/St Helena/BBN output maps/Honey_medium.tif", overwrite=TRUE)
writeRaster(bn5$Probability$Low,"D:/St Helena/BBN output maps/Honey_low.tif", overwrite=TRUE)

bn6 <- bnspatial(network, 'Fuel', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn6$Probability$High, main="Probability of High")
#plot(bn6$Probability$Medium, main="Probability of Medium")

writeRaster(bn6$Probability$High,"D:/St Helena/BBN output maps/Fuel_high.tif", overwrite=TRUE)
writeRaster(bn6$Probability$Medium,"D:/St Helena/BBN output maps/Fuel_medium.tif", overwrite=TRUE)
writeRaster(bn6$Probability$Low,"D:/St Helena/BBN output maps/Fuel_low.tif", overwrite=TRUE)

bn7 <- bnspatial(network, 'ConstructionMaterials', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn7$Probability$High, main="Probability of High")
#plot(bn7$Probability$Low, main="Probability of Low")

writeRaster(bn7$Probability$High,"D:/St Helena/BBN output maps/ConstructionMaterials_high.tif", overwrite=TRUE)
writeRaster(bn7$Probability$Medium,"D:/St Helena/BBN output maps/ConstructionMaterials_medium.tif", overwrite=TRUE)
writeRaster(bn7$Probability$Low,"D:/St Helena/BBN output maps/ConstructionMaterials_low.tif", overwrite=TRUE)

bn8 <- bnspatial(network, 'RecreationLocal', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn8$Probability$High, main="Probability of High")
#plot(bn8$Probability$Low, main="Probability of Low")

writeRaster(bn8$Probability$High,"D:/St Helena/BBN output maps/RecreationLocal_high.tif", overwrite=TRUE)
writeRaster(bn8$Probability$Medium,"D:/St Helena/BBN output maps/RecreationLocal_medium.tif", overwrite=TRUE)
writeRaster(bn8$Probability$Low,"D:/St Helena/BBN output maps/RecreationLocal_low.tif", overwrite=TRUE)

bn9 <- bnspatial(network, 'RecreationTourists', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn9$Probability$High, main="Probability of High")
#plot(bn9$Probability$Medium, main="Probability of Medium")

writeRaster(bn9$Probability$High,"D:/St Helena/BBN output maps/RecreationTourists_high.tif", overwrite=TRUE)
writeRaster(bn9$Probability$Medium,"D:/St Helena/BBN output maps/RecreationTourists_medium.tif", overwrite=TRUE)
writeRaster(bn9$Probability$Low,"D:/St Helena/BBN output maps/RecreationTourists_low.tif", overwrite=TRUE)

bn10 <- bnspatial(network, 'GeneticMedicalResources', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn10$Probability$High, main="Probability of High")
#plot(bn10$Probability$Medium, main="Probability of Medium")

writeRaster(bn10$Probability$High,"D:/St Helena/BBN output maps/GeneticMedicalResources_high.tif", overwrite=TRUE)
writeRaster(bn10$Probability$Medium,"D:/St Helena/BBN output maps/GeneticMedicalResources_medium.tif", overwrite=TRUE)
writeRaster(bn10$Probability$Low,"D:/St Helena/BBN output maps/GeneticMedicalResources_low.tif", overwrite=TRUE)

bn11 <- bnspatial(network, 'ReductionDamageInfraProperty', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn11$Probability$High, main="Probability of High")
#plot(bn11$Probability$Medium, main="Probability of Medium")

writeRaster(bn11$Probability$High,"D:/St Helena/BBN output maps/ReductionDamageInfraProperty_high.tif", overwrite=TRUE)
writeRaster(bn11$Probability$Medium,"D:/St Helena/BBN output maps/ReductionDamageInfraProperty_medium.tif", overwrite=TRUE)
writeRaster(bn11$Probability$Low,"D:/St Helena/BBN output maps/ReductionDamageInfraProperty_low.tif", overwrite=TRUE)

bn12 <- bnspatial(network, 'WaterProvision', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn12$Probability$High, main="Probability of High")
#plot(bn12$Probability$Medium, main="Probability of Medium")

writeRaster(bn12$Probability$High,"D:/St Helena/BBN output maps/WaterProvision_high.tif", overwrite=TRUE)
writeRaster(bn12$Probability$Medium,"D:/St Helena/BBN output maps/WaterProvision_medium.tif", overwrite=TRUE)
writeRaster(bn12$Probability$Low,"D:/St Helena/BBN output maps/WaterProvision_low.tif", overwrite=TRUE)

bn13 <- bnspatial(network, 'PrimProdInputs', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn13$Probability$High, main="Probability of High")
#plot(bn13$Probability$Medium, main="Probability of Medium")

writeRaster(bn13$Probability$High,"D:/St Helena/BBN output maps/PrimProd_high.tif", overwrite=TRUE)
writeRaster(bn13$Probability$Medium,"D:/St Helena/BBN output maps/PrimProd_medium.tif", overwrite=TRUE)
writeRaster(bn13$Probability$Low,"D:/St Helena/BBN output maps/PrimProd_low.tif", overwrite=TRUE)

#addtional network target
target14 <- 'CleanWaterDrinking'
bnt14 <- bnspatial(network, target13, spatialData, intervals_lk, msk=a_raster)

writeRaster(bnt14$Class,"D:/St Helena/BBN output maps/ClWaterDrink_class.tif", overwrite=TRUE)
writeRaster(bnt14$Entropy,"D:/St Helena/BBN output maps/ClWaterDrink_entropy.tif", overwrite=TRUE)

bn14 <- bnspatial(network, 'CleanWaterDrinking', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))
#par(mfrow=c(1,2))
#plot(bn14$Probability$High, main="Probability of High")
#plot(bn14$Probability$Medium, main="Probability of Medium")

writeRaster(bn14$Probability$High,"D:/St Helena/BBN output maps/ClWaterDrink_high.tif", overwrite=TRUE)
writeRaster(bn14$Probability$Medium,"D:/St Helena/BBN output maps/ClWaterDrink_medium.tif", overwrite=TRUE)
writeRaster(bn14$Probability$Low,"D:/St Helena/BBN output maps/ClWaterDrink_low.tif", overwrite=TRUE)




#set the targets for the scenario
target_s1 <-'ArableHortExp'
target_s2 <- 'CoffeeExp'
target_s3 <- 'HoneyExp'
target_s4 <- 'FuelExp'
target_s5 <- 'TimberExp'
target_s6 <- 'FlaxTimber'
target_s7 <-'CSFlaxTimber'
target_s8 <- 'RLFlaxTimber'
target_s9 <- 'RTFlaxTimber'
target_s10 <-'WPFlaxTimber'
target_s11 <- 'FRFlaxTimber'
target_s12 <-'FlaxWoodland'
target_s13 <- 'CSFlaxWoodland'
target_s14 <- 'RLFlaxWoodland'
target_s15 <-'RTFlaxWoodland'
target_s16 <- 'WPFlaxWoodland'
target_s17 <- 'FRFlaxWoodland'
target_s18 <-'Development'


#calculate scenarios
bn_sce1 <- bnspatial(networkscenario, target_s1, spatialData, intervals_lk, msk=a_raster)

writeRaster(bn_sce1$Class,"D:/St Helena/BBN output maps/ArableHortExp_class.tif", overwrite=TRUE)
writeRaster(bn_sce1$Entropy,"D:/St Helena/BBN output maps/ArableHortExp_entropy.tif", overwrite=TRUE)

bn_sce1p <- bnspatial(networkscenario, 'ArableHortExp', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce1p$Probability$High,"D:/St Helena/BBN output maps/ArableHortExp_high.tif", overwrite=TRUE)
writeRaster(bn_sce1p$Probability$Medium,"D:/St Helena/BBN output maps/ArableHortExp_medium.tif", overwrite=TRUE)
writeRaster(bn_sce1p$Probability$Low,"D:/St Helena/BBN output maps/ArableHortExp_low.tif", overwrite=TRUE)


bn_sce2 <- bnspatial(networkscenario, target_s2, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce2$Class,"D:/St Helena/BBN output maps/CoffeeExp_class.tif", overwrite=TRUE)
writeRaster(bn_sce2$Entropy,"D:/St Helena/BBN output maps/CoffeeExp_entropy.tif", overwrite=TRUE)

bn_sce2p <- bnspatial(networkscenario, 'CoffeeExp', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce2p$Probability$High,"D:/St Helena/BBN output maps/CoffeeExp_high.tif", overwrite=TRUE)
writeRaster(bn_sce2p$Probability$Medium,"D:/St Helena/BBN output maps/CoffeeExp_medium.tif", overwrite=TRUE)
writeRaster(bn_sce2p$Probability$Low,"D:/St Helena/BBN output maps/CoffeeExp_low.tif", overwrite=TRUE)

bn_sce3 <- bnspatial(networkscenario, target_s3, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce3$Class,"D:/St Helena/BBN output maps/HoneyExp_class.tif", overwrite=TRUE)
writeRaster(bn_sce3$Entropy,"D:/St Helena/BBN output maps/HoneyExp_entropy.tif", overwrite=TRUE)

bn_sce3p <- bnspatial(networkscenario, 'HoneyExp', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce3p$Probability$High,"D:/St Helena/BBN output maps/HoneyExp_high.tif", overwrite=TRUE)
writeRaster(bn_sce3p$Probability$Medium,"D:/St Helena/BBN output maps/HoneyExp_medium.tif", overwrite=TRUE)
writeRaster(bn_sce3p$Probability$Low,"D:/St Helena/BBN output maps/HoneyExp_low.tif", overwrite=TRUE)


bn_sce4 <- bnspatial(networkscenario, target_s4, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce4$Class,"D:/St Helena/BBN output maps/FuelExp_class.tif", overwrite=TRUE)
writeRaster(bn_sce4$Entropy,"D:/St Helena/BBN output maps/FuelExp_entropy.tif", overwrite=TRUE)

bn_sce4p <- bnspatial(networkscenario, 'FuelExp', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce4p$Probability$High,"D:/St Helena/BBN output maps/FuelExp_high.tif", overwrite=TRUE)
writeRaster(bn_sce4p$Probability$Medium,"D:/St Helena/BBN output maps/FuelExp_medium.tif", overwrite=TRUE)
writeRaster(bn_sce4p$Probability$Low,"D:/St Helena/BBN output maps/FuelExp_low.tif", overwrite=TRUE)


bn_sce5 <- bnspatial(networkscenario, target_s5, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce5$Class,"D:/St Helena/BBN output maps/TimberExp_class.tif", overwrite=TRUE)
writeRaster(bn_sce5$Entropy,"D:/St Helena/BBN output maps/TimberExp_entropy.tif", overwrite=TRUE)

bn_sce5p <- bnspatial(networkscenario, 'TimberExp', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce4p$Probability$High,"D:/St Helena/BBN output maps/TimberExp_high.tif", overwrite=TRUE)
writeRaster(bn_sce4p$Probability$Medium,"D:/St Helena/BBN output maps/TimberExp_medium.tif", overwrite=TRUE)
writeRaster(bn_sce4p$Probability$Low,"D:/St Helena/BBN output maps/TimberExp_low.tif", overwrite=TRUE)

bn_sce6 <- bnspatial(networkscenario, target_s6, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce6$Class,"D:/St Helena/BBN output maps/FlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce6$Entropy,"D:/St Helena/BBN output maps/FlaxTimber_entropy.tif", overwrite=TRUE)


bn_sce6p <- bnspatial(networkscenario, 'FlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce6p$Probability$High,"D:/St Helena/BBN output maps/FlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce6p$Probability$Medium,"D:/St Helena/BBN output maps/FlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce6p$Probability$Low,"D:/St Helena/BBN output maps/FlaxTimber_low.tif", overwrite=TRUE)

bn_sce7 <- bnspatial(networkscenario, target_s7, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce7$Class,"D:/St Helena/BBN output maps/CSFlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce7$Entropy,"D:/St Helena/BBN output maps/CSFlaxTimber_entropy.tif", overwrite=TRUE)

bn_sce7p <- bnspatial(networkscenario, 'CSFlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce7p$Probability$High,"D:/St Helena/BBN output maps/CSFlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce7p$Probability$Medium,"D:/St Helena/BBN output maps/CSFlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce7p$Probability$Low,"D:/St Helena/BBN output maps/CSFlaxTimber_low.tif", overwrite=TRUE)

bn_sce8 <- bnspatial(networkscenario, target_s8, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce8$Class,"D:/St Helena/BBN output maps/RLFlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce8$Entropy,"D:/St Helena/BBN output maps/RLFlaxTimber_entropy.tif", overwrite=TRUE)

bn_sce8p <- bnspatial(networkscenario, 'RLFlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce8p$Probability$High,"D:/St Helena/BBN output maps/RLFlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce8p$Probability$Medium,"D:/St Helena/BBN output maps/RLFlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce8p$Probability$Low,"D:/St Helena/BBN output maps/RLFlaxTimber_low.tif", overwrite=TRUE)


bn_sce9 <- bnspatial(networkscenario, target_s9, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce9$Class,"D:/St Helena/BBN output maps/RTFlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce9$Entropy,"D:/St Helena/BBN output maps/RTFlaxTimber_entropy.tif", overwrite=TRUE)

bn_sce9p <- bnspatial(networkscenario, 'RTFlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce9p$Probability$High,"D:/St Helena/BBN output maps/RTFlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce9p$Probability$Medium,"D:/St Helena/BBN output maps/RTFlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce9p$Probability$Low,"D:/St Helena/BBN output maps/RTFlaxTimber_low.tif", overwrite=TRUE)


bn_sce10 <- bnspatial(networkscenario, target_s10, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce10$Class,"D:/St Helena/BBN output maps/WPFlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce10$Entropy,"D:/St Helena/BBN output maps/WPFlaxTimber_entropy.tif", overwrite=TRUE)

bn_sce10p <- bnspatial(networkscenario, 'WPFlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce10p$Probability$High,"D:/St Helena/BBN output maps/WPFlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce10p$Probability$Medium,"D:/St Helena/BBN output maps/WPFlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce10p$Probability$Low,"D:/St Helena/BBN output maps/WPFlaxTimber_low.tif", overwrite=TRUE)

bn_sce11 <- bnspatial(networkscenario, target_s11, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce11$Class,"D:/St Helena/BBN output maps/FRFlaxTimber_class.tif", overwrite=TRUE)
writeRaster(bn_sce11$Entropy,"D:/St Helena/BBN output maps/FRFlaxTimber_entropy.tif", overwrite=TRUE)

bn_sce11p <- bnspatial(networkscenario, 'FRFlaxTimber', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce11p$Probability$High,"D:/St Helena/BBN output maps/FRFlaxTimber_high.tif", overwrite=TRUE)
writeRaster(bn_sce11p$Probability$Medium,"D:/St Helena/BBN output maps/FRFlaxTimber_medium.tif", overwrite=TRUE)
writeRaster(bn_sce11p$Probability$Low,"D:/St Helena/BBN output maps/FRFlaxTimber_low.tif", overwrite=TRUE)

bn_sce12 <- bnspatial(networkscenario, target_s12, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce12$Class,"D:/St Helena/BBN output maps/FlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce12$Entropy,"D:/St Helena/BBN output maps/FlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce12p <- bnspatial(networkscenario, 'FlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce12p$Probability$High,"D:/St Helena/BBN output maps/FlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce12p$Probability$Medium,"D:/St Helena/BBN output maps/FlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce12p$Probability$Low,"D:/St Helena/BBN output maps/FlaxWoodland_low.tif", overwrite=TRUE)


bn_sce13 <- bnspatial(networkscenario, target_s13, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce13$Class,"D:/St Helena/BBN output maps/CSFlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce13$Entropy,"D:/St Helena/BBN output maps/CSFlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce13p <- bnspatial(networkscenario, 'CSFlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce13p$Probability$High,"D:/St Helena/BBN output maps/CSFlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce13p$Probability$Medium,"D:/St Helena/BBN output maps/CSFlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce13p$Probability$Low,"D:/St Helena/BBN output maps/CSFlaxWoodland_low.tif", overwrite=TRUE)

bn_sce14 <- bnspatial(networkscenario, target_s14, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce14$Class,"D:/St Helena/BBN output maps/RLFlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce14$Entropy,"D:/St Helena/BBN output maps/RLFlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce14p <- bnspatial(networkscenario, 'RLFlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce14p$Probability$High,"D:/St Helena/BBN output maps/RLFlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce14p$Probability$Medium,"D:/St Helena/BBN output maps/RLFlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce14p$Probability$Low,"D:/St Helena/BBN output maps/RLFlaxWoodland_low.tif", overwrite=TRUE)

bn_sce15 <- bnspatial(networkscenario, target_s15, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce15$Class,"D:/St Helena/BBN output maps/RTFlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce15$Entropy,"D:/St Helena/BBN output maps/RTFlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce15p <- bnspatial(networkscenario, 'RTFlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce15p$Probability$High,"D:/St Helena/BBN output maps/RTFlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce15p$Probability$Medium,"D:/St Helena/BBN output maps/RTFlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce15p$Probability$Low,"D:/St Helena/BBN output maps/RTFlaxWoodland_low.tif", overwrite=TRUE)

bn_sce16 <- bnspatial(networkscenario, target_s16, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce16$Class,"D:/St Helena/BBN output maps/WPFlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce16$Entropy,"D:/St Helena/BBN output maps/WPFlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce16p <- bnspatial(networkscenario, 'WPFlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce16p$Probability$High,"D:/St Helena/BBN output maps/WPFlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce16p$Probability$Medium,"D:/St Helena/BBN output maps/WPFlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce16p$Probability$Low,"D:/St Helena/BBN output maps/WPFlaxWoodland_low.tif", overwrite=TRUE)

bn_sce17 <- bnspatial(networkscenario, target_s17, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce17$Class,"D:/St Helena/BBN output maps/FRFlaxWoodland_class.tif", overwrite=TRUE)
writeRaster(bn_sce17$Entropy,"D:/St Helena/BBN output maps/FRFlaxWoodland_entropy.tif", overwrite=TRUE)

bn_sce17p <- bnspatial(networkscenario, 'FRFlaxWoodland', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce17p$Probability$High,"D:/St Helena/BBN output maps/FRFlaxWoodland_high.tif", overwrite=TRUE)
writeRaster(bn_sce17p$Probability$Medium,"D:/St Helena/BBN output maps/FRFlaxWoodland_medium.tif", overwrite=TRUE)
writeRaster(bn_sce17p$Probability$Low,"D:/St Helena/BBN output maps/FRFlaxWoodland_low.tif", overwrite=TRUE)

bn_sce18 <- bnspatial(networkscenario, target_s18, spatialData, intervals_lk, msk=a_raster)
writeRaster(bn_sce18$Class,"D:/St Helena/BBN output maps/Development_class.tif", overwrite=TRUE)
writeRaster(bn_sce18$Entropy,"D:/St Helena/BBN output maps/Development_entropy.tif", overwrite=TRUE)

bn_sce18p <- bnspatial(networkscenario, 'Development', spatialData, intervals_lk, msk=a_raster, what="probability", targetState=c("High","Medium","Low"))

writeRaster(bn_sce18p$Probability$High,"D:/St Helena/BBN output maps/Development_high.tif", overwrite=TRUE)
writeRaster(bn_sce18p$Probability$Medium,"D:/St Helena/BBN output maps/Development_medium.tif", overwrite=TRUE)
writeRaster(bn_sce18p$Probability$Low,"D:/St Helena/BBN output maps/Development_low.tif", overwrite=TRUE)
