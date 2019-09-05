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
# Set all layers to same resolution (some of them were marginally different).
layersClean <- lapply(layers, function(x) {
    res(x) <- c(100,100)
    origin(x) <- c(0,0)
    return(x)
})

# Find minimum extent from all layers
minExt <- extent(c(max(sapply(layersClean, xmin)),
                   min(sapply(layersClean, xmax)),
                   max(sapply(layersClean, ymin)),
                   min(sapply(layersClean, ymax))))

# Crop all layers to minimum extent
layersCrop <- lapply(layersClean, function(x) {
    crop(x, minExt)
})

# Stack layers to confirm resolution and extent all match.        
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
#setEvidence(stH_network, nodes = names(layersDFClass), states = layersDFClass[1, ])
#querygrain(stH_network, nodes = unlist(targetNodes), evidence = setEvidence(stH_network, 
                                                                            
## Better method:
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
