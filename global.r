library(shiny)
library(shinydashboard)
library(raster)
library(RColorBrewer)
library(leaflet)
library(bnspatial)
library(tidyverse)

habMap <- raster("habMap.grd") %>%
    {
    NAvalue(.) <- 0
    .
    }

habMapLatest <- habMap

zoomRatPlot <- function(map, zoomExt) {
    mapZoom <- crop(map, zoomExt)
    habitats <- data.frame(ID = 1:10, 
                           habitat = c("Native forest", "Introduced forest", "Native shrubland", 
                                       "Introduced shrubland", "Flax", "Arable", "Pasture", 
                                       "Plantations", "Rural gardens", "Other"),
                           cols = rev(brewer.pal(10, "Paired")),
                           stringsAsFactors = FALSE
    )
    mapFactor <- ratify(mapZoom)
    mapRat <- data.frame(ID = levels(mapFactor)[[1]])
    mapRat$habitat <- habitats$habitat[match(mapRat$ID, habitats$ID)]
    mapRat$cols <- habitats$cols[match(mapRat$ID, habitats$ID)]
    
    mapFactor <- reclassify(mapFactor, rcl = matrix(c(levels(mapFactor)[[1]]$ID, 
                                                      as.numeric(as.factor(levels(mapFactor)[[1]]$ID))),
                                                    ncol = 2))
    levels(mapFactor) <- mapRat
    plot(mapFactor, col = levels(mapFactor)[[1]]$cols, legend = FALSE)
    legend("right", legend = levels(mapFactor)[[1]]$habitat, fill = levels(mapFactor)[[1]]$cols, xpd = TRUE, inset = -0.25, bty = "n")
}

## Read in environmental layers

load("inputLayersDF.r")

intervals_lk <- importClasses('D:/St Helena/BBN input layers/lookup_table_short.txt')

## Read in network
stH_network <- loadNetwork('StH_BBN_draft1r6_181220.net')

## Read in initial ES layers

resultLayers <- c("FoodProvMeat", "FoodProvVeg", "CarbonSequestration", "Coffee", "Honey", "Fuel", "ConstructionMaterials", "RecreationLocal", "RecreationTourists", "GeneticMedicalResources", "ReductionDamageInfraProperty", "WaterProvision", "PrimProdInputs")
initialResults <- lapply(paste0("InitialResults/", resultLayers), raster) %>%
    stack() %>%
    {
        NAvalue(.) <- 0
        .
    }

## Function to convert results of running network back to Raster
predToRas <- function(x){
    result <- match(x, c("Low", "Medium", "High"))
    resultRasVals <- rep(NA, nrow(layersDF))
    resultRasVals[complete.cases(layersDFClass)] <- result
    resultRas <- setValues(habMap, resultRasVals)
    return(resultRas)
}

