library(shiny)
library(shinydashboard)
library(raster)
library(RColorBrewer)

habMap <- raster("D:/St Helena/BBN input layers/LandcoverType.tif")

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
