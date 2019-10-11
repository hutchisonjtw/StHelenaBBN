library(shiny)
library(shinydashboard)

shinyServer(function(input, output) {
    
    library(raster)
    library(RColorBrewer)
    
    brushBox <- reactiveValues(crop = NULL, update = NULL)
    
    boundary <- reactiveValues(box = NULL, cells =  NULL)
    
    boundary2 <- reactiveValues(topLeft = NULL, bottomRight = NULL)
    
    habMap <- raster("D:/St Helena/BBN input layers/LandcoverType.tif")
    
    habitats <- data.frame(ID = 1:10, habitat = c("Native forest", "Introduced forest", "Native shrubland", 
                                                  "Introduced shrubland", "Flax", "Arable", "Pasture", 
                                                  "Plantations", "Rural gardens", "Other"))
    habitats$cols <- rev(brewer.pal(10, "Paired"))
    
    createRat <- function(map, habitats) {
        mapFactor <- ratify(map)
        mapRat <- data.frame(ID = levels(mapFactor)[[1]])
        mapRat$habitat <- habitats$habitat[match(mapRat$ID, habitats$ID)]
        mapRat$cols <- habitats$cols[match(mapRat$ID, habitats$ID)]
        levels(mapFactor) <- mapRat
        return(mapFactor)
    }
    
    
    habMapRat <- createRat(habMap, habitats)
    
    habMapPlot <- reactiveValues(map = habMapRat)
    
    habMapMat <- reactive(as.matrix(habMapPlot$map))
    
    
## Plot map, with zoom if boundary drawn - tried re-writing this with rasterVis::levelplot, but it uses lattice graphics and the plotOutput brush function does not return the plot coordinates with lattice graphics so cannot be used to interact with the raster.    
    output$habMap <- renderPlot({
        par(mar = c(4,4,2,15))
        plot(habMapPlot$map, col = levels(habMapPlot$map)[[1]]$cols, legend = FALSE)
        legend("right", legend = levels(habMapPlot$map)[[1]]$habitat, fill = levels(habMapPlot$map)[[1]]$cols, xpd = TRUE, inset = -0.25, bty = "n")
    })
    
    # output$clickCoords <- renderText({
    #     paste0("x=", input$habMap_click$x, "\ny=", input$habMap_click$y)
    # })

## Crop to zoom map
        
    observeEvent(input$habMap_dblclick, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            plotZoom$x <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
            croppedMap <- crop(habMap, plotZoom$x)
            croppedMapRat <- createRat(croppedMap, habitats)
            habMapPlot$map <- croppedMapRat
        } else {
            plotZoom$x <- NULL
            habMapPlot$map <- habMapRat
        }
    })
    
    observeEvent(input$habMap_click, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            boundary$box <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
            boundary$cells <- cellsFromExtent(habMap, boundary$box)
        } else {
            boundary$box <- NULL
            boundary$cells <- NULL
        }
    })
    
    output$boundaryBox <- renderText({
        paste0("xmin = ", boundary$box@xmin,
               "\nxmax = ", boundary$box@xmax,
               "\nymin = ", boundary$box@ymin,
               "\nymax = ", boundary$box@ymax,
               "\nncells = ", length(boundary$cells),
               "\nfromClass = ", input$fromClass,
               "\ntoClass = ", input$toClass,
               "\nboundary2 = ", boundary2$topLeft
        )
    })
    
    # observeEvent(input$change, {
    #     brush <- input$habMap_brush
    #     if (!is.null(brush)) {
    #         boundary2$topLeft <- rowColFromCell(habMap, cellFromXY(habMap, c(brush$xmin, brush$ymax)))
    #         boundary2$bottomRight <- rowColFromCell(habMap, cellFromXY(habMap, c(brush$xmax, brush$ymin)))
    #         habMapMatrix <- habMapMat()
    #         habMapMatrix[boundary2$topLeft[ , 1]:boundary2$bottomRight[ , 1], 
    #                   boundary2$topLeft[ , 2]:boundary2$bottomRight[ , 2]][which(
    #                       habMapMatrix[boundary2$topLeft[ , 1]:boundary2$bottomRight[ , 1],
    #                                 boundary2$topLeft[ , 2]:boundary2$bottomRight[ , 2]]
    #                       == input$fromClass)] <- as.numeric(input$toClass)
    #         habMapPlot$map <- raster(habMapMatrix, template = habMap)
    #         #writeRaster(habMapPlot, file = "D:/St Helena/habmap.tif", overwrite = TRUE)
    #     } else {
    #         showModal(modalDialog(
    #             "Please click and drag on the map to indicate the area in which you want to change land cover.",
    #             easyClose = TRUE))
    #     }
    # })

    
## Update cell values
    
    observeEvent(input$btnChange, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            brushBox$update <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
            habMapUpdate <- habMap
            cells <- cellsFromExtent(habMapUpdate, plotZoom$x)
            vals <- habMapUpdate[cells]
            vals[vals==input$fromClass] <- as.numeric(input$toClass)
            habMapUpdate[cells] <- vals
            habMapUpdateRat <- createRat(habMapUpdate, habitats)
            habMapPlot$map <- habMapUpdateRat
            #writeRaster(habMapPlot, file = "D:/St Helena/habmap.tif", overwrite = TRUE)
        } else {
            showModal(modalDialog(
                "Please click and drag on the map to indicate the area in which you want to change land cover.",
                easyClose = TRUE))
        }
    })
  
})
