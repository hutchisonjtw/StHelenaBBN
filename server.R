library(shiny)
library(shinydashboard)

shinyServer(function(input, output) {
    
    library(raster)
    library(RColorBrewer)
    
    plotZoom <- reactiveValues(x = NULL)
    
    boundary <- reactiveValues(box = NULL, cells =  NULL)
    
    boundary2 <- reactiveValues(topLeft = NULL, bottomRight = NULL)
    
    habMap <- raster("D:/St Helena/BBN input layers/LandcoverType.tif")
    
    habitats <- data.frame(ID = 1:10, habitat = c("Native forest", "Introduced forest", "Native shrubland", 
                                                  "Introduced shrubland", "Flax", "Arable", "Pasture", 
                                                  "Plantations", "Rural gardens", "Other"))
    
    createRat <- function(map, habitats) {
        mapFactor <- ratify(map)
        mapRat <- data.frame(ID = levels(mapFactor)[[1]])
        mapRat$habitat <- habitats$habitat[habitats$ID == mapRat$ID]
        return(mapRat)
    }
    
    habMapRat <- createRat(habMap, habitats)

    
    habMapPlot <- reactiveValues(map = habMap, rat = habMapRat)
    
    habMapMat <- reactive(as.matrix(habMapPlot$map))
    
    output$habMap <- renderPlot({
        par(mar = c(4,4,2,15))
        plot(habMapPlot$map, col = rev(brewer.pal(10, "Paired")), legend = FALSE, ext = plotZoom$x)
        legend("right", legend = habMapPlot$rat$habitat, fill = rev(brewer.pal(10, "Paired")), xpd = TRUE, inset = -0.25, bty = "n")
    })
    
    # output$clickCoords <- renderText({
    #     paste0("x=", input$habMap_click$x, "\ny=", input$habMap_click$y)
    # })
    
    observeEvent(input$habMap_dblclick, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            plotZoom$x <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
            
        } else {
            plotZoom$x <- NULL
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
               "\ntoClass = ", input$toClass
        )
    })
    
    observeEvent(input$change, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            boundary2$topLeft <- rowColFromCell(habMap, cellFromXY(habMap, c(brush$xmin, brush$ymax)))
            boundary2$bottomRight <- rowColFromCell(habMap, cellFromXY(habMap, c(brush$xmax, brush$ymin)))
            habMapMatrix <- habMapMat()
            habMapMatrix[boundary2$topLeft[ , 1]:boundary2$bottomRight[ , 1], 
                      boundary2$topLeft[ , 2]:boundary2$bottomRight[ , 2]][which(
                          habMapMatrix[boundary2$topLeft[ , 1]:boundary2$bottomRight[ , 1],
                                    boundary2$topLeft[ , 2]:boundary2$bottomRight[ , 2]]
                          == input$fromClass)] <- as.numeric(input$toClass)
            habMapPlot$map <- raster(habMapMatrix, template = habMap)
            #writeRaster(habMapPlot, file = "D:/St Helena/habmap.tif", overwrite = TRUE)
        } else {
            showModal(modalDialog(
                "Please click and drag on the map to indicate the area in which you want to change land cover.",
                easyClose = TRUE))
        }
    })
  
})
