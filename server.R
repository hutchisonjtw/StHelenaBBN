
#source("./global.R")

shinyServer(function(input, output) {
    
    brushBox <- reactiveValues(zoom = extent(habMap), update = NULL)
    
    boundary <- reactiveValues(box = NULL, cells =  NULL)
    
    #boundary2 <- reactiveValues(topLeft = NULL, bottomRight = NULL)

    habMapPlot <- reactiveValues(map = habMap)
    
    #habMapMat <- reactive(as.matrix(habMapPlot$map))
    
    
## Plot map, with zoom if boundary drawn - tried re-writing this with rasterVis::levelplot, but it uses lattice graphics and the plotOutput brush function does not return the plot coordinates with lattice graphics so cannot be used to interact with the raster.    
    output$habMap <- renderPlot({
        par(mar = c(4,4,2,15))
        zoomRatPlot(habMapPlot$map, brushBox$zoom)
    })
    
    # output$clickCoords <- renderText({
    #     paste0("x=", input$habMap_click$x, "\ny=", input$habMap_click$y)
    # })

## Crop to zoom map
        
    observeEvent(input$habMap_dblclick, {
        brush <- input$habMap_brush
        if (!is.null(brush)) {
            brushBox$zoom <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
        } else {
            brushBox$zoom <- extent(habMap)
        }
    })
    
    # observeEvent(input$habMap_click, {
    #     brush <- input$habMap_brush
    #     if (!is.null(brush)) {
    #         boundary$box <- extent(c(brush$xmin, brush$xmax, brush$ymin, brush$ymax))
    #         boundary$cells <- cellsFromExtent(habMap, boundary$box)
    #     } else {
    #         boundary$box <- NULL
    #         boundary$cells <- NULL
    #     }
    # })
    # 
    # output$boundaryBox <- renderText({
    #     paste0("xmin = ", boundary$box@xmin,
    #            "\nxmax = ", boundary$box@xmax,
    #            "\nymin = ", boundary$box@ymin,
    #            "\nymax = ", boundary$box@ymax,
    #            "\nncells = ", length(boundary$cells),
    #            "\nfromClass = ", input$fromClass,
    #            "\ntoClass = ", input$toClass
    #     )
    # })
    
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
            habMapUpdate <- habMapPlot$map
            cells <- cellsFromExtent(habMapUpdate, brushBox$update)
            vals <- habMapUpdate[cells]
            vals[vals==input$fromClass] <- as.numeric(input$toClass)
            habMapUpdate[cells] <- vals
            habMapPlot$map <- habMapUpdate
            #writeRaster(habMapPlot, file = "D:/St Helena/habmap.tif", overwrite = TRUE)
        } else {
            showModal(modalDialog(
                "Please click and drag on the map to indicate the area in which you want to change land cover.",
                easyClose = TRUE))
        }
    })
    
    observeEvent(input$btnReset, {
        habMapPlot$map <- habMap
    })
    
    output$serviceMap <- renderLeaflet({
        leaflet() %>%
            fitBounds(lng1 = -5.8045, lat1 = -16.04712, lng2 = -5.616973, lat2 = -15.8891) %>%
            addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
            addRasterImage(as.factor(habMapPlot$map), colors = rev(brewer.pal(10, "Paired")), group = "habMap") %>%
            addRasterImage(initialResults$FoodProvMeat, colors = brewer.pal(3, "Purples"), group = "Food Provision - meat") %>%
            hideGroup("Food Provision - meat") %>%
            addRasterImage(initialResults$FoodProvVeg, colors = brewer.pal(3, "Purples"), group = "Food Provision - vegetables") %>%
            hideGroup("Food Provision - vegetables") %>%
            addRasterImage(initialResults$CarbonSequestration, colors = brewer.pal(3, "Purples"), group = "Carbon sequestration") %>%
            hideGroup("Carbon sequestration") %>%
            addRasterImage(initialResults$Coffee, colors = brewer.pal(3, "Purples"), group = "Coffee") %>%
            hideGroup("Coffee") %>%
            addRasterImage(initialResults$Honey, colors = brewer.pal(3, "Purples"), group = "Honey") %>%
            hideGroup("Honey") %>%
            addRasterImage(initialResults$Fuel, colors = brewer.pal(3, "Purples"), group = "Firewood") %>%
            hideGroup("Firewood") %>%
            addRasterImage(initialResults$ConstructionMaterials, colors = brewer.pal(3, "Purples"), group = "Timber") %>%
            hideGroup("Timber") %>%
            addRasterImage(initialResults$RecreationLocal, colors = brewer.pal(3, "Purples"), group = "Recreation (Residents)") %>%
            hideGroup("Recreation (Residents)") %>%
            addRasterImage(initialResults$RecreationTourists, colors = brewer.pal(3, "Purples"), group = "Recreation (Tourists)") %>%
            hideGroup("Recreation (Tourists)") %>%
            addRasterImage(initialResults$GeneticMedicalResources, colors = brewer.pal(3, "Purples"), group = "Genetic & Medical resources") %>%
            hideGroup("Genetic & Medical resources") %>%
            addRasterImage(initialResults$ReductionDamageInfraProperty, colors = brewer.pal(3, "Purples"), group = "Environmental hazard mitigation") %>%
            hideGroup("Environmental hazard mitigation") %>%
            addRasterImage(initialResults$WaterProvision, colors = brewer.pal(3, "Purples"), group = "Water provision") %>%
            hideGroup("Water provision") %>%
            addRasterImage(initialResults$PrimProdInputs, colors = brewer.pal(3, "Purples"), group = "Inputs to primary productivity") %>%
            hideGroup("Inputs to primary productivity") %>%
            addLayersControl(overlayGroups = c("habMap", "Food Provision - meat", "Food Provision - vegetables", "Carbon sequestration", "Coffee", "Honey", "Firewood", "Timber", "Recreation (Residents)", "Recreation (Tourists)", "Genetic & Medical resources", "Environmental hazard mitigation", "Water provision", "Inputs to primary productivity"),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(position = "bottomright", colors = brewer.pal(3, "Purples"), labels = c("Low", "Medium", "High"), title = "Ecosystem service provision level")
    })
  
})
