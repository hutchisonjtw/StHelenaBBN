library(shiny)
library(shinydashboard)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "St Helena Natural Capital Mapping Tool", titleWidth = 400),
        dashboardSidebar(
            width = 200,
            sidebarMenu(
                menuItem(text = "Land cover", tabName = "landcover", icon = icon("tree")),
                menuItem(text = "Ecosystem service maps", tabName = "outputMaps", icon = icon("leaf"))
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "landcover",
                        class = "active",
                        fluidRow(
                            column(width = 8, 
                                   box(width = 12,
                                       plotOutput("habMap", height = "800px",
                                                  click = "habMap_click",
                                                  dblclick = "habMap_dblclick",
                                                  brush = brushOpts(id = "habMap_brush", resetOnNew = TRUE)
                                       )#,
                                       #verbatimTextOutput("boundaryBox")
                                   )
                            ),
                            column(width = 4,
                                   box(width = 12,
                                       title = "Change cell values",
                                       selectInput(inputId = "fromClass", 
                                                   label = "Change from",
                                                   choices = list("NA" = 0,
                                                                  "Native forest" = 1,
                                                                  "Introduced forest" = 2,
                                                                  "Native shrubland" = 3,
                                                                  "Introduced shrubland" = 4,
                                                                  "Flax" = 5,
                                                                  "Arable" = 6,
                                                                  "Pasture" = 7,
                                                                  "Plantations" = 8,
                                                                  "Rural gardens" = 9,
                                                                  "Other" = 10),
                                                   selected = NULL
                                       ),
                                       selectInput(inputId = "toClass",
                                                   label = "Change to",
                                                   choices = list("NA" = 0, 
                                                                  "Native forest" = 1,
                                                                  "Introduced forest" = 2,
                                                                  "Native shrubland" = 3,
                                                                  "Introduced shrubland" = 4,
                                                                  "Flax" = 5,
                                                                  "Arable" = 6,
                                                                  "Pasture" = 7,
                                                                  "Plantations" = 8,
                                                                  "Rural gardens" = 9,
                                                                  "Other" = 10),
                                                   selected = NULL
                                       ),
                                       actionButton("change", "Change cells")
                                   ), 
                                   box(width = 12,
                                       actionButton(width = "100%", "recalc", "Recalculate services")
                                   )
                            )
                        )
                ),
                tabItem(tabName = "outputMaps",
                        fluidRow(
                            box(selectInput(inputId = "service",
                                            label = "Select service:",
                                            choices = c('Food Provision - Meat' = 1,
                                                        'Food Provision - Crops' = 2,
                                                        'Carbon Sequestration' = 3,
                                                        'Coffee' = 4,
                                                        'Honey' = 5,
                                                        'Fuel' = 6,
                                                        'Construction Materials' = 7,
                                                        'Local Recreation' = 8,
                                                        'Tourist Recreation' = 9,
                                                        'Genetic/Medical Resources' = 10,
                                                        'Reduction in Damage to Infrastructure & Property' = 11,
                                                        'Water Provision' = 12,
                                                        'PrimProdInputs' = 13)
                                            ))
                        )
                )
            )
        )
        
    )
)
