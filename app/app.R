library(shinydashboard)
library(leaflet)
library(htmlwidgets)
library(dplyr)
library(DT)

treedata <- read.csv("../data/sewickley_trees.csv")

name_list <- sort(unique(treedata$common_name))
condition_list <- unique(treedata$condition_class)
treeid_list <- unique(treedata$treeid)
age_list <- unique(treedata$age_class)

ntree <- treedata %>%
  filter(removed_flg == 0) %>%
  tally()

nremoved <- treedata %>%
  filter(removed_flg == 1) %>%
  tally()


temp_folder <- tempdir()
download.file(
  "https://raw.githubusercontent.com/torfsen/leaflet.zoomhome/master/dist/leaflet.zoomhome.js",
  file.path(temp_folder, 'leaflet.zoomhome.js')
)
download.file(
  "https://raw.githubusercontent.com/torfsen/leaflet.zoomhome/master/dist/leaflet.zoomhome.css",
  file.path(temp_folder, 'leaflet.zoomhome.css')
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Sewickley Trees"
  ),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_trees"),
      valueBoxOutput("planted_trees"),
      valueBoxOutput("removed_trees")
    ),
    tabsetPanel(id = "tabs",
                tabPanel("Map",
                         fluidRow(
                           column(width = 9,
                                  leafletOutput("map"),
                                  tags$head(
                                    # Include our custom CSS
                                    includeScript(file.path(temp_folder, 'leaflet.zoomhome.js')),
                                    includeCSS(file.path(temp_folder, 'leaflet.zoomhome.css')),
                                    tags$link(rel="stylesheet", href="http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"),
                                  )),
                           column(width = 3,
                                  box(width = NULL, title =tagList(shiny::icon("filter",class = 'fa-lg'), "Filter Data") ,
                                      solidHeader = T, collapsible = T, status = 'primary',
                                      # pickerInput('name','Common Name',choices = name_list, multiple = T, options = list(`actions-box` = TRUE)),
                                      selectizeInput('name','Common Name',choices = c("All"="", name_list), multiple = T, selected=NULL),
                                      selectizeInput('treeid','Tree ID', choices = c("All"="",treeid_list), multiple = T, selected=NULL),
                                      selectizeInput('age','Age', choices = c("All "="",age_list), multiple = T, selected=NULL),
                                      selectizeInput('condition','Condition', choices = c("All"="",condition_list), multiple = T, selected=NULL)
                                      )
                                  )
                         )
                ),
                tabPanel("Tree Data",
                         DT::dataTableOutput("treetable"),
                         downloadButton("downloadData", "Download Data")),
                tabPanel("Planting Sites Data")
    )
  )
)


server <- function(input, output, session) {
  # tab_list <- NULL
  filtereddata <- reactive({
    treedata %>%
      filter(is.null(input$name) | common_name %in% input$name,
             is.null(input$treeid) | treeid %in% input$treeid,
             is.null(input$condition) | condition_class %in% input$condition,
             is.null(input$age) | age_class %in% input$age)
    }
  )
  
  output$map <- renderLeaflet({
    leaflet(data = treedata, options = leafletOptions(zoomControl = FALSE, minZoom = 14.5)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
       # addLegend(values = exampleValuesOne, group = "Trees", 
      #           position = "bottomright") %>%
      setView(lng = -80.182, lat = 40.539, zoom = 14.5) %>%
      onRender(
        "
function(el,x) {
  var zoomHome = L.Control.zoomHome();
  zoomHome.addTo(this);
}
")
  })
  
  observe({
    leafletProxy("map", data = filtereddata()) %>%
      clearShapes() %>%
      addCircles(lng = ~longitude,
                 lat = ~latitude,
                 radius=1,
                 color = "#009E73",
                 popup = ~paste("<h4>", common_name, " (#",treeid,")", "</h4>","<br>",
                                "<b>Scientific Name:</b> ", genus_species,"<br>",
                                "<b>Trunk Diameter, feet:</b> ", dbh_1, "<br>",
                                "<b>Age Class:</b> ", age_class, "<br>",
                                "<b>Condition:</b> ", condition_class, sep=""),
                 group = "Trees") %>% 
      addLayersControl(
        overlayGroups = c("Trees", "Planting Sites"),
        options = layersControlOptions(collapsed = TRUE))
  })
  
  output$treetable <- DT::renderDataTable({
    df <- treedata %>%
      select(treeid,common_name,genus_species,dbh_1,age_class,condition_class,addr_1) %>%
      arrange(treeid) %>% 
      rename(TreeID = treeid, "Common Name" = common_name, "Scientific Name" = genus_species, "Trunk Diameter" = dbh_1, "Age Class" = age_class, Condition = condition_class, Address = addr_1)
    datatable(df, escape = FALSE, rownames= FALSE)
  })
  
  # output to download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sewickley_tree_inventory_data_", format(Sys.Date(), "%Y%m%d"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(treedata, file, row.names = FALSE)
    }
  )
  
  # Number Trees (server) -------------------------------------------
  
  output$total_trees<- renderValueBox({
    ntree %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Total Trees")
  })
  
  # Number Planted (server) -----------------------------------------
  output$planted_trees <- renderValueBox({
    ntree %>%
      prettyNum(big.mark = ",") %>%
      valueBox(
        subtitle = "Number of Planted Trees",
        color = "blue"
      ) 
  })
  
  # Number Removed (server) -----------------------------------------
  output$removed_trees <- renderValueBox({
    nremoved %>%
      prettyNum(big.mark = ",") %>%
      valueBox(
        subtitle = "Number of Removed Trees",
        color = "teal"
      )
  })

}

shinyApp(ui, server)