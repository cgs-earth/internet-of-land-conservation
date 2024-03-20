library(shiny)
library(leaflet)
library(sf)
library(DT)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("flatly"), "WPDA Data Wizard", id = "nav",
                 tabPanel("Step 1: Metadata Editor",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("title", "Title"),
                              textAreaInput("abstract", "Abstract", ""),
                              textInput("contact", "Contact"),
                              actionButton("saveMetadata", "Save Metadata"),
                              actionButton("next1", "Next") # Added "Next" button
                            ),
                            mainPanel()
                          )),
                 tabPanel("Step 2: Upload and Inspect",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("fileUpload", "Choose a Geospatial File (zipped shapefile .zip, .geojson", accept = c(".zip", ".geojson", ".json")),
                              actionButton("next2", "Next") # Added "Next" button
                            ),
                            mainPanel(
                              leafletOutput("map")
                            )
                          )),
                 tabPanel("Step 3: Mapping and Download",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("standardMappingUI"),
                              actionButton("applyMapping", "Apply Mapping"),
                              hr(),
                              downloadButton("download", "Download Package")
                            ),
                            mainPanel(
                              DTOutput("editableTable"),
                              tags$style(HTML("
                                /* Custom CSS for DataTables input fields */
                                table.dataTable input[type='text'] {
                                  color: #000000; /* Black text color */
                                  background-color: #FFFFFF; /* White background */
                                }
                                /* Custom CSS for DataTables input fields in focus */
                                table.dataTable input[type='text']:focus {
                                  color: #000000; /* Black text color for focus */
                                  background-color: #FFFF00; /* Yellow background for focus */
                                  border-color: #000000; /* Black border for focus */
                                }
                              "))
                            )
                          ))
)



server <- function(input, output, session) {
  spatialData <- reactiveVal()
  attributeData <- reactiveVal() # To store non-geometry attribute data
  geometryData <- reactiveVal()
  validateStandard3 <- function(data) {
    # Assuming 'standard3' is the name after mapping. Adjust if it's dynamic.
    if ("standard3" %in% names(data)) {
      validValues <- c("1", "2", "3")
      invalidEntries <- !data$standard3 %in% validValues
      if (any(invalidEntries)) {
        return(paste("Invalid entries found in 'standard3'. Entries must be one of '1', '2', '3'. Found:",
                     paste(data$standard3[invalidEntries], collapse=", ")))
      }
    }
    return(NULL)
  }
  metadata <- reactiveValues(title = NULL, abstract = NULL, contact = NULL)
  
  observeEvent(input$saveMetadata, {
    metadata$title <- input$title
    metadata$abstract <- input$abstract
    metadata$contact <- input$contact
    # Automatically move to the next step after saving metadata
    updateNavbarPage(session, "nav", selected = "Step 2: Upload and Inspect")
  })
  
  observeEvent(input$next1, {
    # Move to Step 2 upon clicking "Next"
    updateNavbarPage(session, "nav", selected = "Step 2: Upload and Inspect")
  })
  
  observeEvent(input$fileUpload, {
    # Once a file is uploaded, proceed to the next step
    updateNavbarPage(session, "nav", selected = "Step 3: Mapping and Download")
  })
  
  observeEvent(input$next2, {
    # This button ensures users can proceed even if they want to re-upload or wait
    updateNavbarPage(session, "nav", selected = "Step 3: Mapping and Download")
  })
  
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    fileExt <- tools::file_ext(input$fileUpload$name)
    if (fileExt == "zip") {
      tempDir <- tempdir()
      unzipPath <- file.path(tempDir, tools::file_path_sans_ext(basename(input$fileUpload$name)))
      unzip(input$fileUpload$datapath, exdir = unzipPath)
      shpFiles <- list.files(unzipPath, pattern = "\\.shp$", full.names = TRUE)
      if (length(shpFiles) == 1) {
        spatialData(st_read(shpFiles[1]))
        attributeData(st_drop_geometry(spatialData()))
        geometryData(st_geometry(spatialData()))
      } else {
        showModal(modalDialog(title = "Error", "The ZIP file must contain exactly one shapefile (.shp, .shx, .dbf).", easyClose = TRUE, footer = modalButton("Close")))
      }
    } else if (fileExt == "geojson" || fileExt == "json") {
      spatialData(st_read(input$fileUpload$datapath))
      attributeData(st_drop_geometry(spatialData()))
      geometryData(st_geometry(spatialData()))
    } else {
      showModal(modalDialog(title = "Error", "Unsupported file type. Please upload a zipped shapefile or a GeoJSON file.", easyClose = TRUE, footer = modalButton("Close")))
    }
  })
  
  output$map <- renderLeaflet({
    req(spatialData())
    leaflet() %>%
      addTiles() %>%
      {
        if ("sfc_POINT" %in% class(spatialData()$geometry)) {
          addCircleMarkers(., data = spatialData())
        } else if ("sfc_POLYGON" %in% class(spatialData()$geometry) || "sfc_MULTIPOLYGON" %in% class(spatialData()$geometry)) {
          addPolygons(., data = spatialData())
        }
      }
  })
  
  output$standardMappingUI <- renderUI({
    req(spatialData())
    df <- spatialData()
    colNames <- names(df)
    list(
      selectInput("mapStandard1", "Map to standard1:", choices = c("None" = "", colNames)),
      selectInput("mapStandard2", "Map to standard2:", choices = c("None" = "", colNames)),
      selectInput("mapStandard3", "Map to standard3:", choices = c("None" = "", colNames))
    )
  })
  
  observeEvent(input$applyMapping, {
    req(spatialData())
    df <- spatialData()
    standards <- c(input$mapStandard1, input$mapStandard2, input$mapStandard3)
    standardNames <- c("standard1", "standard2", "standard3")
    for (i in seq_along(standards)) {
      if (standards[i] != "") {
        names(df)[names(df) == standards[i]] <- standardNames[i]
      }
    }
    spatialData(df)
  })
  
  output$editableTable <- renderDT({
    req(spatialData())
    datatable(
      spatialData(),
      editable = TRUE,
      options = list(
        autoWidth = TRUE,      # Automatically adjust column widths
        columnDefs = list(list(className = 'dt-center', targets = '_all')), # Center align text
        pageLength = 10,       # Set number of rows per page
        searchHighlight = TRUE # Highlight search terms
      ),
      class = 'cell-border stripe', # Add cell borders and stripe styling
      rownames = FALSE
    ) %>%
      formatStyle(           # Apply conditional formatting (example)
        columns = 1:ncol(spatialData()), 
        fontWeight = 'bold', 
        color = 'black', 
        backgroundColor = styleEqual(c(1, 0), c('lightblue', 'lightgrey'))
      ) 
      
  }, server = FALSE)

  
  # Observe changes in the DataTable and update spatialData accordingly
  observeEvent(input$editableTable_cell_edit, {
    info <- input$editableTable_cell_edit
    str(info) # For debugging
    
    # Adjust column indexing for R
    correctedCol <- info$col + 1
    
    df <- spatialData()
    if(correctedCol <= ncol(df)) {
      df[info$row, correctedCol] <- DT::coerceValue(info$value, df[info$row, correctedCol])
      spatialData(df) # Update the attribute data
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("package-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      validationMessage <- validateStandard3(spatialData())
      if (!is.null(validationMessage)) {
        showModal(modalDialog(title = "Validation Error", validationMessage, easyClose = TRUE, footer = modalButton("Close")))
        stop(validationMessage)  # Prevent download
      }
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      geojsonFile <- paste0("data-", timestamp, ".geojson")
      metadataFile <- "metadata.csv"
      st_write(spatialData(), geojsonFile, driver = "GeoJSON", append=FALSE)
      metadataList <- reactiveValuesToList(metadata)
      metadataDf <- data.frame(key = names(metadataList), value = unlist(metadataList), stringsAsFactors = FALSE)
      write.csv(metadataDf, metadataFile, row.names = FALSE)
      zip(file, files = c(geojsonFile, metadataFile))
    }
  )
}


shinyApp(ui, server)
