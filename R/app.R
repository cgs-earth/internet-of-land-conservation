library(shiny)
library(leaflet)
library(sf)
library(DT)

ui <- fluidPage(
  titlePanel("CWPDA data wizard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Choose a Geospatial File (zipped shapefile .zip, .geojson", accept = c(".zip", ".geojson", ".json")),
      hr(),
      h4("Metadata Editor (ISO 19115)"),
      textInput("title", "Title"),
      textAreaInput("abstract", "Abstract", ""),
      textInput("contact", "Contact"),
      actionButton("saveMetadata", "Save Metadata"),
      hr(),
      uiOutput("standardMappingUI"),
      actionButton("applyMapping", "Apply Mapping"),
      downloadButton("download", "Download Package")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("editableTable")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

server <- function(input, output, session) {
  spatialData <- reactiveVal()
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
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    
    # Determine the file extension
    fileExt <- tools::file_ext(input$fileUpload$name)
    
    if (fileExt == "zip") {
      # Handle ZIP file (assuming it's a zipped shapefile)
      tempDir <- tempdir()
      unzipPath <- file.path(tempDir, tools::file_path_sans_ext(basename(input$fileUpload$name)))
      unzip(input$fileUpload$datapath, exdir = unzipPath)
      
      # Attempt to find a .shp file within the unzipped contents
      shpFiles <- list.files(unzipPath, pattern = "\\.shp$", full.names = TRUE)
      
      if (length(shpFiles) == 1) {
        # If exactly one .shp file is found, attempt to read it
        spatialData(st_read(shpFiles[1]))
      } else {
        # Handle the case where no .shp file or multiple .shp files are found
        showModal(modalDialog(
          title = "Error",
          "The ZIP file must contain exactly one shapefile (.shp, .shx, .dbf).",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    } else if (fileExt == "geojson" || fileExt == "json") {
      # Handle GeoJSON file
      spatialData(st_read(input$fileUpload$datapath))
    } else {
      # File type not supported
      showModal(modalDialog(
        title = "Error",
        "Unsupported file type. Please upload a zipped shapefile or a GeoJSON file.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
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
    datatable(spatialData(), editable = TRUE)
  })
  
  observeEvent(input$editableTable_cell_edit, {
    info <- input$editableTable_cell_edit
    str(info)
    df <- spatialData()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    spatialData(df)
  })
  
  output$map <- renderLeaflet({
 #   req(spatialData())
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tile layer
      {
        # Check if the spatial data is of type POINT
        if ("POINT" %in% class(spatialData())) {
          addCircleMarkers(., data = spatialData())
        }
        # Handle POLYGON data by adding polygons to the map
        else if ("POLYGON" %in% class(spatialData()) || "MULTIPOLYGON" %in% class(spatialData())) {
          addPolygons(., data = spatialData())
        }
      }
  })
  
  observeEvent(input$saveMetadata, {
    metadata$title <- input$title
    metadata$abstract <- input$abstract
    metadata$contact <- input$contact
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("package-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      validationMessage <- validateStandard3(spatialData())
      if (!is.null(validationMessage)) {
        # Stop the download and show an error message
        showModal(modalDialog(
          title = "Validation Error",
          validationMessage,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        stop(validationMessage)  # Stop execution
      }
      geojsonFile <- "data.geojson"
      metadataFile <- "metadata.csv"
      if (file.exists(geojsonFile)) {
        file.remove(geojsonFile)
      }
      if (file.exists(metadataFile)) {
        file.remove(metadataFile)
      }
      st_write(spatialData(), geojsonFile, driver = "GeoJSON",append=FALSE)
      # Convert reactiveValues to a list
      metadataList <- reactiveValuesToList(metadata)
      metadataList <- lapply(metadataList, function(x) if(is.null(x)) NA else x)
      
      # Optionally convert the list to a data frame for CSV writing
      # This assumes your metadata is flat (key-value pairs)
      metadataDf <- data.frame(
        key = names(metadataList),
        value = unlist(metadataList),
        stringsAsFactors = FALSE
      )
      
      # Write metadata to CSV
      write.csv(metadataDf, metadataFile, row.names = FALSE, append = FALSE)
      zip(file, files = c(geojsonFile, metadataFile))
    }
  )
}

shinyApp(ui, server)

