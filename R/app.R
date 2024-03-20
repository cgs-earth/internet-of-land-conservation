library(shiny)
library(leaflet)
library(sf)
library(DT)
library(shinythemes)
library(mapview)
library(shinyBS)

ui <- navbarPage(theme = shinytheme("flatly"), "WPDA Data Wizard", id = "nav",
                 tabPanel("Step 1: Metadata Editor",
                          sidebarLayout(
                            sidebarPanel(width = 12,
                              fluidRow(
                                column(4, textInput("METADATAID", "Metadata ID", value = "Not Reported")),
                                column(8, "An ID assigned by UNEP-WCMC that is used to link each record in the WDPA or OECM database to the relevant source information in the Source Table.")
                              ),
                              fluidRow(
                                column(4, textInput("DATA_TITLE", "Data Set Title", value = "")),
                                column(8, "The title of the dataset e.g., Protected Areas of Ireland; OECMs of South Africa.")
                              ),
                              fluidRow(
                                column(4, textInput("RESP_PARTY", "Responsible Party", value = "")),
                                column(8, "The data provider, i.e., the organisation, national government, or other actor that claims ownership of the data or that is providing the data on behalf of their owner.")
                              ),
                              fluidRow(
                                column(4, textInput("RESP_EMAIL", "Responsible Party Contact E-mail(s)", value = "Not Reported")),
                                column(8, "Contact e-mail address of the organisation listed as the responsible party which maintains the ownership of the data or that is providing the data on behalf of its owner.")
                              ),
                              fluidRow(
                                column(4, textInput("VERIFIER", "Verifying Party", value = "None")),
                                column(8, "The organisation, national government or other actor that has verified the data. For information submitted prior to the introduction of this attribute in March 2015, this field is assigned a value of 'None'.")
                              ),
                              fluidRow(
                                column(4, textInput("V_EMAIL", "Verifying Party Contact Email(s)", value = "None")),
                                column(8, "Contact e-mail address of person(s) and organisation(s) responsible for verifying the data. For information submitted prior to the introduction of this attribute in March 2015, this field is assigned a value of 'None'.")
                              ),
                              fluidRow(
                                column(4, textInput("YEAR", "Year", value = "Not Reported")),
                                column(8, "The year when the dataset was first submitted to the WDPA or OECM database. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textInput("UPDATE_YR", "Update Year", value = "")),
                                column(8, "The year in which the dataset was last updated in the WDPA or OECM database.")
                              ),
                              fluidRow(
                                column(4, textInput("LANGUAGE", "Dataset Language", value = "Not Reported")),
                                column(8, "Language(s) used within the dataset. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textInput("CHAR_SET", "Dataset Character Set", value = "Not Reported")),
                                column(8, "Full name of the character coding standard used in the dataset. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textInput("REF_SYSTEM", "Coordinate System", value = "Not Reported")),
                                column(8, "Name and parameters of the coordinate system of the original dataset including, where applicable, datum, ellipsoid, or projection. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textInput("SCALE", "Scale", value = "Not Reported")),
                                column(8, "The scale of the dataset used when the data was originally created (digitized, surveyed, etc.). 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textAreaInput("LINEAGE", "Lineage", value = "Not Reported")),
                                column(8, "Information about the creation, events, changes or transformations in the life of a dataset including the process used to create and maintain the dataset and associated dates. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textInput("CITATION", "Citation", value = "Not Reported")),
                                column(8, "Recommended text to be used when referencing the dataset. 'Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(4, textAreaInput("DISCLAIMER", "Disclaimer", value = "Not Reported")),
                                column(8, "Warnings/exceptions to use of the data. '`Not Reported' is used when these data are not available.")
                              ),
                              fluidRow(
                                column(12, actionButton("saveMetadata", "Save Metadata", class = "btn-primary mt-3")),
                                column(12, actionButton("next1", "Next", class = "btn-success mt-3"))
                              )
                            )
                          ,
                            mainPanel()
                          )),
                 tabPanel("Step 2: Upload and Inspect",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("fileUpload", "Choose a Geospatial File (zipped shapefile .zip, .geojson", accept = c(".zip", ".geojson", ".json")),
                              actionButton("next2", "Next") # Added "Next" button
                            ),
                            mainPanel(
                              mapviewOutput("map")
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
    metadata$METADATAID <- input$METADATAID
    metadata$DATA_TITLE <- input$DATA_TITLE
    metadata$RESP_PARTY <- input$RESP_PARTY
    metadata$RESP_EMAIL <- input$RESP_EMAIL
    metadata$VERIFIER <- input$VERIFIER
    metadata$V_EMAIL <- input$V_EMAIL
    metadata$YEAR <- input$YEAR
    metadata$UPDATE_YR <- input$UPDATE_YR
    metadata$LANGUAGE <- input$LANGUAGE
    metadata$CHAR_SET <- input$CHAR_SET
    metadata$REF_SYSTEM <- input$REF_SYSTEM
    metadata$SCALE <- input$SCALE
    metadata$LINEAGE <- input$LINEAGE
    metadata$CITATION <- input$CITATION
    metadata$DISCLAIMER <- input$DISCLAIMER
    # Automatically move to the next step after saving metadata
    updateNavbarPage(session, "nav", selected = "Step 2: Upload and Inspect")
  })
  
  # observeEvent(input$next1, {
  #   # Move to Step 2 upon clicking "Next"
  #   updateNavbarPage(session, "nav", selected = "Step 2: Upload and Inspect")
  # })
  
  # observeEvent(input$fileUpload, {
  #   # Once a file is uploaded, proceed to the next step
  #   updateNavbarPage(session, "nav", selected = "Step 3: Mapping and Download")
  # })
  
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
      # Filter out macOS system files and then search for .shp files
      shpFiles <- list.files(unzipPath, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      shpFiles <- shpFiles[!grepl("__MACOSX", shpFiles)] # Exclude macOS system files
      
      if (length(shpFiles) == 1) {
        spatialData(sf::st_read(shpFiles[1]))
        attributeData(st_drop_geometry(spatialData()))
        geometryData(st_geometry(spatialData()))
      } else {
        showModal(modalDialog(title = "Error", "The ZIP file must contain exactly one shapefile (.shp, .shx, .dbf).", easyClose = TRUE, footer = modalButton("Close")))
      }
    } else if (fileExt == "geojson" || fileExt == "json") {
      spatialData(sf::st_read(input$fileUpload$datapath))
      attributeData(st_drop_geometry(spatialData()))
      geometryData(st_geometry(spatialData()))
    } else {
      showModal(modalDialog(title = "Error", "Unsupported file type. Please upload a zipped shapefile or a GeoJSON file.", easyClose = TRUE, footer = modalButton("Close")))
    }
  })
  
  
  output$map <- renderLeaflet({
    req(spatialData()) # Ensure the spatial data is loaded
    map <- mapview(spatialData(), layer.name = "Spatial Data", color = "blue")@map # Use mapview to create the map
    leaflet() %>% addTiles() %>%
      {
        if (!is.null(map)) {
          map %>% addTiles()
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
      metadataDf <- rev(stack(metadata))
      colnames(metadataDf) <- c("MetadataElement","Value")
     # metadataDf <- data.frame(key = names(metadataList), value = unlist(metadataList), stringsAsFactors = FALSE)
      write.csv(metadataDf, metadataFile, row.names = FALSE)
      zip(file, files = c(geojsonFile, metadataFile))
    }
  )
}


shinyApp(ui, server)
