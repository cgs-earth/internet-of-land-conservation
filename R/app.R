library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(DT)
library(shinythemes)
library(mapview)
library(shinyBS)
library(shinyWidgets)
library(rlang)

ui <-
  navbarPage(
    theme = shinytheme("flatly"),
    "WDPA/OECM Data Wizard",
    id = "nav",
    tags$head(tags$style(
      HTML(
        "
      .navbar {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 1030; /* Ensure navbar stays on top */
      }
      body {
        padding-top: 70px; /* Adjust based on navbar height */
      }
    "
      )
    )),
    tabPanel("Introduction",
             fluidPage(
               div(class = "content-wrapper",  # This div uses the custom CSS class
                   wellPanel(
                     # The welcome and introductory text
                     h1("Welcome to the Protected Planet Data Wizard"),
                     p("This interactive tool is your assistant for preparing and validating geospatial data for submission to the World Database on Protected Areas (WDPA) and the Other Effective Area-Based Conservation Measures (OECM) database. The WDPA and OECM are pivotal global resources for the conservation of biodiversity, compiled through collaboration with various organizations under the guidance of the United Nations Environment Programme World Conservation Monitoring Centre (UNEP-WCMC) and the International Union for Conservation of Nature (IUCN)."),
                     
                     h2("How Does It Work?"),
                     p("The Data Wizard simplifies the data preparation process, ensuring that your datasets meet the Protected Planet data standards, including proper formatting and attribute requirements."),
                     
                     h2("Key Requirements for Data Submission:"),
                    tags$ul(
                       tags$li("Areas must conform to the IUCN definition of a protected area or the CBD definition of an OECM."),
                       tags$li("Submissions must include both spatial data (in GIS format) and an associated attribute table."),
                       tags$li("The source of the information must be clearly stated in a Source Table."),
                       tags$li("A signed WDPA Data Contributor Agreement is required.")
                     ),
                     
                     h2("Getting Started:"),
                    tags$ul(
                      tags$li(strong("Before You Begin: Select your database:"), " Choose whether you are preparing data for the WDPA or OECM database."),
                      tags$li(strong("Step 1: Fill out the Source Table:"), " Provide detailed information about your dataset, including metadata ID, dataset title, responsible party, and more."),
                      tags$li(strong("Step 2: Upload your geospatial file:"), " Acceptable formats include zipped shapefiles (.zip) or GeoJSON files (.geojson, .json)."),
                      tags$li(strong("Step 3: Map your data and download your package"), " Align your data with the Protected Planet standards by mapping attributes to the required fields. Once validated, download your data package for submission to Protected Planet.")
                    ),
                     
                     p("We encourage data providers to include information beyond the minimum required attributes to enhance the analysis and reporting capabilities on protected areas. Your contribution is vital for the ongoing conservation efforts and helps in maintaining up-to-date and accurate global databases."),
                     p("Thank you for contributing to the global conservation effort through Protected Planet! https://protectedplanet.net"),
                     
                     #download buttons
                     downloadButton("downloadGuide", "Download Data Submission Guide"),
                     downloadButton("downloadStandards", "Download Data Standards Excel"),
                     downloadButton("downloadAgreement", "Download Contributor Agreement Template")
                     
 
                   ),
                     
                     radioButtons("databaseType", "Select Database you would like to prepare data for:",
                                  choices = c("WDPA" = "WDPA", "OECM" = "OECM"),
                                  selected = "WDPA"),
                     actionButton("startBtn", "Get Started", class = "btn-primary")
                   
             ))),
    tabPanel(
      "Step 1: Source Table Editor",
      div(class = "content-wrapper",  # This div uses the custom CSS class
          sidebarLayout(
            sidebarPanel(
              width = 12,
              h4("Please fill out the Source Table for your dataset."),
              p(
                "This information helps to identify and describe your dataset. Required fields are marked with an asterisk (*)."
              ),
              fluidRow(
                column(
                  4,
                  textInput("METADATAID", "Metadata ID*", value = "Not Reported")
                ),
                column(
                  8,
                  "An ID assigned by UNEP-WCMC that is used to link each record in the WDPA or OECM database to the relevant source information in the Source Table."
                )
              ),
              fluidRow(
                column(4, textInput("DATA_TITLE", "Data Set Title*", value = "")),
                column(
                  8,
                  "The title of the dataset e.g., Protected Areas of Ireland; OECMs of South Africa."
                )
              ),
              fluidRow(
                column(4, textInput(
                  "RESP_PARTY", "Responsible Party*", value = ""
                )),
                column(
                  8,
                  "The data provider, i.e., the organisation, national government, or other actor that claims ownership of the data or that is providing the data on behalf of their owner."
                )
              ),
              fluidRow(
                column(
                  4,
                  textInput("RESP_EMAIL", "Responsible Party Contact E-mail(s)*", value = "Not Reported")
                ),
                column(
                  8,
                  "Contact e-mail address of the organisation listed as the responsible party which maintains the ownership of the data or that is providing the data on behalf of its owner."
                )
              ),
              fluidRow(
                column(4, textInput("VERIFIER", "Verifying Party*", value = "None")),
                column(
                  8,
                  "The organisation, national government or other actor that has verified the data. For information submitted prior to the introduction of this attribute in March 2015, this field is assigned a value of 'None'."
                )
              ),
              fluidRow(
                column(
                  4,
                  textInput("V_EMAIL", "Verifying Party Contact Email(s)", value = "None")
                ),
                column(
                  8,
                  "Contact e-mail address of person(s) and organisation(s) responsible for verifying the data. For information submitted prior to the introduction of this attribute in March 2015, this field is assigned a value of 'None'."
                )
              ),
              fluidRow(
                column(4, textInput("YEAR", "Year*", value = "Not Reported")),
                column(
                  8,
                  "The year when the dataset was first submitted to the WDPA or OECM database. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(4, textInput("UPDATE_YR", "Update Year*", value = "")),
                column(
                  8,
                  "The year in which the dataset was last updated in the WDPA or OECM database."
                )
              ),
              fluidRow(
                column(
                  4,
                  textInput("LANGUAGE", "Dataset Language*", value = "Not Reported")
                ),
                column(
                  8,
                  "Language(s) used within the dataset. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(
                  4,
                  textInput("CHAR_SET", "Dataset Character Set*", value = "Not Reported")
                ),
                column(
                  8,
                  "Full name of the character coding standard used in the dataset. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(
                  4,
                  textInput("REF_SYSTEM", "Coordinate System*", value = "Not Reported")
                ),
                column(
                  8,
                  "Name and parameters of the coordinate system of the original dataset including, where applicable, datum, ellipsoid, or projection. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(4, textInput("SCALE", "Scale", value = "Not Reported")),
                column(
                  8,
                  "The scale of the dataset used when the data was originally created (digitized, surveyed, etc.). 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(4, textAreaInput("LINEAGE", "Lineage", value = "Not Reported")),
                column(
                  8,
                  "Information about the creation, events, changes or transformations in the life of a dataset including the process used to create and maintain the dataset and associated dates. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(4, textInput("CITATION", "Citation", value = "Not Reported")),
                column(
                  8,
                  "Recommended text to be used when referencing the dataset. 'Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(
                column(
                  4,
                  textAreaInput("DISCLAIMER", "Disclaimer", value = "Not Reported")
                ),
                column(
                  8,
                  "Warnings/exceptions to use of the data. '`Not Reported' is used when these data are not available."
                )
              ),
              fluidRow(column(
                12,
                actionButton("saveMetadata", "Save Metadata", class = "btn-primary mt-3")
              ),
              column(
                12, actionButton("next1", "Next", class = "btn-success mt-3")
              ))
            )
            ,
            mainPanel()
          ))
    ),
    tabPanel(
      "Step 2: Upload and Inspect",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "fileUpload",
            "Choose a Geospatial File (zipped shapefile .zip, .geojson",
            accept = c(".zip", ".geojson", ".json")
          ),
          actionButton("next2", "Next") # Added "Next" button
        ),
        mainPanel(leafletOutput("map", height = "600px"))
      )
    ),
    tabPanel(
      "Step 3: Mapping and Download",
      sidebarLayout(
        sidebarPanel(
          h5("Use these dopdown menus to select which fields in your data correspond to the Protected Planet standard fields"),
          uiOutput("standardMappingUI"),
          actionButton("applyMapping", "Apply Mapping"),
          hr(),
          downloadButton("download", "Download Package")
        ),
        mainPanel(DTOutput("editableTable"),
                  tags$style(
                    HTML(
                      "
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
                              "
                    )
                  ))
      )
    )
  )



server <- function(input, output, session) {
  spatialData <- reactiveVal()
  attributeData <-
    reactiveVal() # To store non-geometry attribute data
  geometryData <- reactiveVal()
  
  
  
  validateStandard3 <- function(data) {
    # Assuming 'standard3' is the name after mapping. Adjust if it's dynamic.
    if ("standard3" %in% names(data)) {
      validValues <- c("1", "2", "3")
      invalidEntries <- !data$standard3 %in% validValues
      if (any(invalidEntries)) {
        return(
          paste(
            "Invalid entries found in 'standard3'. Entries must be one of '1', '2', '3'. Found:",
            paste(data$standard3[invalidEntries], collapse = ", ")
          )
        )
      }
    }
    return(NULL)
  }
  metadata <-
    reactiveValues(title = NULL,
                   abstract = NULL,
                   contact = NULL)
  observeEvent(input$startBtn, {
    updateNavbarPage(session, "nav", selected = "Step 1: Source Table Editor")
  })
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
      unzipPath <-
        file.path(tempDir, tools::file_path_sans_ext(basename(input$fileUpload$name)))
      unzip(input$fileUpload$datapath, exdir = unzipPath)
      # Filter out macOS system files and then search for .shp files
      shpFiles <-
        list.files(
          unzipPath,
          pattern = "\\.shp$",
          full.names = TRUE,
          recursive = TRUE
        )
      shpFiles <-
        shpFiles[!grepl("__MACOSX", shpFiles)] # Exclude macOS system files
      
      if (length(shpFiles) == 1) {
        spatialData(sf::st_read(shpFiles[1]) %>% st_transform(4326))
        attributeData(st_drop_geometry(spatialData()))
        geometryData(st_geometry(spatialData()))
      } else {
        showModal(
          modalDialog(
            title = "Error",
            "The ZIP file must contain exactly one shapefile (.shp, .shx, .dbf).",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      }
    } else if (fileExt == "geojson" || fileExt == "json" || fileExt =="gpkg") {
      spatialData(sf::st_read(input$fileUpload$datapath) %>% st_transform(4326))
      attributeData(st_drop_geometry(spatialData()))
      geometryData(st_geometry(spatialData()))
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Unsupported file type. Please upload a zipped shapefile or a GeoJSON or a GeoPackag file.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
  
  # output$dynamicMetadataFields <- renderUI({
  #   # Check the selected database
  #   dbType <- input$databaseType
  #   
  #   # Define metadata fields for each database
  #   wdpaFields <- c("WDPAID", "NAME", "STATUS") # Example fields for WDPA
  #   oecmFields <- c("OECMID", "NAME", "STATUS") # Example fields for OECM
  #   
  #   # Choose fields based on selection
  #   fieldsToUse <- if(dbType == "WDPA") wdpaFields else oecmFields
  #   
  #   # Generate UI for the selected fields
  #   lapply(fieldsToUse, function(field) {
  #     textInput(field, label = field, value = "")
  #   })
  # })
  
  output$standardMappingUI <- renderUI({
    req(spatialData())
    df <- spatialData()
    colNames <- names(df)
    
    # Categorize fields by their requirement level
    # Check the selected database
    dbType <- input$databaseType
    
    minimalFields_wdpa <-
      c(
        "WDPAID",
        "WDPA_PID",
        "PA_DEF",
        "NAME",
        "ORIG_NAME",
        "DESIG",
        "DESIG_TYPE",
        "INT_CRIT",
        "MARINE",
        "REP_M_AREA",
        "GIS_M_AREA",
        "REP_AREA",
        "GIS_AREA",
        "STATUS",
        "STATUS_YR",
        "VERIF",
        "RESTRICT",
        "METADATAID",
        "PARENT_ISO3",
        "ISO3"
      )
    
    minimalFields_oecm <-
      c(
        "WDPAID",
        "WDPA_PID",
        "PA_DEF",
        "NAME",
        "ORIG_NAME",
        "DESIG",
        "DESIG_TYPE",
        "MARINE",
        "REP_M_AREA",
        "GIS_M_AREA",
        "REP_AREA",
        "GIS_AREA",
        "STATUS",
        "STATUS_YR",
        "VERIF",
        "RESTRICT",
        "METADATAID",
        "PARENT_ISO3",
        "ISO3"
      )
    
    completeFields_wdpa <-
      c(
        "DESIG_ENG",
        "IUCN_CAT",
        "NO_TAKE",
        "NO_TK_AREA",
        "GOV_TYPE",
        "OWN_TYPE",
        "MANG_AUTH",
        "MANG_PLAN",
        "SUB_LOC"
      )
    
    completeFields_oecm <-
      c(
        "DESIG_ENG",
        "NO_TAKE",
        "NO_TK_AREA",
        "GOV_TYPE",
        "OWN_TYPE",
        "MANG_AUTH",
        "MANG_PLAN",
        "CONS_OBJ",
        "SUPP_INFO",
        "SUB_LOC"
      )
    

    # Define field descriptions (examples given, extend as necessary)
    fieldDescriptions <- list(
      WDPAID = "Unique identifier for each protected area.",
      NAME = "The name of a protected area or OECM is the name assigned to the site in legal texts or by its governance authority. The Name field is an open string field and any text is allowed, except for ‘Unnamed’, ‘Unknown’ or similar equivalents. Names do not have to be translated into English but text must be in Latin characters. Accented characters are accepted.",
      STATUS = "The current status of the protected area: Proposed, Inscribed, Adopted, Designated, Established", 
      PA_DEF = "This attribute indicates whether the site meets the IUCN definition of a protected area (WDPA only) or the CBD definition of an OECM (OECM database only).",
      ORIG_NAME = "The name of the protected area or OECM in any language supported by UTF 8 encoding. The Original Name field is an open string field and any text is allowed, except for ‘Unnamed’, ‘Unknown’ or similar equivalents.",
      DESIG = "The designation of the protected area or OECM in the native language (provided it is supported by UTF 8 encoding)",
      DESIG_ENG = "The designation of the protected area or OECM in English. This field contains the same value as the “DESIG” field where English is the original language.",
      DESIG_TYPE = "Category or type of protected area as legally/officially designated or proposed. National, Regional, International, or Not Applicable",
      IUCN_CAT = "The IUCN protection category. Ia, Ib, II, III, IV, V, VI, Not Reported, Not Applicable, or Not Assigned",
      INT_CRIT = "Only for UNESCO WOrld Heritage Sites and Ramsar sites. All others “Not Applicable”",
      MARINE = "Describes whether area is totally or partially marine. 0 (completely terrestrial water), 1 (partially marine and terrestrial), 2 (completely marine)",
      REP_M_AREA = "Reported area that is within the marine environment in sq km",
      GIS_M_AREA = "Calculated area within the marine environment in sq km using Mollweide projection",
      REP_AREA = "Reported total area in sq km",
      GIS_AREA = "Calculated area in sq km using the Mollweide projection",
      NO_TAKE = "Taking of liviing or dead natural resources is prohibited in the Marine area: All, PArt, None, Not Reported, Not Applicable (i.e. non-marine area)",
      NO_TK_AREA = "Total area in sq km of the marine area which is No Take",
      STATUS_YR = "The year the current status came into force: YYYY or 0 if unknown",
      GOV_TYPE = "The type of entity that makes decisions about how the area is managed: “Federal or national ministry or agency”, “Sub-national ministry or agency”, “Government-delegated management”, “Transboundary governance”, “Collaborative governance”, “Joint governance”, “Individual landowners”, “Non-profit organizations”, “For-profit organizations”, “Indigenous peoples”, “Local communities”, “Not Reported”",
      OWN_TYPE = "Legal Ownership type (independent of governance): State, Communal, Individual Landowners, For-profit organisations, Non-profit organisations, Joint ownership, Multiple ownership, Contested, Not Reported",
      MANG_AUTH = "Management Authority, the specific individual or organization that manages the protected area “Not Reported” if unknown.",
      MANG_PLAN = "A URL linking to a document or website detailing a management plan or mechanism, or “Not Reported” if unknown.",
      SUPP_INFO = "Any supporting information on an OECM, such as details on how it fulfills the CBD OECM definition.“Not Reported” if not provided",
      CONS_OBJ = "For OECM only. The exentent to which biodiversity is a conservation objective: Primary, Secondart, Ancillary",
      VERIF = "How the site has been verified: State Verified, Expert Verified, Not Reported",
      METADATAID = "The Metadata ID from Step 1, linking the data with the metadata source table",
      SUB_LOC = "The subnational location as an ISO 3166-2 sub-national code. See https://en.wikipedia.org/wiki/ISO_3166-2",
      PARENT_ISO3 = "The ISO3 code of the country that the area resides in. See https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes",
      ISO3 = "The ISO3 code of the territory that the area resides in. Different from PARENT_ISO3 only in the case for dependent terrirories/ overseas departments"
      
      # Add other fields and descriptions as needed
    )
    
    # Function to generate selectInput for a given field
    generateSelectInput <- function(fieldName) {
      selectId <- paste0("map", gsub("[^[:alnum:]]", "", fieldName))
      inputField <- selectInput(
        inputId = selectId,
        label = paste(fieldName, ":"),
        choices = c("None" = "", colNames)
      )
      
      # Use an ID for the help icon
      helpIconId <- paste0("helpIcon", gsub("[^[:alnum:]]", "", fieldName))
      helpIcon <- tags$span(
        tags$i(class = "fa fa-info-circle", id = helpIconId),
        style = "margin-left: 5px; cursor: help;"
      )
      
      tooltipText = fieldDescriptions[[fieldName]] %||% "No description available"
      
      uiOutput <- tags$div(
        style = "display: flex; align-items: center;",
        inputField,
        tipify(helpIcon,  tooltipText, placement="right")
      )
      
      # Return the UI element along with the tooltip ID
      #list(uiOutput = uiOutput, tooltipId = helpIconId)#
      uiOutput
    }
    
    # Choose fields based on selection
    minimalFields <- if(dbType == "WDPA") {minimalFields_wdpa} else {minimalFields_oecm}
    completeFields <- if(dbType == "WDPA") {completeFields_wdpa} else {completeFields_oecm}
    
    fluidRow(column(
      6,
      h4("Minimum Required Fields"),
      lapply(minimalFields, generateSelectInput)
    ),
    column(
      6,
      h4("Complete (Optional) Fields"),
      lapply(completeFields, generateSelectInput)
    ))
  })
  
  output$map <- renderLeaflet({
    req(spatialData()) # Ensure the spatial data is loaded
    map <-
      mapview(spatialData(),
              layer.name = "Spatial Data",
              color = "blue") # Use mapview to create the map
    map@map
  })
  
  # # Function to create HTML content for popups
  # htmlPopup <- function(x) {
  #   attrs <- as.character(x)
  #   html <- paste(names(attrs), attrs, sep = ": ", collapse = "<br/>")
  #   return(html)
  # }
  #
  # output$map <- renderLeaflet({
  #   req(spatialData()) # Ensure the spatial data is loaded
  #
  #   # Start with a base Leaflet map
  #   map <- leaflet() %>%
  #     addTiles() # Adding a base map
  #
  #   # Conditionally add geometries based on their type
  #   if (any(st_is(spatialData(), "POINT")) | any(st_is(spatialData(), "MULTIPOINT"))){
  #     map <- map %>% addMarkers(data = spatialData(),  popup = ~htmlPopup(spatialData()[rownames(.), ]))
  #   }
  #   if (any(st_is(spatialData(), "POLYGON")) | any(st_is(spatialData(), "MULTIPOLYGON"))) {
  #     map <- map %>% addPolygons(data = spatialData(),  popup = ~htmlPopup(spatialData()[rownames(.), ]))
  #   }
  #   if (any(st_is(spatialData(), "LINESTRING"))) {
  #     map <- map %>% addPolylines(data = spatialData(),  popup = ~htmlPopup(spatialData()[rownames(.), ]))
  #   }
  #
  #   map # Render the map with the added layers
  # })
  
  
  
  observeEvent(input$applyMapping, {
    req(spatialData())
    df <- spatialData() # Fetch the current data frame
    
    # The field names from the Protected Planet Data Standard
    fieldNames <-
      c(
        "WDPAID",
        "WDPA_PID",
        "PA_DEF",
        "NAME",
        "ORIG_NAME",
        "DESIG",
        "DESIG_ENG",
        "DESIG_TYPE",
        "IUCN_CAT",
        "INT_CRIT",
        "MARINE",
        "REP_M_AREA",
        "GIS_M_AREA",
        "REP_AREA",
        "GIS_AREA",
        "NO_TAKE",
        "NO_TK_AREA",
        "STATUS",
        "STATUS_YR",
        "GOV_TYPE",
        "OWN_TYPE",
        "MANG_AUTH",
        "MANG_PLAN",
        "CONS_OBJ",
        "SUPP_INFO",
        "VERIF",
        "RESTRICT",
        "METADATAID",
        "SUB_LOC",
        "PARENT_ISO3",
        "ISO3"
      )
    
    # Iterate through each field name, applying mappings as specified by the user
    for (fieldName in fieldNames) {
      inputId <- paste0("map", gsub("[^[:alnum:]]", "", fieldName))
      selectedColumn <- input[[inputId]]
      print(paste("Checking if", selectedColumn, "is in names(df):", selectedColumn %in% names(df)))
      
      if (!is.null(selectedColumn) && !is.na(selectedColumn) && selectedColumn != "None" && selectedColumn %in% names(df)) {
        names(df)[names(df) == selectedColumn] <- fieldName
      }
    }
    
    # Update the reactive value with the modified data frame
    spatialData(df)
  })
  
  output$editableTable <- renderDT({
    req(spatialData())
    datatable(
      spatialData(),
      editable = TRUE,
      options = list(
        autoWidth = TRUE,
        # Automatically adjust column widths
        columnDefs = list(list(
          className = 'dt-center', targets = '_all'
        )),
        # Center align text
        pageLength = 10,
        # Set number of rows per page
        searchHighlight = TRUE # Highlight search terms
      ),
      class = 'cell-border stripe',
      # Add cell borders and stripe styling
      rownames = FALSE
    ) %>%
      formatStyle(
        # Apply conditional formatting (example)
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
    if (correctedCol <= ncol(df)) {
      df[info$row, correctedCol] <-
        DT::coerceValue(info$value, df[info$row, correctedCol])
      spatialData(df) # Update the attribute data
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      paste("package-", timestamp, ".zip", sep = "")
    },
    content = function(file) {
      validationMessage <- validateStandard3(spatialData())
      if (!is.null(validationMessage)) {
        showModal(
          modalDialog(
            title = "Validation Error",
            validationMessage,
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        stop(validationMessage)  # Prevent download
      }
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      geojsonFile <- paste0("data-", timestamp, ".geojson")
      metadataFile <- "metadata.csv"
      st_write(spatialData(),
               geojsonFile,
               driver = "GeoJSON",
               append = FALSE)
      metadataList <- reactiveValuesToList(metadata)
      metadataDf <- rev(stack(metadata))
      colnames(metadataDf) <- c("MetadataElement", "Value")
      # metadataDf <- data.frame(key = names(metadataList), value = unlist(metadataList), stringsAsFactors = FALSE)
      write.csv(metadataDf, metadataFile, row.names = FALSE)
      zip(file, files = c(geojsonFile, metadataFile))
    }
  )
  
  output$downloadGuide <- downloadHandler(
    filename = function() {
      "protectedplanet_data_guide.pdf"
    },
    content = function(file) {
      file.copy("protectedplanet_data_guide.pdf", file)
    }
  )
  
  # Handler for downloading the Data Standards Excel file
  output$downloadStandards <- downloadHandler(
    filename = function() {
      "protectedplanet_data_standards.xlsx"
    },
    content = function(file) {
      file.copy("protectedplanet_data_standards.xlsx", file)
    }
  )
  
  output$downloadAgreement <- downloadHandler(
    filename = function() {
      "agreement.pdf"
    },
    content = function(file) {
      file.copy("agreement.pdf", file)
    }
  )
}


shinyApp(ui, server)
