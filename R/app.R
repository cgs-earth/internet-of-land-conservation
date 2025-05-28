library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(DT)
library(shinythemes)
# Remove mapview - replace with leaflet only
library(shinyBS)
library(shinyWidgets)
library(rlang)
library(readr)
library(dplyr)
library(rhandsontable)

# Function to clean spatial data and handle complex data types
clean_spatial_data <- function(spatial_data) {
  tryCatch({
    # Convert all columns to handle complex data types
    cleaned_data <- spatial_data %>%
      mutate(across(everything(), ~ {
        if (is.list(.x) && !inherits(.x, "sfc")) {
          # Handle list columns by converting to character
          sapply(.x, function(item) {
            if (is.null(item)) {
              return(NA_character_)
            } else if (length(item) == 0) {
              return(NA_character_)
            } else if (length(item) == 1) {
              return(as.character(item))
            } else {
              return(paste(as.character(item), collapse = "; "))
            }
          })
        } else if (inherits(.x, "POSIXt") || inherits(.x, "Date")) {
          # Convert date/time to character
          as.character(.x)
        } else if (is.factor(.x)) {
          # Convert factors to character
          as.character(.x)
        } else if (is.logical(.x)) {
          # Convert logical to character
          as.character(.x)
        } else if (is.numeric(.x) && any(is.infinite(.x), na.rm = TRUE)) {
          # Handle infinite values
          ifelse(is.infinite(.x), NA, .x)
        } else {
          # Keep as is for simple types
          .x
        }
      }))
    
    # Ensure column names are valid
    names(cleaned_data) <- make.names(names(cleaned_data), unique = TRUE)
    
    # Remove any remaining problematic columns that couldn't be converted
    for (col_name in names(cleaned_data)) {
      if (col_name != "geometry") {
        col_data <- cleaned_data[[col_name]]
        if (is.list(col_data) && !inherits(col_data, "sfc")) {
          # If still a list, convert the entire column to character
          cleaned_data[[col_name]] <- as.character(col_data)
        }
      }
    }
    
    return(cleaned_data)
    
  }, error = function(e) {
    # If cleaning fails, try a more aggressive approach
    warning("Standard cleaning failed, trying aggressive cleaning: ", e$message)
    
    # Get geometry first
    geom <- st_geometry(spatial_data)
    
    # Convert to regular data frame, clean, then reattach geometry
    df <- st_drop_geometry(spatial_data)
    
    # Convert all columns to character except geometry
    for (i in seq_along(df)) {
      col_data <- df[[i]]
      if (is.list(col_data)) {
        df[[i]] <- sapply(col_data, function(x) {
          if (is.null(x) || length(x) == 0) {
            return(NA_character_)
          } else {
            return(paste(as.character(x), collapse = "; "))
          }
        })
      } else {
        df[[i]] <- as.character(col_data)
      }
    }
    
    # Reattach geometry
    cleaned_data <- st_sf(df, geometry = geom)
    return(cleaned_data)
  })
}

# Define validation rules
validation_rules <- list(
  WDPAID = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  WDPA_PID = function(value) {
    if (nchar(value) > 52) {
      return("ERR: Must not exceed 52 characters")
    }
    NULL
  },
  PA_DEF = function(value) {
    valid_values <- c("1", "0")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  NAME = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  ORIG_NAME = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  DESIG = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  DESIG_ENG = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  DESIG_TYPE = function(value) {
    valid_values <- c("National", "Regional", "International", "Not Applicable")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  IUCN_CAT = function(value) {
    valid_values <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Applicable", "Not Assigned", "Not Reported")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  INT_CRIT = function(value) {
    if (nchar(value) > 100) {
      return("ERR: Must not exceed 100 characters")
    }
    NULL
  },
  MARINE = function(value) {
    valid_values <- c("0", "1", "2")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  REP_M_AREA = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  GIS_M_AREA = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  REP_AREA = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  GIS_AREA = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  NO_TAKE = function(value) {
    valid_values <- c("All", "Part", "None", "Not Reported", "Not Applicable")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  NO_TK_AREA = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  STATUS = function(value) {
    valid_values <- c("Proposed", "Inscribed", "Adopted", "Designated", "Established")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  STATUS_YR = function(value) {
    if (!is.numeric(value) || nchar(value) > 12) {
      return("ERR: Must be a number not exceeding 12 digits")
    }
    NULL
  },
  GOV_TYPE = function(value) {
    valid_values <- c("Federal or national ministry or agency", "Sub-national ministry or agency", "Government-delegated management", "Transboundary governance", "Collaborative governance", "Joint governance", "Individual landowners", "Non-profit organisations", "For-profit organisations", "Indigenous peoples", "Local communities", "Not Reported")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  OWN_TYPE = function(value) {
    valid_values <- c("State", "Communal", "Individual landowners", "For-profit organisations", "Non-profit organisations", "Joint ownership", "Multiple ownership", "Contested", "Not Reported")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  MANG_AUTH = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  MANG_PLAN = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  VERIF = function(value) {
    valid_values <- c("State Verified", "Expert Verified", "Not Reported")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  },
  METADATAID = function(value) {
    if (!is.numeric(value)) {
      return("ERR: Must be a number")
    }
    NULL
  },
  SUB_LOC = function(value) {
    if (nchar(value) > 100) {
      return("ERR: Must not exceed 100 characters")
    }
    NULL
  },
  PARENT_ISO3 = function(value) {
    if (nchar(value) > 20) {
      return("ERR: Must not exceed 20 characters")
    }
    NULL
  },
  ISO3 = function(value) {
    valid_values <- c("AFG", "ALA", "ALB", "DZA", "ASM", "AND", "AGO", "AIA", "ATA", "ATG", "ARG", "ARM", "ABW", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", "BMU", "BTN", "BOL", "BES", "BIH", "BWA", "BVT", "BRA", "IOT", "BRN", "BGR", "BFA", "BDI", "CPV", "KHM", "CMR", "CAN", "CYM", "CAF", "TCD", "CHL", "CHN", "CXR", "CCK", "COL", "COM", "COG", "COD", "COK", "CRI", "CIV", "HRV", "CUB", "CUW", "CYP", "CZE", "DNK", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "SWZ", "ETH", "FLK", "FRO", "FJI", "FIN", "FRA", "GUF", "PYF", "ATF", "GAB", "GMB", "GEO", "DEU", "GHA", "GIB", "GRC", "GRL", "GRD", "GLP", "GUM", "GTM", "GGY", "GIN", "GNB", "GUY", "HTI", "HMD", "VAT", "HND", "HKG", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "IMN", "ISR", "ITA", "JAM", "JPN", "JEY", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MAC", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MTQ", "MRT", "MUS", "MYT", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MSR", "MAR", "MOZ", "MMR", "NAM", "NRU", "NPL", "NLD", "NCL", "NZL", "NIC", "NER", "NGA", "NIU", "NFK", "MKD", "MNP", "NOR", "OMN", "PAK", "PLW", "PSE", "PAN", "PNG", "PRY", "PER", "PHL", "PCN", "POL", "PRT", "PRI", "QAT", "REU", "ROU", "RUS", "RWA", "BLM", "SHN", "KNA", "LCA", "MAF", "SPM", "VCT", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SXM", "SVK", "SVN", "SLB", "SOM", "ZAF", "SGS", "SSD", "ESP", "LKA", "SDN", "SUR", "SJM", "SWE", "CHE", "SYR", "TWN", "TJK", "TZA", "THA", "TLS", "TGO", "TKL", "TON", "TTO", "TUN", "TUR", "TKM", "TCA", "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "UMI", "URY", "UZB", "VUT", "VEN", "VNM", "VGB", "VIR", "WLF", "ESH", "YEM", "ZMB", "ZWE")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be a valid ISO 3166-1 alpha-3 country code"))
    }
    NULL
  },
  SUPP_INFO = function(value) {
    if (nchar(value) > 254) {
      return("ERR: Must not exceed 254 characters")
    }
    NULL
  },
  CONS_OBJ = function(value) {
    valid_values <- c("Primary", "Secondary", "Ancillary")
    if (!value %in% valid_values) {
      return(paste("ERR: Must be one of:", paste(valid_values, collapse = ", ")))
    }
    NULL
  }
)

# Define fields that will be assigned by UNEP-WCMC and should not be mapped by users
UNEP_WCMC_FIELDS <- c("WDPAID", "WDPA_PID", "INT_CRIT", "MARINE", "GIS_M_AREA", "GIS_AREA", "VERIF", "RESTRICT", "METADATAID")

# Function to apply validations to a single value
validate_value <- function(field, value, required) {
  # Handle NA, empty strings, and "Not Reported"
  if (is.na(value) || value == "" || (is.character(value) && value %in% c("NA", "Not Reported"))) {
    if (required) {
      return("ERR: Required field cannot be NA or empty")
    } else {
      return("")
    }
  }
  
  # Convert numeric fields appropriately before validation
  numeric_fields <- c("WDPAID", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TK_AREA", "STATUS_YR", "METADATAID")
  
  if (field %in% numeric_fields) {
    # Try to convert to numeric if it's not already
    if (!is.numeric(value)) {
      converted_value <- suppressWarnings(as.numeric(as.character(value)))
      if (is.na(converted_value)) {
        return("ERR: Must be a number")
      }
      value <- converted_value
    }
  }
  
  if (field %in% names(validation_rules)) {
    rule <- validation_rules[[field]]
    # Try to apply the validation rule, catch any errors
    result <- tryCatch({
      rule(value)
    }, error = function(e) {
      return(paste("ERR:", e$message))
    })
    if (!is.null(result) && result != "") {
      return(result)
    }
  }
  
  return("")
}

validate_data <- function(data, required_fields) {
  # Clean data types more robustly
  data <- data %>% mutate(across(everything(), ~ {
    if (is.list(.x) && !inherits(.x, "sfc")) {
      # Handle list columns
      sapply(.x, function(item) {
        if (is.null(item) || length(item) == 0) {
          return(NA_character_)
        } else if (length(item) == 1) {
          return(as.character(item))
        } else {
          return(paste(as.character(item), collapse = "; "))
        }
      })
    } else if (inherits(.x, "POSIXt") || inherits(.x, "Date")) {
      as.character(.x)
    } else if (is.factor(.x)) {
      as.character(.x)
    } else if (is.logical(.x)) {
      as.character(.x)
    } else if (is.numeric(.x) && any(is.infinite(.x), na.rm = TRUE)) {
      # Handle infinite values
      ifelse(is.infinite(.x), NA, .x)
    } else {
      .x
    }
  }))
  
  # Initialize validation_results
  validation_results <- as.data.frame(matrix("", nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, names(data))), stringsAsFactors = FALSE)
  
  for (col in names(data)) {
    for (row in seq_len(nrow(data))) {
      value <- data[[col]][row]
      required <- col %in% required_fields
      validation_result <- validate_value(col, value, required)
      validation_results[[col]][row] <- validation_result
    }
  }
  
  return(validation_results)
}

ui <- navbarPage(
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
        z-index: 1030;
      }
      body {
        padding-top: 70px;
      }
      .warning {
        background-color: #FFCCCC;
      }
      .invalid-cell {
        background-color: #FFCCCC !important;
      }
      .htCommentCell {
        position: relative;
      }
      .htComments {
        background-color: #ffcccc;
        border: 1px solid #ff0000;
        border-radius: 4px;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
        color: black;
        font-size: 12px;
        font-weight: bold;
        max-width: 300px;
        padding: 8px;
        word-wrap: break-word;
      }
      .unep-wcmc-info {
        background-color: #e6f3ff;
        border: 1px solid #b3d9ff;
        border-radius: 4px;
        padding: 10px;
        margin: 10px 0;
      }
      .warning-cell {
        background-color: #fff3cd !important;
      }
      .unep-wcmc-cell {
        background-color: #e6f3ff !important;
      }
      .unmapped-cell {
        background-color: #f8f9fa !important;
        color: #6c757d !important;
      }
    "
    )
  )),
  tabPanel("Introduction",
           fluidPage(
             div(class = "content-wrapper",
                 wellPanel(
                   h1("Welcome to the Protected Planet Data Wizard"),
                   p("This interactive tool is your assistant for preparing and validating geospatial data for submission to the ", 
                     tags$a("World Database on Protected Areas (WDPA)", href="https://www.protectedplanet.net/en/thematic-areas/wdpa", target="_blank"),
                     " and the ", 
                     tags$a("Other Effective Area-Based Conservation Measures (OECM)", href="https://www.protectedplanet.net/en/thematic-areas/oecms", target="_blank"),
                     " database."), 
                   
                   h2("How Does It Work?"),
                   p("The Data Wizard simplifies the data preparation process, ensuring that your datasets meet the Protected Planet data standards, including proper formatting and attribute requirements."),
                   
                   h2("Key Requirements for Data Submission:"),
                   tags$ul(
                     tags$li("Areas must conform to the ", 
                             tags$a("IUCN definition of a protected area", href="https://www.iucn.org/theme/protected-areas/about/protected-area-categories", target="_blank"),
                             " or the ", 
                             tags$a("Convention on Biological Diversity's definition of an OECM", href="https://www.cbd.int/decisions/cop/14/08/02", target="_blank"), 
                             "."),
                     tags$li("Submissions must include both spatial data (in GIS format) and an associated attribute table."),
                     tags$li("The source of the information must be clearly stated in a Source Table."),
                     tags$li("A signed WDPA Data Contributor Agreement is required.")
                   ),
                   
                   h2("Getting Started:"),
                   tags$ul(
                     tags$li(strong("Before You Begin: Select your database:"), " Choose whether you are preparing data for the WDPA or OECM database."),
                     tags$li(strong("Step 1: Fill out the Source Table:"), " Provide detailed information about your dataset."),
                     tags$li(strong("Step 2: Upload your geospatial file:"), " Acceptable formats include zipped shapefiles, GeoJSON, GeoPackage, or KML files."),
                     tags$li(strong("Step 3: Map your data and download your package:"), " Align your data with the Protected Planet standards.")
                   ),
                   
                   div(class = "unep-wcmc-info",
                       h4("Important Note:"),
                       p(strong("Some fields will be automatically assigned by UNEP-WCMC and do not need to be mapped:"), 
                         "WDPAID, WDPA_PID, INT_CRIT, MARINE, GIS_M_AREA, GIS_AREA, VERIF, RESTRICT, and METADATAID.")
                   ),
                   
                   h2("Helpful Resources:"),
                   tags$ul(
                     tags$li(tags$a("Protected Planet Portal", href="https://www.protectedplanet.net/", target="_blank")),
                     tags$li(tags$a("IUCN Protected Area Categories", href="https://www.iucn.org/theme/protected-areas/about/protected-area-categories", target="_blank")),
                     tags$li(tags$a("OECM Guidelines", href="https://www.iucn.org/resources/conservation-tools/other-effective-area-based-conservation-measures", target="_blank")),
                     tags$li(tags$a("WDPA Manual", href="hhttps://www.protectedplanet.net/en/resources/wdpa-manual", target="_blank")),
                     tags$li(tags$a("UNEP-WCMC", href="https://www.unep-wcmc.org/", target="_blank"))
                   ),
                   
                   p("Thank you for contributing to the global conservation effort through Protected Planet!"),
                   
                   downloadButton("downloadGuide", "Download Data Submission Guide"),
                   downloadButton("downloadStandards", "Download Data Standards Excel")
                 ),
                 
                 radioButtons("databaseType", "Select Database you would like to prepare data for:",
                              choices = c("WDPA" = "WDPA", "OECM" = "OECM"),
                              selected = "WDPA"),
                 actionButton("startBtn", "Get Started", class = "btn-primary")
             ))),
  
  tabPanel("Step 1: Source Table Editor",
           div(class = "content-wrapper",
               sidebarLayout(
                 sidebarPanel(
                   width = 12,
                   h4("Please fill out the Source Table for your dataset."),
                   p("This information helps to identify and describe your dataset. Required fields are marked with an asterisk (*)."),
                   
                   fluidRow(
                     column(4, textInput("METADATAID", "Metadata ID*", value = "Not Reported")),
                     column(8, "An ID assigned by UNEP-WCMC that is used to link each record in the WDPA or OECM database to the relevant source information in the Source Table.")
                   ),
                   fluidRow(
                     column(4, textInput("DATA_TITLE", "Data Set Title*", value = "")),
                     column(8, "The title of the dataset e.g., Protected Areas of Ireland; OECMs of South Africa.")
                   ),
                   fluidRow(
                     column(4, textInput("RESP_PARTY", "Responsible Party*", value = "")),
                     column(8, "The data provider, organization, or government providing the data.")
                   ),
                   fluidRow(
                     column(4, textInput("RESP_EMAIL", "Responsible Party Contact E-mail(s)*", value = "Not Reported")),
                     column(8, "Contact e-mail address of the organization providing the data.")
                   ),
                   fluidRow(
                     column(4, textInput("VERIFIER", "Verifying Party*", value = "None")),
                     column(8, "The organization or government that has verified the data.")
                   ),
                   fluidRow(
                     column(4, textInput("V_EMAIL", "Verifying Party Contact Email(s)", value = "None")),
                     column(8, "Contact e-mail address of the verifying party.")
                   ),
                   fluidRow(
                     column(4, textInput("YEAR", "Year*", value = "Not Reported")),
                     column(8, "The year when the dataset was first submitted to the database.")
                   ),
                   fluidRow(
                     column(4, textInput("UPDATE_YR", "Update Year*", value = "")),
                     column(8, "The year in which the dataset was last updated.")
                   ),
                   fluidRow(
                     column(4, textInput("LANGUAGE", "Dataset Language*", value = "Not Reported")),
                     column(8, "Language(s) used within the dataset.")
                   ),
                   fluidRow(
                     column(4, textInput("CHAR_SET", "Dataset Character Set*", value = "Not Reported")),
                     column(8, "Character coding standard used in the dataset.")
                   ),
                   fluidRow(
                     column(4, textInput("REF_SYSTEM", "Coordinate System*", value = "Not Reported")),
                     column(8, "Name and parameters of the coordinate system of the original dataset.")
                   ),
                   fluidRow(
                     column(4, textInput("SCALE", "Scale", value = "Not Reported")),
                     column(8, "The scale of the dataset when originally created.")
                   ),
                   fluidRow(
                     column(4, textAreaInput("LINEAGE", "Lineage", value = "Not Reported")),
                     column(8, "Information about the creation and maintenance of the dataset.")
                   ),
                   fluidRow(
                     column(4, textInput("CITATION", "Citation", value = "Not Reported")),
                     column(8, "Recommended text for referencing the dataset.")
                   ),
                   fluidRow(
                     column(4, textAreaInput("DISCLAIMER", "Disclaimer", value = "Not Reported")),
                     column(8, "Warnings or exceptions to use of the data.")
                   ),
                   fluidRow(column(
                     12,
                     actionButton("saveMetadata", "Save Metadata to Proceed", class = "btn-primary mt-3")
                   ))
                 ),
                 mainPanel()
               ))),
  
  tabPanel("Step 2: Upload and Inspect",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 "fileUpload",
                 "Choose a Geospatial File (zipped shapefile .zip, .geojson, .gpkg, .gdb, .kml)",
                 accept = c(".zip", ".geojson", ".json", ".gdb", ".kml")
               ),
               actionButton("next2", "Next")
             ),
             mainPanel(leafletOutput("map", height = "600px"))
           )),
  
  tabPanel("Step 3: Mapping and Download",
           fluidPage(
             div(class = "unep-wcmc-info",
                 h5("The following fields will be automatically assigned by UNEP-WCMC and do not need to be mapped:"),
                 p(paste(UNEP_WCMC_FIELDS, collapse = ", "))
             ),
             tabsetPanel(
               id = "stepTabs",
               tabPanel("Field Mapping", value = "mappingTab",
                        fluidRow(
                          h5("Use the dropdown menus below to select which columns in your data correspond to the standard columns. Then, press 'Apply Mapping' to confirm your choices."),
                          actionButton("applyMapping", "Apply Mapping", class = "btn-primary")
                        ),
                        fluidRow(
                          column(6,
                                 h4("Required Fields (for mapping)"),
                                 uiOutput("minimalFieldsUI")
                          ),
                          column(6,
                                 h4("Optional Fields"),
                                 uiOutput("completeFieldsUI")
                          )
                        )
               ),
               tabPanel("Data Table and Validation", value = "dataTab",
                        fluidRow(
                          h5("Inspect your data in the table below. Fields with validation errors are highlighted in red. Optional empty fields are highlighted in yellow. UNEP-WCMC fields are highlighted in blue. Click 'Validate' to check your data, then 'Download Package' when ready."),
                          actionButton("validateButton", "Validate", class = "btn-primary"),
                          downloadButton("download", "Download Package", class = "btn-success")
                        ),
                        rHandsontableOutput("editableTable")
               )
             )
           ))
)

server <- function(input, output, session) {
  # Reactive values to track mapping
  spatialData <- reactiveVal()
  attributeData <- reactiveVal()
  geometryData <- reactiveVal()  # Store geometry separately
  originalGeometry <- reactiveVal()  # Store the original geometry
  mappingApplied <- reactiveVal(FALSE)
  mappedFields <- reactiveVal(c())
  unmappedFields <- reactiveVal(c())
  
  # Define categorical fields and their valid options
  categoricalFields <- list(
    PA_DEF = c("1", "0"),
    DESIG_TYPE = c("National", "Regional", "International", "Not Applicable"),
    IUCN_CAT = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Applicable", "Not Assigned", "Not Reported"),
    MARINE = c("0", "1", "2"),
    NO_TAKE = c("All", "Part", "None", "Not Reported", "Not Applicable"),
    STATUS = c("Proposed", "Inscribed", "Adopted", "Designated", "Established"),
    GOV_TYPE = c("Federal or national ministry or agency", "Sub-national ministry or agency", "Government-delegated management", "Transboundary governance", "Collaborative governance", "Joint governance", "Individual landowners", "Non-profit organisations", "For-profit organisations", "Indigenous peoples", "Local communities", "Not Reported"),
    OWN_TYPE = c("State", "Communal", "Individual landowners", "For-profit organisations", "Non-profit organisations", "Joint ownership", "Multiple ownership", "Contested", "Not Reported"),
    VERIF = c("State Verified", "Expert Verified", "Not Reported"),
    ISO3 = c("AFG", "ALA", "ALB", "DZA", "ASM", "AND", "AGO", "AIA", "ATA", "ATG", "ARG", "ARM", "ABW", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", "BMU", "BTN", "BOL", "BES", "BIH", "BWA", "BVT", "BRA", "IOT", "BRN", "BGR", "BFA", "BDI", "CPV", "KHM", "CMR", "CAN", "CYM", "CAF", "TCD", "CHL", "CHN", "CXR", "CCK", "COL", "COM", "COG", "COD", "COK", "CRI", "CIV", "HRV", "CUB", "CUW", "CYP", "CZE", "DNK", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "SWZ", "ETH", "FLK", "FRO", "FJI", "FIN", "FRA", "GUF", "PYF", "ATF", "GAB", "GMB", "GEO", "DEU", "GHA", "GIB", "GRC", "GRL", "GRD", "GLP", "GUM", "GTM", "GGY", "GIN", "GNB", "GUY", "HTI", "HMD", "VAT", "HND", "HKG", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "IMN", "ISR", "ITA", "JAM", "JPN", "JEY", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MAC", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MTQ", "MRT", "MUS", "MYT", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MSR", "MAR", "MOZ", "MMR", "NAM", "NRU", "NPL", "NLD", "NCL", "NZL", "NIC", "NER", "NGA", "NIU", "NFK", "MKD", "MNP", "NOR", "OMN", "PAK", "PLW", "PSE", "PAN", "PNG", "PRY", "PER", "PHL", "PCN", "POL", "PRT", "PRI", "QAT", "REU", "ROU", "RUS", "RWA", "BLM", "SHN", "KNA", "LCA", "MAF", "SPM", "VCT", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SXM", "SVK", "SVN", "SLB", "SOM", "ZAF", "SGS", "SSD", "ESP", "LKA", "SDN", "SUR", "SJM", "SWE", "CHE", "SYR", "TWN", "TJK", "TZA", "THA", "TLS", "TGO", "TKL", "TON", "TTO", "TUN", "TUR", "TKM", "TCA", "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "UMI", "URY", "UZB", "VUT", "VEN", "VNM", "VGB", "VIR", "WLF", "ESH", "YEM", "ZMB", "ZWE"),
    CONS_OBJ = c("Primary", "Secondary", "Ancillary")
  )
  
  fieldDescriptions <- list(
    NAME = "The name of a protected area or OECM assigned in legal texts or by its governance authority.",
    STATUS = "The current status of the protected area: Proposed, Inscribed, Adopted, Designated, Established",
    PA_DEF = "This attribute indicates whether the site meets the IUCN definition of a protected area (WDPA only) or the CBD definition of an OECM (OECM database only).",
    ORIG_NAME = "The name of the protected area or OECM in any language supported by UTF 8 encoding.",
    DESIG = "The designation of the protected area or OECM in the native language.",
    DESIG_ENG = "The designation of the protected area or OECM in English.",
    DESIG_TYPE = "Category or type of protected area: National, Regional, International, or Not Applicable",
    IUCN_CAT = "The IUCN protection category: Ia, Ib, II, III, IV, V, VI, Not Reported, Not Applicable, or Not Assigned",
    REP_M_AREA = "Reported area that is within the marine environment in sq km",
    REP_AREA = "Reported total area in sq km",
    NO_TAKE = "Taking of living or dead natural resources is prohibited in the Marine area",
    NO_TK_AREA = "Total area in sq km of the marine area which is No Take",
    STATUS_YR = "The year the current status came into force: YYYY or 0 if unknown",
    GOV_TYPE = "The type of entity that makes decisions about how the area is managed",
    OWN_TYPE = "Legal Ownership type (independent of governance)",
    MANG_AUTH = "Management Authority, the specific individual or organization that manages the protected area",
    MANG_PLAN = "A URL linking to a document or website detailing a management plan or mechanism",
    SUPP_INFO = "Any supporting information on an OECM",
    CONS_OBJ = "For OECM only. The extent to which biodiversity is a conservation objective: Primary, Secondary, Ancillary",
    SUB_LOC = "The subnational location as an ISO 3166-2 sub-national code",
    PARENT_ISO3 = "The ISO3 code of the country that the area resides in",
    ISO3 = "The ISO3 code of the territory that the area resides in"
  )
  
  # Function to generate selectInput for a given field (only for user-mappable fields)
  generateSelectInput <- function(fieldName) {
    # Skip UNEP-WCMC fields
    if (fieldName %in% UNEP_WCMC_FIELDS) {
      return(NULL)
    }
    
    selectId <- paste0("map", gsub("[^[:alnum:]]", "", fieldName))
    inputField <- selectInput(
      inputId = selectId,
      label = paste(fieldName, ":"),
      choices = c("None" = "", names(spatialData())),
      width = "80%"
    )
    
    helpIconId <- paste0("helpIcon", gsub("[^[:alnum:]]", "", fieldName))
    helpIcon <- tags$span(
      tags$i(class = "fa fa-info-circle", id = helpIconId),
      style = "margin-left: 5px; cursor: help;"
    )
    
    tooltipText = fieldDescriptions[[fieldName]] %||% "No description available"
    
    uiOutput <- tags$div(
      style = "display: flex; align-items: center;",
      inputField,
      tipify(helpIcon, tooltipText, placement="right")
    )
    
    uiOutput
  }
  
  metadata <- reactiveValues(title = NULL, abstract = NULL, contact = NULL)
  
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
    updateNavbarPage(session, "nav", selected = "Step 2: Upload and Inspect")
  })
  
  observeEvent(input$next2, {
    updateNavbarPage(session, "nav", selected = "Step 3: Mapping and Download")
  })
  
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    fileExt <- tools::file_ext(input$fileUpload$name)
    
    tryCatch({
      if (fileExt == "zip") {
        tempDir <- tempdir()
        unzipPath <- file.path(tempDir, tools::file_path_sans_ext(basename(input$fileUpload$name)))
        unzip(input$fileUpload$datapath, exdir = unzipPath)
        shpFiles <- list.files(unzipPath, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
        shpFiles <- shpFiles[!grepl("__MACOSX", shpFiles)]
        
        if (length(shpFiles) == 1) {
          raw_data <- sf::st_read(shpFiles[1]) %>% st_transform(4326)
          cleaned_data <- clean_spatial_data(raw_data)
          spatialData(cleaned_data)
          attributeData(st_drop_geometry(cleaned_data))
          # Store geometry separately for safer handling
          originalGeometry(st_geometry(cleaned_data))
          geometryData(st_geometry(cleaned_data))
        } else {
          showModal(modalDialog(
            title = "Error",
            "The ZIP file must contain exactly one shapefile (.shp, .shx, .dbf).",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
      } else if (fileExt %in% c("geojson", "json", "gpkg", "gdb", "kml")) {
        raw_data <- sf::st_read(input$fileUpload$datapath) %>% st_transform(4326)
        cleaned_data <- clean_spatial_data(raw_data)
        spatialData(cleaned_data)
        attributeData(st_drop_geometry(cleaned_data))
        # Store geometry separately for safer handling
        originalGeometry(st_geometry(cleaned_data))
        geometryData(st_geometry(cleaned_data))
      } else {
        showModal(modalDialog(
          title = "Error",
          "Unsupported file type. Please upload a GeoJSON, GeoPackage, ESRI GeoDatabase, KML, or zipped Shapefile",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "File Upload Error",
        paste("Error reading file:", e$message, 
              "\n\nThis may be due to complex nested data structures in your file.",
              "Please ensure your file contains simple attribute data."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
  })
  
  # Render UI for minimal fields (excluding UNEP-WCMC fields)
  output$minimalFieldsUI <- renderUI({
    req(spatialData())
    dbType <- input$databaseType
    minimalFields_wdpa <- c("PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "REP_M_AREA", "REP_AREA", "STATUS", "STATUS_YR", "PARENT_ISO3", "ISO3")
    minimalFields_oecm <- c("PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "REP_M_AREA", "REP_AREA", "STATUS", "STATUS_YR", "PARENT_ISO3", "ISO3")
    
    minimalFields <- if(dbType == "WDPA") {minimalFields_wdpa} else {minimalFields_oecm}
    minimalFields <- setdiff(minimalFields, UNEP_WCMC_FIELDS)
    lapply(minimalFields, generateSelectInput)
  })
  
  # Render UI for complete fields (excluding UNEP-WCMC fields)
  output$completeFieldsUI <- renderUI({
    req(spatialData())
    dbType <- input$databaseType
    completeFields_wdpa <- c("DESIG_ENG", "IUCN_CAT", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "SUB_LOC")
    completeFields_oecm <- c("DESIG_ENG", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "CONS_OBJ", "SUPP_INFO", "SUB_LOC")
    
    completeFields <- if(dbType == "WDPA") {completeFields_wdpa} else {completeFields_oecm}
    completeFields <- setdiff(completeFields, UNEP_WCMC_FIELDS)
    lapply(completeFields, generateSelectInput)
  })
  
  # REPLACED: mapview with pure leaflet implementation
  output$map <- renderLeaflet({
    req(spatialData())
    
    # Convert sf object to leaflet-compatible format
    data_for_map <- spatialData()
    
    # Get the attribute data (without geometry)
    attr_data <- st_drop_geometry(data_for_map)
    
    # Create popups showing all attributes for each feature
    popups <- character(nrow(attr_data))
    
    for(i in 1:nrow(attr_data)) {
      # Get attribute names and values for this row
      row_data <- attr_data[i, ]
      
      # Create a simple HTML table
      table_content <- ""
      for(col_name in names(row_data)) {
        value <- row_data[[col_name]]
        # Handle NA values and convert to character
        display_value <- if(is.na(value) || value == "" || is.null(value)) {
          "NA"
        } else {
          as.character(value)
        }
        
        # Truncate very long values
        if(nchar(display_value) > 60) {
          display_value <- paste0(substr(display_value, 1, 57), "...")
        }
        
        table_content <- paste0(table_content,
                                "<tr>",
                                "<td style='font-weight:bold; padding:3px; border-bottom:1px solid #ddd;'>", col_name, "</td>",
                                "<td style='padding:3px; border-bottom:1px solid #ddd;'>", display_value, "</td>",
                                "</tr>"
        )
      }
      
      popups[i] <- paste0(
        "<div style='max-height:400px; overflow-y:auto; min-width:200px;'>",
        "<table style='width:100%; border-collapse:collapse; font-size:11px;'>",
        table_content,
        "</table>",
        "</div>"
      )
    }
    
    # Get bounds for the data
    bbox <- st_bbox(data_for_map)
    
    # Create leaflet map
    leaflet(data_for_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "blue",
        color = "darkblue",
        weight = 2,
        opacity = 1,
        fillOpacity = 0.5,
        popup = popups,
        label = ~paste("Feature", seq_len(nrow(data_for_map))),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
  })
  
  observeEvent(input$applyMapping, {
    req(spatialData())
    df <- spatialData()
    
    fieldNames <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG", 
                    "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT", "MARINE",
                    "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE",
                    "NO_TK_AREA", "STATUS", "STATUS_YR", "GOV_TYPE", "OWN_TYPE",
                    "MANG_AUTH", "MANG_PLAN", "CONS_OBJ", "SUPP_INFO", "VERIF",
                    "RESTRICT", "METADATAID", "SUB_LOC", "PARENT_ISO3", "ISO3")
    
    original_columns <- names(df)[names(df) != "geometry"]
    mapped_fields <- c()
    mapped_original_columns <- c()
    
    # Apply field mappings (only for user-mappable fields)
    for (fieldName in fieldNames) {
      if (fieldName %in% UNEP_WCMC_FIELDS) {
        next
      }
      
      inputId <- paste0("map", gsub("[^[:alnum:]]", "", fieldName))
      selectedColumn <- input[[inputId]]
      
      if (!is.null(selectedColumn) && !is.na(selectedColumn) && selectedColumn != "" && selectedColumn != "None" && selectedColumn %in% names(df)) {
        if (fieldName %in% mapped_fields) {
          showModal(modalDialog(
            title = "Error",
            paste("Duplicate mapping detected for field '", fieldName, 
                  "'. Please ensure each standardized field is mapped only once.", sep = ""),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        names(df)[names(df) == selectedColumn] <- fieldName
        mapped_fields <- c(mapped_fields, fieldName)
        mapped_original_columns <- c(mapped_original_columns, selectedColumn)
      }
    }
    
    unmapped_columns <- setdiff(original_columns, mapped_original_columns)
    mappedFields(mapped_fields)
    unmappedFields(unmapped_columns)
    
    if (any(duplicated(names(df)))) {
      dupFields <- names(df)[duplicated(names(df))]
      showModal(modalDialog(
        title = "Error",
        paste("Duplicate field names detected after mapping: ", paste(dupFields, collapse = ", "),
              ". Please ensure each standardized field is mapped only once.", sep = ""),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    dbType <- input$databaseType
    minimalFields_wdpa <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "STATUS", "STATUS_YR", "VERIF", "RESTRICT", "METADATAID", "PARENT_ISO3", "ISO3")
    minimalFields_oecm <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "STATUS", "STATUS_YR", "VERIF", "RESTRICT", "METADATAID", "PARENT_ISO3", "ISO3")
    completeFields_wdpa <- c("DESIG_ENG", "IUCN_CAT", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "SUB_LOC")
    completeFields_oecm <- c("DESIG_ENG", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "CONS_OBJ", "SUPP_INFO", "SUB_LOC")
    
    fields <- if(dbType == "WDPA") {c(minimalFields_wdpa, completeFields_wdpa)} else {c(minimalFields_oecm, completeFields_oecm)}
    
    fieldTypes <- list(
      WDPAID = "numeric", WDPA_PID = "character", PA_DEF = "character", NAME = "character", ORIG_NAME = "character", DESIG = "character",
      DESIG_ENG = "character", DESIG_TYPE = "character", IUCN_CAT = "character", INT_CRIT = "character", MARINE = "character",
      REP_M_AREA = "numeric", GIS_M_AREA = "numeric", REP_AREA = "numeric", GIS_AREA = "numeric", NO_TAKE = "character",
      NO_TK_AREA = "numeric", STATUS = "character", STATUS_YR = "numeric", GOV_TYPE = "character", OWN_TYPE = "character",
      MANG_AUTH = "character", MANG_PLAN = "character", CONS_OBJ = "character", SUPP_INFO = "character", VERIF = "character",
      RESTRICT = "character", METADATAID = "numeric", SUB_LOC = "character", PARENT_ISO3 = "character", ISO3 = "character"
    )
    
    # Ensure all standard fields exist in df, with correct data types
    for (field in fields) {
      if (!(field %in% names(df))) {
        fieldType <- fieldTypes[[field]]
        
        if (field %in% UNEP_WCMC_FIELDS) {
          if (fieldType == "numeric") {
            df[[field]] <- NA_real_
          } else {
            df[[field]] <- "To be assigned by UNEP-WCMC"
          }
        } else {
          if (fieldType == "numeric") {
            df[[field]] <- NA_real_
          } else if (fieldType == "character") {
            df[[field]] <- NA_character_
          } else {
            df[[field]] <- NA
          }
        }
      } else {
        fieldType <- fieldTypes[[field]]
        if (fieldType == "numeric") {
          df[[field]] <- as.numeric(df[[field]])
        } else if (fieldType == "character") {
          df[[field]] <- as.character(df[[field]])
        }
      }
    }
    
    # Convert factors to character
    df <- df %>% mutate(across(where(is.factor), as.character))
    df <- df %>% mutate(across(where(~ inherits(.x, "POSIXt") | inherits(.x, "Date")), as.character))
    df <- df %>% mutate(across(where(is.list), ~ sapply(.x, toString)))
    df <- df %>% mutate(across(where(is.logical), as.character))
    
    spatialData(df)
    mappingApplied(TRUE)
    updateTabsetPanel(session, "stepTabs", selected = "dataTab")
    
    unmapped_message <- ""
    if (length(unmapped_columns) > 0) {
      unmapped_message <- paste0("<br><br><strong>Unmapped fields (will not be included in download package):</strong><br>",
                                 paste(unmapped_columns, collapse = ", "))
    }
    
    showModal(modalDialog(
      title = "Mapping Applied",
      HTML(paste(
        "Field mapping has been applied successfully. You have been redirected to the Data Table tab.<br><br>",
        "<strong>UNEP-WCMC fields have been automatically populated:</strong><br>",
        paste(UNEP_WCMC_FIELDS, collapse = ", "),
        unmapped_message
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Reactive value to store validation results
  validation_results <- reactiveVal(NULL)
  
  observeEvent(input$validateButton, {
    req(spatialData())
    
    # Get the current data from the table if it exists, otherwise use spatialData
    if (!is.null(input$editableTable)) {
      # Get data from the edited table
      df_for_validation <- hot_to_r(input$editableTable)
      
      # Clean up any message markers from the data BEFORE validation
      for (col in names(df_for_validation)) {
        if (is.character(df_for_validation[[col]])) {
          df_for_validation[[col]] <- gsub(" \\*ERROR\\*$", "", df_for_validation[[col]])
          df_for_validation[[col]] <- ifelse(df_for_validation[[col]] == "*ERROR*", NA, df_for_validation[[col]])
          df_for_validation[[col]] <- ifelse(df_for_validation[[col]] == "Consider completing this optional field", NA, df_for_validation[[col]])
          df_for_validation[[col]] <- ifelse(df_for_validation[[col]] == "To be assigned by UNEP-WCMC", NA, df_for_validation[[col]])
        }
      }
    } else {
      # Use the original spatial data
      df_for_validation <- spatialData() %>% st_drop_geometry()
    }
    
    all_required_fields <- c("WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG", 
                             "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", 
                             "REP_AREA", "GIS_AREA", "STATUS", "STATUS_YR", "VERIF", 
                             "METADATAID", "PARENT_ISO3", "ISO3")
    
    required_fields <- setdiff(all_required_fields, UNEP_WCMC_FIELDS)
    validation_res <- validate_data(df_for_validation, required_fields)
    validation_results(validation_res)
    
    showModal(modalDialog(
      title = "Validation Complete",
      HTML(paste(
        "Validation has been completed. Please review the table for any errors highlighted.<br><br>",
        "<strong>The following fields were automatically set to be assigned by UNEP-WCMC:</strong><br>",
        paste(UNEP_WCMC_FIELDS, collapse = ", ")
      )),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$editableTable <- renderRHandsontable({
    req(spatialData())
    req(mappingApplied())
    
    df <- spatialData()
    dbType <- input$databaseType
    required_fields <- if(dbType == "WDPA") {
      c("PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "REP_M_AREA", "REP_AREA", "STATUS", "STATUS_YR", "PARENT_ISO3", "ISO3")
    } else {
      c("PA_DEF", "NAME", "ORIG_NAME", "DESIG", "DESIG_TYPE", "REP_M_AREA", "REP_AREA", "STATUS", "STATUS_YR", "PARENT_ISO3", "ISO3")
    }
    
    optional_fields <- if(dbType == "WDPA") {
      c("DESIG_ENG", "IUCN_CAT", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "SUB_LOC")
    } else {
      c("DESIG_ENG", "NO_TAKE", "NO_TK_AREA", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "CONS_OBJ", "SUPP_INFO", "SUB_LOC")
    }
    
    df_no_geom <- df %>% st_drop_geometry()
    
    # Clean data types more robustly
    df_no_geom <- df_no_geom %>% mutate(across(everything(), ~ {
      if (is.list(.x) && !inherits(.x, "sfc")) {
        sapply(.x, function(item) {
          if (is.null(item) || length(item) == 0) {
            return(NA_character_)
          } else if (length(item) == 1) {
            return(as.character(item))
          } else {
            return(paste(as.character(item), collapse = "; "))
          }
        })
      } else if (inherits(.x, "POSIXt") || inherits(.x, "Date")) {
        as.character(.x)
      } else if (is.factor(.x)) {
        as.character(.x)
      } else if (is.logical(.x)) {
        as.character(.x)
      } else if (is.numeric(.x) && any(is.infinite(.x), na.rm = TRUE)) {
        ifelse(is.infinite(.x), NA_character_, as.character(.x))
      } else {
        .x
      }
    }))
    
    # Ensure all columns are either numeric or character (no lists)
    for (col_name in names(df_no_geom)) {
      if (is.list(df_no_geom[[col_name]])) {
        df_no_geom[[col_name]] <- as.character(df_no_geom[[col_name]])
      }
    }
    
    validation_res <- validation_results()
    
    # Create custom renderers for different field types
    unep_wcmc_renderer <- JS("
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e6f3ff';
        td.style.fontStyle = 'italic';
        td.setAttribute('title', 'This field will be assigned by UNEP-WCMC');
        return td;
      }
    ")
    
    unmapped_renderer <- JS("
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#f8f9fa';
        td.style.color = '#6c757d';
        td.setAttribute('title', 'This field was not mapped and will not be included in the download package');
        return td;
      }
    ")
    
    # Create validation messages with proper styling
    if (!is.null(validation_res)) {
      for (colName in names(df_no_geom)) {
        for (row in 1:nrow(df_no_geom)) {
          current_value <- df_no_geom[row, colName]
          
          if (colName %in% UNEP_WCMC_FIELDS) {
            if (is.na(current_value) || current_value == "") {
              df_no_geom[row, colName] <- "To be assigned by UNEP-WCMC"
            }
          } else if (colName %in% names(validation_res) && validation_res[[colName]][row] != "") {
            error_msg <- validation_res[[colName]][row]
            if (!is.na(current_value) && current_value != "") {
              df_no_geom[row, colName] <- paste0(current_value, " *ERROR*")
            } else {
              df_no_geom[row, colName] <- "*ERROR*"
            }
          } else if (colName %in% optional_fields && (is.na(current_value) || current_value == "" || current_value == "NA")) {
            df_no_geom[row, colName] <- "Consider completing this optional field"
          }
        }
      }
    }
    
    # Create rhandsontable with error handling
    tryCatch({
      rh <- rhandsontable(df_no_geom, useTypes = TRUE, stretchH = "all", height = 500)
      
      # Apply custom renderers for each column type
      for (colName in names(df_no_geom)) {
        if (colName %in% UNEP_WCMC_FIELDS) {
          rh <- hot_col(rh, colName, readOnly = TRUE, renderer = unep_wcmc_renderer)
        } else if (colName %in% unmappedFields()) {
          rh <- hot_col(rh, colName, renderer = unmapped_renderer)
        } else if (colName %in% optional_fields) {
          optional_renderer <- JS(paste0("
            function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.TextRenderer.apply(this, arguments);
              
              if (value === 'Consider completing this optional field') {
                td.style.backgroundColor = '#fff3cd';
                td.style.fontStyle = 'italic';
                td.setAttribute('title', 'This is an optional field that you may want to complete');
              }
              
              return td;
            }
          "))
          rh <- hot_col(rh, colName, renderer = optional_renderer)
        } else if (!is.null(validation_res) && colName %in% names(validation_res)) {
          col_errors <- list()
          for (row in 1:nrow(validation_res)) {
            if (validation_res[[colName]][row] != "") {
              col_errors[[as.character(row-1)]] <- validation_res[[colName]][row]
            }
          }
          
          if (length(col_errors) > 0) {
            error_json <- jsonlite::toJSON(col_errors)
            error_renderer <- JS(paste0("
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);
                
                var errors = ", error_json, ";
                if (errors[row]) {
                  td.style.backgroundColor = '#ffcccc';
                  td.setAttribute('title', errors[row]);
                }
                
                return td;
              }
            "))
            rh <- hot_col(rh, colName, renderer = error_renderer)
          }
        }
        
        # Set dropdowns for categorical fields
        if (colName %in% names(categoricalFields)) {
          rh <- hot_col(rh, colName, type = "dropdown", source = categoricalFields[[colName]])
        }
      }
      
      rh$elementId <- "validationTable"
      return(rh)
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Table Display Error",
        paste("Error creating editable table:", e$message,
              "\n\nThis may be due to complex data types in your file.",
              "The data has been loaded but the table view is not available."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return(NULL)
    })
  })
  
  # FIXED: Better geometry handling in the observe block
  observe({
    # Update spatialData when table is edited
    if (!is.null(input$editableTable)) {
      tryCatch({
        df <- hot_to_r(input$editableTable)
        
        # Clean up any message markers from the data
        for (col in names(df)) {
          if (is.character(df[[col]])) {
            df[[col]] <- gsub(" \\*ERROR\\*$", "", df[[col]])
            df[[col]] <- ifelse(df[[col]] == "*ERROR*", NA, df[[col]])
            df[[col]] <- ifelse(df[[col]] == "Consider completing this optional field", NA, df[[col]])
            df[[col]] <- ifelse(df[[col]] == "To be assigned by UNEP-WCMC", NA, df[[col]])
          }
        }
        
        # Use the stored original geometry instead of trying to extract from potentially corrupted spatialData
        geom <- originalGeometry()
        if (!is.null(geom) && length(geom) == nrow(df)) {
          df <- st_sf(df, geometry = geom)
          spatialData(df)
        } else {
          # Fallback: try to get geometry from current spatialData if available
          current_spatial <- spatialData()
          if (!is.null(current_spatial) && inherits(current_spatial, "sf")) {
            tryCatch({
              current_geom <- st_geometry(current_spatial)
              if (length(current_geom) == nrow(df)) {
                df <- st_sf(df, geometry = current_geom)
                spatialData(df)
              }
            }, error = function(e) {
              warning("Could not reattach geometry: ", e$message)
            })
          }
        }
      }, error = function(e) {
        warning("Error updating spatial data from table: ", e$message)
      })
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      paste("package-", timestamp, ".zip", sep = "")
    },
    content = function(file) {
      # Create a clean temporary directory specifically for this download
      tempDir <- file.path(tempdir(), paste0("package-", format(Sys.time(), "%Y%m%d-%H%M%S")))
      dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
      
      # Simple filenames without timestamps for cleaner structure
      geojsonFile <- file.path(tempDir, "data.geojson")
      shapefileDir <- file.path(tempDir, "shapefile")
      dir.create(shapefileDir, showWarnings = FALSE)
      shpFile <- file.path(shapefileDir, "data.shp")
      gdbFile <- file.path(tempDir, "data.gdb")
      sourceTableFile <- file.path(tempDir, "source_table.csv")
      validationFile <- file.path(tempDir, "validation_report.csv")
      
      # Define the list of standardized fields (required and optional)
      standardized_fields <- c(
        "WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG",
        "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "INT_CRIT", "MARINE",
        "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE",
        "NO_TK_AREA", "STATUS", "STATUS_YR", "GOV_TYPE", "OWN_TYPE",
        "MANG_AUTH", "MANG_PLAN", "CONS_OBJ", "SUPP_INFO", "VERIF",
        "RESTRICT", "METADATAID", "SUB_LOC", "PARENT_ISO3", "ISO3"
      )
      
      # Copy the spatial data
      df <- spatialData()
      
      # Check if df is not NULL
      if (is.null(df)) {
        showModal(modalDialog(
          title = "Error",
          "No spatial data available. Please upload and map your data first.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      
      # Create a working copy for processing
      working_df <- df
      
      # Ensure all standardized fields exist in the working_df
      for (field in standardized_fields) {
        if (!(field %in% names(working_df))) {
          # Add missing fields with appropriate default values
          if (field %in% UNEP_WCMC_FIELDS) {
            # UNEP-WCMC fields get default values
            if (field %in% c("WDPAID", "GIS_M_AREA", "GIS_AREA", "METADATAID")) {
              working_df[[field]] <- NA_real_
            } else {
              working_df[[field]] <- "To be assigned by UNEP-WCMC"
            }
          } else {
            # Other fields get NA values
            if (field %in% c("REP_M_AREA", "REP_AREA", "NO_TK_AREA", "STATUS_YR")) {
              working_df[[field]] <- NA_real_
            } else {
              working_df[[field]] <- NA_character_
            }
          }
        }
      }
      
      # Subset to only include standardized fields and geometry (exclude unmapped fields)
      standardized_fields_with_geom <- c(standardized_fields, "geometry")
      fields_to_keep <- intersect(standardized_fields_with_geom, names(working_df))
      working_df <- working_df[, fields_to_keep]
      
      # If geometry is missing, we can't create spatial files
      if (!("geometry" %in% names(working_df))) {
        showModal(modalDialog(
          title = "Error",
          "Geometry column is missing from the data. Cannot create spatial files.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      
      # Define required fields for validation (excluding UNEP-WCMC fields)
      required_fields <- setdiff(c(
        "WDPAID", "WDPA_PID", "PA_DEF", "NAME", "ORIG_NAME", "DESIG",
        "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA",
        "REP_AREA", "GIS_AREA", "STATUS", "STATUS_YR", "VERIF",
        "METADATAID", "PARENT_ISO3", "ISO3"
      ), UNEP_WCMC_FIELDS)
      
      # Create validation report
      tryCatch({
        validation_res <- validate_data(working_df %>% st_drop_geometry(), required_fields)
        write.csv(validation_res, validationFile, row.names = FALSE)
      }, error = function(e) {
        # If validation fails, create a simple error report
        write.csv(data.frame(Error = paste("Validation error:", e$message)), 
                  validationFile, row.names = FALSE)
      })
      
      # Write spatial files - use tryCatch to handle errors
      tryCatch({
        st_write(working_df, geojsonFile, driver = "GeoJSON", quiet = TRUE, append = FALSE)
      }, error = function(e) {
        # Create a simple file noting the error
        writeLines(paste("Error creating GeoJSON:", e$message), 
                   file.path(tempDir, "geojson_error.txt"))
      })
      
      tryCatch({
        st_write(working_df, shpFile, driver = "ESRI Shapefile", quiet = TRUE, append = FALSE)
      }, error = function(e) {
        # Create a simple file noting the error
        writeLines(paste("Error creating Shapefile:", e$message), 
                   file.path(tempDir, "shapefile_error.txt"))
      })
      
      # Write to File Geodatabase (.gdb)
      tryCatch({
        # Check if GDAL supports FileGDB driver
        gdal_drivers <- sf::st_drivers()
        if ("FileGDB" %in% gdal_drivers$name) {
          st_write(working_df, gdbFile, driver = "FileGDB", layer = "protected_areas", quiet = TRUE, append = FALSE)
        } else if ("OpenFileGDB" %in% gdal_drivers$name) {
          # Fallback to OpenFileGDB driver if FileGDB is not available
          st_write(working_df, gdbFile, driver = "OpenFileGDB", layer = "protected_areas", quiet = TRUE, append = FALSE)
        } else {
          # If no GDB drivers available, create an info file
          writeLines(paste("File Geodatabase export not available.",
                           "GDAL FileGDB or OpenFileGDB driver not found.",
                           "Available drivers:", paste(gdal_drivers$name, collapse = ", ")), 
                     file.path(tempDir, "gdb_not_available.txt"))
        }
      }, error = function(e) {
        # Create a simple file noting the error
        writeLines(paste("Error creating File Geodatabase:", e$message,
                         "\nThis may be due to GDAL configuration or missing drivers."), 
                   file.path(tempDir, "gdb_error.txt"))
      })
      
      # Write metadata with better filename
      tryCatch({
        metadataList <- reactiveValuesToList(metadata)
        metadataDf <- data.frame(
          MetadataElement = names(metadataList),
          Value = unlist(metadataList, use.names = FALSE),
          stringsAsFactors = FALSE
        )
        write.csv(metadataDf, sourceTableFile, row.names = FALSE)
      }, error = function(e) {
        # Create a simple metadata file with error information
        write.csv(data.frame(Error = paste("Source Table error:", e$message)), 
                  sourceTableFile, row.names = FALSE)
      })
      
      # Create a README file explaining UNEP-WCMC fields
      readme_file <- file.path(tempDir, "README.txt")
      readme_content <- paste(
        "WDPA/OECM Data Package",
        "======================",
        "",
        "This package contains your prepared data for submission to Protected Planet.",
        "",
        "Files included:",
        "- data.geojson: Your spatial data in GeoJSON format",
        "- shapefile/data.shp (and related files): Your spatial data in Shapefile format",
        "- data.gdb: Your spatial data in ESRI File Geodatabase format (if available)",
        "- source_table.csv: Source table with dataset metadata",
        "- validation_report.csv: Data validation results",
        "- README.txt: This file",
        "",
        "UNEP-WCMC Assigned Fields:",
        "The following fields have been automatically populated and will be",
        "assigned final values by UNEP-WCMC during data processing:",
        paste("- ", UNEP_WCMC_FIELDS, collapse = "\n"),
        "",
        "Please review the validation report for any errors that need to be addressed",
        "before submitting your data to Protected Planet.",
        "",
        "For more information, visit: https://protectedplanet.net",
        sep = "\n"
      )
      writeLines(readme_content, readme_file)
      
      # Get list of all files in temp directory (includes all related shapefile files like .dbf, .shx, etc.)
      all_files <- list.files(tempDir, full.names = TRUE)
      
      # Create zip file with all files
      setwd(tempDir)  # Change working directory to temp folder 
      files_to_zip <- list.files(tempDir, full.names = FALSE)  # Get relative paths for cleaner zip structure
      zip(file, files = files_to_zip)
      
      # Clean up - remove the temporary directory after creating the zip
      unlink(tempDir, recursive = TRUE)
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
