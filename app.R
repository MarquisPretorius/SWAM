options(shiny.sanitize.errors = FALSE)

# Load required libraries
library(spdep)       # Loaded first to prevent namespace conflict with bslib
library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(plotly)
library(DT)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(fastDummies)
library(mgm)
library(qgraph)
library(igraph)
library(visNetwork)
library(htmlwidgets)
library(htmltools)

# Explicitly assign bslib card elements to resolve conflicts with spdep::card
card <- bslib::card
card_header <- bslib::card_header
card_body <- bslib::card_body

# System file paths
DATA_FILE <- "full_AIARMS_df.csv"
SHAPEFILE <- "gadm41_ZAF_4.shp"

find_app_file <- function(filename) {
  if (file.exists(filename)) return(filename)
  cwd_path <- file.path(getwd(), filename)
  if (file.exists(cwd_path)) return(cwd_path)
  base_path <- file.path(getwd(), basename(filename))
  if (file.exists(base_path)) return(base_path)
  fallback_path <- file.path("C:/Users/Marquis/Desktop/Honours Research", filename)
  if (file.exists(fallback_path)) return(fallback_path)
  return(NULL)
}

# --- ANALYSIS ARTEFACTS FOR THE SPATIAL & MGM EXPLORER SECTIONS ---
# These two sections read objects produced by the analysis pipeline rather than
# recomputing them from the CSV. Located with the same find_app_file() helper
# the rest of the app uses, so no new path convention is introduced.
#
#   mgm_spatial_core.R          tangled from spatial_autocorrelation_MGM.Rmd
#   mgm_spatial_bundle.rds      its exported results
#   output/AIARMS_mgm_spatial.rds   built by 01_clean_AIARMS.R + 01b_add_spatial.R
#
# If any is absent the corresponding section shows a status message and the
# rest of the dashboard is unaffected.

SPATIAL_CORE   <- "mgm_spatial_core.R"
SPATIAL_BUNDLE <- "mgm_spatial_bundle.rds"
MGM_OBJECT     <- file.path("output", "AIARMS_mgm_spatial.rds")

SP_OK <- FALSE; SPB <- NULL
local({
  core_path   <- find_app_file(SPATIAL_CORE)
  bundle_path <- find_app_file(SPATIAL_BUNDLE)
  if (!is.null(core_path) && !is.null(bundle_path)) {
    ok <- tryCatch({
      source(core_path, local = FALSE)
      SPB  <<- readRDS(bundle_path)
      SP_OK <<- all(c("X", "meta", "coords", "lonlat", "cov_vars",
                      "sp_vars", "global", "lee", "k") %in% names(SPB))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) SP_OK <<- FALSE
  }
})

MGM_OK <- FALSE; AIARMS_OBJ <- NULL; MGM_REG <- NULL
local({
  obj_path <- find_app_file(MGM_OBJECT)
  if (!is.null(obj_path)) {
    tryCatch({
      AIARMS_OBJ <<- readRDS(obj_path)
      MGM_REG    <<- AIARMS_OBJ$registry
      MGM_OK     <<- TRUE
    }, error = function(e) MGM_OK <<- FALSE)
  }
})
MGM_HAS_SPATIAL <- MGM_OK && !is.null(AIARMS_OBJ$coords)

# Length-safe default. An eventReactive with ignoreNULL = FALSE runs at
# start-up, and a sidebar input on a nav_panel that has not been opened yet can
# still be NULL or zero-length at that instant. Passing that straight into a
# model call is what produced "Error in : : argument of length 0".
`%|z|%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Palettes. Named apart from anything already in the app so nothing is masked.
PAL_LISA <- c("High-High" = "#FF4D4D", "Low-Low"  = "#4CC9F0",
              "High-Low"  = "#F4A582", "Low-High" = "#92C5DE")
PAL_NS   <- "#6C7A93"
PAL_DIV  <- colorRampPalette(c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B"))
PAL_MGM  <- c("Carriage" = "#B2182B", "Demographic" = "#EF8A62",
              "Socioeconomic" = "#FDDBC7", "WASH" = "#67A9CF",
              "Health" = "#2166AC", "Antibiotic" = "#7B3294",
              "Food/animal" = "#5AAE61", "Spatial" = "#1B7837")

# Base-R plots are drawn on white cards (the .mgm-card class already defined in
# the theme), so they use the app's light-panel convention rather than fighting
# the dark background.
sp_theme_dark <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#1C2541", color = NA),
      panel.background = ggplot2::element_rect(fill = "#1C2541", color = NA),
      text             = ggplot2::element_text(color = "#E0E6ED"),
      axis.text        = ggplot2::element_text(color = "#E0E6ED")
    )
  )
}

# --- MGM & DATA PREPROCESSING HELPER FUNCTIONS ---
clean_str <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  return(x)
}

dedupe_dataframe <- function(df) {
  colnames(df) <- make.unique(colnames(df), sep = "_")
  return(df)
}

create_dummies_from_text <- function(df, col_name) {
  if (is.null(col_name) || is.na(col_name) || !col_name %in% colnames(df)) {
    return(list(df = df, items = character(0)))
  }
  
  raw_vals <- as.character(df[[col_name]])
  clean_items <- df %>%
    mutate(cleaned = str_squish(gsub("\\s*,\\s*", ",", .data[[col_name]]))) %>%
    filter(!cleaned %in% c("None", NA, "", "NA")) %>%
    separate_rows(cleaned, sep = "[,/]") %>%
    pull(cleaned) %>%
    unique() %>%
    sort()
  
  cleaned_items_named <- clean_str(clean_items)
  cleaned_items_named <- cleaned_items_named[!is.na(cleaned_items_named) & cleaned_items_named != ""]
  
  for (i in seq_along(clean_items)) {
    orig_item <- clean_items[i]
    new_col_name <- cleaned_items_named[i]
    if (!is.na(new_col_name) && new_col_name != "") {
      df[[new_col_name]] <- as.numeric(str_detect(replace_na(raw_vals, ""), fixed(orig_item)))
      df[[new_col_name]][is.na(df[[new_col_name]])] <- 0
    }
  }
  df <- dedupe_dataframe(df)
  return(list(df = df, items = unique(cleaned_items_named)))
}

make_clean_label <- function(x) {
  lbls <- gsub("^(Sex_|Education_Level_|Income_Category_|Water_Source_|Toilet_Type_|study_code_)", "", x)
  lbls <- gsub("Home_flush_pour_flush_toilet_to_main_sewer", "Flush Toilet (Sewer)", lbls)
  lbls <- gsub("Home_flush_pour_flush_toilet_to_septic_tank", "Flush Toilet (Septic)", lbls)
  lbls <- gsub("Home_Pit_latrine_with_slab_and_cover", "Pit Latrine (Slab)", lbls)
  lbls <- gsub("Home_Pit_latrine_with_soil_wood_floor", "Pit Latrine (Open)", lbls)
  lbls <- gsub("distance_to_nearest_clinic_km", "Clinic Distance (km)", lbls)
  lbls <- gsub("Standing_Water", "Standing Water", lbls)
  lbls <- gsub("_+", " ", lbls)
  return(str_squish(lbls))
}

# --- THEME STYLING ---
app_theme <- bs_theme(
  version = 5,
  bg = "#0B132B",
  fg = "#E0E6ED",
  primary = "#4CC9F0",
  secondary = "#3A506B",
  success = "#4AD66D"
)

# --- UI DEFINITION ---
ui <- page_navbar(
  theme = app_theme,
  title = "SWAM",
  window_title = "SWAM - Spatial Wastewater & Antimicrobial Monitor",
  fillable = FALSE,
  header = tagList(
    tags$head(tags$style(HTML("
      body, .bslib-page-navbar { background-color: #0B132B !important; color: #E0E6ED !important; }
      .card:not(.mgm-card), .bslib-card:not(.mgm-card), .well, .accordion-body { 
        background-color: #1C2541 !important; color: #E0E6ED !important; border: 1px solid #3A506B !important; 
      }
      .accordion-button { background-color: #1C2541 !important; color: #4CC9F0 !important; border-bottom: 1px solid #3A506B !important; }
      .accordion-button:not(.collapsed) { background-color: #162447 !important; color: #4CC9F0 !important; }
      .sidebar, .bslib-sidebar-layout > .sidebar { background-color: #162447 !important; border-right: 1px solid #3A506B !important; }
      .nav-tabs .nav-link.active { background-color: #4CC9F0 !important; color: #0B132B !important; font-weight: bold; }
      .nav-tabs .nav-link { color: #A0AEC0 !important; }
      .table, .table td, .table th, .dataTables_wrapper { color: #E0E6ED !important; }
      .form-control, .form-select { background-color: #0B132B !important; color: #E0E6ED !important; border: 1px solid #3A506B !important; }
      .value-box { background-color: #1C2541 !important; border: 1px solid #3A506B !important; }
      .mgm-card { background-color: #FFFFFF !important; color: #1A202C !important; border: 1px solid #CBD5E0 !important; }
      .mgm-card p, .mgm-card h5, .mgm-card div { color: #1A202C !important; }
      .navbar-brand { font-weight: 700 !important; letter-spacing: 0.06em; }
      .swam-subtitle { color: #A0AEC0; font-size: 0.95rem; letter-spacing: 0.02em;
                       padding: 4px 0 10px 4px; }
    "))),
    div(class = "swam-subtitle", "Spatial Wastewater & Antimicrobial Monitor")
  ),
  
  # SECTION 1: INTRODUCTION
  nav_panel(
    "Introduction",
    icon = bsicons::bs_icon("book-half"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Background & Research Context"),
        card_body(
          p("Antimicrobial Resistance (AMR) represents one of the most critical global public health threats. In resource-constrained settings, pathogen transmission dynamics are mediated by clinical exposure, environmental hygiene, socio-demographic vulnerabilities, and spatial proximity to healthcare facilities."),
          p("The AIARMS framework tracks longitudinal biological and socio-environmental indicators across KwaZulu-Natal (KZN), South Africa. This dashboard utilizes interactive spatial mapping and dynamic Mixed Graphical Models (MGMs) to disentangle direct conditional dependencies.")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Core Study Objectives"),
          card_body(
            tags$ul(
              tags$li(tags$b("Longitudinal Surveillance: "), "Track monthly positivity dynamics for Escherichia coli (EC), Klebsiella pneumoniae (KP), and ESBL producers across 27 distinct monthly markers."),
              tags$li(tags$b("Spatial Mapping: "), "Map household spatial distributions against provincial ward boundaries and healthcare access points."),
              tags$li(tags$b("Dynamic Mixed Graphical Models: "), "Estimate conditionally independent dependencies between WASH factors, symptoms, diseases, chronic conditions, site codes, and resistance profiles.")
            )
          )
        ),
        card(
          card_header("Target Pathogens & Markers"),
          card_body(
            tags$ul(
              tags$li(tags$b("Escherichia coli (EC): "), "Indicator pathogen for fecal-oral environmental transmission pathways across 9 sampling months."),
              tags$li(tags$b("Klebsiella pneumoniae (KP): "), "Opportunistic pathogen associated with plasmid-mediated multi-drug resistance acquisition."),
              tags$li(tags$b("ESBL Phenotype: "), "Enzymatic resistance conferring immunity to broad-spectrum beta-lactam antibiotics.")
            )
          )
        )
      )
    )
  ),
  
  # SECTION 2: METHODOLOGY
  nav_panel(
    "Methodology",
    icon = bsicons::bs_icon("gear-wide-connected"),
    navset_card_tab(
      nav_panel(
        "Study Design & Filtering Framework",
        card_body(
          h5("Cohort Structure & Marker / Area / Covariate Stratification"),
          p("The analytical pipeline integrates demographic, clinical, WASH, and biological indicators across KZN household cohorts. Stratification allows focused filtering based on longitudinal target markers, area study codes, and specific environmental covariates extracted via temp_final.")
        )
      ),
      nav_panel(
        "Spatial Epidemiology",
        card_body(
          h5("Spatial Distance Calculations"),
          p("Household GPS coordinates (WGS 84 / EPSG:4326) are projected to evaluate Euclidean proximity to healthcare services:"),
          p("$$d(h, C) = \\min_{c \\in C} \\sqrt{(x_h - x_c)^2 + (y_h - y_c)^2}$$")
        )
      ),
      nav_panel(
        "Mixed Graphical Models (MGM)",
        card_body(
          h5("Regularized Network Estimation"),
          p("Mixed Graphical Models capture direct conditional dependencies between continuous variables (e.g., age, spatial clinic distance) and discrete binary variables (e.g., WASH indicators, 27 longitudinal markers, infections, site codes, and symptoms)."),
          p("Edge selection is penalized using EBIC-LASSO tuning parameter optimization:"),
          p("$$\\text{EBIC}_{\\gamma} = -2 \\ln L + k \\ln n + 2 \\gamma k \\ln p$$")
        )
      )
    )
  ),
  
  # SECTION 3: APPLICATION & DASHBOARD
  nav_panel(
    "Application & Dashboard",
    icon = bsicons::bs_icon("speedometer2"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        accordion(
          open = c("Dataset & Status", "Surveillance & Spatial Filters"),
          accordion_panel(
            "Dataset & Status",
            icon = bsicons::bs_icon("folder-check"),
            verbatimTextOutput("file_status_diag"),
            fileInput("file1", "Manual CSV Upload", accept = c(".csv"))
          ),
          accordion_panel(
            "Surveillance & Spatial Filters",
            icon = bsicons::bs_icon("sliders"),
            selectInput(
              "pathogen", "Primary Target Marker Family:",
              choices = c("E. coli (EC)" = "EC", "K. pneumoniae (KP)" = "KP", "ESBL" = "ESBL"),
              selected = "EC"
            ),
            selectInput(
              "region_filter", "Area / Subsection Filter:",
              choices = c(
                "All Areas / KZN Cohort" = "ALL", "Subsection ARUE" = "ARUE",
                "Subsection ARUO" = "ARUO", "Subsection ARUL" = "ARUL",
                "Subsection ARUT" = "ARUT", "Subsection ARUS" = "ARUS",
                "Subsection ARUU" = "ARUU", "Subsection ARUF" = "ARUF"
              ),
              selected = "ALL"
            ),
            checkboxGroupInput(
              "gender_filter", "Filter Sex / Gender:",
              choices = c("Female" = "F", "Male" = "M"),
              selected = c("F", "M"), inline = TRUE
            ),
            sliderInput("age_range", "Filter Age Range:", min = 18, max = 90, value = c(18, 90)),
            hr(),
            h6("Covariate & Infrastructure Filters"),
            uiOutput("dynamic_covariate_filters")
          )
        )
      ),
      
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Filtered Cohort Size",
          value = textOutput("kpi_total_pts"),
          showcase = bsicons::bs_icon("people-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Pathogen Positivity",
          value = textOutput("kpi_pos_rate"),
          showcase = bsicons::bs_icon("activity"),
          theme = "danger"
        ),
        value_box(
          title = "Active Subsections",
          value = textOutput("kpi_active_subsections"),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme = "info"
        )
      ),
      
      navset_card_tab(
        nav_panel(
          "Interactive Spatial Map",
          icon = bsicons::bs_icon("map"),
          leafletOutput("kzn_map_plot", height = "600px")
        ),
        nav_panel(
          "Pathogen Surveillance",
          icon = bsicons::bs_icon("graph-up-arrow"),
          layout_columns(
            col_widths = c(8, 4),
            plotlyOutput("prevalence_plot", height = "420px"),
            DTOutput("prevalence_table")
          )
        ),
        nav_panel(
          "Subsection Comparison",
          icon = bsicons::bs_icon("bar-chart-steps"),
          plotlyOutput("subsection_plot", height = "450px")
        ),
        nav_panel(
          "Demographic Analysis",
          icon = bsicons::bs_icon("person-bounding-box"),
          layout_columns(
            col_widths = c(4, 8),
            verbatimTextOutput("demo_summary"),
            plotlyOutput("age_dist_plot", height = "420px")
          )
        ),
        nav_panel(
          "MGM Network Analysis",
          icon = bsicons::bs_icon("diagram-3"),
          card(
            class = "mgm-card",
            card_body(
              div(
                style = "margin-bottom: 10px;",
                h5("Interactive Mixed Graphical Model", style = "margin-bottom: 2px; font-weight: bold;"),
                p("Direct conditional dependency network calculated across all covariates and 27 longitudinal markers.")
              ),
              uiOutput("mgm_status_msg"),
              visNetworkOutput("mgm_network_plot", height = "680px")
            )
          )
        ),
        nav_panel(
          "Data Explorer",
          icon = bsicons::bs_icon("table"),
          DTOutput("raw_data_table")
        )
      )
    )
  ),
  

  # SECTION 4: SPATIAL AUTOCORRELATION
  nav_panel(
    "Spatial Autocorrelation",
    icon = bsicons::bs_icon("bullseye"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        accordion(
          open = c("Spatial Weights", "Inference"),
          accordion_panel(
            "Spatial Weights",
            icon = bsicons::bs_icon("diagram-2"),
            selectInput(
              "sp_wtype", "Neighbourhood Definition:",
              choices = c("k nearest neighbours" = "knn",
                          "Distance band"        = "dist",
                          "Inverse distance"     = "idw",
                          "Matrix built in 01b"  = "their"),
              selected = "knn"
            ),
            conditionalPanel(
              "input.sp_wtype == 'knn'",
              sliderInput("sp_k", "k (neighbours):", min = 2, max = 15, value = 8, step = 1)
            ),
            conditionalPanel(
              "input.sp_wtype == 'dist' || input.sp_wtype == 'idw'",
              sliderInput("sp_dband", "Band (metres):", min = 50, max = 1500,
                          value = 400, step = 50)
            ),
            conditionalPanel(
              "input.sp_wtype == 'idw'",
              sliderInput("sp_alpha", "Distance decay exponent:", min = 0.5, max = 3,
                          value = 1, step = 0.5)
            ),
            conditionalPanel(
              "input.sp_wtype != 'their'",
              selectInput("sp_style", "Standardisation:",
                          choices = c("Row (W)" = "W", "Binary (B)" = "B",
                                      "Global (C)" = "C"),
                          selected = "W")
            ),
            checkboxInput("sp_show_spatial",
                          "Include Site / density controls", value = FALSE)
          ),
          accordion_panel(
            "Inference",
            icon = bsicons::bs_icon("shuffle"),
            selectInput("sp_nsim", "Permutation replicates:",
                        choices = c("999 (quick look)" = "999",
                                    "4999" = "4999",
                                    "9999 (report quality)" = "9999"),
                        selected = "9999"),
            actionButton("sp_go", "Recompute", class = "btn-primary", width = "100%")
          )
        )
      ),

      layout_columns(
        fill = FALSE,
        value_box(
          title = "Households (spatial frame)",
          value = textOutput("sp_kpi_n"),
          showcase = bsicons::bs_icon("house-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Covariates Tested",
          value = textOutput("sp_kpi_vars"),
          showcase = bsicons::bs_icon("list-ol"),
          theme = "info"
        ),
        value_box(
          title = "Clustered (q < 0.05)",
          value = textOutput("sp_kpi_sig"),
          showcase = bsicons::bs_icon("bullseye"),
          theme = "danger"
        )
      ),

      uiOutput("sp_status_msg"),

      navset_card_tab(
        nav_panel(
          "Global I and C",
          icon = bsicons::bs_icon("table"),
          card_body(
            p("Moran's I and Geary's C, equations (1) and (2). I > -1/(n-1) means a location tends to be connected to locations with similar values; I < -1/(n-1) means connected locations hold dissimilar values. C < 1 is positive autocorrelation and C > 1 negative, so C moves opposite to I. Both are reported because C is the more sensitive of the two to differences between immediate neighbours while I responds to the broader pattern, so agreement between them is stronger evidence than either alone."),
            layout_columns(
              col_widths = c(5, 7),
              DTOutput("sp_gtab"),
              card(
                class = "mgm-card",
                card_body(plotOutput("sp_gplot", height = "760px"))
              )
            )
          )
        ),
        nav_panel(
          "Local Indicators (LISA)",
          icon = bsicons::bs_icon("geo-fill"),
          card_body(
            layout_columns(
              col_widths = c(4, 3, 5),
              uiOutput("sp_lvar_ui"),
              selectInput("sp_lsig", "Significance:",
                          choices = c("p < 0.05 (unadjusted)" = "p05",
                                      "p < 0.01 (unadjusted)" = "p01",
                                      "q < 0.05 (BH)"         = "q05"),
                          selected = "p05"),
              radioButtons("sp_llayer", "Map layer:", inline = TRUE,
                           choices = c("LISA quadrants" = "lisa",
                                       "Getis-Ord Gi*"  = "gstar",
                                       "Raw values"     = "raw"))
            ),
            leafletOutput("sp_lmap", height = "460px"),
            layout_columns(
              col_widths = c(7, 5),
              card(
                class = "mgm-card",
                card_body(plotOutput("sp_lscatter", height = "400px"))
              ),
              card_body(
                h6("Quadrant counts"),
                tableOutput("sp_ltab"),
                p(tags$b("Interpretation. "), "A significant local statistic indicates only that similar values are grouped together. It does not identify a covariate as the cause of that grouping, so local clustering must not be read as evidence of causation."),
                p(tags$b("Multiple comparisons. "), "No site survives Benjamini-Hochberg across the local tests at any replicate count. That is the ordinary situation for a LISA, so this map is exploratory: it identifies candidate neighbourhoods, it does not confirm them. The global table carries the confirmatory weight."),
                p(tags$b("Binary covariates. "), "At prevalence p, a High-High site contributes (1-p)^2 to I_i and a Low-Low site p^2, so High-High reaches significance more readily. That is a property of the statistic, not a finding.")
              )
            )
          )
        ),
        nav_panel(
          "Between Covariates",
          icon = bsicons::bs_icon("grid-3x3"),
          card_body(
            p(tags$b("Supplementary. "), "Moran's I, Geary's C and the LISA are all univariate: each asks whether one covariate clusters. Lee's L is the bivariate extension, asking whether two covariates cluster in the same places. It is not one of equations (1) to (3), so nothing here should be reported unless a corresponding subsection is added to the methodology first."),
            layout_columns(
              col_widths = c(4, 4, 4),
              radioButtons("sp_lsrc", "Matrix shown:",
                           choices = c("Stored (19,999 permutations)" = "stored",
                                       "Recompute at current settings" = "live"),
                           selected = "stored"),
              checkboxInput("sp_stipple", "Mark q < 0.05 (BH)", TRUE),
              checkboxInput("sp_clust", "Cluster the ordering", TRUE)
            ),
            layout_columns(
              col_widths = c(7, 5),
              card(
                class = "mgm-card",
                card_body(plotOutput("sp_heat", height = "700px", click = "sp_heat_click"))
              ),
              card(
                class = "mgm-card",
                card_body(
                  h6(textOutput("sp_pairname")),
                  plotOutput("sp_pairmap",  height = "300px"),
                  plotOutput("sp_pairnull", height = "300px")
                )
              )
            ),
            DTOutput("sp_leetab")
          )
        ),
        nav_panel(
          "Spatial Weights",
          icon = bsicons::bs_icon("share-fill"),
          card_body(
            p("Every I and C above is conditional on W. This tab is where that dependence is shown rather than assumed. The connectivity graph draws one line per non-zero weight, so it is a picture of the neighbourhood structure the statistics are computed over."),
            layout_columns(
              col_widths = c(6, 6),
              card(
                class = "mgm-card",
                card_body(
                  h6("Connectivity"),
                  plotOutput("sp_wgraph", height = "420px")
                )
              ),
              card_body(
                h6("Neighbours per household"),
                card(class = "mgm-card", card_body(plotOutput("sp_whist", height = "220px"))),
                h6("Summary"),
                verbatimTextOutput("sp_wsummary")
              )
            ),
            h6("Sensitivity of Moran's I to the weights definition"),
            p("Each line is one covariate recomputed under seven neighbourhood definitions with everything else held fixed. A line that stays flat is a finding. A line that swings across E[I] = -1/(n-1) is an artefact of the neighbourhood definition and must not be reported without that caveat."),
            card(
              class = "mgm-card",
              card_body(plotOutput("sp_sens", height = "440px"))
            )
          )
        )
      )
    )
  ),

  # SECTION 5: MGM EXPLORER
  nav_panel(
    "MGM Explorer",
    icon = bsicons::bs_icon("diagram-3-fill"),
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        accordion(
          open = c("Variables", "Estimation"),
          accordion_panel(
            "Variables",
            icon = bsicons::bs_icon("list-check"),
            radioButtons("mgm_preset", NULL,
                         choices = c("Core set" = "core",
                                     "All variables" = "full",
                                     "Pick manually" = "manual"),
                         selected = "core"),
            conditionalPanel(
              "input.mgm_preset == 'manual'",
              uiOutput("mgm_domain_ui"),
              uiOutput("mgm_var_ui")
            )
          ),
          accordion_panel(
            "Estimation",
            icon = bsicons::bs_icon("sliders2"),
            radioButtons("mgm_k", "Interaction order (k):",
                         choices = c("Pairwise (k = 2)" = "2",
                                     "Include 3-way (k = 3)" = "3"),
                         selected = "2"),
            radioButtons("mgm_lamSel", "Select lambda by:",
                         choices = c("EBIC" = "EBIC", "Cross-validation" = "CV"),
                         selected = "EBIC"),
            conditionalPanel(
              "input.mgm_lamSel == 'EBIC'",
              sliderInput("mgm_gamma", "EBIC gamma (higher = sparser):",
                          min = 0, max = 1, value = 0.25, step = 0.25)
            ),
            radioButtons("mgm_rule", "Combine neighbourhoods with:",
                         choices = c("AND (conservative)" = "AND",
                                     "OR (sensitive)" = "OR"),
                         selected = "AND"),
            checkboxInput("mgm_thresh", "Apply beta-min threshold (tau)", TRUE),
            actionButton("mgm_go", "Fit Model", class = "btn-primary", width = "100%"),
            helpText("A 20-node pairwise fit takes a few seconds; k = 3 or CV takes longer.")
          ),
          accordion_panel(
            "Display",
            icon = bsicons::bs_icon("eye"),
            sliderInput("mgm_cut", "Hide edges weaker than:", 0, 0.5, 0, step = 0.01),
            selectInput("mgm_focus", "Highlight neighbourhood of:", choices = c("(none)")),
            selectInput("mgm_layout", "Layout:", choices = c("spring", "circle"),
                        selected = "spring"),
            checkboxInput("mgm_rings", "Show predictability rings", TRUE)
          )
        )
      ),

      layout_columns(
        fill = FALSE,
        value_box(
          title = "Nodes in Model",
          value = textOutput("mgm_kpi_nodes"),
          showcase = bsicons::bs_icon("diagram-3"),
          theme = "primary"
        ),
        value_box(
          title = "Edges Shown",
          value = textOutput("mgm_kpi_edges"),
          showcase = bsicons::bs_icon("share"),
          theme = "success"
        ),
        value_box(
          title = "Observations",
          value = textOutput("mgm_kpi_obs"),
          showcase = bsicons::bs_icon("people"),
          theme = "info"
        )
      ),

      uiOutput("mgm_explorer_status"),

      navset_card_tab(
        nav_panel(
          "Network",
          icon = bsicons::bs_icon("bezier2"),
          card(
            class = "mgm-card",
            card_body(
              plotOutput("mgm_net", height = "700px"),
              verbatimTextOutput("mgm_summary")
            )
          )
        ),
        nav_panel(
          "Edges",
          icon = bsicons::bs_icon("list-ul"),
          DTOutput("mgm_edgetab")
        ),
        nav_panel(
          "Predictability",
          icon = bsicons::bs_icon("bar-chart-fill"),
          card(
            class = "mgm-card",
            card_body(plotOutput("mgm_predplot", height = "520px"))
          ),
          DTOutput("mgm_errtab")
        ),
        nav_panel(
          "Interaction Detail",
          icon = bsicons::bs_icon("zoom-in"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              selectInput("mgm_i1", "Node A", choices = NULL),
              selectInput("mgm_i2", "Node B", choices = NULL)
            ),
            verbatimTextOutput("mgm_intdetail"),
            helpText("For an edge involving a variable with more than two categories, the weight shown in the network is the mean absolute value of several parameters. This tab prints them all.")
          )
        ),
        nav_panel(
          "Map",
          icon = bsicons::bs_icon("pin-map"),
          card_body(
            layout_columns(
              col_widths = c(5, 4, 3),
              selectInput("mgm_mapvar", "Colour households by:", choices = NULL),
              checkboxInput("mgm_mapsize", "Size by local density", TRUE),
              checkboxInput("mgm_maplag", "Show neighbour average instead", FALSE)
            ),
            card(
              class = "mgm-card",
              card_body(plotOutput("mgm_map", height = "560px"))
            ),
            verbatimTextOutput("mgm_moran"),
            helpText("Moran's I is the correlation between a household's value and the average of its k nearest neighbours. Positive means nearby households resemble each other; near zero means location carries no information about this variable. The p-value comes from 499 random permutations of the values across locations.")
          )
        ),
        nav_panel(
          "Variable Dictionary",
          icon = bsicons::bs_icon("journal-text"),
          DTOutput("mgm_dict")
        )
      )
    )
  ),

  # SECTION 6: CONCLUSION
  nav_panel(
    "Conclusion",
    icon = bsicons::bs_icon("check2-circle"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Synthesis of Findings"),
        card_body(
          p("The integrated surveillance framework demonstrates significant interaction between environmental sanitation infrastructure, socio-demographic factors, study sites, and pathogen colonization across all longitudinal sampling months."),
          tags$ul(
            tags$li(tags$b("MGM Network Sparsity: "), "Dynamic regularized network estimation isolates conditionally independent associations, controlling for area-level and temporal covariates."),
            tags$li(tags$b("Spatial Risk Mapping: "), "Household proximity to healthcare access points correlates with distinct pathogen burden profiles across regional subsections.")
          )
        )
      )
    )
  )
)

# --- SERVER DEFINITION ---
server <- function(input, output, session) {
  
  output$file_status_diag <- renderText({
    csv_path <- find_app_file(DATA_FILE)
    shp_path <- find_app_file(SHAPEFILE)
    manual_loaded <- !is.null(input$file1)
    
    paste0(
      "CSV: ", ifelse(manual_loaded, "Manual Upload", ifelse(!is.null(csv_path), "Auto-Loaded", "Missing")), "\n",
      "SHP: ", ifelse(!is.null(shp_path), "Loaded", "Missing"), "\n",
      "SPA: ", ifelse(SP_OK, "Loaded", "Missing"), "\n",
      "MGM: ", ifelse(MGM_OK, "Loaded", "Missing")
    )
  })
  
  df_raw <- reactive({
    data <- NULL
    if (!is.null(input$file1)) {
      data <- tryCatch(read.csv(input$file1$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    } else {
      csv_path <- find_app_file(DATA_FILE)
      if (!is.null(csv_path)) data <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
    }
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    colnames(data) <- trimws(colnames(data))
    nms <- colnames(data)
    find_col <- function(cand) { m <- cand[tolower(cand) %in% tolower(nms)]; if (length(m) > 0) m[1] else NULL }
    
    c_code <- find_col(c("Study_code", "StudyCode", "code"))
    c_no   <- find_col(c("Study_no", "StudyNo", "ID"))
    c_age  <- find_col(c("Age", "age"))
    c_gen  <- find_col(c("Gender", "Sex", "gender"))
    c_long <- find_col(c("Longitude.x", "Longitude", "long", "lng"))
    c_lat  <- find_col(c("Latitude.x", "Latitude", "lat"))
    
    if (!is.null(c_code) && c_code != "Study_code") data <- rename(data, Study_code = !!sym(c_code))
    if (!is.null(c_no)   && c_no   != "Study_no")   data <- rename(data, Study_no = !!sym(c_no))
    if (!is.null(c_age)  && c_age  != "Age")        data <- rename(data, Age = !!sym(c_age))
    if (!is.null(c_gen)  && c_gen  != "Gender")     data <- rename(data, Gender = !!sym(c_gen))
    if (!is.null(c_long) && c_long != "Longitude.x") data <- rename(data, Longitude.x = !!sym(c_long))
    if (!is.null(c_lat)  && c_lat  != "Latitude.x")  data <- rename(data, Latitude.x = !!sym(c_lat))
    
    if ("Age" %in% names(data)) data$Age <- suppressWarnings(as.numeric(as.character(data$Age)))
    if ("Longitude.x" %in% names(data)) data$Longitude.x <- suppressWarnings(as.numeric(as.character(data$Longitude.x)))
    if ("Latitude.x" %in% names(data)) data$Latitude.x <- suppressWarnings(as.numeric(as.character(data$Latitude.x)))
    data
  })
  
  df_processed <- reactive({
    data <- df_raw()
    if (is.null(data)) return(NULL)
    
    if ("Study_code" %in% names(data)) {
      data <- data %>% mutate(Sub_Section = substr(as.character(Study_code), 1, 4))
    } else {
      data$Sub_Section <- "Unknown"
    }
    
    st_water_col <- grep("standing.water|Standing_Water|126", colnames(data), value = TRUE, ignore.case = TRUE)[1]
    edu_col      <- grep("Education|Education_Level|52..A02", colnames(data), value = TRUE, ignore.case = TRUE)[1]
    inc_col      <- grep("Income|Income_Category|61..B02", colnames(data), value = TRUE, ignore.case = TRUE)[1]
    wat_col      <- grep("Water|Water_Source|112..NEWD09a", colnames(data), value = TRUE, ignore.case = TRUE)[1]
    toi_col      <- grep("Toilet|Toilet_Type|118..D010a", colnames(data), value = TRUE, ignore.case = TRUE)[1]
    
    if (!is.na(wat_col)) data$Water_Source_Filter <- as.character(data[[wat_col]]) else data$Water_Source_Filter <- "All"
    if (!is.na(toi_col)) data$Toilet_Type_Filter <- as.character(data[[toi_col]]) else data$Toilet_Type_Filter <- "All"
    if (!is.na(edu_col)) data$Education_Filter <- as.character(data[[edu_col]]) else data$Education_Filter <- "All"
    if (!is.na(inc_col)) data$Income_Filter <- as.character(data[[inc_col]]) else data$Income_Filter <- "All"
    if (!is.na(st_water_col)) {
      data$Standing_Water_Filter <- ifelse(grepl("Yes|1|True", as.character(data[[st_water_col]]), ignore.case = TRUE), "Yes", "No")
    } else {
      data$Standing_Water_Filter <- "All"
    }
    
    data
  })
  
  output$dynamic_covariate_filters <- renderUI({
    data <- df_processed()
    if (is.null(data)) {
      return(p("Upload or load dataset to view filters.", style = "font-size: 0.85rem; color: #A0AEC0;"))
    }
    
    w_opts <- c("All", sort(unique(na.omit(data$Water_Source_Filter))))
    t_opts <- c("All", sort(unique(na.omit(data$Toilet_Type_Filter))))
    sw_opts <- c("All", sort(unique(na.omit(data$Standing_Water_Filter))))
    e_opts <- c("All", sort(unique(na.omit(data$Education_Filter))))
    i_opts <- c("All", sort(unique(na.omit(data$Income_Filter))))
    
    tagList(
      selectInput("cov_water", "Water Access:", choices = w_opts, selected = "All"),
      selectInput("cov_toilet", "Toilet Facility:", choices = t_opts, selected = "All"),
      selectInput("cov_swater", "Standing Water Exposure:", choices = sw_opts, selected = "All"),
      selectInput("cov_edu", "Education Level:", choices = e_opts, selected = "All"),
      selectInput("cov_inc", "Income Category:", choices = i_opts, selected = "All")
    )
  })
  
  df_filtered <- reactive({
    data <- df_processed()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    req(input$gender_filter, input$age_range, input$region_filter)
    
    filtered <- data
    if ("Gender" %in% names(filtered)) filtered <- filtered %>% filter(Gender %in% input$gender_filter)
    if ("Age" %in% names(filtered)) filtered <- filtered %>% filter(!is.na(Age) & Age >= input$age_range[1] & Age <= input$age_range[2])
    if (input$region_filter != "ALL" && "Sub_Section" %in% names(filtered)) filtered <- filtered %>% filter(Sub_Section == input$region_filter)
    
    if (!is.null(input$cov_water) && input$cov_water != "All" && "Water_Source_Filter" %in% names(filtered)) {
      filtered <- filtered %>% filter(Water_Source_Filter == input$cov_water)
    }
    if (!is.null(input$cov_toilet) && input$cov_toilet != "All" && "Toilet_Type_Filter" %in% names(filtered)) {
      filtered <- filtered %>% filter(Toilet_Type_Filter == input$cov_toilet)
    }
    if (!is.null(input$cov_swater) && input$cov_swater != "All" && "Standing_Water_Filter" %in% names(filtered)) {
      filtered <- filtered %>% filter(Standing_Water_Filter == input$cov_swater)
    }
    if (!is.null(input$cov_edu) && input$cov_edu != "All" && "Education_Filter" %in% names(filtered)) {
      filtered <- filtered %>% filter(Education_Filter == input$cov_edu)
    }
    if (!is.null(input$cov_inc) && input$cov_inc != "All" && "Income_Filter" %in% names(filtered)) {
      filtered <- filtered %>% filter(Income_Filter == input$cov_inc)
    }
    
    filtered
  })
  
  df_pathogen <- reactive({
    data <- df_filtered()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    prefix <- paste0(input$pathogen, "_")
    target_cols <- grep(paste0("^", prefix), names(data), value = TRUE, ignore.case = TRUE)
    if (length(target_cols) == 0) return(NULL)
    meta_cols <- intersect(c("Study_no", "Study_code", "Sub_Section", "Age", "Gender"), names(data))
    
    data %>%
      select(all_of(meta_cols), all_of(target_cols)) %>%
      pivot_longer(cols = all_of(target_cols), names_to = "Month", values_to = "Result") %>%
      mutate(
        Month = sub(paste0("^", prefix), "", Month, ignore.case = TRUE),
        Result_Clean = trimws(as.character(Result)),
        Is_Neg = grepl("Neg|No Growth|0", Result_Clean, ignore.case = TRUE),
        Is_Pos = grepl("Pos|EC|KP|ESBL|E.coli|E. coli", Result_Clean, ignore.case = TRUE) & !Is_Neg,
        Result_Cat = ifelse(is.na(Result_Clean) | Result_Clean == "" | Result_Clean == "NA", "Missing/NA",
                            ifelse(Is_Pos, "Positive", "Negative"))
      )
  })
  
  df_participant_status <- reactive({
    data <- df_filtered()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    prefix <- paste0(input$pathogen, "_")
    target_cols <- grep(paste0("^", prefix), names(data), value = TRUE, ignore.case = TRUE)
    if (length(target_cols) == 0) return(NULL)
    group_cols <- intersect(c("Study_no", "Study_code", "Sub_Section", "Age", "Gender", "Longitude.x", "Latitude.x"), names(data))
    
    data %>%
      pivot_longer(cols = all_of(target_cols), names_to = "Month", values_to = "Result") %>%
      mutate(
        Result_Clean = trimws(as.character(Result)),
        Is_Neg = grepl("Neg|No Growth|0", Result_Clean, ignore.case = TRUE),
        Is_Pos = grepl("Pos|EC|KP|ESBL|E.coli|E. coli", Result_Clean, ignore.case = TRUE) & !Is_Neg
      ) %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        Total_Tested = sum(!is.na(Result_Clean) & Result_Clean != "" & Result_Clean != "NA", na.rm = TRUE),
        Pos_Count = sum(Is_Pos, na.rm = TRUE),
        Marker_Status = case_when(
          Pos_Count > 0 ~ paste(input$pathogen, "Positive"),
          Total_Tested > 0 ~ paste(input$pathogen, "Negative"),
          TRUE ~ "Missing"
        ),
        .groups = "drop"
      )
  })
  
  output$kpi_total_pts <- renderText({
    data <- df_filtered()
    if (is.null(data)) "0" else format(nrow(data), big.mark = ",")
  })
  
  output$kpi_pos_rate <- renderText({
    p_data <- df_pathogen()
    if (is.null(p_data)) return("0.0%")
    total <- sum(p_data$Result_Cat != "Missing/NA", na.rm = TRUE)
    pos <- sum(p_data$Is_Pos, na.rm = TRUE)
    if (total == 0) "0.0%" else paste0(round((pos / total) * 100, 1), "%")
  })
  
  output$kpi_active_subsections <- renderText({
    data <- df_filtered()
    if (is.null(data) || !"Sub_Section" %in% names(data)) "0" else as.character(length(unique(data$Sub_Section)))
  })
  
  output$kzn_map_plot <- renderLeaflet({
    pts <- df_participant_status()
    req(pts, "Longitude.x" %in% names(pts), "Latitude.x" %in% names(pts))
    pts_clean <- pts %>% filter(!is.na(Longitude.x) & !is.na(Latitude.x))
    req(nrow(pts_clean) > 0)
    
    pal <- colorFactor(
      palette = c("#FF4D4D", "#4CC9F0", "grey70"),
      domain = c(paste(input$pathogen, "Positive"), paste(input$pathogen, "Negative"), "Missing")
    )
    
    leaflet(pts_clean) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~Longitude.x, lat = ~Latitude.x,
        color = ~pal(Marker_Status),
        radius = 6, fillOpacity = 0.85, stroke = FALSE,
        popup = ~paste0("<b>Participant: </b>", Study_no, "<br><b>Subsection: </b>", Sub_Section, "<br><b>Status: </b>", Marker_Status)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~Marker_Status, title = "Marker Status", opacity = 1)
  })
  
  output$prevalence_plot <- renderPlotly({
    p_data <- df_pathogen()
    req(p_data)
    
    summary_df <- p_data %>%
      group_by(Month) %>%
      summarise(
        Total = sum(Result_Cat != "Missing/NA", na.rm = TRUE),
        Pos = sum(Is_Pos, na.rm = TRUE),
        Rate = ifelse(Total > 0, round((Pos / Total) * 100, 1), 0),
        .groups = "drop"
      )
    
    p <- ggplot(summary_df, aes(x = Month, y = Rate, group = 1, text = paste0("Month: ", Month, "<br>Rate: ", Rate, "%"))) +
      geom_line(color = "#4CC9F0", linewidth = 1) +
      geom_point(color = "#4CC9F0", size = 2.5) +
      labs(title = paste("Monthly Positivity Rate:", input$pathogen), y = "Positivity Rate (%)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1C2541", color = NA),
        panel.background = element_rect(fill = "#1C2541", color = NA),
        text = element_text(color = "#E0E6ED"),
        axis.text = element_text(color = "#E0E6ED")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$prevalence_table <- renderDT({
    p_data <- df_pathogen()
    req(p_data)
    
    tbl <- p_data %>%
      group_by(Month) %>%
      summarise(
        Total_Samples = sum(Result_Cat != "Missing/NA", na.rm = TRUE),
        Positives = sum(Is_Pos, na.rm = TRUE),
        `Positivity Rate` = ifelse(Total_Samples > 0, paste0(round((Positives / Total_Samples) * 100, 1), "%"), "0.0%"),
        .groups = "drop"
      )
    
    datatable(tbl, options = list(dom = 't', pageLength = 12), rownames = FALSE)
  })
  
  output$subsection_plot <- renderPlotly({
    p_data <- df_pathogen()
    req(p_data)
    
    p_sub <- p_data %>%
      filter(Result_Cat != "Missing/NA") %>%
      group_by(Sub_Section) %>%
      summarise(
        Total = n(),
        Positives = sum(Is_Pos, na.rm = TRUE),
        Rate = ifelse(Total > 0, round((Positives / Total) * 100, 1), 0),
        .groups = "drop"
      )
    
    p <- ggplot(p_sub, aes(x = Sub_Section, y = Rate, fill = Sub_Section, text = paste0("Subsection: ", Sub_Section, "<br>Rate: ", Rate, "%"))) +
      geom_col(show.legend = FALSE) +
      scale_fill_viridis_d(option = "mako") +
      labs(title = "Positivity Rate by Subsection", x = "Subsection Code", y = "Rate (%)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1C2541", color = NA),
        panel.background = element_rect(fill = "#1C2541", color = NA),
        text = element_text(color = "#E0E6ED"),
        axis.text = element_text(color = "#E0E6ED")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$demo_summary <- renderPrint({
    data <- df_filtered()
    req(data)
    cat("--- DEMOGRAPHIC SUMMARY ---\n")
    cat("Target Family:", input$pathogen, "\n")
    cat("Area Filter:", input$region_filter, "\n")
    cat("Cohort Sample Size:", nrow(data), "\n\n")
    if ("Age" %in% names(data)) {
      cat("Mean Age:", round(mean(data$Age, na.rm = TRUE), 1), "years\n")
      cat("Median Age:", median(data$Age, na.rm = TRUE), "years\n")
    }
  })
  
  output$age_dist_plot <- renderPlotly({
    data <- df_filtered()
    req(data, "Age" %in% names(data), "Gender" %in% names(data))
    
    p <- ggplot(data, aes(x = Age, fill = Gender)) +
      geom_histogram(binwidth = 5, color = "#1C2541", position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c("F" = "#FF4D4D", "M" = "#4CC9F0")) +
      labs(title = "Age Distribution by Gender", x = "Age (Years)", y = "Count") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1C2541", color = NA),
        panel.background = element_rect(fill = "#1C2541", color = NA),
        text = element_text(color = "#E0E6ED"),
        axis.text = element_text(color = "#E0E6ED")
      )
    
    ggplotly(p)
  })
  
  output$raw_data_table <- renderDT({
    data <- df_filtered()
    req(data)
    datatable(data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # -----------------------------------------------------------------------
  # MGM NETWORK PIPELINE
  # -----------------------------------------------------------------------
  
  output$mgm_status_msg <- renderUI({
    data <- df_raw()
    if (is.null(data) || nrow(data) < 10) {
      return(div(class = "alert alert-warning", style = "color: #856404; background-color: #fff3cd; border-color: #ffeeba; padding: 10px; border-radius: 4px;", "Dataset size is too small to fit MGM model. Please ensure full CSV is loaded."))
    }
    return(NULL)
  })
  
  output$mgm_network_plot <- renderVisNetwork({
    AMR_raw_data <- df_raw()
    req(AMR_raw_data)
    if (nrow(AMR_raw_data) < 10) return(NULL)
    
    AMR_dummies <- dedupe_dataframe(AMR_raw_data)
    
    # 1. Longitudinal Target Biological & Resistance Markers
    marker_cols <- c(
      "EC_March", "EC_May", "EC_June", "EC_July", "EC_August", "EC_September", "EC_October", "EC_November", "EC_December",
      "KP_March", "KP_May", "KP_June", "KP_July", "KP_August", "KP_September", "KP_October", "KP_November", "KP_December",
      "ESBL_March", "ESBL_May", "ESBL_June", "ESBL_July", "ESBL_August", "ESBL_September", "ESBL_October", "ESBL_November", "ESBL_December"
    )
    
    marker_vars <- c()
    for (m_col in marker_cols) {
      if (m_col %in% colnames(AMR_dummies)) {
        if (is.character(AMR_dummies[[m_col]]) || is.factor(AMR_dummies[[m_col]])) {
          res_m <- create_dummies_from_text(AMR_dummies, m_col)
          AMR_dummies <- res_m$df
          marker_vars <- c(marker_vars, res_m$items)
        } else {
          AMR_dummies[[m_col]][is.na(AMR_dummies[[m_col]])] <- 0
          cleaned_m_name <- clean_str(m_col)
          colnames(AMR_dummies)[colnames(AMR_dummies) == m_col] <- cleaned_m_name
          marker_vars <- c(marker_vars, cleaned_m_name)
        }
      }
    }
    marker_vars <- unique(marker_vars[!is.na(marker_vars) & marker_vars != ""])
    
    # 2. Extract numeric features for MGM
    df_mgm_cols <- intersect(c(marker_vars, "Age", "distance_to_nearest_clinic_km"), colnames(AMR_dummies))
    
    if (length(df_mgm_cols) < 2) return(NULL)
    
    df_mgm <- AMR_dummies %>%
      select(all_of(df_mgm_cols)) %>%
      mutate(across(everything(), ~ as.numeric(scale(.))))
    
    # Filter non-zero variance columns
    valid_cols <- sapply(df_mgm, function(x) var(x, na.rm = TRUE) > 0 && !all(is.na(x)))
    df_mgm <- df_mgm[, valid_cols, drop = FALSE]
    df_mgm[is.na(df_mgm)] <- 0
    
    if (ncol(df_mgm) < 2) return(NULL)
    
    # 3. Fit Pairwise MGM with zero EBIC penalty
    type_vec  <- rep("g", ncol(df_mgm))
    level_vec <- rep(1, ncol(df_mgm))
    
    fit_mgm <- mgm(
      data = as.matrix(df_mgm),
      type = type_vec,
      levels = level_vec,
      k = 2,
      lambdaSel = "EBIC",
      ebicGam = 0,
      ruleReg = "OR",
      pbar = FALSE
    )
    
    # 4. Extract adjacency matrix and edge directions
    wadj <- fit_mgm$pairwise$wadj
    signs <- fit_mgm$pairwise$signs
    if (is.null(signs)) signs <- matrix(1, nrow(wadj), ncol(wadj))
    
    colnames(wadj) <- rownames(wadj) <- colnames(df_mgm)
    
    edges_indices <- which(lower.tri(wadj) & wadj > 0, arr.ind = TRUE)
    
    if (nrow(edges_indices) == 0) return(NULL)
    
    edges <- data.frame(
      from = colnames(df_mgm)[edges_indices[, 1]],
      to = colnames(df_mgm)[edges_indices[, 2]],
      weight = wadj[edges_indices],
      sign = signs[edges_indices]
    ) %>%
      mutate(
        width = weight * 5,
        color = ifelse(sign > 0, "#2B7CE9", "#E41A1C"), # Blue = Positive, Red = Negative
        title = paste0("<b>Strength:</b> ", round(weight, 3), 
                       "<br><b>Direction:</b> ", ifelse(sign > 0, "Positive (+)", "Negative (-)"))
      )
    
    # Build node data frame
    node_degrees <- table(c(edges$from, edges$to))
    nodes <- data.frame(id = colnames(df_mgm)) %>%
      mutate(
        label = make_clean_label(id),
        value = as.numeric(node_degrees[id]),
        value = ifelse(is.na(value), 1, value),
        title = paste0("<b>Variable:</b> ", label)
      )
    
    # 5. Render interactive network
    visNetwork(nodes = nodes, edges = edges, width = "100%", height = "650px") %>%
      visNodes(shape = "dot", font = list(size = 16)) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -25),
        stabilization = list(iterations = 100)
      )
  })

  # -----------------------------------------------------------------------
  # SPATIAL AUTOCORRELATION SECTION
  #
  # No statistics are implemented here. Every estimator comes from
  # mgm_spatial_core.R, which is tangled from the analysis Rmd, so this
  # section and the written report cannot disagree.
  # -----------------------------------------------------------------------

  output$sp_status_msg <- renderUI({
    if (SP_OK) return(NULL)
    div(
      class = "alert alert-warning",
      style = "color: #856404; background-color: #fff3cd; border-color: #ffeeba; padding: 12px; border-radius: 4px;",
      tags$b("Spatial analysis objects not found. "),
      "This section needs mgm_spatial_core.R and mgm_spatial_bundle.rds in the app folder. ",
      "The bundle is written by the export chunk of spatial_autocorrelation_MGM.Rmd, ",
      "which in turn needs output/AIARMS_mgm_spatial.rds from 01_clean_AIARMS.R and 01b_add_spatial.R."
    )
  })

  output$sp_kpi_n    <- renderText(if (SP_OK) format(nrow(SPB$X), big.mark = ",") else "-")
  output$sp_kpi_vars <- renderText(if (SP_OK) as.character(length(SPB$cov_vars)) else "-")
  output$sp_kpi_sig  <- renderText({
    if (!SP_OK) return("-")
    g <- sp_global()
    if (is.null(g)) return("-")
    as.character(sum(g$q_BH < 0.05, na.rm = TRUE))
  })

  sp_vars_now <- reactive({
    req(SP_OK)
    if (isTRUE(input$sp_show_spatial)) c(SPB$cov_vars, SPB$sp_vars) else SPB$cov_vars
  })

  sp_wcfg <- reactive({
    req(SP_OK)
    sty <- input$sp_style %|z|% "W"
    switch(input$sp_wtype %|z|% "knn",
      knn  = list(type = "knn",  k = input$sp_k %|z|% SPB$k, style = sty),
      dist = list(type = "dist", d = input$sp_dband %|z|% 400, style = sty),
      idw  = list(type = "idw",  d = input$sp_dband %|z|% 400,
                  alpha = input$sp_alpha %|z|% 1, style = sty),
      their = NULL)
  })

  # Either build the weights, or use the matrix 01b already made.
  sp_Wmat <- reactive({
    req(SP_OK)
    if (identical(input$sp_wtype %|z|% "knn", "their")) SPB$W_01b
    else listw_to_W(do.call(make_listw, c(list(coords = SPB$coords), sp_wcfg())))
  })

  # Geary, the LISA and Gi* need a listw object. 01b's matrix is a plain
  # matrix, so for that option the nearest spdep equivalent stands in.
  sp_lw <- reactive({
    req(SP_OK)
    if (identical(input$sp_wtype %|z|% "knn", "their")) {
      spdep::nb2listw(
        spdep::make.sym.nb(spdep::knn2nb(spdep::knearneigh(SPB$coords, k = SPB$k))),
        style = "W", zero.policy = TRUE)
    } else {
      do.call(make_listw, c(list(coords = SPB$coords), sp_wcfg()))
    }
  })

  sp_nsim <- reactive(as.integer(input$sp_nsim %|z|% "9999"))

  output$sp_lvar_ui <- renderUI({
    if (!SP_OK) return(NULL)
    v <- sp_vars_now()
    grp <- setNames(SPB$meta$group, SPB$meta$var)
    selectInput("sp_lvar", "Covariate:", width = "100%",
                choices = setNames(v, sprintf("%s  (%s)", v, grp[v])),
                selected = SPB$global$variable[1])
  })

  ## ---- global Moran's I and Geary's C ----
  sp_global <- eventReactive(list(input$sp_go, input$sp_show_spatial),
                             ignoreNULL = FALSE, {
    if (!SP_OK) return(NULL)
    withProgress(message = "Global Moran's I and Geary's C", value = 0.4, {
      W <- sp_Wmat(); lw <- sp_lw(); vs <- sp_vars_now()
      grp <- setNames(SPB$meta$group, SPB$meta$var)
      out <- do.call(rbind, lapply(vs, function(v) {
        pm <- perm_moran_W(SPB$X[[v]], W, nsim = sp_nsim(), seed = 1)
        data.frame(variable = v, group = unname(grp[v]),
                   moran_I = pm$statistic, EI = -1 / (nrow(SPB$X) - 1),
                   sd_perm = pm$sd_sim, z_perm = pm$z_sim, p_perm = pm$p_two,
                   geary_C = geary_C(SPB$X[[v]], lw), stringsAsFactors = FALSE)
      }))
      out$q_BH <- stats::p.adjust(out$p_perm, "BH")
      out[order(-out$moran_I), ]
    })
  })

  output$sp_gtab <- renderDT({
    g <- sp_global(); req(g)
    tb <- datatable(g, rownames = FALSE,
                    options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
                    colnames = c("Variable", "Group", "Moran I", "E[I]", "SD perm",
                                 "z perm", "p", "Geary C", "q (BH)"))
    tb <- formatRound(tb, c("moran_I", "EI", "sd_perm", "geary_C"), 3)
    tb <- formatRound(tb, "z_perm", 2)
    formatSignif(tb, c("p_perm", "q_BH"), 3)
  })

  output$sp_gplot <- renderPlot({
    g <- sp_global(); req(g)
    g <- g[order(g$moran_I), ]
    cols <- ifelse(g$q_BH < 0.05, "#B2182B",
            ifelse(g$p_perm < 0.05, "#EF8A62", "grey72"))
    op <- par(mar = c(4, 11, 2, 1)); on.exit(par(op))
    bp <- barplot(g$moran_I, horiz = TRUE, names.arg = g$variable, las = 1,
                  col = cols, border = NA, cex.names = 0.62,
                  xlim = range(g$moran_I - 2 * g$sd_perm, g$moran_I + 2 * g$sd_perm),
                  xlab = "Global Moran's I")
    abline(v = mean(g$EI), lty = 2, col = "grey40")
    segments(g$moran_I - 1.96 * g$sd_perm, bp,
             g$moran_I + 1.96 * g$sd_perm, bp, col = "grey30")
    legend("bottomright", bty = "n", cex = 0.85, border = NA,
           fill = c("#B2182B", "#EF8A62", "grey72"),
           legend = c("q < 0.05 (BH)", "p < 0.05 only", "not significant"))
    title(main = "Dashed line: E[I] = -1/(n-1) under the null", cex.main = 0.9,
          font.main = 1, col.main = "grey30")
  })

  ## ---- local indicators of spatial association ----
  sp_lisa <- reactive({
    req(SP_OK, input$sp_lvar)
    v <- input$sp_lvar; lw <- sp_lw()
    li <- local_moran_perm(SPB$X[[v]], lw, nsim = sp_nsim(), seed = 3)
    li$sig <- switch(input$sp_lsig %|z|% "p05",
                     q05 = li$q_BH < 0.05,
                     p05 = li$p     < 0.05,
                     p01 = li$p     < 0.01)
    li$sig[is.na(li$sig)] <- FALSE
    gs <- try(getis_gstar(SPB$X[[v]], lw, nsim = min(sp_nsim(), 999), seed = 3),
              silent = TRUE)
    list(li = li, gs = if (inherits(gs, "try-error")) NULL else gs,
         I = moran_I(SPB$X[[v]], lw))
  })

  output$sp_lmap <- renderLeaflet({
    req(SP_OK)
    L <- sp_lisa(); li <- L$li; v <- input$sp_lvar
    dd <- data.frame(lon = SPB$lonlat[, 1], lat = SPB$lonlat[, 2],
                     site = SPB$site_group)
    if (identical(input$sp_llayer, "gstar") && !is.null(L$gs)) {
      gg  <- L$gs
      pal <- colorNumeric(PAL_DIV(64),
                          domain = c(-max(abs(gg$Gstar)), max(abs(gg$Gstar))))
      cols <- pal(gg$Gstar); opac <- ifelse(gg$p < 0.05, 0.95, 0.35)
      lab  <- sprintf("<b>Site: </b>%s<br><b>Gi*: </b>%.2f<br><b>p: </b>%.3f",
                      dd$site, gg$Gstar, gg$p)
    } else if (identical(input$sp_llayer, "raw")) {
      x   <- SPB$X[[v]]
      pal <- colorNumeric(PAL_DIV(64), domain = range(x))
      cols <- pal(x); opac <- 0.9
      lab  <- sprintf("<b>Site: </b>%s<br><b>%s: </b>%s", dd$site, v,
                      format(x, digits = 3))
    } else {
      cols <- ifelse(li$sig, unname(PAL_LISA[li$quadrant]), PAL_NS)
      opac <- ifelse(li$sig, 0.95, 0.45)
      lab  <- sprintf("<b>Site: </b>%s<br><b>Quadrant: </b>%s<br><b>I_i: </b>%.2f<br><b>p: </b>%.3f<br><b>q: </b>%.3f",
                      dd$site, li$quadrant, li$Ii, li$p, li$q_BH)
    }
    m <- leaflet(dd) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = 6, stroke = TRUE,
                       weight = 1, color = "#0B132B", fillColor = cols,
                       fillOpacity = opac, popup = lab)
    if (identical(input$sp_llayer, "lisa")) {
      m <- m %>% addLegend("bottomright", colors = c(unname(PAL_LISA), PAL_NS),
                           labels = c(names(PAL_LISA), "not significant"),
                           opacity = 1, title = v)
    }
    m
  })

  output$sp_lscatter <- renderPlot({
    req(SP_OK)
    L <- sp_lisa(); li <- L$li
    op <- par(mar = c(4, 4, 3, 1)); on.exit(par(op))
    plot(li$Zc, li$lag_Zc, pch = 21, cex = 1.2,
         bg  = ifelse(li$sig, unname(PAL_LISA[li$quadrant]), "white"),
         col = unname(PAL_LISA[li$quadrant]),
         xlab = expression(Z(s[i]) - bar(Z)),
         ylab = expression(sum(w[ij] * (Z(s[j]) - bar(Z)), j, )),
         main = paste("Moran scatterplot:", input$sp_lvar))
    abline(h = 0, v = 0, col = "grey75")
    abline(a = 0, b = L$I, col = "#B2182B", lwd = 2)
    legend("topleft", bty = "n", cex = 0.8, legend = names(PAL_LISA), pch = 21,
           col = unname(PAL_LISA), pt.bg = unname(PAL_LISA))
    mtext(sprintf("fitted slope = global I = %.3f", L$I), side = 3, line = -1.3,
          adj = 0.98, cex = 0.85, col = "#B2182B")
  })

  output$sp_ltab <- renderTable({
    req(SP_OK)
    li <- sp_lisa()$li
    tb <- as.data.frame.matrix(
      table(li$quadrant, ifelse(li$sig, "significant", "not significant")))
    cbind(Quadrant = rownames(tb), tb)
  })

  ## ---- Lee's L between covariates (supplementary) ----
  sp_leemat <- eventReactive(list(input$sp_go, input$sp_lsrc),
                             ignoreNULL = FALSE, {
    if (!SP_OK) return(NULL)
    if (identical(input$sp_lsrc, "stored")) return(SPB$lee)
    withProgress(message = "Lee's L across all covariate pairs", value = 0.5, {
      lee_matrix_fast(SPB$X, SPB$coords, SPB$cov_vars,
                      wcfg = if (is.null(sp_wcfg()))
                               list(type = "knn", k = SPB$k, style = "W")
                             else sp_wcfg(),
                      nsim = sp_nsim(), block = 1000, seed = 1)
    })
  })

  sp_heat_order <- reactive({
    LM <- sp_leemat(); req(LM)
    if (!isTRUE(input$sp_clust)) return(seq_len(ncol(LM$L)))
    stats::hclust(stats::as.dist(1 - LM$L / max(abs(LM$L), na.rm = TRUE)))$order
  })

  output$sp_heat <- renderPlot({
    LM <- sp_leemat(); req(LM)
    ord <- sp_heat_order()
    Lo <- LM$L[ord, ord]; Po <- LM$P[ord, ord]
    lim <- max(abs(LM$L), na.rm = TRUE)
    op <- par(mar = c(10, 10, 2, 1)); on.exit(par(op))
    image(seq_len(ncol(Lo)), seq_len(nrow(Lo)), t(Lo),
          col = PAL_DIV(64), breaks = seq(-lim, lim, length.out = 65),
          axes = FALSE, xlab = "", ylab = "")
    axis(1, seq_len(ncol(Lo)), colnames(Lo), las = 2, cex.axis = 0.56, tick = FALSE)
    axis(2, seq_len(nrow(Lo)), rownames(Lo), las = 2, cex.axis = 0.56, tick = FALSE)
    if (isTRUE(input$sp_stipple)) {
      qm <- bh_matrix(Po)
      ij <- which(!is.na(qm) & qm < 0.05, arr.ind = TRUE)
      if (nrow(ij)) points(ij[, 2], ij[, 1], pch = 20, cex = 0.5)
    }
    box(col = "grey60")
    title("Lee's L between covariates", cex.main = 1)
    mtext("click a cell for the pair detail", side = 3, line = -0.6,
          adj = 1, cex = 0.75, col = "grey40")
  })

  sp_sel <- reactiveVal(NULL)
  observeEvent(input$sp_heat_click, {
    LM <- sp_leemat(); req(LM)
    nm <- colnames(LM$L)[sp_heat_order()]
    i <- round(input$sp_heat_click$y); j <- round(input$sp_heat_click$x)
    if (i >= 1 && i <= length(nm) && j >= 1 && j <= length(nm) && i != j)
      sp_sel(c(nm[i], nm[j]))
  })

  sp_pair <- reactive({
    LM <- sp_leemat(); req(LM)
    s <- sp_sel()
    if (is.null(s)) {
      ut <- upper.tri(LM$L); k <- which.min(replace(LM$P, !ut, NA))
      s  <- c(rownames(LM$L)[row(LM$L)[k]], colnames(LM$L)[col(LM$L)[k]])
    }
    W <- sp_Wmat()
    list(v = s, W = W,
         local = local_lee(SPB$X[[s[1]]], SPB$X[[s[2]]], sp_lw()),
         perm  = perm_lee_W(SPB$X[[s[1]]], SPB$X[[s[2]]], W,
                            nsim = sp_nsim(), seed = 5, joint = TRUE),
         r = stats::cor(SPB$X[[s[1]]], SPB$X[[s[2]]]))
  })

  output$sp_pairname <- renderText({
    p <- sp_pair(); sprintf("%s  vs  %s", p$v[1], p$v[2])
  })

  output$sp_pairmap <- renderPlot({
    p <- sp_pair(); ll <- p$local
    lim <- max(abs(ll), na.rm = TRUE)
    cols <- PAL_DIV(64)[cut(ll, seq(-lim, lim, length.out = 65),
                            include.lowest = TRUE)]
    op <- par(mar = c(4, 4, 2, 1)); on.exit(par(op))
    plot(SPB$lonlat[, 1], SPB$lonlat[, 2],
         asp = 1 / cos(mean(SPB$lonlat[, 2]) * pi / 180),
         pch = 21, cex = 1.3, bg = cols, col = "grey35",
         xlab = "Longitude", ylab = "Latitude")
    title(sprintf("Local Lee's L  (global L = %.3f)", p$perm$statistic),
          cex.main = 1)
  })

  output$sp_pairnull <- renderPlot({
    p <- sp_pair(); n <- nrow(SPB$X)
    A_ <- sum(p$W^2); Bd <- sum(rowSums(p$W)^2)
    theory <- p$r * (n * A_ - Bd) / (Bd * (n - 1))
    op <- par(mar = c(4, 4, 3, 1)); on.exit(par(op))
    hist(p$perm$sim, breaks = 40, col = "grey88", border = "white",
         main = "Joint-permutation null",
         xlab = "Lee's L", xlim = range(p$perm$sim, p$perm$statistic, theory))
    abline(v = theory, col = "#2166AC", lwd = 2, lty = 2)
    abline(v = p$perm$statistic, col = "#B2182B", lwd = 2)
    legend("topright", bty = "n", cex = 0.75, lwd = 2, lty = c(1, 2),
           col = c("#B2182B", "#2166AC"),
           legend = c(sprintf("observed L = %.3f", p$perm$statistic),
                      sprintf("E[L] = r(nA-B)/(B(n-1)) = %.3f", theory)))
    mtext(sprintf("aspatial r = %+.3f    two-sided p = %.4f",
                  p$r, p$perm$p_two), side = 3, line = 0.2, adj = 0, cex = 0.8)
  })

  output$sp_leetab <- renderDT({
    LM <- sp_leemat(); req(LM)
    ut <- upper.tri(LM$L)
    res <- data.frame(a = rownames(LM$L)[row(LM$L)[ut]],
                      b = colnames(LM$L)[col(LM$L)[ut]],
                      L = LM$L[ut], p = LM$P[ut])
    res$q_BH <- stats::p.adjust(res$p, "BH")
    res <- res[order(res$p, -abs(res$L)), ]
    tb <- datatable(res, rownames = FALSE,
                    options = list(pageLength = 10, dom = "tip"),
                    colnames = c("Covariate", "Covariate", "Lee L", "p", "q (BH)"))
    tb <- formatRound(tb, "L", 4)
    formatSignif(tb, c("p", "q_BH"), 3)
  })

  ## ---- weights diagnostics ----
  output$sp_wgraph <- renderPlot({
    req(SP_OK)
    nb <- sp_lw()$neighbours
    lon <- SPB$lonlat[, 1]; lat <- SPB$lonlat[, 2]
    op <- par(mar = c(4, 4, 2, 1)); on.exit(par(op))
    plot(lon, lat, asp = 1 / cos(mean(lat) * pi / 180), type = "n",
         xlab = "Longitude", ylab = "Latitude",
         main = "One line per non-zero weight")
    for (i in seq_along(nb)) {
      j <- nb[[i]]; if (length(j) == 0L || j[1] == 0L) next
      segments(lon[i], lat[i], lon[j], lat[j],
               col = grDevices::adjustcolor("#4A6FA5", 0.28), lwd = 0.5)
    }
    points(lon, lat, pch = 21, bg = "#B2182B", col = "white", cex = 0.85)
  })

  output$sp_whist <- renderPlot({
    req(SP_OK)
    cd <- spdep::card(sp_lw()$neighbours)
    op <- par(mar = c(4, 4, 1, 1)); on.exit(par(op))
    hist(cd, breaks = seq(-0.5, max(cd) + 0.5, 1), col = "#92C5DE",
         border = "white", main = "", xlab = "Neighbours")
  })

  output$sp_wsummary <- renderPrint({
    if (!SP_OK) return(cat("Spatial objects not loaded."))
    lw <- sp_lw(); cd <- spdep::card(lw$neighbours)
    dm <- as.matrix(stats::dist(SPB$coords)); diag(dm) <- Inf
    cat("Households        :", nrow(SPB$coords), "\n")
    cat("Coordinates       : 01b equirectangular, metres\n")
    cat("Neighbours        : mean", round(mean(cd), 2),
        "| min", min(cd), "| max", max(cd), "\n")
    cat("Isolated units    :", sum(cd == 0L), "\n")
    cat("w.. (Szero)       :", round(spdep::Szero(lw), 2),
        " (equals n under row standardisation)\n")
    cat("E[I] under null   :", round(-1 / (nrow(SPB$coords) - 1), 5), "\n")
    cat("Nearest-neighbour distance (m):\n")
    print(round(stats::quantile(apply(dm, 1, min),
                                c(0, .25, .5, .75, .95, 1)), 1))
    cat("Zero-distance pairs:", sum(dm == 0) / 2, "\n")
  })

  output$sp_sens <- renderPlot({
    req(SP_OK)
    cfgs <- list(`knn 4` = list(type = "knn", k = 4),
                 `knn 6` = list(type = "knn", k = 6),
                 `knn 8` = list(type = "knn", k = 8),
                 `knn 12`= list(type = "knn", k = 12),
                 `250 m` = list(type = "dist", d = 250),
                 `400 m` = list(type = "dist", d = 400),
                 `IDW`   = list(type = "idw", d = 400, alpha = 1))
    top <- head(SPB$global$variable, 6)
    withProgress(message = "Sensitivity sweep", value = 0.5, {
      S <- sapply(cfgs, function(cf) sapply(top, function(v)
        moran_I(SPB$X[[v]], do.call(make_listw,
          c(list(coords = SPB$coords), cf, list(style = "W"))))))
      S <- cbind(S, `01b W` = sapply(top, function(v)
        moran_I_W(SPB$X[[v]], SPB$W_01b)))
    })
    op <- par(mar = c(6, 4, 2, 1)); on.exit(par(op))
    matplot(t(S), type = "b", pch = 16, lty = 1, xaxt = "n",
            col = seq_len(nrow(S)), xlab = "", ylab = "Moran's I",
            main = "Same covariate, eight definitions of W")
    axis(1, seq_len(ncol(S)), colnames(S), las = 2)
    abline(h = -1 / (nrow(SPB$X) - 1), lty = 2, col = "grey50")
    legend("topright", rownames(S), col = seq_len(nrow(S)), lty = 1,
           pch = 16, bty = "n", cex = 0.8)
  })

  # -----------------------------------------------------------------------
  # MGM EXPLORER SECTION
  #
  # The fitting logic is carried over unchanged from 03_mgm_explorer_app.R:
  # every refit calls mgm() with exactly the arguments shown in the sidebar,
  # so what is displayed is always a real model, never a cached redraw.
  # -----------------------------------------------------------------------

  output$mgm_explorer_status <- renderUI({
    if (MGM_OK) return(NULL)
    div(
      class = "alert alert-warning",
      style = "color: #856404; background-color: #fff3cd; border-color: #ffeeba; padding: 12px; border-radius: 4px;",
      tags$b("MGM object not found. "),
      "This section reads output/AIARMS_mgm_spatial.rds. Run 01_clean_AIARMS.R ",
      "and then 01b_add_spatial.R to create it. The MGM Network Analysis tab in ",
      "the Application & Dashboard section is independent of this and continues ",
      "to work from the CSV."
    )
  })

  output$mgm_domain_ui <- renderUI({
    req(MGM_OK)
    checkboxGroupInput("mgm_domains", "Domains to include:",
                       choices  = sort(unique(MGM_REG$group)),
                       selected = sort(unique(MGM_REG$group)))
  })

  output$mgm_var_ui <- renderUI({
    req(MGM_OK)
    selectizeInput("mgm_vars", "Fine-tune nodes:", choices = MGM_REG$var,
                   multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  mgm_selected_vars <- reactive({
    req(MGM_OK)
    switch(input$mgm_preset %|z|% "core",
           core   = AIARMS_OBJ$core_vars,
           full   = MGM_REG$var,
           manual = {
             v <- MGM_REG$var[MGM_REG$group %in% input$mgm_domains]
             if (length(input$mgm_vars)) union(v, input$mgm_vars) else v
           })
  })

  # eventReactive means nothing is recomputed until "Fit Model" is pressed, so
  # dragging the display sliders redraws instantly without refitting.
  mgm_model <- eventReactive(input$mgm_go, {
    req(MGM_OK)
    v <- mgm_selected_vars()
    validate(need(length(v) >= 4, "Select at least four variables."))

    ## Sidebar values, with the sidebar's own defaults as fallbacks so the
    ## start-up fit cannot pass a zero-length argument into mgm().
    k_ord  <- as.numeric(input$mgm_k  %|z|% "2")
    lamSel <- input$mgm_lamSel        %|z|% "EBIC"
    gam    <- input$mgm_gamma         %|z|% 0.25
    rule   <- input$mgm_rule          %|z|% "AND"
    thr    <- isTRUE(input$mgm_thresh %|z|% TRUE)

    idx <- match(v, AIARMS_OBJ$vars)
    validate(need(!anyNA(idx), paste(
      "Not present in the fitted object:", paste(v[is.na(idx)], collapse = ", "))))

    X     <- AIARMS_OBJ$data[, idx, drop = FALSE]
    type  <- AIARMS_OBJ$type[idx]
    level <- AIARMS_OBJ$level[idx]
    labs  <- AIARMS_OBJ$labels[idx]
    grp   <- MGM_REG$group[idx]

    ## Check the arguments before they reach mgm(), so a bad one is reported
    ## in the UI rather than surfacing as an error from deep inside the package.
    validate(
      need(nrow(X) > 0 && ncol(X) >= 4, "Not enough data to fit."),
      need(length(type) == ncol(X) && length(level) == ncol(X),
           "type / level lengths do not match the number of columns."),
      need(!anyNA(type) && !anyNA(level), "type or level contains NA."),
      need(length(k_ord) == 1 && is.finite(k_ord),
           "Interaction order k is not set.")
    )

    args <- list(data = X, type = type, level = level,
                 k = k_ord,
                 lambdaSel = lamSel,
                 ruleReg = rule,
                 threshold = if (thr) "LW" else "none",
                 binarySign = TRUE, scale = TRUE,
                 overparameterize = (k_ord == 3),
                 pbar = FALSE)
    if (lamSel == "EBIC") args$lambdaGam   <- gam
    if (lamSel == "CV")   args$lambdaFolds <- 10

    withProgress(message = "Fitting MGM...", value = 0.5, {
      fit <- tryCatch(do.call(mgm, args), error = function(e)
        validate(need(FALSE, paste0(
          "mgm() failed: ", conditionMessage(e), "\n",
          "  nodes = ", ncol(X), ",  n = ", nrow(X), ",  k = ", k_ord,
          ",  lambdaSel = ", lamSel, ",  rule = ", rule,
          ",  threshold = ", if (thr) "LW" else "none", "\n",
          "  types:  ", paste(names(table(type)), table(type),
                              sep = " = ", collapse = ",  "), "\n",
          "  levels: ", paste(range(level), collapse = " to ")))))
      pr  <- tryCatch(predict(fit, X, errorCon = c("RMSE", "R2"),
                              errorCat = c("CC", "nCC")),
                      error = function(e)
        validate(need(FALSE, paste("predict() failed:", conditionMessage(e)))))
    })

    # keep the node choosers in sync with the fitted model
    updateSelectInput(session, "mgm_focus", choices = c("(none)", labs))
    updateSelectInput(session, "mgm_i1", choices = labs)
    updateSelectInput(session, "mgm_i2", choices = labs, selected = labs[2])

    list(fit = fit, X = X, type = type, level = level, labels = labs,
         groups = grp, pred = pr)
  }, ignoreNULL = FALSE)   # fit once on start-up with the defaults

  mgm_wadj_display <- reactive({
    m <- mgm_model(); req(m)
    w <- m$fit$pairwise$wadj
    w[abs(w) < (input$mgm_cut %|z|% 0)] <- 0   # display cut-off only, not a refit
    dimnames(w) <- list(m$labels, m$labels)
    w
  })

  output$mgm_kpi_nodes <- renderText({
    if (!MGM_OK) return("-")
    m <- mgm_model(); if (is.null(m)) "-" else as.character(ncol(m$X))
  })
  output$mgm_kpi_edges <- renderText({
    if (!MGM_OK) return("-")
    w <- mgm_wadj_display(); if (is.null(w)) "-" else
      as.character(sum(w[upper.tri(w)] != 0))
  })
  output$mgm_kpi_obs <- renderText({
    if (!MGM_OK) return("-")
    m <- mgm_model(); if (is.null(m)) "-" else format(nrow(m$X), big.mark = ",")
  })

  output$mgm_net <- renderPlot({
    req(MGM_OK)
    m   <- mgm_model()
    w   <- mgm_wadj_display()
    ec  <- m$fit$pairwise$edgecolor
    ec[w == 0] <- NA

    # fade everything not attached to the focus node
    if (!is.null(input$mgm_focus) && input$mgm_focus != "(none)") {
      f <- match(input$mgm_focus, m$labels)
      keep <- matrix(FALSE, nrow(w), ncol(w))
      keep[f, ] <- TRUE; keep[, f] <- TRUE
      w[!keep] <- 0
    }

    rings <- if (isTRUE(input$mgm_rings)) {
      r <- ifelse(m$type == "c", m$pred$errors[, "nCC"], m$pred$errors[, "R2"])
      r[is.na(r)] <- 0; r
    } else NULL

    glist <- split(seq_along(m$labels), m$groups)
    qgraph(w,
           edge.color = ec,
           layout     = input$mgm_layout,
           repulsion  = 1.1,
           pie        = rings,
           pieColor   = ifelse(m$type == "c", "tomato", "lightblue"),
           groups     = glist,
           color      = unname(PAL_MGM[names(glist)]),
           nodeNames  = m$labels,
           labels     = seq_along(m$labels),
           legend     = TRUE, legend.cex = 0.35,
           vsize = 4.5, esize = 14)
  })

  output$mgm_summary <- renderPrint({
    req(MGM_OK)
    m <- mgm_model(); w <- mgm_wadj_display()
    cat("Nodes:", ncol(m$X), " Observations:", nrow(m$X), "\n")
    cat("Edges shown:", sum(w[upper.tri(w)] != 0),
        "of", choose(ncol(m$X), 2), "possible\n")
    cat("Node types: ", paste(names(table(m$type)), table(m$type),
                              sep = " = ", collapse = ",  "), "\n")
  })

  output$mgm_edgetab <- renderDT({
    req(MGM_OK)
    m <- mgm_model(); w <- mgm_wadj_display()
    ut <- which(upper.tri(w) & w != 0, arr.ind = TRUE)
    if (!nrow(ut))
      return(datatable(data.frame(Message = "No edges above cut-off."),
                       rownames = FALSE))
    sgn <- m$fit$pairwise$signs[ut]
    d <- data.frame(From = m$labels[ut[, 1]], To = m$labels[ut[, 2]],
                    Weight = round(w[ut], 3),
                    Sign = ifelse(is.na(sgn), "undefined",
                                  ifelse(sgn > 0, "positive", "negative")))
    datatable(d[order(-abs(d$Weight)), ], rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })

  output$mgm_predplot <- renderPlot({
    req(MGM_OK)
    m <- mgm_model()
    r <- ifelse(m$type == "c", m$pred$errors[, "nCC"], m$pred$errors[, "R2"])
    r[is.na(r)] <- 0
    o <- order(r)
    op <- par(mar = c(4, 12, 2, 2)); on.exit(par(op))
    barplot(r[o], horiz = TRUE, names.arg = m$labels[o], las = 1,
            cex.names = 0.7, xlim = c(0, 1),
            col = ifelse(m$type[o] == "c", "tomato", "lightblue"),
            xlab = "Predictability  (R2 for continuous, normalised accuracy for categorical)")
  })

  output$mgm_errtab <- renderDT({
    req(MGM_OK)
    m <- mgm_model()
    datatable(data.frame(Variable = m$labels, Type = m$type,
                         round(m$pred$errors[, -1, drop = FALSE], 3)),
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$mgm_intdetail <- renderPrint({
    req(MGM_OK)
    m <- mgm_model()
    i <- match(input$mgm_i1, m$labels); j <- match(input$mgm_i2, m$labels)
    if (is.na(i) || is.na(j) || i == j) return(cat("Choose two different nodes."))
    showInteraction(m$fit, int = c(i, j))
  })

  observe({
    req(MGM_OK)
    m <- mgm_model()
    updateSelectInput(session, "mgm_mapvar", choices = m$labels,
                      selected = if ("ESBL+ months" %in% m$labels) "ESBL+ months"
                                 else m$labels[1])
  })

  # Moran's I with a permutation test, kept inline exactly as in
  # 03_mgm_explorer_app.R so this tab needs nothing from the spatial section.
  mgm_moran_test <- function(v, W, nperm = 499) {
    v <- as.numeric(v)
    if (sd(v) == 0) return(list(I = NA, p = NA))
    n <- length(v); z <- v - mean(v)
    Ifun <- function(z) (n / sum(W)) * as.numeric(t(z) %*% W %*% z) /
                        as.numeric(t(z) %*% z)
    I_obs <- Ifun(z)
    I_perm <- replicate(nperm, Ifun(sample(z)))
    list(I = I_obs, p = (sum(abs(I_perm) >= abs(I_obs)) + 1) / (nperm + 1))
  }

  mgm_map_values <- reactive({
    req(MGM_HAS_SPATIAL)
    m <- mgm_model()
    j <- match(input$mgm_mapvar, m$labels); req(!is.na(j))
    v <- m$X[, j]
    # The neighbour average is the spatial lag: each household replaced by the
    # mean of its neighbours. Smoothing this way makes clustering visible that
    # individual points can hide.
    if (isTRUE(input$mgm_maplag)) v <- as.numeric(AIARMS_OBJ$W %*% v)
    v
  })

  output$mgm_map <- renderPlot({
    req(MGM_HAS_SPATIAL)
    m  <- mgm_model(); xy <- AIARMS_OBJ$coords; v <- mgm_map_values()
    pal_v <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(10)
    cols  <- pal_v[cut(v, breaks = 10, labels = FALSE)]
    dj <- grep("^Neighbours within", m$labels)[1]
    cx <- if (isTRUE(input$mgm_mapsize) && !is.na(dj))
            0.7 + 1.8 * m$X[, dj] / max(m$X[, dj]) else 1.3
    op <- par(mar = c(4, 4, 3, 1)); on.exit(par(op))
    plot(xy[, 1], xy[, 2], asp = 1, pch = 21, bg = cols, col = "grey30", cex = cx,
         xlab = "Easting (m from centroid)", ylab = "Northing (m from centroid)",
         main = paste0(input$mgm_mapvar,
                       if (isTRUE(input$mgm_maplag)) "  (neighbour average)" else ""))
    legend("topleft", legend = c("low", "", "", "", "high"),
           pt.bg = pal_v[c(1, 3, 5, 7, 10)], pch = 21, bty = "n", cex = 0.85)
  })

  output$mgm_moran <- renderPrint({
    if (!MGM_HAS_SPATIAL) return(cat("Run 01b_add_spatial.R to enable the map."))
    m <- mgm_model(); j <- match(input$mgm_mapvar, m$labels); req(!is.na(j))
    r  <- mgm_moran_test(m$X[, j], AIARMS_OBJ$W)
    rj <- if (m$type[j] %in% c("g", "p"))
            mgm_moran_test(m$X[, j] - m$pred$predicted[, j], AIARMS_OBJ$W) else NULL
    cat("Variable:", input$mgm_mapvar, "  (k =", AIARMS_OBJ$K_NN, "neighbours)\n")
    cat(sprintf("  raw       Moran's I = %+.3f   p = %.3f\n", r$I, r$p))
    if (!is.null(rj))
      cat(sprintf("  residual  Moran's I = %+.3f   p = %.3f\n", rj$I, rj$p))
    cat(sprintf("  expected under no autocorrelation: %+.3f\n",
                -1 / (nrow(m$X) - 1)))
  })

  output$mgm_dict <- renderDT({
    req(MGM_OK)
    datatable(MGM_REG[, c("var", "label", "type", "level", "group")],
              rownames = FALSE,
              colnames = c("Column", "Label", "mgm type", "Levels", "Domain"),
              options = list(pageLength = 35, scrollX = TRUE))
  })
}

# --- RUN SHINY APPLICATION ---
shinyApp(ui = ui, server = server)
