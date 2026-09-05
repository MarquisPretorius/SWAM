options(shiny.sanitize.errors = FALSE)

# Load required libraries
library(spdep)       # Loaded first to prevent namespace conflict with bslib
library(shiny)
library(bslib)
library(bsicons)
library(leaflet)     # also supplies the %>% pipe used below
library(DT)
library(sf)          # required by mgm_spatial_core.R
library(mgm)
library(qgraph)

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

# Length-safe default. An eventReactive with ignoreNULL = FALSE runs at
# start-up, and a sidebar input on a nav_panel that has not been opened yet can
# still be NULL or zero-length at that instant. Passing that straight into a
# model call is what produced "Error in : : argument of length 0".
`%|z|%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

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

# --- SELF-BUILDING PIPELINE ------------------------------------------------
# Builds the analysis artefacts on first launch. Deliberately restricted to
# interactive sessions with a writable app folder:
#
#   * On a published server (Connect Cloud, shinyapps.io) the session is not
#     interactive, the app directory should not be treated as writable, and
#     spawning an Rscript subprocess is not something to rely on. Deployed
#     copies must therefore ship the .rds artefacts alongside app.R.
#   * Locally, runApp() from the console or the Run App button is interactive,
#     so the build still happens automatically.
#
# Force it either way by setting this to TRUE or FALSE by hand.
BUILD_ON_START <- interactive() && file.access(".", 2L) == 0L

# The two cleaning steps may exist either as standalone .R scripts or only as
# chunks inside MGM_Research.Rmd, so both are handled.
CLEAN_SCRIPTS <- c("01_clean_AIARMS.R", "01_clean.R")
SPAT_SCRIPTS  <- c("01b_add_spatial.R", "01b_spatial.R")
CLEAN_CHUNK   <- "Data Cleaning"
SPAT_CHUNK    <- "Spatial component"

# Pull a named chunk out of an .Rmd as plain text.
rmd_chunk <- function(rmd, label) {
  x      <- readLines(rmd, warn = FALSE)
  opens  <- grep("^```\\{r", x)
  fences <- grep("^```\\s*$", x)
  for (o in opens) {
    lab <- trimws(sub("^```\\{r[ ,]*([^,}]*).*$", "\\1", x[o]))
    if (identical(lab, label)) {
      cl <- fences[fences > o][1]
      if (!is.na(cl) && cl > o + 1) return(paste(x[(o + 1):(cl - 1)], collapse = "\n"))
    }
  }
  NULL
}

# Find the first .Rmd in the folder that carries both chunks.
find_pipeline_rmd <- function() {
  for (f in list.files(".", pattern = "\\.Rmd$", ignore.case = TRUE)) {
    if (!is.null(rmd_chunk(f, CLEAN_CHUNK)) && !is.null(rmd_chunk(f, SPAT_CHUNK)))
      return(f)
  }
  NULL
}

# The cleaning scripts are run in a SEPARATE R PROCESS, which is how you would
# run them by hand. Two reasons it has to be a subprocess rather than source():
# 01_clean_AIARMS.R opens with rm(list = ls()), which would wipe this app's
# objects, and 01b_add_spatial.R checks its own prerequisites with
# vapply(need, exists, ...), which only resolves against the global environment
# and so fails if the script is sourced into a private one.
run_pipeline <- function() {
  if (is.null(find_app_file(DATA_FILE)))
    stop(DATA_FILE, " not found; the cleaning step needs it.")

  # Return a runnable .R path for a step, writing the .Rmd chunk out to a
  # temporary file when no standalone script exists.
  resolve_step <- function(script_names, chunk_label) {
    for (nm in script_names) {
      f <- find_app_file(nm)
      if (!is.null(f)) return(normalizePath(f, winslash = "/"))
    }
    rmd <- find_pipeline_rmd()
    if (is.null(rmd))
      stop("Cannot find ", paste(script_names, collapse = " or "),
           ", and no .Rmd here contains a '", chunk_label, "' chunk.")
    tmp <- file.path(tempdir(),
                     paste0(gsub("[^A-Za-z0-9]+", "_", chunk_label), ".R"))
    writeLines(rmd_chunk(rmd, chunk_label), tmp)
    normalizePath(tmp, winslash = "/")
  }

  f1 <- resolve_step(CLEAN_SCRIPTS, CLEAN_CHUNK)
  f2 <- resolve_step(SPAT_SCRIPTS,  SPAT_CHUNK)

  rscript <- file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  code <- sprintf('setwd("%s"); source("%s"); source("%s")',
                  normalizePath(getwd(), winslash = "/"), f1, f2)
  out <- suppressWarnings(
    system2(rscript, c("-e", shQuote(code)), stdout = TRUE, stderr = TRUE))
  st  <- attr(out, "status")
  if (!is.null(st) && st != 0)
    stop("the cleaning pipeline exited with status ", st, ":\n",
         paste(utils::tail(out, 12), collapse = "\n"))
  invisible(out)
}

# Rebuild mgm_spatial_bundle.rds. This is the same sequence as the export chunk
# of spatial_autocorrelation_MGM.Rmd and calls the same functions out of
# mgm_spatial_core.R, so the statistics cannot diverge from the report; only
# the assembly is restated here so the app can stand alone.
build_spatial_bundle <- function(obj_path, out_path,
                                 k = 8, nsim_global = 9999, nsim_lee = 19999) {
  A  <- readRDS(obj_path)
  E  <- expand_registry(A)
  xy <- mgm_coords(A)
  cov_vars <- E$meta$var[E$meta$group != "Spatial"]
  sp_vars  <- E$meta$var[E$meta$group == "Spatial"]
  wcfg <- list(type = "knn", k = k, style = "W")

  G <- global_table(E$X, xy, cov_vars, wcfg = wcfg, nsim = nsim_global, seed = 1)
  G <- merge(G, E$meta[, c("var", "group", "label")], by.x = "variable", by.y = "var")
  G <- G[order(-G$moran_I), ]
  LM <- lee_matrix_fast(E$X, xy, cov_vars, wcfg = wcfg, nsim = nsim_lee, seed = 1)

  saveRDS(list(X = E$X, meta = E$meta, coords = xy, lonlat = A$lonlat,
               registry = A$registry, W_01b = A$W,
               cov_vars = cov_vars, sp_vars = sp_vars,
               global = G, lee = LM, site_group = A$site_group,
               k = k, built = Sys.time()), out_path)
  invisible(out_path)
}

# Number of distinct sampling months, read from the CSV header only.
N_MONTHS <- NA_integer_
local({
  f <- find_app_file(DATA_FILE)
  if (!is.null(f)) {
    nm <- tryCatch(names(utils::read.csv(f, nrows = 1L)), error = function(e) character(0))
    m  <- grep("^(EC|KP|ESBL)_", nm, value = TRUE)
    if (length(m)) N_MONTHS <<- length(unique(sub("^(EC|KP|ESBL)_", "", m)))
  }
})

BUILD_LOG <- character(0)
if (BUILD_ON_START) {
  # Step 1: the MGM object. Seconds.
  if (is.null(find_app_file(MGM_OBJECT))) {
    message("Building ", MGM_OBJECT, " ...")
    ok <- tryCatch({ run_pipeline(); TRUE },
                   error = function(e) { BUILD_LOG <<- c(BUILD_LOG,
                     paste("Cleaning pipeline failed:", conditionMessage(e))); FALSE })
    if (ok) BUILD_LOG <- c(BUILD_LOG, "Built output/AIARMS_mgm_spatial.rds from the cleaning scripts.")
  }

  # Step 2: the spatial results. About 35 seconds; only ever done once.
  if (!is.null(find_app_file(MGM_OBJECT)) &&
      is.null(find_app_file(SPATIAL_BUNDLE)) &&
      !is.null(find_app_file(SPATIAL_CORE))) {
    message("Building ", SPATIAL_BUNDLE, " (about 35 seconds, once) ...")
    tryCatch({
      source(find_app_file(SPATIAL_CORE), local = FALSE)
      build_spatial_bundle(find_app_file(MGM_OBJECT), SPATIAL_BUNDLE)
      BUILD_LOG <- c(BUILD_LOG, "Built mgm_spatial_bundle.rds from the MGM object.")
    }, error = function(e)
      BUILD_LOG <<- c(BUILD_LOG, paste("Bundle build failed:", conditionMessage(e))))
  }
  if (length(BUILD_LOG)) for (l in BUILD_LOG) message("  ", l)
}

SP_OK <- FALSE; SPB <- NULL; SP_LOAD_ERR <- NULL
local({
  core_path   <- find_app_file(SPATIAL_CORE)
  bundle_path <- find_app_file(SPATIAL_BUNDLE)
  if (is.null(core_path) || is.null(bundle_path)) {
    here <- tryCatch(sort(list.files(".", recursive = TRUE, no.. = TRUE)),
                     error = function(e) character(0))
    SP_LOAD_ERR <<- paste0(
      "Missing ",
      paste(c(if (is.null(core_path)) SPATIAL_CORE,
              if (is.null(bundle_path)) SPATIAL_BUNDLE), collapse = " and "),
      ". Working directory: ", getwd(),
      ". Files deployed alongside the app: ",
      if (length(here)) paste(utils::head(here, 40), collapse = ", ") else "(none)",
      ".")
    return(invisible(NULL))
  }
  res <- tryCatch({
    source(core_path, local = FALSE)
    SPB <<- readRDS(bundle_path)
    need <- c("X", "meta", "coords", "lonlat", "cov_vars",
              "sp_vars", "global", "lee", "k")
    if (!is.list(SPB) || !all(need %in% names(SPB)))
      stop(bundle_path, " is missing: ",
           paste(setdiff(need, names(SPB)), collapse = ", "))
    SP_OK <<- TRUE
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(res)) { SP_OK <<- FALSE; SP_LOAD_ERR <<- res }
})

# Loading the MGM object must never be able to stop the app. Anything wrong
# with the file leaves MGM_OK FALSE and a message for the section to display;
# the rest of the dashboard carries on. A readable .rds is not enough -- it has
# to actually be the object 01b builds, so the structure is checked before use.
MGM_OK <- FALSE; AIARMS_OBJ <- NULL; MGM_REG <- NULL
MGM_OBJ_PATH <- NULL; MGM_LOAD_ERR <- NULL
local({
  obj_path <- find_app_file(MGM_OBJECT)
  if (is.null(obj_path)) {
    # On a deployed server you cannot inspect the container, so list what is
    # actually there. If the file is in the repo but missing from this listing,
    # it was not bundled -- regenerate manifest.json and republish.
    here <- tryCatch(sort(list.files(".", recursive = TRUE, no.. = TRUE)),
                     error = function(e) character(0))
    MGM_LOAD_ERR <<- paste0(
      MGM_OBJECT, " was not found. Working directory: ", getwd(),
      ". Files deployed alongside the app: ",
      if (length(here)) paste(utils::head(here, 40), collapse = ", ") else "(none)",
      ".")
    return(invisible(NULL))
  }
  obj <- tryCatch(readRDS(obj_path), error = function(e) e)
  if (inherits(obj, "error")) {
    MGM_LOAD_ERR <<- paste0("Could not read ", obj_path, ": ",
                            conditionMessage(obj))
    return(invisible(NULL))
  }
  bad <- NULL
  if (!is.list(obj))                                 bad <- "it is not a list"
  else if (!all(c("data", "registry") %in% names(obj)))
    bad <- paste0("it has no $", paste(setdiff(c("data", "registry"), names(obj)),
                                       collapse = " and no $"))
  else if (is.null(ncol(obj$data)) || ncol(obj$data) < 1)
    bad <- "$data is not a matrix or data frame"
  else if (is.null(nrow(obj$registry)) || nrow(obj$registry) < 1)
    bad <- "$registry is empty"
  if (!is.null(bad)) {
    MGM_LOAD_ERR <<- paste0(obj_path, " is not the object 01b_add_spatial.R ",
                            "builds: ", bad, ". Rebuild it, or point MGM_OBJECT ",
                            "at the right file.")
    return(invisible(NULL))
  }
  AIARMS_OBJ   <<- obj
  MGM_REG      <<- obj$registry
  MGM_OBJ_PATH <<- obj_path
  MGM_OK       <<- TRUE
})
MGM_HAS_SPATIAL <- MGM_OK && !is.null(AIARMS_OBJ$coords)

# The explorer was written against an object carrying vars / type / level /
# labels / core_vars. Older builds of 01b do not all store every one of them,
# so each is resolved here with a fall-back drawn from the registry, and what
# was actually used is reported in the section's status line. Without this a
# missing core_vars shows up only as "Select at least four variables", which
# says nothing about the cause.
MGM_VARS <- MGM_TYPE <- MGM_LEVEL <- MGM_LABELS <- MGM_CORE <- NULL
MGM_NOTES <- character(0)
if (MGM_OK) tryCatch({
  n_col      <- ncol(AIARMS_OBJ$data)
  MGM_VARS   <- AIARMS_OBJ$vars   %|z|% colnames(AIARMS_OBJ$data) %|z|% MGM_REG$var
  MGM_TYPE   <- AIARMS_OBJ$type   %|z|% MGM_REG$type
  MGM_LEVEL  <- AIARMS_OBJ$level  %|z|% MGM_REG$level
  MGM_LABELS <- AIARMS_OBJ$labels %|z|% MGM_REG$label %|z|% MGM_VARS
  MGM_CORE   <- AIARMS_OBJ$core_vars

  if (is.null(AIARMS_OBJ$vars))   MGM_NOTES <- c(MGM_NOTES, "vars taken from the data columns")
  if (is.null(AIARMS_OBJ$type))   MGM_NOTES <- c(MGM_NOTES, "type taken from the registry")
  if (is.null(AIARMS_OBJ$level))  MGM_NOTES <- c(MGM_NOTES, "level taken from the registry")
  if (is.null(AIARMS_OBJ$labels)) MGM_NOTES <- c(MGM_NOTES, "labels taken from the registry")
  if (length(MGM_CORE) < 4) {
    MGM_CORE  <- MGM_VARS
    MGM_NOTES <- c(MGM_NOTES,
      "no usable core_vars in the object, so \"Core set\" falls back to all variables")
  }
  # Any core variable not present in the data cannot be fitted; drop it rather
  # than letting match() produce an NA index later.
  drop_core <- setdiff(MGM_CORE, MGM_VARS)
  if (length(drop_core)) {
    MGM_CORE  <- intersect(MGM_CORE, MGM_VARS)
    MGM_NOTES <- c(MGM_NOTES, paste("core variables absent from the data and dropped:",
                                    paste(drop_core, collapse = ", ")))
  }
  if (length(MGM_VARS) != n_col)
    MGM_NOTES <- c(MGM_NOTES, sprintf(
      "WARNING: %d variable names for %d data columns", length(MGM_VARS), n_col))
  if (length(MGM_TYPE) != n_col || length(MGM_LEVEL) != n_col) {
    MGM_OK       <<- FALSE
    MGM_LOAD_ERR <<- sprintf(
      "type (%d) and level (%d) do not match the %d data columns, so the model cannot be specified.",
      length(MGM_TYPE), length(MGM_LEVEL), n_col)
  }
}, error = function(e) {
  MGM_OK       <<- FALSE
  MGM_LOAD_ERR <<- paste("Could not interpret the MGM object:", conditionMessage(e))
})

# Palettes. Named apart from anything already in the app so nothing is masked.
PAL_LISA <- c("High-High" = "#FF6B6B", "Low-Low"  = "#22D3EE",
              "High-Low"  = "#F5A77E", "Low-High" = "#8ECFDD")
PAL_NS   <- "#5A7C89"
PAL_DIV  <- colorRampPalette(c("#1F7A99", "#8ECFDD", "#F2F7F8", "#F5A77E", "#D6455B"))
PAL_MGM  <- c("Carriage" = "#D6455B", "Demographic" = "#F58A5E",
              "Socioeconomic" = "#FBD9C4", "WASH" = "#5AB8D4",
              "Health" = "#1F7A99", "Antibiotic" = "#8E5BC4",
              "Food/animal" = "#4FB477", "Spatial" = "#128C7E")

# Everything that still plots does so in base R on white .mgm-card panels, or
# through leaflet. The ggplot / text-munging helpers that served the removed
# Data & Cohort section have gone with it.

# --- THEME STYLING ---
# "Deep water" palette. Cool teals carry the structure, so anything warm on the
# page is an epidemiological signal rather than decoration.
app_theme <- bs_theme(
  version   = 5,
  bg        = "#041F29",   # abyss
  fg        = "#DCEEF3",   # foam
  primary   = "#22D3EE",   # bioluminescent aqua
  secondary = "#16576B",   # shelf
  success   = "#3DDC97",   # algal green
  info      = "#5AB8D4",   # shallow water
  warning   = "#FFC15E",   # caution
  danger    = "#FF6B6B"    # contamination
)

# --- Small presentational helpers for the Methodology section ---------------
# eqbox() frames an equation so it reads as the focal point of a passage rather
# than a line of text among others. chip() and defrow() carry short facts and
# definitions without turning them into paragraphs.
eqbox <- function(label, latex, caption = NULL) {
  div(class = "eqbox",
      div(class = "eqlabel", label),
      p(latex),
      if (!is.null(caption)) div(class = "eqcap", caption))
}

chip <- function(key, value, detail = NULL, tone = NULL) {
  div(class = paste("chip", tone),
      div(class = "k", key),
      div(class = "v", value),
      if (!is.null(detail)) div(class = "d", detail))
}

chiprow <- function(...) div(class = "chiprow", ...)

defrow <- function(term, desc)
  div(class = "defrow", div(class = "term", term), div(class = "desc", desc))

# Every section opens with the same banner: an eyebrow label, a heading and one
# line saying what the section answers. Consistency here is what stops the app
# reading as a pile of unrelated tabs.
sec_head <- function(eyebrow, title, lede) {
  div(class = "sec-head",
      div(class = "eyebrow", eyebrow),
      tags$h2(title),
      p(class = "lede", lede))
}

# --- UI DEFINITION ---
ui <- page_navbar(
  id = "main_nav",                 # nav_select() targets this
  theme = app_theme,
  title = "SWAM",
  window_title = "SWAM - Spatial Wastewater & Antimicrobial Monitor",
  fillable = FALSE,
  header = tagList(
    tags$head(tags$style(HTML("
      body, .bslib-page-navbar { background-color: #041F29 !important; color: #DCEEF3 !important; }
      .card:not(.mgm-card), .bslib-card:not(.mgm-card), .well, .accordion-body { 
        background-color: #0B3140 !important; color: #DCEEF3 !important; border: 1px solid #16576B !important; 
      }
      .accordion-button { background-color: #0B3140 !important; color: #22D3EE !important; border-bottom: 1px solid #16576B !important; }
      .accordion-button:not(.collapsed) { background-color: #072A36 !important; color: #22D3EE !important; }
      .sidebar, .bslib-sidebar-layout > .sidebar { background-color: #072A36 !important; border-right: 1px solid #16576B !important; }
      .nav-tabs .nav-link.active { background-color: #22D3EE !important; color: #041F29 !important; font-weight: bold; }
      .nav-tabs .nav-link { color: #8FAFBC !important; }
      .table, .table td, .table th, .dataTables_wrapper { color: #DCEEF3 !important; }
      .form-control, .form-select { background-color: #041F29 !important; color: #DCEEF3 !important; border: 1px solid #16576B !important; }
      .value-box { background-color: #0B3140 !important; border: 1px solid #16576B !important; }
      .mgm-card { background-color: #FFFFFF !important; color: #1A202C !important; border: 1px solid #C3D9E0 !important; }
      .mgm-card p, .mgm-card h5, .mgm-card div { color: #1A202C !important; }
      .navbar-brand { font-weight: 700 !important; letter-spacing: 0.06em; }
      .swam-subtitle { color: #8FAFBC; font-size: 0.95rem; letter-spacing: 0.02em;
                       padding: 4px 0 10px 4px; }

      /* Landing page */
      .hero { background: linear-gradient(135deg, #072A36 0%, #041F29 70%);
              border: 1px solid #16576B; border-radius: 8px;
              padding: 34px 38px; margin-bottom: 22px; }
      .hero h1 { font-size: 2.15rem; font-weight: 700; letter-spacing: 0.02em;
                 margin: 0 0 6px 0; color: #FFFFFF; }
      .hero .lede { font-size: 1.05rem; color: #BBD7E0; max-width: 60em;
                    line-height: 1.55; margin-bottom: 4px; }
      .hero .eyebrow { text-transform: uppercase; letter-spacing: 0.18em;
                       font-size: 0.72rem; color: #22D3EE; margin-bottom: 10px; }
      .jump-row .btn { margin: 14px 10px 0 0; font-weight: 600; }
      .aim { border-left: 3px solid #22D3EE; padding: 2px 0 2px 14px;
             margin-bottom: 16px; }
      .aim b { color: #22D3EE; }
      .step-num { display: inline-block; width: 26px; height: 26px;
                  border-radius: 50%; background: #22D3EE; color: #041F29;
                  text-align: center; font-weight: 700; line-height: 26px;
                  margin-right: 10px; }
      .theory { line-height: 1.62; }
      .theory .eqnote { color: #8FAFBC; font-size: 0.88rem; }
      .MathJax, .MathJax_Display { color: #DCEEF3 !important; }

      /* --- Section banners: every section opens the same way --- */
      .sec-head { border-left: 4px solid #22D3EE; padding: 2px 0 2px 18px;
                  margin: 4px 0 22px 0; }
      .sec-head .eyebrow { text-transform: uppercase; letter-spacing: 0.16em;
                           font-size: 0.68rem; color: #22D3EE; font-weight: 700;
                           margin-bottom: 6px; }
      .sec-head h2 { font-size: 1.55rem; font-weight: 700; color: #FFFFFF;
                     margin: 0 0 8px 0; letter-spacing: 0.01em; }
      .sec-head .lede { color: #9CBAC6; font-size: 0.97rem; line-height: 1.55;
                        max-width: 62em; margin: 0; }

      /* --- Headings inside cards --- */
      .card-header { font-weight: 700 !important; letter-spacing: 0.03em;
                     background-color: #072A36 !important;
                     border-bottom: 1px solid #16576B !important;
                     color: #22D3EE !important; font-size: 0.95rem; }
      h5 { color: #22D3EE; font-weight: 700; font-size: 1.02rem;
           letter-spacing: 0.02em; margin-top: 4px; }
      h6 { color: #DCEEF3; font-weight: 700; font-size: 0.88rem;
           text-transform: uppercase; letter-spacing: 0.09em; }
      .mgm-card h5, .mgm-card h6 { color: #1A202C !important; }

      /* --- Tabs: clearer active state, more breathing room --- */
      .nav-tabs { border-bottom: 1px solid #16576B !important; }
      .nav-tabs .nav-link { padding: 10px 18px !important; font-weight: 600;
                            border: none !important; }
      .nav-tabs .nav-link:hover { color: #22D3EE !important; }
      .nav-tabs .nav-link.active { border-radius: 6px 6px 0 0 !important; }
      .navbar .nav-link { font-weight: 600; letter-spacing: 0.02em; }

      /* --- Value boxes --- */
      .value-box .value-box-title { text-transform: uppercase;
                                    letter-spacing: 0.11em; font-size: 0.7rem;
                                    color: #8FAFBC !important; }
      .value-box .value-box-value { font-weight: 700; }

      /* --- Rhythm --- */
      .card { margin-bottom: 16px; }
      .card-body { padding: 20px 22px; }
      .theory p { margin-bottom: 0.95rem; }
      .accordion-button { font-weight: 600; font-size: 0.9rem; }
      hr { border-color: #16576B !important; opacity: 1; margin: 18px 0; }
      .sidebar h6 { margin-top: 2px; }

      /* --- Themed notices, instead of Bootstrap's cream boxes --- */
      .note-warn { color: #FFD79A !important;
                   background: rgba(255,193,94,0.10) !important;
                   border: 1px solid rgba(255,193,94,0.45) !important;
                   border-left: 4px solid #FFC15E !important;
                   padding: 12px 16px; border-radius: 4px; }
      .note-info { color: #A9EDF7 !important;
                   background: rgba(34,211,238,0.09) !important;
                   border: 1px solid rgba(34,211,238,0.40) !important;
                   border-left: 4px solid #22D3EE !important;
                   padding: 12px 16px; border-radius: 4px; }
      .note-warn b, .note-info b { color: inherit !important; }

      /* --- Equation panels: the maths becomes the focal point --- */
      .eqbox { background: linear-gradient(90deg, rgba(34,211,238,0.07), rgba(34,211,238,0.0));
               border-left: 3px solid #22D3EE; border-radius: 0 6px 6px 0;
               padding: 6px 20px 10px 20px; margin: 14px 0 20px 0; }
      .eqbox .eqlabel { text-transform: uppercase; letter-spacing: 0.15em;
                        font-size: 0.66rem; color: #22D3EE; font-weight: 700;
                        margin-bottom: 2px; }
      .eqbox .eqcap { color: #8FAFBC; font-size: 0.85rem; margin-top: 2px; }

      /* --- Small fact chips used across the methodology tabs --- */
      .chiprow { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 6px; }
      .chip { flex: 1 1 210px; background: #072A36; border: 1px solid #16576B;
              border-top: 3px solid #22D3EE; border-radius: 6px;
              padding: 12px 16px; }
      .chip .k { text-transform: uppercase; letter-spacing: 0.12em;
                 font-size: 0.64rem; color: #8FAFBC; font-weight: 700; }
      .chip .v { font-size: 1.02rem; color: #DCEEF3; font-weight: 700;
                 margin-top: 3px; }
      .chip .d { font-size: 0.82rem; color: #8FAFBC; margin-top: 4px;
                 line-height: 1.45; }
      .chip.warm { border-top-color: #FF6B6B; }
      .chip.cool { border-top-color: #1F7A99; }

      /* --- Definition rows --- */
      .defrow { display: flex; gap: 14px; padding: 9px 0;
                border-bottom: 1px solid rgba(22,87,107,0.55); }
      .defrow:last-child { border-bottom: none; }
      .defrow .term { flex: 0 0 190px; color: #22D3EE; font-weight: 700;
                      font-size: 0.9rem; }
      .defrow .desc { flex: 1; color: #DCEEF3; font-size: 0.92rem;
                      line-height: 1.55; }
    "))),
    withMathJax(),                 # without this the $$...$$ render as plain text
    div(class = "swam-subtitle", "Spatial Wastewater & Antimicrobial Monitor")
  ),
  
  # SECTION 1: INTRODUCTION
  nav_panel(
    "Introduction",
    icon = bsicons::bs_icon("house-door-fill"),

    div(
      class = "hero",
      div(class = "eyebrow", "WST795 Research Report  |  University of Pretoria"),
      h1("Spatial modelling of wastewater data in epidemiology"),
      p(class = "lede",
        "Antimicrobial resistance in KwaZulu-Natal, read two ways: a mixed ",
        "graphical model for the conditional dependencies between covariates, ",
        "and spatial autocorrelation for where those covariates cluster on the ",
        "ground. Both run on one cleaned dataset of 162 households."),
      div(
        class = "jump-row",
        actionButton("jump_method",  "Read the methodology",
                     icon = bsicons::bs_icon("book"), class = "btn-outline-info"),
        actionButton("jump_spatial", "Spatial autocorrelation",
                     icon = bsicons::bs_icon("bullseye"), class = "btn-primary"),
        actionButton("jump_mgm",     "MGM explorer",
                     icon = bsicons::bs_icon("diagram-3-fill"), class = "btn-primary")
      )
    ),

    layout_columns(
      fill = FALSE,
      value_box(title = "Households", value = textOutput("lp_kpi_hh"),
                showcase = bsicons::bs_icon("house-fill"), theme = "primary"),
      value_box(title = "Sampling Months", value = textOutput("lp_kpi_months"),
                showcase = bsicons::bs_icon("calendar3"), theme = "secondary"),
      value_box(title = "MGM Nodes", value = textOutput("lp_kpi_nodes"),
                showcase = bsicons::bs_icon("diagram-3"), theme = "info"),
      value_box(title = "Covariates Tested", value = textOutput("lp_kpi_cov"),
                showcase = bsicons::bs_icon("list-ol"), theme = "success"),
      value_box(title = "Spatially Clustered", value = textOutput("lp_kpi_sig"),
                showcase = bsicons::bs_icon("bullseye"), theme = "danger")
    ),

    layout_columns(
      col_widths = c(7, 5),

      card(
        card_header("Why this study"),
        card_body(
          class = "theory",
          p("Wastewater-based epidemiology (WBE) has emerged as a powerful tool in ",
            "public health management, particularly after the COVID-19 pandemic. ",
            "An effective outbreak response requires fast and accurate detection, ",
            "efficient allocation of resources, and the ability to react ",
            "predictively rather than retrospectively. Clinical trials remain the ",
            "standard choice, but they are expensive, time consuming, invasive, ",
            "and require the consent of the individuals involved, which makes them ",
            "poorly suited as a standalone method. WBE complements them: it is ",
            "inexpensive, non-invasive, allows real-time decision making, and is ",
            "less biased."),
          p("Spatial statistics shifts the viewpoint from where something is to ",
            "why it may occur there. Demographic-based mapping connects physical ",
            "location with the characteristics of the population present, treated ",
            "as covariates. Elevation, for instance, was found to be associated ",
            "with the distribution of cholera during the 2008-2009 epidemic in ",
            "Harare, Zimbabwe."),
          p("This report focuses on antimicrobial resistance (AMR): the process of ",
            "a micro-organism surviving despite the presence of an antibiotic. The ",
            "global rise in AMR threatens to undo decades of progress in treating ",
            "bacterial infectious disease, which is why it is studied here in ",
            "place of COVID-19, and specifically how resistance can be identified ",
            "through covariates.")
        )
      ),

      card(
        card_header("The four aims"),
        card_body(
          div(class = "aim", tags$b("1. "),
              "Investigate the significance of demographic factors through mixed ",
              "graphical models, then connect those factors to physical location ",
              "and to AMR markers."),
          div(class = "aim", tags$b("2. "),
              "Use the MGM results to make connections between these factors, and ",
              "focus on those showing high correlation to both location and the ",
              "markers."),
          div(class = "aim", tags$b("3. "),
              "Apply spatial autocorrelation to those covariates to establish ",
              "which are spatially significant within KwaZulu-Natal."),
          div(class = "aim", tags$b("4. "),
              "Draw conclusions that improve the effectiveness of public health ",
              "intervention.")
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      card(
        card_header("Target pathogens and markers"),
        card_body(
          tags$ul(
            tags$li(tags$b("Escherichia coli (EC). "),
                    "Indicator organism for faecal-oral environmental ",
                    "transmission, across nine sampling months."),
            tags$li(tags$b("Klebsiella pneumoniae (KP). "),
                    "Opportunistic pathogen associated with plasmid-mediated ",
                    "multi-drug resistance."),
            tags$li(tags$b("ESBL phenotype. "),
                    "Enzymatic resistance conferring immunity to broad-spectrum ",
                    "beta-lactam antibiotics.")
          )
        )
      ),

      card(
        card_header("The data"),
        card_body(
          p("A household survey conducted in KwaZulu-Natal, carrying variables of ",
            "several measurement types at once: binary resistance markers, ",
            "categorical demographic and sanitation responses, count variables ",
            "and continuous measurements."),
          p("That mixture is exactly why a mixed graphical model is required ",
            "rather than a single-type network, and why each node in this app ",
            "carries a declared type and level."),
          uiOutput("lp_data_status")
        )
      ),

      card(
        card_header("How to use this app"),
        card_body(
          p(span(class = "step-num", "1"),
            "Read the ", tags$b("Methodology"), " for the definitions and ",
            "equations every result below is computed from."),
          p(span(class = "step-num", "2"),
            tags$b("Spatial Autocorrelation"), " answers aim 3 -- Moran's I, ",
            "Geary's C and the LISA, under a weights matrix you choose."),
          p(span(class = "step-num", "3"),
            tags$b("MGM Explorer"), " answers aims 1 and 2 -- the conditional ",
            "dependency network, refitted live as you change its settings."),
          p(span(class = "step-num", "4"),
            tags$b("Conclusion"), " draws the two together.")
        )
      )
    )
  ),

  # SECTION 2: METHODOLOGY
  nav_panel(
    "Methodology",
    icon = bsicons::bs_icon("gear-wide-connected"),
    sec_head("Section 2  |  Background theory",
             "Methodology",
             paste("The definitions, equations and estimation procedure behind",
                   "every number this app reports. Each tab corresponds to a",
                   "subsection of the written report.")),
    navset_card_tab(

      nav_panel(
        "Spatial Data & Dependence",
        icon = bsicons::bs_icon("globe-americas"),
        card_body(
          class = "theory",
          h5("Types of spatial data"),
          p("Spatial data are classified by how the locations, and the values at ",
            "those locations, are treated. Two types matter here."),
          layout_columns(
            col_widths = c(6, 6),
            div(
              eqbox("Geostatistical",
                    "$$\\{Z(s) : s \\in D\\}$$",
                    "Locations are fixed, values are modelled randomly."),
              p("The locations \\(s \\in D\\) are fixed and the values \\(Z\\) ",
                "are modelled randomly.")
            ),
            div(
              eqbox("Lattice",
                    "$$\\{Z(A_i) : A_i \\subset D\\}, \\quad \\bigcup_{i=1}^{m} A_i = D$$",
                    "Areas are fixed, values are modelled randomly."),
              p("The areas \\(A_i \\subset D\\) are fixed and the values ",
                "\\(Z\\) are modelled randomly.")
            )
          ),
          hr(),
          h5("Tobler's first law of geography"),
          div(style = paste("border-left:3px solid #22D3EE; padding:10px 0 10px 20px;",
                            "margin:6px 0 14px 0; font-size:1.06rem;",
                            "color:#BBD7E0; font-style:italic;"),
              "\"Everything is related to everything else, but near things are ",
              "more related than distant things.\""),
          hr(),
          h5("Spatial dependence and spatial autocorrelation"),
          div(
            defrow("Spatial dependence",
                   "Observations that are close to each other are more similar than observations far apart."),
            defrow("Spatial autocorrelation",
                   HTML(paste0("The extent to which a variable is correlated with itself through space. ",
                               "It looks at the same attribute in different locations, seen through ",
                               "\\(Z(s_i)\\) and \\(Z(s_j)\\): the \\(s\\) indicates the same ",
                               "attribute being observed, while \\(i\\) and \\(j\\) indicate ",
                               "different locations.")))
          ),
          chiprow(
            chip("Positive autocorrelation", "Clusters",
                 paste("Locations with similar values lie close together, so the",
                       "map shows clusters."), tone = "warm"),
            chip("Negative autocorrelation", "Checkerboard",
                 paste("Neighbouring locations tend to hold dissimilar values, so",
                       "the map shows a checkerboard-like alternation."), tone = "cool")
          ),
          div(
            class = "alert note-info",
            tags$b("Applied here. "),
            "The 162 households are point-referenced, so by the classification ",
            "above the data are geostatistical. Moran's I, Geary's C and the LISA ",
            "are lattice methods, so a neighbourhood must be imposed on the points ",
            "before they can be applied. That construction is the subject of the ",
            "Spatial Weights tab of the Spatial Autocorrelation section."
          )
        )
      ),

      nav_panel(
        "Moran's I & Geary's C",
        icon = bsicons::bs_icon("rulers"),
        card_body(
          class = "theory",
          h5("Notation"),
          p("Let \\(Z\\) be a continuous attribute with \\(E[Z(\\mathbf{s})] = \\mu\\) ",
            "and constant variance, let \\(w_{ij}\\) denote the spatial weight ",
            "linking areas \\(i\\) and \\(j\\), let ",
            "\\(w_{\\cdot\\cdot} = \\sum_i \\sum_j w_{ij}\\), and let"),
          eqbox("Sample variance",
                "$$S^2 = \\tfrac{1}{n-1}\\sum_{i=1}^{n}\\bigl(Z(\\mathbf{s}_i) - \\bar{Z}\\bigr)^2$$"),
          hr(),
          h5("Moran's I"),
          eqbox("Equation (1)  |  Moran's I",
                "$$I = \\frac{n}{(n-1)\\,S^2\\,w_{\\cdot\\cdot}} \\sum_{i=1}^{n} \\sum_{j=1}^{n} w_{ij} \\bigl(Z(\\mathbf{s}_i) - \\bar{Z}\\bigr)\\bigl(Z(\\mathbf{s}_j) - \\bar{Z}\\bigr)$$",
                "Cross-products about the mean, weighted by proximity."),
          chiprow(
            chip("Null value", "\\(E[I] = -\\tfrac{1}{n-1}\\)",
                 "The reference is this value, not zero."),
            chip("Above the null", "Similar neighbours",
                 "A location tends to be connected to locations with similar attribute values.",
                 tone = "warm"),
            chip("Below the null", "Dissimilar neighbours",
                 "Connected locations tend to hold dissimilar values.", tone = "cool")
          ),
          hr(),
          h5("Geary's C"),
          p("Geary's \\(C\\) measures the same phenomenon through squared ",
            "differences between neighbouring values rather than through ",
            "cross-products about the mean:"),
          eqbox("Equation (2)  |  Geary's C",
                "$$C = \\frac{1}{2\\,S^2\\,w_{\\cdot\\cdot}} \\sum_{i=1}^{n} \\sum_{j=1}^{n} w_{ij} \\bigl(Z(\\mathbf{s}_i) - Z(\\mathbf{s}_j)\\bigr)^2$$",
                "Squared differences between neighbouring values."),
          chiprow(
            chip("C < 1", "Similar neighbours",
                 "Locations are connected to locations with similar values.", tone = "warm"),
            chip("C > 1", "Dissimilar neighbours",
                 "Locations are connected to locations with dissimilar values.", tone = "cool")
          ),
          hr(),
          h5("Why both are reported"),
          p("The two measures are reported together because Geary's \\(C\\) is the ",
            "more sensitive of the two to differences between immediate ",
            "neighbours, whereas Moran's \\(I\\) responds more to the broader ",
            "pattern; agreement between them is therefore stronger evidence than ",
            "either alone."),
          div(
            defrow("Moran's I", "Responds more to the broader pattern across the study region."),
            defrow("Geary's C", "More sensitive to differences between immediate neighbours."),
            defrow("Direction", "C moves opposite to I: positive autocorrelation is a high I and a low C.")
          ),
          div(
            class = "alert note-warn",
            tags$b("Assumption. "),
            "Both assume a constant mean and variance. If the mean is not ",
            "constant, apparent clustering may reflect nothing more than a trend ",
            "in the mean."
          )
        )
      ),

      nav_panel(
        "Local Indicators (LISA)",
        icon = bsicons::bs_icon("geo-fill"),
        card_body(
          class = "theory",
          p("Global measures summarise the entire study region in a single number ",
            "and so cannot indicate ", tags$em("where"), " clustering occurs, ",
            "which is precisely what is needed if resources are to be directed at ",
            "particular catchments. This is the role of a local indicator of ",
            "spatial association (LISA)."),
          p("A LISA is any statistic that indicates the extent of spatial ",
            "clustering of similar values around a given observation, and whose ",
            "sum over all observations is proportional to a global measure of ",
            "spatial association such as Moran's \\(I\\). In general a LISA for a ",
            "variable \\(y_i\\) observed at location \\(i\\) may be written"),
          eqbox("General form of a LISA",
                "$$L_i = f\\bigl(y_i,\\, y_{J_i}\\bigr)$$"),
          p("where \\(f\\) is a function, \\(y_i\\) is the observed value at ",
            "\\(i\\), and \\(y_{J_i}\\) are the values observed in the ",
            "neighbourhood \\(J_i\\) of \\(i\\). The local Moran statistic is"),
          eqbox("Equation (3)  |  Local Moran",
                "$$I_i = \\frac{n\\,(Z_i - \\bar{Z}) \\sum_{j} w_{ij} (Z_j - \\bar{Z})}{\\sum_{i} (Z_i - \\bar{Z})^2}$$",
                "Sums over all observations in proportion to the global I."),
          chiprow(
            chip("High-High", "Cluster of high values",
                 "A high value surrounded by high values.", tone = "warm"),
            chip("Low-Low", "Cluster of low values",
                 "A low value surrounded by low values.", tone = "cool"),
            chip("High-Low / Low-High", "Spatial outlier",
                 "A value unlike its neighbours: negative local association.")
          ),
          hr(),
          h5("Interpreting local results"),
          p("A significant positive local statistic indicates only that similar ",
            "values are grouped together; it does not identify a covariate as the ",
            "cause of that grouping, so local clustering must not be read as ",
            "evidence of causation."),
          p("Results are also sensitive to the choice of neighbourhood structure ",
            "and spatial weight matrix, and to boundary effects at the edge of the ",
            "study region, where areas have fewer neighbours than those in the ",
            "interior.")
        )
      ),

      nav_panel(
        "Mixed Graphical Models",
        icon = bsicons::bs_icon("diagram-3"),
        card_body(
          class = "theory",
          h5("Why a mixed model is needed"),
          p("The survey data include combined binary resistance markers, ",
            "multi-categorical questionnaire responses and continuous ",
            "measurements. A framework is therefore required that models all ",
            "covariates simultaneously while illustrating the relationships ",
            "between them, which is what a mixed graphical model provides."),
          hr(),
          h5("Graphical models and the meaning of an edge"),
          p("A mixed graphical model is a family of probability distributions ",
            "whose conditional independence structure is represented by a graph. ",
            "Let \\(G = (V,E)\\) be an undirected graph with nodes ",
            "\\(V = \\{1,\\dots,p\\}\\), one per measurement variable, and edges ",
            "\\(E \\subseteq V \\times V\\). Each node \\(v\\) carries a random ",
            "variable \\(X_v\\), collected in \\(X = (X_1,\\dots,X_p)\\)."),
          p("The graph states conditional independence rather than mere ",
            "association."),
          chiprow(
            chip("An edge means", "Direct dependence",
                 paste("Two variables remain dependent after conditioning on",
                       "every other variable in the model."), tone = "warm"),
            chip("No edge means", "Explained away",
                 paste("Any marginal association between them is explained away",
                       "by the remaining covariates."), tone = "cool")
          ),
          hr(),
          h5("Factorisation"),
          p("The joint distribution factorises over the cliques of \\(G\\), where a ",
            "clique \\(C \\subseteq V\\) is a subset of nodes in which every pair ",
            "is connected:"),
          eqbox("Equation (4)  |  Clique factorisation",
                "$$P(X) = \\exp\\left( \\sum_{C \\in \\mathcal{C}} \\theta_C \\phi_C(X_C) - \\Phi(\\theta) \\right)$$",
                "All structural information sits in the zero pattern of theta."),
          p("where \\(\\mathcal{C}\\) is the set of all cliques, \\(\\phi_C\\) is ",
            "the sufficient statistic of clique \\(C\\), and \\(\\Phi(\\theta)\\) ",
            "is a log-normalising constant whose purpose is to make the density ",
            "integrate to one. Since \\(\\Phi(\\theta)\\) carries no structural ",
            "information, the conditional dependence lies entirely in the pattern ",
            "of zero and non-zero entries in \\(\\theta\\)."),
          hr(),
          h5("The mixed model"),
          p("A mixed graphical model allows the node-conditional distribution of ",
            "each variable to be a different member of the exponential family, ",
            "assigned on the basis of its measurement scale, so the sufficient ",
            "statistic function \\(\\phi\\) differs between variables."),
          p("These \\(p\\) node-conditional distributions are consistent with a ",
            "single joint distribution that is Markov with respect to \\(G\\) ",
            "provided each canonical parameter is a linear combination of products ",
            "of its neighbours' sufficient statistics up to order \\(k\\), the ",
            "maximum clique size. Here \\(p\\) is the number of nodes, \\(n\\) the ",
            "number of observations, and \\(k\\) the order of the model.")
        )
      ),

      nav_panel(
        "MGM Estimation",
        icon = bsicons::bs_icon("sliders2"),
        card_body(
          class = "theory",
          h5("Neighbourhood selection"),
          p("Because the joint distribution factorises into univariate ",
            "conditionals from the exponential family, it can be estimated as a ",
            "series of \\(p\\) generalised linear model regressions: the ",
            "neighbourhood \\(N(v)\\) of each node is estimated separately and the ",
            "results combined into the full graph."),
          p("To obtain estimates that are exactly zero, and hence a sparse and ",
            "interpretable graph, each regression carries an \\(\\ell_1\\) penalty, ",
            "giving the LASSO:"),
          eqbox("Equation (5)  |  LASSO",
                "$$\\hat{\\theta} = \\arg\\min_{\\theta} \\left\\{ -\\mathcal{L}(\\theta, X) + \\lambda \\|\\theta\\|_1 \\right\\}$$",
                "The L1 penalty is what makes estimates exactly zero."),
          p("Larger values of \\(\\lambda\\) shrink more parameters to zero, ",
            "yielding a sparser graph; the penalty also ensures identification when ",
            "\\(p > n\\)."),
          hr(),
          h5("Algorithm 1  |  Estimating mixed graphical models via neighbourhood regression"),
          tags$ol(
            tags$li("For each \\(v \\in V\\):",
              tags$ol(type = "a",
                tags$li("Construct the design matrix defined by \\(k\\), the order of the MGM."),
                tags$li("Solve the LASSO problem with regularisation parameter \\(\\lambda\\)."),
                tags$li("Threshold the estimates at \\(\\tau\\)."),
                tags$li("Aggregate interactions with several parameters into a single edge-weight.")
              )),
            tags$li("Combine the edge-weights with the AND- or OR-rule."),
            tags$li("Define \\(G\\) based on the zero / non-zero pattern in the combined parameter vector.")
          ),
          hr(),
          h5("Reconciling the two regressions"),
          p("Since each node is regressed separately, node \\(v\\) may select node ",
            "\\(r\\) as a neighbour while \\(r\\) does not select \\(v\\)."),
          chiprow(
            chip("OR-rule", "Sensitive",
                 "Retains an edge if either regression selects it."),
            chip("AND-rule", "Conservative  |  adopted here",
                 "Retains an edge only if both regressions select it.", tone = "warm")
          ),
          hr(),
          h5("Selecting the regularisation parameter"),
          p("This study uses the extended Bayesian information criterion:"),
          eqbox("Equation (6)  |  Extended BIC",
                "$$\\mathrm{EBIC}_{\\gamma}(\\hat{\\theta}) = -2L(\\hat{\\theta}) + \\hat{s}_0 \\log n + 2\\gamma\\, \\hat{s}_0 \\log p$$",
                "The lambda minimising this is retained."),
          p("The value of \\(\\lambda\\) minimising this is retained. The ",
            "hyper-parameter \\(\\gamma\\) trades sensitivity against precision: ",
            "larger values penalise dense graphs more heavily and return fewer ",
            "edges, while \\(\\gamma = 0\\) recovers the ordinary BIC."),
          div(
            class = "alert note-info",
            tags$b("In the app. "),
            "Every one of these choices -- \\(k\\), \\(\\lambda\\) selection, ",
            "\\(\\gamma\\), the AND/OR rule and the \\(\\tau\\) threshold -- is a ",
            "control in the MGM Explorer sidebar, and changing any of them refits ",
            "the model rather than redrawing a cached one."
          )
        )
      ),

      nav_panel(
        "Data Preparation",
        icon = bsicons::bs_icon("funnel"),
        card_body(
          class = "theory",
          h5("As stated in the report"),
          p("Any duplicate in the dataset must be removed, as mixed graphical ",
            "models assume independent observations. Unmatched entries were ",
            "treated as missing. Covariate headings were standardised so the ",
            "information is easier to work with."),
          hr(),
          h5("What the pipeline actually does"),
          p(class = "eqnote",
            "The cleaning scripts carry out several further steps that affect the ",
            "results and belong in the written methods:"),
          tags$ul(
            tags$li("171 raw rows reduced to 164 households: seven duplicate ",
                    "Study_codes collapsed to their most complete record."),
            tags$li("Two households roughly 62 km from the rest of the sample were ",
                    "excluded, leaving ", tags$b("162"), " for analysis."),
            tags$li("Missing cells filled by median (numeric) or mode ",
                    "(categorical), because mgm() cannot accept NA at all."),
            tags$li("Free-text laboratory results normalised; an ESBL entry naming ",
                    "no organism is treated as missing rather than guessed."),
            tags$li("Two survey versions for water supply and meat consumption ",
                    "coalesced into single variables."),
            tags$li("Education, work status and income collapsed to ordered or ",
                    "nominal categories with the level counts shown in the ",
                    "Variable Dictionary.")
          ),
          div(
            class = "alert note-warn",
            tags$b("Worth reporting. "),
            "Imputation is required by mgm() but is not neutral for a spatial ",
            "statistic. Listwise deletion instead costs 22 households and rebuilds ",
            "the neighbour graph for every variable, which weakens three of the ",
            "six significant covariates. The robust results are refuse collection ",
            "and formal dwelling."
          )
        )
      )
    )
  ),

  # SECTION 3: SPATIAL AUTOCORRELATION
  nav_panel(
    "Spatial Autocorrelation",
    icon = bsicons::bs_icon("bullseye"),
    sec_head("Section 3  |  Research aim 3",
             "Spatial Autocorrelation",
             paste("Which covariates cluster in space, and where. Moran's I and",
                   "Geary's C globally, local indicators site by site, and the",
                   "weights matrix all of it is conditional on.")),
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
            accordion(
              open = FALSE,
              accordion_panel(
                "How to read this table",
                icon = bsicons::bs_icon("info-circle"),
                p("Moran's I and Geary's C, equations (1) and (2). I > -1/(n-1) means a location tends to be connected to locations with similar values; I < -1/(n-1) means connected locations hold dissimilar values. C < 1 is positive autocorrelation and C > 1 negative, so C moves opposite to I. Both are reported because C is the more sensitive of the two to differences between immediate neighbours while I responds to the broader pattern, so agreement between them is stronger evidence than either alone.")
              )
            ),
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
                accordion(
                  open = FALSE,
                  accordion_panel(
                    "Interpretation",
                    icon = bsicons::bs_icon("info-circle"),
                    p("A significant local statistic indicates only that similar values are grouped together. It does not identify a covariate as the cause of that grouping, so local clustering must not be read as evidence of causation.")
                  ),
                  accordion_panel(
                    "Multiple comparisons",
                    icon = bsicons::bs_icon("exclamation-triangle"),
                    p("No site survives Benjamini-Hochberg across the local tests at any replicate count. That is the ordinary situation for a LISA, so this map is exploratory: it identifies candidate neighbourhoods, it does not confirm them. The global table carries the confirmatory weight.")
                  ),
                  accordion_panel(
                    "Binary covariates",
                    icon = bsicons::bs_icon("toggles"),
                    p("At prevalence p, a High-High site contributes (1-p)^2 to I_i and a Low-Low site p^2, so High-High reaches significance more readily. That is a property of the statistic, not a finding.")
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Between Covariates",
          icon = bsicons::bs_icon("grid-3x3"),
          card_body(
            div(class = "alert note-warn", style = "font-size:0.86rem; margin-bottom:14px;",
                bsicons::bs_icon("exclamation-triangle-fill"), " ",
                tags$b("Supplementary. "),
                "Moran's I, Geary's C and the LISA are all univariate: each asks whether one covariate clusters. Lee's L is the bivariate extension, asking whether two covariates cluster in the same places. It is not one of equations (1) to (3), so nothing here should be reported unless a corresponding subsection is added to the methodology first."),
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
            accordion(
              open = FALSE,
              accordion_panel(
                "Why this tab exists",
                icon = bsicons::bs_icon("info-circle"),
                p("Every I and C above is conditional on W. This tab is where that dependence is shown rather than assumed. The connectivity graph draws one line per non-zero weight, so it is a picture of the neighbourhood structure the statistics are computed over.")
              )
            ),
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

  # SECTION 4: MGM EXPLORER
  nav_panel(
    "MGM Explorer",
    icon = bsicons::bs_icon("diagram-3-fill"),
    sec_head("Section 4  |  Research aims 1 and 2",
             "Mixed Graphical Model Explorer",
             paste("Conditional dependencies between covariates of different",
                   "measurement types. Every control in the sidebar refits the",
                   "model rather than redrawing a cached one.")),
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
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("The estimated graph. Every node is one variable; every line is an edge that survived the LASSO penalty and the AND-rule, meaning the two variables stay dependent after conditioning on all the others. Absence of a line is a claim, not a gap in the data."),
                p("Line thickness is the edge weight. Colour is the sign where one is definable: green for positive, red for negative. For an edge involving a variable with more than two categories no sign exists, so it is drawn grey."),
                p("The rings around each node are its predictability, drawn only if that box is ticked in the sidebar: how much of that variable its neighbours account for."),
                p("Node colour is the domain the variable belongs to. Use the sidebar to hide weak edges or to highlight the neighbourhood of one node; neither refits the model.")
              )
            ),
            
          ),
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
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("The same graph as a sortable list, strongest edge first. This is the tab to read numbers off, since a network drawing is good for seeing structure and poor for comparing two similar weights."),
                p("Weight is the aggregated parameter for that pair after thresholding. Sign is positive, negative, or undefined. Undefined is not missing: it means the pair involves a variable with more than two categories, for which no single direction exists."),
                p("The list respects the edge cut-off slider, so it always matches what the network is showing.")
              )
            ),
            
            DTOutput("mgm_edgetab")
          )
        ),
        nav_panel(
          "Predictability",
          icon = bsicons::bs_icon("bar-chart-fill"),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("How much of each variable its neighbours in the network account for. An edge says two variables are connected; predictability says whether those connections amount to anything."),
                p("For continuous and count nodes the measure is R-squared. For categorical nodes it is normalised accuracy: the proportion correctly classified above what guessing the most common category would already achieve, so 0 means the neighbours add nothing."),
                p("A node with many edges but low predictability is weakly determined by the rest of the network. A node with high predictability is one the other covariates genuinely explain, and is the kind worth acting on.")
              )
            ),
            
            card(
              class = "mgm-card",
              card_body(plotOutput("mgm_predplot", height = "520px"))
            ),
            DTOutput("mgm_errtab")
          )
        ),
        nav_panel(
          "Interaction Detail",
          icon = bsicons::bs_icon("zoom-in"),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("The parameters behind a single edge. The network draws one line per pair, but a pair involving a categorical variable with m categories is estimated with several parameters, and the line only shows their aggregate."),
                p("Pick any two nodes and this prints every parameter for that interaction, which is where to look when an edge is surprising and you want to know which category is driving it."),
                p("For an edge involving a variable with more than two categories, the weight shown in the network is the mean absolute value of several parameters. This tab prints them all.")
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6),
              selectInput("mgm_i1", "Node A", choices = NULL),
              selectInput("mgm_i2", "Node B", choices = NULL)
            ),
            verbatimTextOutput("mgm_intdetail")
          )
        ),
        nav_panel(
          "Map",
          icon = bsicons::bs_icon("pin-map"),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("Where each variable in the fitted model actually sits on the ground. The MGM itself has no notion of location, so this is the bridge between the network and the Spatial Autocorrelation section."),
                p("The neighbour-average toggle replaces each household by the mean of its neighbours. Smoothing that way makes clustering visible that individual points can hide."),
                p("Two Moran statistics are printed. The raw one asks whether the variable clusters at all. The residual one asks whether it still clusters after the network has explained what it can: if the raw value is high and the residual is near zero, the covariates in the model already account for the spatial pattern.")
              )
            ),
            
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
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "What this tab shows",
                icon = bsicons::bs_icon("info-circle"),
                p("The registry: every variable available to the model, with the measurement type and number of levels declared for it during cleaning."),
                p("The type column drives everything else. Type g is conditional Gaussian, p is conditional Poisson for non-negative counts, and c is conditional categorical. That declaration decides which exponential-family member each node-conditional regression uses."),
                p("It also decides which spatial statistic a variable is eligible for. A c node with more than two levels has no meaningful numeric ordering, so Moran's I on its integer code would be an artefact of arbitrary numbering; those are expanded into level indicators before the spatial section touches them.")
              )
            ),
            
            DTOutput("mgm_dict")
          )
        )
      )
    )
  ),

  # SECTION 5: CONCLUSION
  nav_panel(
    "Conclusion",
    icon = bsicons::bs_icon("check2-circle"),
    sec_head("Section 5  |  Synthesis",
             "Conclusion",
             paste("What the two analyses say together, and what the limits of",
                   "the design are.")),
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
      class = "alert note-warn",
      tags$b("Spatial analysis objects not available. "),
      if (!is.null(SP_LOAD_ERR)) tags$span(SP_LOAD_ERR, tags$br()),
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
    cols <- ifelse(g$q_BH < 0.05, "#D6455B",
            ifelse(g$p_perm < 0.05, "#F58A5E", "#B9C7CC"))
    op <- par(mar = c(4, 11, 2, 1)); on.exit(par(op))
    bp <- barplot(g$moran_I, horiz = TRUE, names.arg = g$variable, las = 1,
                  col = cols, border = NA, cex.names = 0.62,
                  xlim = range(g$moran_I - 2 * g$sd_perm, g$moran_I + 2 * g$sd_perm),
                  xlab = "Global Moran's I")
    abline(v = mean(g$EI), lty = 2, col = "grey40")
    segments(g$moran_I - 1.96 * g$sd_perm, bp,
             g$moran_I + 1.96 * g$sd_perm, bp, col = "grey30")
    legend("bottomright", bty = "n", cex = 0.85, border = NA,
           fill = c("#D6455B", "#F58A5E", "#B9C7CC"),
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
                       weight = 1, color = "#041F29", fillColor = cols,
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
    abline(a = 0, b = L$I, col = "#D6455B", lwd = 2)
    legend("topleft", bty = "n", cex = 0.8, legend = names(PAL_LISA), pch = 21,
           col = unname(PAL_LISA), pt.bg = unname(PAL_LISA))
    mtext(sprintf("fitted slope = global I = %.3f", L$I), side = 3, line = -1.3,
          adj = 0.98, cex = 0.85, col = "#D6455B")
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
    hist(p$perm$sim, breaks = 40, col = "#DCE7EA", border = "white",
         main = "Joint-permutation null",
         xlab = "Lee's L", xlim = range(p$perm$sim, p$perm$statistic, theory))
    abline(v = theory, col = "#1F7A99", lwd = 2, lty = 2)
    abline(v = p$perm$statistic, col = "#D6455B", lwd = 2)
    legend("topright", bty = "n", cex = 0.75, lwd = 2, lty = c(1, 2),
           col = c("#D6455B", "#1F7A99"),
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
               col = grDevices::adjustcolor("#2E7F96", 0.28), lwd = 0.5)
    }
    points(lon, lat, pch = 21, bg = "#D6455B", col = "white", cex = 0.9)
  })

  output$sp_whist <- renderPlot({
    req(SP_OK)
    cd <- spdep::card(sp_lw()$neighbours)
    op <- par(mar = c(4, 4, 1, 1)); on.exit(par(op))
    hist(cd, breaks = seq(-0.5, max(cd) + 0.5, 1), col = "#5AB8D4",
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
    if (MGM_OK) {
      if (!length(MGM_NOTES)) return(NULL)
      return(div(
        class = "alert note-info",
        tags$b("Object notes: "),
        tags$ul(lapply(MGM_NOTES, tags$li))))
    }
    div(
      class = "alert note-warn",
      tags$b("MGM object not available. "),
      if (!is.null(MGM_LOAD_ERR)) tags$span(MGM_LOAD_ERR, tags$br()),
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
           core   = MGM_CORE,
           full   = MGM_VARS,
           manual = {
             v <- MGM_REG$var[MGM_REG$group %in% input$mgm_domains]
             v <- intersect(v, MGM_VARS)
             if (length(input$mgm_vars)) union(v, input$mgm_vars) else v
           })
  })

  # eventReactive means nothing is recomputed until "Fit Model" is pressed, so
  # dragging the display sliders redraws instantly without refitting.
  mgm_model <- eventReactive(input$mgm_go, {
    req(MGM_OK)
    v <- mgm_selected_vars()
    validate(need(length(v) >= 4, paste0(
      "Only ", length(v), " variable(s) selected; mgm() needs at least four.\n",
      "Preset: ", input$mgm_preset %|z|% "core",
      "   |  core set: ", length(MGM_CORE),
      "   |  all variables: ", length(MGM_VARS),
      if (length(MGM_NOTES)) paste0("\n", paste("Note:", MGM_NOTES, collapse = "\n")) else "")))

    ## Sidebar values, with the sidebar's own defaults as fallbacks so the
    ## start-up fit cannot pass a zero-length argument into mgm().
    k_ord  <- as.numeric(input$mgm_k  %|z|% "2")
    lamSel <- input$mgm_lamSel        %|z|% "EBIC"
    gam    <- input$mgm_gamma         %|z|% 0.25
    rule   <- input$mgm_rule          %|z|% "AND"
    thr    <- isTRUE(input$mgm_thresh %|z|% TRUE)

    idx <- match(v, MGM_VARS)
    validate(need(!anyNA(idx), paste(
      "Not present in the data:", paste(v[is.na(idx)], collapse = ", "))))

    X     <- AIARMS_OBJ$data[, idx, drop = FALSE]
    type  <- MGM_TYPE[idx]
    level <- MGM_LEVEL[idx]
    labs  <- MGM_LABELS[idx]
    grp   <- MGM_REG$group[match(v, MGM_REG$var)]
    grp[is.na(grp)] <- "Other"

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
           pieColor   = ifelse(m$type == "c", "#F58A5E", "#5AB8D4"),
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
            col = ifelse(m$type[o] == "c", "#F58A5E", "#5AB8D4"),
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
    if (isTRUE(input$mgm_maplag) && !is.null(AIARMS_OBJ$W))
      v <- as.numeric(AIARMS_OBJ$W %*% v)
    v
  })

  output$mgm_map <- renderPlot({
    req(MGM_HAS_SPATIAL)
    m  <- mgm_model(); xy <- AIARMS_OBJ$coords; v <- mgm_map_values()
    pal_v <- colorRampPalette(c("#1F7A99", "#F2F7F8", "#D6455B"))(10)
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

  # -----------------------------------------------------------------------
  # LANDING PAGE
  # -----------------------------------------------------------------------

  output$lp_kpi_hh <- renderText(
    if (SP_OK) format(nrow(SPB$X), big.mark = ",") else "-")

  output$lp_kpi_months <- renderText(
    if (is.na(N_MONTHS)) "-" else as.character(N_MONTHS))

  output$lp_kpi_nodes <- renderText(if (MGM_OK) as.character(nrow(MGM_REG)) else "-")
  output$lp_kpi_cov   <- renderText(if (SP_OK) as.character(length(SPB$cov_vars)) else "-")

  output$lp_kpi_sig <- renderText({
    if (!SP_OK) return("-")
    as.character(sum(SPB$global$q_BH < 0.05, na.rm = TRUE))
  })

  output$lp_data_status <- renderUI({
    ok  <- function(x) if (x) bsicons::bs_icon("check-circle-fill") else
                              bsicons::bs_icon("exclamation-circle-fill")
    col <- function(x) if (x) "#3DDC97" else "#FFC15E"
    row <- function(lab, x, note)
      div(style = paste0("color:", col(x), "; font-size:0.88rem; margin-top:6px;"),
          ok(x), " ", tags$b(lab), tags$span(style = "color:#8FAFBC;", paste0("  ", note)))
    csv <- !is.null(find_app_file(DATA_FILE))
    tagList(
      hr(),
      row("Survey CSV",   csv,    if (csv) "loaded" else "not found"),
      row("MGM object",   MGM_OK, if (MGM_OK) paste(nrow(MGM_REG), "nodes") else "not built"),
      row("Spatial results", SP_OK,
          if (SP_OK) paste(length(SPB$cov_vars), "covariates") else "not built"),
      if (length(BUILD_LOG))
        div(style = "color:#8FAFBC; font-size:0.8rem; margin-top:10px; line-height:1.5;",
            tags$b(style = "color:#22D3EE;", "Built this session:"), tags$br(),
            HTML(paste(BUILD_LOG, collapse = "<br/>")))
    )
  })

  # The landing-page buttons move the navbar rather than duplicating content.
  observeEvent(input$jump_method,  nav_select("main_nav", "Methodology"))
  observeEvent(input$jump_spatial, nav_select("main_nav", "Spatial Autocorrelation"))
  observeEvent(input$jump_mgm,     nav_select("main_nav", "MGM Explorer"))

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
