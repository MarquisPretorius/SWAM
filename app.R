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
#   mgm_spatial_core.R          tangled from WST795_analysis.Rmd
#   mgm_spatial_bundle.rds      its exported results
#   output/AIARMS_mgm_spatial.rds   built by WST795_analysis.Rmd (Spatial component)
#
# If any is absent the corresponding section shows a status message and the
# rest of the dashboard is unaffected.

# Raise this whenever the cleaning pipeline or mgm_spatial_core.R changes in a
# way that alters the numbers. Any bundle carrying a different value is treated
# as stale and rebuilt (or refused, on a deployed server).
#   2  w_ii = 0 fix in 01b's weights matrix; blank survey items no longer
#      coded as "No"; joincount_cat column fix.
#   3  site encoding is "dummy_full": one binary node per fieldwork code, all
#      seven present and named. Site-to-site edges are suppressed on display.
#   5  Weights symmetrised by averaging, (B + B')/2 row-standardised, so the
#      spatial section and the model-fitting section use ONE definition. Before
#      this the two disagreed by up to 0.024 on Moran's I.
#   4  Lee's L removed throughout. Section 2.1 of the report defines Moran's I,
#      Geary's C and the local Moran only, and the bivariate statistic was never
#      plotted in the app, so it was carried without being used.
BUNDLE_SCHEMA <- 5L

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
# of WST795_analysis.Rmd and calls the same functions out of
# mgm_spatial_core.R, so the statistics cannot diverge from the report; only
# the assembly is restated here so the app can stand alone.
build_spatial_bundle <- function(obj_path, out_path,
                                 k = 8, nsim_global = 9999) {
  A  <- readRDS(obj_path)
  E  <- expand_registry(A)
  xy <- mgm_coords(A)
  cov_vars <- E$meta$var[E$meta$group != "Spatial"]
  sp_vars  <- E$meta$var[E$meta$group == "Spatial"]
  wcfg <- list(type = "knn", k = k, style = "W")

  G <- global_table(E$X, xy, cov_vars, wcfg = wcfg, nsim = nsim_global, seed = 1)
  G <- merge(G, E$meta[, c("var", "group", "label")], by.x = "variable", by.y = "var")
  G <- G[order(-G$moran_I), ]

  saveRDS(list(X = E$X, meta = E$meta, coords = xy, lonlat = A$lonlat,
               registry = A$registry, W_01b = A$W,
               cov_vars = cov_vars, sp_vars = sp_vars,
               global = G, site_group = A$site_group,
               ## carried so every label, tooltip and printout can name the
               ## fieldwork site rather than an integer code
               site_labels   = A$site_labels,
               site_encoding = A$site_encoding,
               k = k, built = Sys.time(),
               ## bumped whenever the shape or the meaning of the bundle
               ## changes, so a stale .rds is refused rather than displayed
               schema = BUNDLE_SCHEMA), out_path)
  invisible(out_path)
}

# A bundle is stale if it predates the object it was derived from or the core
# script that computed it. Without this check a corrected pipeline leaves the
# OLD numbers on screen, because step 2 below used to run only when the bundle
# was missing entirely -- silently the worst possible failure mode for a
# dashboard whose results go into a report.
bundle_is_stale <- function() {
  b <- find_app_file(SPATIAL_BUNDLE)
  if (is.null(b)) return(TRUE)
  newer_than_bundle <- function(f) {
    p <- find_app_file(f)
    !is.null(p) && isTRUE(file.mtime(p) > file.mtime(b))
  }
  if (newer_than_bundle(MGM_OBJECT) || newer_than_bundle(SPATIAL_CORE)) return(TRUE)
  sc <- tryCatch(readRDS(b)$schema, error = function(e) NULL)
  !identical(sc, BUNDLE_SCHEMA)
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

  # Step 2: the spatial results. About 35 seconds. Rebuilt whenever the bundle
  # is missing OR stale -- see bundle_is_stale().
  if (!is.null(find_app_file(MGM_OBJECT)) &&
      bundle_is_stale() &&
      !is.null(find_app_file(SPATIAL_CORE))) {
    message("Building ", SPATIAL_BUNDLE, " (about 35 seconds) ...")
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
              "sp_vars", "global", "k")
    if (!is.list(SPB) || !all(need %in% names(SPB)))
      stop(bundle_path, " is missing: ",
           paste(setdiff(need, names(SPB)), collapse = ", "))
    ## A bundle from an earlier pipeline holds numbers this version of the
    ## core script would not produce. Showing them would be worse than showing
    ## nothing, so say so instead.
    if (!identical(SPB$schema, BUNDLE_SCHEMA))
      stop(basename(bundle_path), " was built by an earlier version of the ",
           "pipeline (schema ", SPB$schema %|z|% "none", "; this app expects ",
           BUNDLE_SCHEMA, "). Delete it and re-knit WST795_analysis.Rmd, ",
           "which rebuilds the bundle and the core script together. ",
           "On a deployed copy, rebuild locally and redeploy the .rds files.")
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
    MGM_LOAD_ERR <<- paste0(obj_path, " is not the object the Spatial component chunk ",
                            "builds: ", bad, ". Rebuild it, or point MGM_OBJECT ",
                            "at the right file.")
    return(invisible(NULL))
  }
  AIARMS_OBJ   <<- obj
  MGM_REG      <<- obj$registry
  MGM_OBJ_PATH <<- obj_path
  MGM_OK       <<- TRUE
})

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

## ---------------------------------------------------------------------------
## NODE_NOTES -- the written justification for each node
## ---------------------------------------------------------------------------
## Only the PROSE lives here. Type, level, domain, label and the summary
## statistics are read live from the fitted object, so this block can never
## drift out of step with the model that is actually loaded: change
## SITE_ENCODING or a recode in 01_clean_AIARMS.R and the factual columns
## follow automatically, while the explanation stays attached to its variable.
## A node with no entry here still appears, with its facts and a note saying
## the justification has not been written yet.
NODE_NOTES <- list(
  `ESBL_pos_n` = list(
    what = "The number of monthly stool/rectal cultures in which an ESBL-producing organism was recovered, over up to nine sampling months. The free-text laboratory entries were normalised first — twelve spelling variants of an ESBL result appear in the export — and a cell naming no organism is treated as missing rather than guessed.",
    why  = "A tally of events, so a Poisson node. It is the primary AMR outcome of the study: ESBL phenotype is the marker with the clearest resistance interpretation of the three, and the one with enough variation to model (mean 2.0 of up to 9 months).",
    read = "An edge to a WASH or socio-economic node is the result the report exists to find: that exposure predicts ESBL carriage <b>conditional on everything else in the model, including the number of months the household was actually cultured</b>. An edge to <code>Months_tested</code> is not that — it is sampling effort."
  ),
  `EC_pos_n` = list(
    what = "Months positive for <i>Escherichia coli</i>. Carriage is near-universal: 97.1% of all readable EC cells across the cohort are positive (952 of 981).",
    why  = "A count, so Poisson, on the same footing as the other two markers.",
    read = "Read this node with care and say why in the text. Because carriage is saturated, most of the variance in the count comes from <b>how many months the household was cultured</b>, not from whether it was colonised — the correlation with <code>Months_tested</code> is 0.93. Expect its dominant edge to be with exposure.",
    flag = "<b>Why keep it at all?</b> So that all three target markers named in the introduction are represented in the model rather than quietly dropped. That is a defensible choice, but it has to be declared: an earlier version of the pipeline excluded this node precisely because it carries so little independent information, and a reader comparing the two would otherwise wonder."
  ),
  `KP_pos_n` = list(
    what = "Months positive for <i>Klebsiella pneumoniae</i>. Two cells in the KP columns record an E. coli result mis-filed by the laboratory; these become missing rather than being read as negatives.",
    why  = "A count, so Poisson.",
    read = "Of the three markers this one sits at the most informative prevalence — neither saturated like EC nor as sparse as ESBL — so its correlation with exposure (0.66) leaves the most room for a genuine carriage signal. If any carriage node is going to show structure, this is the one to look at first."
  ),
  `Months_tested` = list(
    what = "The number of months in which the household returned a readable culture result for any of the three organisms. Households were sampled an unequal number of times, from none to all nine.",
    why  = "<code>mgm()</code> has <b>no offset term</b>, so a rate model is not available. Conditioning on the exposure as its own covariate is the closest equivalent, and that is the whole reason this node is in the model. It is declared <code>\"g\"</code> rather than <code>\"p\"</code> so that <code>scale = TRUE</code> z-scores it onto the same footing as the other continuous covariates.",
    read = "An edge from a carriage node to this one is a statement about <b>fieldwork, not epidemiology</b>. Its value in the model is that it absorbs that dependence so the remaining carriage edges are closer to rates than to raw counts.",
    flag = "<b>Declare the type choice.</b> This is a bounded 0–9 count declared Gaussian, which is inconsistent with <code>HHsize</code> — also a bounded count — being declared Poisson. Both are defensible; the inconsistency is not, unless it is stated. Four households have <b>zero</b> cultured months, so their carriage counts are structural zeros rather than observed zeros."
  ),
  `Age` = list(
    what = "Age in years of the responding household member.",
    why  = "One of only two genuinely continuous nodes in the model. Unbounded in practice, measured on a real interval scale, and well away from any floor or ceiling — the textbook case for a conditional Gaussian.",
    read = "Edges carry a sign. Age is also the cleanest <b>negative control</b> in the spatial analysis: there is no mechanism by which one household's respondent age should predict a neighbour's, and Moran's <i>I</i> duly finds nothing (−0.022). If it had found clustering here, the method would be suspect."
  ),
  `Sex` = list(
    what = "Sex of the responding household member, coded 1 for female. Respondents were overwhelmingly women — a feature of daytime household surveys, and worth one sentence in the limitations.",
    why  = "Binary, coded 0/1 so that <code>binarySign = TRUE</code> can define a direction for its edges.",
    read = "An edge here describes the <b>respondent</b>, not the household. Given the 76:24 imbalance, treat a weak edge cautiously: there is limited information in the minority class at n = 162."
  ),
  `HHsize` = list(
    what = "Number of people living in the household. The questionnaire asks for people <b>excluding</b> the respondent, so one is added.",
    why  = "A count of people, so a Poisson node.",
    read = "Carries a sign against continuous partners. It also feeds <code>Crowding</code>, so the two are related by construction — an edge between them is not a finding, and they should not be interpreted as independent evidence of the same thing."
  ),
  `Crowding` = list(
    what = "Household size divided by the number of rooms the household lives in. A classic transmission-relevant exposure and the other genuinely continuous node.",
    why  = "A ratio of two counts is a continuous quantity on a real scale, so Gaussian.",
    read = "Interpretable directly: higher values mean more people sharing less space. An edge to a carriage node would be the clearest mechanistic result the model could produce.",
    flag = "<b>A ceiling worth mentioning.</b> The rooms question tops out at \"More than seven\", which is mapped to 8. Households in very large dwellings therefore have their crowding slightly overstated. The effect is small — few households are affected — but the mapping is a decision, not a measurement."
  ),
  `Education` = list(
    what = "The highest level of education completed by anyone in the household, collapsed from 18 raw response options into three: 1 = primary or incomplete secondary, 2 = completed secondary, 3 = tertiary.",
    why  = "Declared unordered with three levels. The collapse is necessary — 18 states cannot be estimated at n = 162 — and the nominal declaration is the conservative choice, since <code>mgm</code> treats every <code>\"c\"</code> node as unordered regardless of whether the underlying scale is ordered.",
    read = "One grey edge per partner, no sign, with the per-level parameters recoverable through <code>showInteraction()</code>. In the spatial analysis it expands into three indicators and only the tertiary one clusters.",
    flag = "<b>The most attackable variable in the analysis.</b> The recode assigns level 3 to any response matching <i>Diploma, Certificate, degree, BTech</i> or <i>Post graduate</i> — which sweeps in \"Certificate with &lt;Std 10/Gr.12\" (18 households) and \"Diploma with &lt;Std 10/ Gr.12\" (7), whose own labels say they were obtained without matric. Move those 25 to level 1 and the clustering of <code>Education_3</code> falls from <code>I = 0.115, p = 0.006</code> to <code>I = 0.043, p = 0.17</code>. Whichever definition you adopt, report the other."
  ),
  `WorkStatus` = list(
    what = "Main activity of the respondent: 1 = employed in any form, 2 = unemployed, 3 = pensioner, student or other.",
    why  = "The clearest case in the whole registry for a nominal declaration. These three states have <b>no natural ordering</b> — a pensioner is not \"more\" of anything than an unemployed person — so any numeric coding would be arbitrary, and the model's answer must not depend on which arbitrary coding was used.",
    read = "Use this node as the worked example when explaining why <code>\"c\"</code> exists. It is also the cleanest illustration of why the spatial analysis expands categoricals: Moran's <i>I</i> on the codes 1, 2, 3 would assert that unemployment sits midway between employment and retirement."
  ),
  `IncomeBand` = list(
    what = "Monthly income before deductions, mapped from the questionnaire's eleven bands onto an ordinal score from 0 (no income) to 9. \"Prefer not to say\" is treated as missing.",
    why  = "Genuinely ordered, and far more informative as a single ordinal score than as eleven categorical states that cannot be estimated — 65 of 162 households sit in one band alone. The trade-off is that Gaussian treatment assumes the bands are equally spaced, which they are not: they roughly double.",
    read = "An edge carries a sign, readable as \"higher income band\". Because the bands are logarithmic in width, read the magnitude loosely.",
    flag = "<b>State the assumption.</b> Treating a band index as Gaussian imposes equal spacing on bands that double in width. A log-income midpoint would be more faithful; the band index was kept because it needs no assumption about where within a band a household sits."
  ),
  `Grant` = list(
    what = "Whether the household receives any government grant or subsidy monthly.",
    why  = "Binary, 0/1.",
    read = "A direct measure of state support and a useful partner to <code>IncomeBand</code>, which it is not redundant with — grant receipt is about eligibility and access, income about total resources."
  ),
  `FormalDwelling` = list(
    what = "Whether the main dwelling is a house or brick/concrete structure, or a room/flatlet on a property — as against a traditional structure or an informal shack.",
    why  = "Binary, 0/1.",
    read = "One of the most robust spatial results (<code>I = 0.147, q = 0.006</code>) and, importantly, one that <b>strengthens</b> under complete-case deletion rather than weakening. Lead with this and refuse collection as the two findings least sensitive to the cleaning decisions."
  ),
  `PipedInside` = list(
    what = "Whether water is piped inside the dwelling, as against a yard tap, a public standpipe or another source. Two versions of the water question exist in the export — an older single-choice item and a newer checkbox block — and the answer is taken from the old item where present, falling back to the checkboxes.",
    why  = "Binary, 0/1, reduced to the distinction that matters for contamination risk: water inside the dwelling or not.",
    read = "Low prevalence (16%) means limited power. No spatial structure was found, but with 26 positives that absence is weak evidence rather than a null result."
  ),
  `FlushToilet` = list(
    what = "Flush or pour-flush toilet, to a sewer or a septic tank, as against a pit latrine, a public toilet or none.",
    why  = "Binary, 0/1. The intermediate group — pit latrine with a slab, 14 households — joins the unimproved arm, since it is too small to support a state of its own at this sample size.",
    read = "A three-level sanitation ladder would be preferable in principle; it was collapsed for power, and that collapse should be stated rather than left implicit in the coding."
  ),
  `ToiletShared` = list(
    what = "Whether the household's toilet is shared with people who are not household members.",
    why  = "Binary, 0/1.",
    read = "Clusters (<code>I = 0.121, q = 0.044</code>), which makes mechanistic sense — a shared facility is shared by neighbours, so the exposure is defined at the street level. Note that it <b>weakens under complete-case deletion</b>, so report it as among the less robust of the significant set."
  ),
  `ToiletFloods` = list(
    what = "Whether the household's toilet ever floods.",
    why  = "Binary, 0/1.",
    read = "Directly relevant to faecal–oral transmission and to wastewater as a medium, which is why it earns a place despite showing no spatial structure of its own."
  ),
  `OpenDefecation` = list(
    what = "Whether any household member ever has to defecate outside the toilet — anything other than \"Never\".",
    why  = "Binary, 0/1. The frequency scale (a few times a year through most days) is collapsed to ever/never because the higher frequencies have only a handful of households each.",
    read = "At 9% prevalence this node has very little power. Its value is descriptive: reporting that open defecation is rare in this cohort is itself a finding for a WASH study, and an absent edge here should not be read as evidence of no effect."
  ),
  `StandingWater` = list(
    what = "Whether there is standing water around the dwelling.",
    why  = "Binary, 0/1.",
    read = "An environmental exposure measured at the household but determined largely by drainage, which is a street-level property — so a priori a candidate for clustering, though none was found here.",
    flag = "<b>Eight households never answered this item.</b> Under the original coding those blanks were read as \"no standing water\". They are now treated as missing and imputed, which is the honest handling but does move the prevalence."
  ),
  `Flooding` = list(
    what = "Whether the area around the house floods at some time.",
    why  = "Binary, 0/1.",
    read = "The second strongest spatial result (<code>I = 0.225, q = 0.004</code>), and mechanistically the most interesting one for a wastewater study: flooding mobilises faecal contamination across properties, so it is exactly the kind of exposure that should be shared between neighbours.",
    flag = "<b>This result depends on a cleaning decision, and the decision goes in your favour.</b> Thirty-two of 164 households never answered the flooding item. The original rule read those blanks as \"does not flood\"; they are now treated as missing. Under the old coding <code>I = 0.116</code>; under the honest coding <code>I = 0.225</code>; restricted to the 131 households that actually answered, <code>I = 0.206 (p = 0.0004)</code>. The old rule was attenuating the finding, not manufacturing it — which is the sentence to write."
  ),
  `RefuseCollected` = list(
    what = "Whether household refuse is removed by the local authority or a private company, as against an own or communal dump.",
    why  = "Binary, 0/1.",
    read = "The strongest spatial result in the study (<code>I = 0.243, q = 0.004</code>, Geary's <code>C = 0.75</code>), and the one to lead with. A refuse round is delivered street by street, so the clustering is not merely consistent with the mechanism — it is what the mechanism predicts. It also strengthens under complete-case deletion, so it is robust to the imputation."
  ),
  `HandwashScore` = list(
    what = "A practice score counting how many of eight prompts (before eating, after using the toilet, after changing a nappy, and so on) the household reports washing hands at. Where a household left some items unanswered, the score is prorated over the items it did answer.",
    why  = "A sum of indicators is exactly what a Poisson node is for. Treating the eight items as separate binary nodes was the earlier approach and had to be abandoned — two of them were positive in 160 of 161 households, which is near-zero variance.",
    read = "Heavily left-skewed: the mean is 7.52 out of 8, so most households report near-universal handwashing and there is very little variance to detect an edge with. An absent edge here is uninformative about behaviour; it is mostly a statement about the ceiling of the instrument.",
    flag = "<b>Half the sample left at least one item blank.</b> Eighty-two of 164 households skipped between one and seven of the eight prompts. Summing \"== Yes\" over the raw block counted every blank as a no and pushed those scores down; the prorated score corrects that, and households answering fewer than five items are treated as missing."
  ),
  `CareMonthly` = list(
    what = "How often the household visits a doctor, clinic, community health centre or hospital, collapsed to monthly-or-more against less often.",
    why  = "Binary, 0/1. \"Never\" has two observations and is merged with \"Yearly\".",
    read = "A proxy for healthcare contact, which matters for AMR because facility exposure is a recognised route for resistant organisms. Note the export's spelling — the raw value is \"Montly\" — which the recode matches deliberately."
  ),
  `HIVinHH` = list(
    what = "Whether anyone in the household is reported as living with HIV.",
    why  = "Binary, 0/1.",
    read = "Survives correction in the spatial analysis (<code>I = 0.112, q = 0.044</code>), and it is the result that complicates the clean \"infrastructure clusters, individual attributes do not\" narrative. Name it rather than leaving it out. Its clustering plausibly reflects the spatial distribution of prevalence and of care-seeking rather than anything about households, and a self-reported household-level measure of this kind carries obvious reporting bias."
  ),
  `ChronicAny` = list(
    what = "Whether any household member is currently managing a chronic condition, excluding HIV. Derived from a select-all question; any response other than \"No chronic conditions\" counts.",
    why  = "Binary, 0/1. The individual conditions are too sparse to model separately at this sample size.",
    read = "A crude indicator of chronic care contact. The collapse loses a great deal — hypertension and asthma have quite different implications for antibiotic exposure — so treat an edge here as a pointer rather than a finding."
  ),
  `DiarrSeverity` = list(
    what = "Typical severity when someone in the household experiences diarrhoea: 1 = no one does, 2 = mild and resolving in one to two days, 3 = moderate or severe.",
    why  = "Declared unordered with three levels, although the levels are substantively ordered. The registry governs, and <code>mgm</code> treats every <code>\"c\"</code> node as nominal.",
    read = "The most direct symptomatic measure of enteric infection in the survey, so a natural partner for the WASH block. Level 1 is not really a severity at all — it is an absence — so the three states mix a presence/absence question with a severity question."
  ),
  `AbxNoRx` = list(
    what = "Whether any household member has ever taken antibiotics without a prescription from a medical professional.",
    why  = "Binary, 0/1.",
    read = "The most direct behavioural driver of AMR in the whole questionnaire, which is why it belongs in the model regardless of whether it produces an edge. Self-reported, so under-reporting is likely and a null result is weak evidence."
  ),
  `AbxSource` = list(
    what = "Where the household usually obtains antibiotics: 1 = does not source them, 2 = government facility only, 3 = private GP or pharmacy, with or without a government facility.",
    why  = "Genuinely nominal — these are different routes, not more or less of anything.",
    read = "Badly unbalanced: 137 of 162 households sit in level 2, leaving 14 and 11 in the others. Any edge involving this node is being driven by very few households and should be treated as hypothesis-generating.",
    flag = "<b>This node also carries the most missingness</b> of any variable in the registry — seven households — and is the main driver of the 22 households lost to complete-case deletion in the sensitivity analysis."
  ),
  `AbxCourse` = list(
    what = "How long household members typically continue antibiotic treatment: 1 = completes the course as advised, 2 = stops when symptoms resolve, 3 = does not take antibiotics.",
    why  = "Nominal. Level 3 is not a point on a completion scale at all — it is non-use — which is precisely why an ordered coding would be wrong here.",
    read = "Level 2 is the behaviour that drives resistance, and it is the one to look at. This node is a good illustration for the methodology of why <code>\"c\"</code> is not just \"a variable with a few values\": the three states are not commensurable."
  ),
  `StreetFood` = list(
    what = "Whether the household consumes street food of any type.",
    why  = "Binary, 0/1, collapsed from the type of street food consumed.",
    read = "A food-safety exposure route. Reaches <code>p = 0.019</code> in the spatial analysis but does not survive correction across 43 tests — a good worked example of why the BH adjustment is reported, and of the difference between a raw and an adjusted p-value."
  ),
  `MeatFreq` = list(
    what = "The highest frequency band at which any meat type is consumed, from the questionnaire's frequency × meat-type grid, scored 0 (never) to 5 (daily). Two versions of the question exist in the export and both are read.",
    why  = "Ordered frequency bands, so the ordering is real information and a Gaussian treatment preserves it. Declared <code>\"g\"</code> for the same reason as <code>IncomeBand</code>.",
    read = "Very concentrated — 108 of 162 households sit at band 4 — so there is little variance to work with. No household scores 0, so the intended 0–5 range is really 1–5 in practice."
  ),
  `AnimalsKept` = list(
    what = "Whether the household keeps any animals.",
    why  = "Binary, 0/1.",
    read = "The One Health link: livestock and poultry are a recognised reservoir for resistant organisms, so this node connects the household survey to the wider AMR literature the introduction cites."
  ),
  `ManureUse` = list(
    what = "Whether the household uses manure of any kind in its garden — human manure from a compost toilet, human manure applied directly, or chicken, cow or horse manure.",
    why  = "Binary, 0/1, an OR across the five manure items the household answered.",
    read = "Now clusters (<code>I = 0.111, q = 0.049</code>), which makes sense as a shared agricultural practice within a settlement. A direct environmental route from animal or human waste to household exposure, so it is substantively interesting for a wastewater study.",
    flag = "<b>This node changed the most under the missing-data correction.</b> Sixty-three of 164 households left all five manure items blank, and the original rule read that as \"does not use manure\" — which took the prevalence from 71% down to 33% and buried the result. Those rows are now missing and imputed. This is the clearest single example of why the blank-as-no rule mattered, and it is worth using as the illustration in the text."
  ),
  `NN_density` = list(
    what = "The number of <b>other</b> study households within 150 m. Median nearest-neighbour distance in the cohort is about 17 m, so a 150 m radius captures the immediate cluster rather than the whole settlement.",
    why  = "A count of points in a disc, so Poisson.",
    read = "This is <b>local context</b>, one of three distinct notions of space in the model. It answers \"how built-up are your immediate surroundings?\" and is the spatial variable most likely to connect to crowding and to the WASH block. Note it counts <i>study</i> households, not all households, so it is a proxy for density conditional on the sampling design.",
    flag = "<b>Spatial by construction, so excluded from the substantive spatial results</b> and reported as a positive control instead. It is defined from the coordinates, so testing it for spatial autocorrelation would be circular."
  ),
  `Site_ARUE` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUE, taken from the alphabetic prefix of the household's study code. 46 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUE than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default.",
    flag = "<b>The collinearity caveat applies to all seven site nodes.</b> Inside a covariate's own regression the seven dummies plus the intercept are rank-deficient, so the lasso picks among equivalent solutions and <i>which</i> site carries an effect can move between bootstrap samples even when the effect itself is stable. Check anything you report against the stability assessment, and against a refit with a single categorical Site node, which is identified."
  ),
  `Site_ARUF` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUF, taken from the alphabetic prefix of the household's study code. 3 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUF than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default.",
    flag = "<b>Three households.</b> Every parameter involving this node is estimated from those three rows, and its positive-control Moran's I of 0.25 sits far below the 0.71–0.91 of every other site. Read any edge that turns on this node as hypothesis-generating, not as a result."
  ),
  `Site_ARUL` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUL, taken from the alphabetic prefix of the household's study code. 24 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUL than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default."
  ),
  `Site_ARUO` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUO, taken from the alphabetic prefix of the household's study code. 32 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUO than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default."
  ),
  `Site_ARUS` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUS, taken from the alphabetic prefix of the household's study code. 17 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUS than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default."
  ),
  `Site_ARUT` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUT, taken from the alphabetic prefix of the household's study code. 25 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUT than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default."
  ),
  `Site_ARUU` = list(
    what = "A 0/1 indicator for membership of fieldwork area ARUU, taken from the alphabetic prefix of the household's study code. 15 of the 162 households belong to it.",
    why  = "Binary, 0/1, so that each site carries its own sign. This is <b>area membership</b> — the first of the three notions of space in the model — capturing discrete between-settlement differences: a shared standpipe, a shared sewer line, a shared refuse round. One node per site means every fieldwork code appears as itself, with no site held out as a reference and no edge that reads as a contrast against another site.",
    read = "An edge to a covariate says that covariate is more or less common in ARUU than elsewhere, conditional on everything else. <b>Ignore edges between site nodes.</b> Every household belongs to exactly one site, so the seven indicators sum to 1 in every row and the clique among them is an artefact of the coding — the app zeroes that block by default."
  ),
  `Site` = list(
    what = "A single categorical node whose seven levels are the fieldwork codes ARUE, ARUF, ARUL, ARUO, ARUS, ARUT and ARUU, taken from the alphabetic prefix of each household's study code. This is the encoding in force when SITE_ENCODING is \"categorical\" rather than \"dummy_full\".",
    why  = "Unordered with seven levels. Every site is a level in its own right, none is held out as a reference, and the model is <b>identified</b> -- which the seven-dummy encoding is not, because those dummies sum to 1 in every row. The cost is that a categorical node with more than two levels carries several parameters per edge, so the network shows one aggregated weight per partner and no sign.",
    read = "One grey edge per partner. Use <code>showInteraction()</code> to recover the per-level parameters, and map the integer codes back with the site labels carried on the object. If you need per-site signs in the network instead, switch to <code>dummy_full</code> and read the caveat attached to the site nodes.",
    flag = "<b>Only one of the two site encodings is active at a time.</b> Whichever is loaded, the other is a one-word change in 01b_add_spatial.R, and reporting both is the honest way to handle the identification trade-off."
  )
)

## Factual columns, read from whatever object is loaded rather than stored.
node_summary <- function(v) {
  if (!MGM_OK) return("")
  j <- match(v, MGM_VARS); if (is.na(j)) return("")
  x  <- AIARMS_OBJ$data[, j]
  ty <- MGM_TYPE[j]; lv <- MGM_LEVEL[j]
  if (ty == "c" && lv == 2)
    sprintf("%d of %d positive (%.1f%%)", sum(x == 1), length(x), 100 * mean(x))
  else if (ty == "c") {
    lab <- if (identical(v, "Site") && length(AIARMS_OBJ$site_labels) >= lv)
             AIARMS_OBJ$site_labels else as.character(sort(unique(x)))
    paste(sprintf("%s = %d", lab, as.integer(table(x))), collapse = ",  ")
  } else
    sprintf("mean %.2f, sd %.2f, range %g-%g", mean(x), sd(x), min(x), max(x))
}

node_type_label <- function(j) {
  if (MGM_TYPE[j] == "g") "\"g\" - Gaussian"
  else if (MGM_TYPE[j] == "p") "\"p\" - Poisson"
  else if (MGM_LEVEL[j] == 2) "\"c\" - binary 0/1"
  else sprintf("\"c\" - %d levels", MGM_LEVEL[j])
}

node_facts <- function() {
  if (!MGM_OK) return(NULL)
  data.frame(
    Variable = MGM_VARS,
    Label    = MGM_LABELS,
    Type     = vapply(seq_along(MGM_VARS), node_type_label, character(1)),
    Domain   = MGM_REG$group[match(MGM_VARS, MGM_REG$var)],
    Summary  = vapply(MGM_VARS, node_summary, character(1)),
    Written  = ifelse(MGM_VARS %in% names(NODE_NOTES), "yes", "-"),
    stringsAsFactors = FALSE, row.names = NULL)
}

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
  bg        = "#0A1A20",   # abyss
  fg        = "#E6EEF1",   # foam
  primary   = "#46C8D8",   # bioluminescent aqua
  secondary = "#2A4A55",   # shelf
  success   = "#3DDC97",   # algal green
  info      = "#6AAEBD",   # shallow water
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

# --- Guide tabs --------------------------------------------------------------
# Each sidebar control is one collapsed accordion panel: the control's name and
# a one-line hint in the header, and inside, what changing it does and where
# that explanation comes from. Sources are the report's reference list only; a
# row with no source describes what the code does and claims no article.
GUIDE_NO_SOURCE <- "Source: app behaviour. Describes what the code does; no article is claimed."

guide_row <- function(control, hint, effect, source = NULL)
  accordion_panel(
    title = div(class = "gtitle",
                span(class = "gname", control),
                span(class = "ghint", hint)),
    value = control,
    div(class = "gbody", effect),
    div(class = "gsrc",
        if (is.null(source)) GUIDE_NO_SOURCE else tagList("Source: ", source)))

guide_group <- function(title, ...)
  div(class = "ggroup",
      div(class = "ggroup-title", title),
      accordion(..., open = FALSE, multiple = TRUE, class = "guide-acc"))

guide_refs <- function(...)
  div(class = "ggroup",
      accordion(accordion_panel(title = div(class = "gtitle", span(class = "gname", "References")),
                                value = "References", ..., icon = bsicons::bs_icon("journal-text")),
                open = FALSE, class = "guide-acc"))

guide_ref <- function(...) p(class = "gref", ...)

# The intro block of a guide, with one button that opens or closes every panel.
guide_intro <- function(...)
  div(class = "guide-intro",
      ...,
      div(class = "guide-bar",
          span(class = "eqnote",
               "Every citation is to the report's reference list. 'App behaviour' ",
               "means the row explains what the code does, with no article claimed."),
          tags$button(type = "button", class = "btn btn-sm btn-outline-info guide-toggle",
                      onclick = "swamToggleGuide(this)", "Expand all")))

# Card headings with the bioluminescent shimmer (see .glow in the stylesheet).
glow_header <- function(title) card_header(span(class = "glow", title))

# Every section opens with the same banner: an eyebrow label, a heading and one
# line saying what the section answers. Consistency here is what stops the app
# reading as a pile of unrelated tabs.
SECTIONS <- c("Introduction", "Methodology", "Spatial Autocorrelation",
              "MGM Explorer", "Conclusion")
SEC_ICON <- c("Introduction" = "house-door-fill", "Methodology" = "gear-wide-connected",
              "Spatial Autocorrelation" = "bullseye", "MGM Explorer" = "diagram-3-fill",
              "Conclusion" = "check2-circle")
sec_id <- function(x) gsub("[^a-z]", "", tolower(x))

# A row of buttons to every OTHER section, shown under each section heading.
# Ids are generated from the pair, and the server registers one observer per
# pair, so adding a section here is the only change needed.
sec_nav <- function(current) {
  others <- setdiff(SECTIONS, current)
  div(class = "sec-nav",
      span(class = "sec-nav-label", "Go to"),
      lapply(others, function(t)
        actionButton(paste0("go_", sec_id(current), "_", sec_id(t)), t,
                     icon  = bsicons::bs_icon(unname(SEC_ICON[t])),
                     class = "btn-sm sec-nav-btn")))
}

sec_head <- function(eyebrow, title, lede) {
  div(class = "sec-head",
      div(class = "eyebrow", eyebrow),
      tags$h2(class = "glow", title),
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
      body, .bslib-page-navbar { background-color: #0A1A20 !important; color: #E6EEF1 !important; }
      .card:not(.mgm-card), .bslib-card:not(.mgm-card), .well, .accordion-body { 
        background-color: #13272F !important; color: #E6EEF1 !important; border: 1px solid #2A4A55 !important; 
      }
      .accordion-button { background-color: #13272F !important; color: #46C8D8 !important; border-bottom: 1px solid #2A4A55 !important; }
      .accordion-button:not(.collapsed) { background-color: #0E2027 !important; color: #46C8D8 !important; }
      .sidebar, .bslib-sidebar-layout > .sidebar { background-color: #0E2027 !important; border-right: 1px solid #2A4A55 !important; }
      .nav-tabs .nav-link.active { background-color: #46C8D8 !important; color: #0A1A20 !important; font-weight: bold; }
      .nav-tabs .nav-link { color: #9DB0B8 !important; }
      .table, .table td, .table th, .dataTables_wrapper { color: #E6EEF1 !important; }
      .form-control, .form-select { background-color: #0A1A20 !important; color: #E6EEF1 !important; border: 1px solid #2A4A55 !important; }
      .value-box { background-color: #13272F !important; border: 1px solid #2A4A55 !important; }
      /* --- Boxes size to their content -------------------------------
         bslib lays cards out as flex/grid fill items, so a card in a row
         stretches to the tallest and its body centres the content in the
         leftover space. These rules turn the body back into ordinary block
         flow and let every card end where its content ends. Cards holding a
         plot, a map or a table are unaffected: those outputs carry an
         explicit pixel height of their own. */
      .bslib-grid > *, .bslib-grid > .card, .bslib-grid > .bslib-card,
      .grid > .card, .grid > .bslib-card { align-self: start !important; }
      .card, .bslib-card, .card.html-fill-container {
        height: auto !important; flex: 0 0 auto !important; }
      .card > .card-body, .card > .card-body.html-fill-item,
      .card-body.bslib-gap-spacing {
        display: block !important; flex: 0 0 auto !important;
        height: auto !important; min-height: 0 !important;
        justify-content: flex-start !important; overflow: visible !important; }
      .card > .card-body > .html-fill-item { flex: 0 0 auto !important; }
      .accordion-body > .card { height: auto !important; }
      .card-body > *:last-child { margin-bottom: 0; }
      .card-body ul, .card-body ol { margin-bottom: 0; padding-left: 1.15rem; }
      .card-body li + li { margin-top: 0.42rem; }

      /* --- Headings carry more weight ---------------------------------- */
      .card-header { font-size: 1.06rem !important; font-weight: 700 !important;
                     letter-spacing: 0.015em; padding: 13px 20px 12px 20px !important;
                     border-left: 3px solid #46C8D8 !important; }
      .card-header:has(.glow) { box-shadow: inset 0 -1px 0 rgba(34,211,238,0.45);
                                font-size: 1.16rem !important;
                                padding: 14px 20px 13px 20px !important; }
      .sec-head { margin: 4px 0 20px 0; }
      .sec-head h2 { font-size: 2.0rem; font-weight: 800; letter-spacing: 0.015em;
                     margin: 2px 0 6px 0; }
      .eyebrow { text-transform: uppercase; letter-spacing: 0.16em;
                 font-size: 0.72rem; font-weight: 700; color: #6AAEBD; }
      h5 { font-size: 1.06rem !important; letter-spacing: 0.015em; }
      h6 { font-size: 0.8rem !important; letter-spacing: 0.13em; color: #9DB0B8 !important; }
      .mgm-card h6 { color: #4A6470 !important; }

      /* --- Title bar ---------------------------------------------------
         The tab strip is hidden. Every section carries its own Go-to row,
         so the navbar is a title bar and nothing else; nav_select() still
         drives the panels underneath. */
      .navbar .navbar-nav, .navbar .nav, .navbar-toggler { display: none !important; }
      .navbar > .container-fluid, .navbar > .container,
      .bslib-page-navbar > .navbar > .container-fluid {
        justify-content: center !important; }
      .navbar-brand { margin: 0 auto !important; float: none !important;
                      font-size: 1.45rem !important; letter-spacing: 0.14em !important;
                      padding: 2px 0 0 0 !important; }
      .navbar { border-bottom: 1px solid #2A4A55 !important;
                padding-top: 10px !important; padding-bottom: 8px !important; }

      /* --- Section navigation ------------------------------------------ */
      .sec-nav { display: flex; flex-wrap: wrap; align-items: center; gap: 8px;
                 margin: -8px 0 22px 0; padding: 10px 14px;
                 background: #0E2027; border: 1px solid #2A4A55;
                 border-radius: 8px; }
      .sec-nav-label { text-transform: uppercase; letter-spacing: 0.15em;
                       font-size: 0.68rem; font-weight: 700; color: #9DB0B8;
                       margin-right: 4px; }
      .sec-nav-btn, .jump-row .btn {
        border: 1px solid #2A4A55 !important; background: #13272F !important;
        color: #C8D9DF !important; font-weight: 600; letter-spacing: 0.01em;
        transition: background 0.15s ease, color 0.15s ease, border-color 0.15s ease; }
      .sec-nav-btn:hover, .jump-row .btn:hover {
        background: #46C8D8 !important; color: #0A1A20 !important;
        border-color: #46C8D8 !important; }
      .jump-row { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 18px; }

      /* Keyless dark basemap: invert plain OSM tiles rather than pull a
         keyed dark-tile service. Applies to the tile images only. */
      .swam-dark-tiles { filter: invert(1) hue-rotate(180deg) brightness(0.93)
                                 contrast(0.86) saturate(0.55); }
      .leaflet-container .leaflet-control-attribution {
        background: rgba(4,31,41,0.72) !important; color: #C8D9DF !important; }
      .leaflet-container .leaflet-control-attribution a { color: #46C8D8 !important; }
      .sp-basemap { margin-bottom: 6px; }
      .sp-basemap .form-group { margin-bottom: 0; }
      .mgm-card { background-color: #FFFFFF !important; color: #1A202C !important; border: 1px solid #C3D9E0 !important; }
      .mgm-card p, .mgm-card h5, .mgm-card div { color: #1A202C !important; }
      .navbar-brand { font-weight: 700 !important; letter-spacing: 0.06em; }

      /* Landing page */
      .hero { background: linear-gradient(135deg, #0E2027 0%, #0A1A20 70%);
              border: 1px solid #2A4A55; border-radius: 8px;
              padding: 34px 38px; margin-bottom: 22px; }
      .hero h1 { font-size: 2.15rem; font-weight: 700; letter-spacing: 0.02em;
                 margin: 0 0 6px 0; color: #FFFFFF; }
      .hero .lede { font-size: 1.05rem; color: #C8D9DF; max-width: 60em;
                    line-height: 1.55; margin-bottom: 4px; }
      .hero .eyebrow { text-transform: uppercase; letter-spacing: 0.18em;
                       font-size: 0.72rem; color: #46C8D8; margin-bottom: 10px; }
      .jump-row .btn { margin: 14px 10px 0 0; font-weight: 600; }
      .aim { border-left: 3px solid #46C8D8; padding: 2px 0 2px 14px;
             margin-bottom: 16px; }
      .aim b { color: #46C8D8; }
      .step-num { display: inline-block; width: 26px; height: 26px;
                  border-radius: 50%; background: #46C8D8; color: #0A1A20;
                  text-align: center; font-weight: 700; line-height: 26px;
                  margin-right: 10px; }
      .theory { line-height: 1.62; }
      .theory .eqnote { color: #9DB0B8; font-size: 0.88rem; }
      .MathJax, .MathJax_Display { color: #E6EEF1 !important; }

      /* --- Section banners: every section opens the same way --- */
      .sec-head { border-left: 4px solid #46C8D8; padding: 2px 0 2px 18px;
                  margin: 4px 0 22px 0; }
      .sec-head .eyebrow { text-transform: uppercase; letter-spacing: 0.16em;
                           font-size: 0.68rem; color: #46C8D8; font-weight: 700;
                           margin-bottom: 6px; }
      .sec-head h2 { font-size: 1.55rem; font-weight: 700; color: #FFFFFF;
                     margin: 0 0 8px 0; letter-spacing: 0.01em; }
      .sec-head .lede { color: #9CBAC6; font-size: 0.97rem; line-height: 1.55;
                        max-width: 62em; margin: 0; }

      /* --- Headings inside cards --- */
      .card-header { font-weight: 700 !important; letter-spacing: 0.03em;
                     background-color: #0E2027 !important;
                     border-bottom: 1px solid #2A4A55 !important;
                     color: #46C8D8 !important; font-size: 0.95rem; }
      h5 { color: #46C8D8; font-weight: 700; font-size: 1.02rem;
           letter-spacing: 0.02em; margin-top: 4px; }
      h6 { color: #E6EEF1; font-weight: 700; font-size: 0.88rem;
           text-transform: uppercase; letter-spacing: 0.09em; }
      .mgm-card h5, .mgm-card h6 { color: #1A202C !important; }
      /* A white panel that is given a header must carry the light treatment
         through it, or the header text inherits the dark-theme colour and
         becomes near-invisible on white. */
      .mgm-card > .card-header { background-color: #FFFFFF !important;
        color: #1A202C !important; border-bottom: 1px solid #C3D9E0 !important;
        border-left: 3px solid #46C8D8 !important; }
      .mgm-card .card-header span, .mgm-card .card-header .glow {
        color: #1A202C !important; -webkit-text-fill-color: #1A202C !important;
        background-image: none !important; animation: none !important;
        filter: none !important; }

      /* --- Tabs: clearer active state, more breathing room --- */
      .nav-tabs { border-bottom: 1px solid #2A4A55 !important; }
      .nav-tabs .nav-link { padding: 10px 18px !important; font-weight: 600;
                            border: none !important; }
      .nav-tabs .nav-link:hover { color: #46C8D8 !important; }
      .nav-tabs .nav-link.active { border-radius: 6px 6px 0 0 !important; }
      .navbar .nav-link { font-weight: 600; letter-spacing: 0.02em; }

      /* --- Value boxes --- */
      .value-box .value-box-title { text-transform: uppercase;
                                    letter-spacing: 0.11em; font-size: 0.7rem;
                                    color: #9DB0B8 !important; }
      .value-box .value-box-value { font-weight: 700; }

      /* --- Rhythm --- */
      .card { margin-bottom: 16px; }
      .card-body { padding: 20px 22px; }
      .theory p { margin-bottom: 0.95rem; }
      .accordion-button { font-weight: 600; font-size: 0.9rem; }
      hr { border-color: #2A4A55 !important; opacity: 1; margin: 18px 0; }
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
                   border-left: 4px solid #46C8D8 !important;
                   padding: 12px 16px; border-radius: 4px; }
      .note-warn b, .note-info b { color: inherit !important; }

      /* --- Equation panels: the maths becomes the focal point --- */
      .eqbox { background: linear-gradient(90deg, rgba(34,211,238,0.07), rgba(34,211,238,0.0));
               border-left: 3px solid #46C8D8; border-radius: 0 6px 6px 0;
               padding: 6px 20px 10px 20px; margin: 14px 0 20px 0; }
      .eqbox .eqlabel { text-transform: uppercase; letter-spacing: 0.15em;
                        font-size: 0.66rem; color: #46C8D8; font-weight: 700;
                        margin-bottom: 2px; }
      .eqbox .eqcap { color: #9DB0B8; font-size: 0.85rem; margin-top: 2px; }

      /* --- Small fact chips used across the methodology tabs --- */
      .chiprow { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 6px; }
      .chip { flex: 1 1 210px; background: #0E2027; border: 1px solid #2A4A55;
              border-top: 3px solid #46C8D8; border-radius: 6px;
              padding: 12px 16px; }
      .chip .k { text-transform: uppercase; letter-spacing: 0.12em;
                 font-size: 0.64rem; color: #9DB0B8; font-weight: 700; }
      .chip .v { font-size: 1.02rem; color: #E6EEF1; font-weight: 700;
                 margin-top: 3px; }
      .chip .d { font-size: 0.82rem; color: #9DB0B8; margin-top: 4px;
                 line-height: 1.45; }
      .chip.warm { border-top-color: #FF6B6B; }
      .chip.cool { border-top-color: #1F7A99; }

      /* --- Definition rows --- */
      .defrow { display: flex; gap: 14px; padding: 9px 0;
                border-bottom: 1px solid rgba(22,87,107,0.55); }
      .defrow:last-child { border-bottom: none; }
      .defrow .term { flex: 0 0 190px; color: #46C8D8; font-weight: 700;
                      font-size: 0.9rem; }
      .defrow .desc { flex: 1; color: #E6EEF1; font-size: 0.92rem;
                      line-height: 1.55; }

      /* --- Guide tabs: one collapsed panel per control --- */
      .guide-intro { max-width: 62em; margin-bottom: 16px; }
      .guide-bar { display: flex; align-items: center; justify-content: space-between;
                   gap: 10px 18px; flex-wrap: wrap; }
      .guide-bar .eqnote { flex: 1 1 22em; }
      .guide-toggle { flex: 0 0 auto; font-weight: 600; }
      .ggroup { margin-bottom: 18px; }
      .ggroup-title { color: #46C8D8; font-weight: 700; font-size: 0.9rem;
                      letter-spacing: 0.02em; margin: 0 0 8px 2px; }
      .guide-acc .accordion-item { background: #0E2027 !important;
                                   border-color: #2A4A55 !important; }
      .guide-acc .accordion-button { background: #0E2027 !important;
                                     color: #E6EEF1 !important; font-weight: 400;
                                     padding: 10px 14px !important;
                                     border-bottom: none !important;
                                     box-shadow: none !important; }
      .guide-acc .accordion-button:not(.collapsed) {
        background: #13272F !important; box-shadow: inset 3px 0 0 #46C8D8 !important; }
      .guide-acc .accordion-button:hover .gname { color: #46C8D8; }
      .guide-acc .accordion-button:focus-visible { outline: 2px solid #46C8D8;
                                                   outline-offset: -2px; }
      .guide-acc .accordion-body { background: #13272F !important; border: none !important;
                                   border-top: 1px solid #2A4A55 !important;
                                   padding: 12px 16px 14px 17px; }
      .gtitle { display: flex; flex-direction: column; gap: 1px; }
      .gname { font-weight: 700; color: #E6EEF1; font-size: 0.9rem;
               transition: color 0.15s ease; }
      .ghint { color: #9DB0B8; font-size: 0.82rem; }
      .gbody { font-size: 0.9rem; line-height: 1.6; color: #E6EEF1; max-width: 68ch; }
      .gsrc { color: #9DB0B8; font-size: 0.8rem; font-style: italic;
              margin-top: 8px; max-width: 68ch; }
      .gref { font-size: 0.85rem; color: #C8D9DF; padding-left: 1.6em;
              text-indent: -1.6em; margin-bottom: 6px; }

      /* --- Glowing headings: light moving through water. Cool colours only,
             because warm colours on this page are reserved for signals. --- */
      .glow { display: inline-block;
              background-image: linear-gradient(100deg, #46C8D8 0%, #3DDC97 18%,
                                #BDF4FF 36%, #46C8D8 50%, #3DDC97 68%,
                                #BDF4FF 86%, #46C8D8 100%);
              background-size: 200% 100%;
              -webkit-background-clip: text; background-clip: text;
              color: transparent; -webkit-text-fill-color: transparent;
              animation: swam-flow 9s linear infinite,
                         swam-breathe 4.5s ease-in-out infinite alternate; }
      @keyframes swam-flow { from { background-position: 0% 50%; }
                             to   { background-position: 200% 50%; } }
      @keyframes swam-breathe {
        from { filter: drop-shadow(0 0 2px rgba(34,211,238,0.30)); }
        to   { filter: drop-shadow(0 0 9px rgba(34,211,238,0.75)); } }
      .card-header:has(.glow) { box-shadow: inset 0 -1px 0 rgba(34,211,238,0.35);
                                font-size: 1.12rem !important; padding: 12px 20px !important; }
      @media (prefers-reduced-motion: reduce) {
        .glow { animation: none; filter: drop-shadow(0 0 5px rgba(34,211,238,0.5)); }
      }
    "))),
    tags$script(HTML("
      function swamToggleGuide(btn) {
        var root = btn.closest('.guide'); if (!root) return;
        var panels = root.querySelectorAll('.accordion-collapse');
        var anyClosed = Array.prototype.some.call(panels,
          function (el) { return !el.classList.contains('show'); });
        panels.forEach(function (el) {
          var c = bootstrap.Collapse.getOrCreateInstance(el, { toggle: false });
          if (anyClosed) c.show(); else c.hide();
        });
        btn.textContent = anyClosed ? 'Collapse all' : 'Expand all';
      }")),
    withMathJax()                  # without this the $$...$$ render as plain text
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
                     icon = bsicons::bs_icon("diagram-3-fill"), class = "btn-primary"),
        actionButton("jump_conclusion", "Conclusion",
                     icon = bsicons::bs_icon("check2-circle"), class = "btn-outline-info")
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

    ## Two columns that each run top to bottom, so the reading order is
    ## left: why -> what data; right: aims -> how to use -> what is measured.
    layout_columns(
      fill = FALSE,
      col_widths = c(7, 5),

      div(
        card(
        glow_header("Why this study"),
        card_body(
        class = "theory",
        p("Water-based epidemiology (WBE) has emerged as a powerful tool in ",
        "public health management, specifically after the COVID-19 pandemic. ",
        "COVID-19 showed that an effective outbreak response requires fast and ",
        "accurate detection, efficient allocation of resources to people in ",
        "need, and the ability to react predictively rather than ",
        "retrospectively."),
        p("Clinical surveillance and WBE have the same objective but different ",
        "ways of achieving it. Clinical surveillance records the state of an ",
        "individual who has consented to testing. WBE measures specific ",
        "biological markers shed by individuals through waste within a ",
        "predefined area, which captures asymptomatic and untested people. ",
        "The two are ", tags$b("complementary"), " tools in disease detection: ",
        "WBE is inexpensive, non-invasive and less biased than clinical ",
        "surveillance used alone, and together they improve the speed of ",
        "disease detection and mapping. Its only real restriction is whether ",
        "the disease of focus leaves a biological marker when shed -- the ",
        "detection of polio in Gaza is one example."),
        p("Spatial statistics shifts the viewpoint from where something is to ",
        "why it may occur there. Demographic-based mapping connects physical ",
        "location with the characteristics of the population present, treated ",
        "as covariates. Elevation, for instance, was found to be associated ",
        "with the distribution of cholera during the 2008-2009 epidemic in ",
        "Harare, Zimbabwe."),
        p("This report focuses on antimicrobial resistance (AMR): the process by ",
        "which a microorganism survives despite the presence of an antibiotic. ",
        "An estimated 4.95 million deaths were associated with bacterial AMR ",
        "in 2019, of which 1.27 million were directly attributed to it -- ",
        "placing AMR among the leading causes of death worldwide, ahead of ",
        "both HIV/AIDS and malaria, with sub-Saharan Africa carrying the ",
        "highest burden."),
        p("AMR suits WBE for two reasons. It is ", tags$b("endemic and ",
        "slow-moving"), ", so a relatively stable pattern can be estimated and ",
        "evaluated; and its markers are ", tags$b("shed into wastewater"), ", ",
        "which allows easy integration with existing wastewater-based ",
        "surveillance.")
        )
        ),
        card(
        glow_header("The data"),
        card_body(
        p("A household survey conducted in KwaZulu-Natal in 2025. 162 ",
        "households were sampled across seven study codes -- ARUE, ARUO, ",
        "ARUT, ARUL, ARUS, ARUU and ARUF -- each individually geolocated, ",
        "over a study area spanning roughly two kilometres in each ",
        "direction. Alongside the survey responses, each household was ",
        "tested over a period of up to nine months for the three AMR ",
        "markers."),
        p("The responses carry several measurement types at once: binary ",
        "indicators, categorical demographic and sanitation responses, ",
        "counts and continuous measurements. Typical correlation and ",
        "regression tools assume a single type, and forcing the data into ",
        "one would discard information and distort the associations being ",
        "detected."),
        p("That mixture is exactly why a mixed graphical model is required ",
        "rather than a single-type network, and why each node in this app ",
        "carries a declared type and level."),
        uiOutput("lp_data_status")
        )
        )
      ),

      div(
        card(
        glow_header("The three aims"),
        card_body(
        div(class = "aim", tags$b("1. "),
        "Investigate the significance of demographic factors through the ",
        "use of mixed graphical models, and then connect those factors to ",
        "both physical location and AMR markers."),
        div(class = "aim", tags$b("2. "),
        "Use spatial autocorrelation on these covariates to investigate ",
        "which factors are spatially significant within KwaZulu-Natal."),
        div(class = "aim", tags$b("3. "),
        "Interpret what was gathered to be able to improve the ",
        "effectiveness of public health intervention.")
        )
        ),
        card(
        glow_header("How to use this app"),
        card_body(
        p(span(class = "step-num", "1"),
        "Read the ", tags$b("Methodology"), " for the definitions and ",
        "equations every result below is computed from."),
        p(span(class = "step-num", "2"),
        tags$b("MGM Explorer"), " answers aim 1 -- the conditional ",
        "dependency network, refitted live as you change its settings."),
        p(span(class = "step-num", "3"),
        tags$b("Spatial Autocorrelation"), " answers aim 2 -- Moran's I, ",
        "Geary's C and the LISA, under a weights matrix you choose."),
        p(span(class = "step-num", "4"),
        tags$b("Conclusion"), " answers aim 3, drawing the two together.")
        )
        ),
        card(
        glow_header("Target markers"),
        card_body(
        ## Names only. Every descriptive claim was removed: nothing in this
        ## app should assert something the report does not cite.
        tags$ul(
        tags$li(tags$i("Escherichia coli"), " (EC)"),
        tags$li(tags$i("Klebsiella pneumoniae"), " (KP)"),
        tags$li("ESBL phenotype")
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
    sec_head("Section 2  |  Background theory",
             "Methodology",
             paste("The definitions, equations and estimation procedure behind",
                   "every number this app reports. Each tab corresponds to a",
                   "subsection of the written report.")),
    sec_nav("Methodology"),
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
      fill = FALSE,
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
          div(style = paste("border-left:3px solid #46C8D8; padding:10px 0 10px 20px;",
                            "margin:6px 0 14px 0; font-size:1.06rem;",
                            "color:#C8D9DF; font-style:italic;"),
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
          h5("The spatial weights"),
          p("Every statistic below is conditional on \\(\\mathbf{W} = [w_{ij}]\\), ",
            "with \\(w_{ii} = 0\\) throughout. Neighbours are the ",
            "\\(k = 8\\) nearest households:"),
          eqbox("Equation (1)  |  Spatial weights",
                "$$w_{ij} = \\begin{cases} 1, & j \\in J_i \\\\ 0, & \\text{otherwise} \\end{cases} \\qquad w_{ii} = 0$$",
                paste("J_i holds the k households nearest to i. The rule is directional,",
                      "so W is symmetrised by averaging it with its transpose before the",
                      "rows are standardised to sum to one.")),
          div(
            class = "alert note-warn",
            tags$b("Why not a distance band. "),
            "The critical-distance rule is the textbook alternative, and it is ",
            "not usable on this cohort. Moran's I decays monotonically with ",
            "distance here, so choosing d at the first peak of the correlogram ",
            "lands at 50-75 m and leaves 14 to 28 of the 162 households with no ",
            "neighbours at all, and a local statistic on an empty neighbourhood ",
            "is undefined. The distance-band and inverse-distance matrices are ",
            "offered in the sidebar as the sensitivity check they are."
          ),
          hr(),
          h5("Moran's I"),
          eqbox("Equation (2)  |  Moran's I",
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
          eqbox("Equation (3)  |  Geary's C",
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
          eqbox("Equation (4)  |  Local Moran",
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
          eqbox("Equation (5)  |  Clique factorisation",
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
          eqbox("Equation (8)  |  LASSO",
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
          eqbox("Equation (10)  |  Extended BIC",
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
      )
    )
  ),

  # SECTION 3: SPATIAL AUTOCORRELATION
  nav_panel(
    "Spatial Autocorrelation",
    icon = bsicons::bs_icon("bullseye"),
    sec_head("Section 3  |  Research aim: where covariates cluster",
             "Spatial Autocorrelation",
             paste("Which covariates cluster in space, and where. Moran's I and",
                   "Geary's C globally, local indicators site by site, and the",
                   "weights matrix all of it is conditional on.")),
    sec_nav("Spatial Autocorrelation"),
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
                          "Distance band"        = "dist"),
              selected = "knn"
            ),
            conditionalPanel(
              "input.sp_wtype == 'knn'",
              sliderInput("sp_k", "k (neighbours):", min = 2, max = 15, value = 8, step = 1)
            ),
            conditionalPanel(
              "input.sp_wtype == 'dist'",
              sliderInput("sp_dband", "Band (metres):", min = 50, max = 1500,
                          value = 400, step = 50)
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
          "Guide",
          icon = bsicons::bs_icon("compass"),
          card_body(
            class = "theory guide",
            guide_intro(
              h5("What the controls in this section do"),
              p(class = "eqnote",
                tags$b("Research aim 2. "),
                "Use spatial autocorrelation on these covariates to investigate which ",
                "factors are spatially significant within KwaZulu-Natal."),
              p("Moran's I, Geary's C and the LISA are lattice methods, but the 162 ",
                "households are point-referenced, so a neighbourhood has to be imposed ",
                "on them before any of these statistics can be computed (Methodology, ",
                "Spatial Data & Dependence). Most of the sidebar controls change that ",
                "neighbourhood, and therefore the weights \\(w_{ij}\\) of equation (1), ",
                "which enter equations (2) to (4). The principle behind every weighting ",
                "choice is Tobler's first ",
                "law: near things are more related than distant things (Tobler, 1970).")
            ),
            layout_columns(
              col_widths = c(6, 6), fill = FALSE,
              div(
                guide_group("Spatial weights (sidebar)",
                  guide_row("Neighbourhood definition", "Which households count as neighbours",
                    tagList(
                      tags$b("k nearest neighbours"), " links each household to its k closest ",
                      "households, so every household has neighbours, and is what the analysis ",
                      "uses. ",
                      tags$b("Distance band"), " links every pair closer than the band, so dense ",
                      "areas get many neighbours and remote households may get none. ",
                      "Both are symmetrised before use: a k-nearest-neighbour graph is ",
                      "directed, so the indicator matrix is averaged with its transpose before the ",
                      "rows are standardised, which is the definition Section 2.1 of the report now ",
                      "gives and the matrix the cleaning pipeline builds."),
                    "Tobler (1970) for the principle that nearer households should carry more weight. Both options themselves are app behaviour."),
                  guide_row("k (neighbours)", "How many neighbours each household gets",
                    tagList(
                      "Only with k nearest neighbours. Under row standardisation each neighbour ",
                      "carries weight 1/k, so a larger k averages each comparison over a wider area ",
                      "and a smaller k keeps it local.")),
                  guide_row("Band (metres)", "How far away a neighbour can be",
                    tagList(
                      "Distance band only. Households closer than this are ",
                      "neighbours. A small band can leave some households with no neighbours at all; ",
                      "the Spatial Weights tab reports how many under 'Isolated units'.")),
                  guide_row("Include Site / density controls", "Adds the built-in spatial checks",
                    tagList(
                      "Adds the site indicators and neighbour density to the global table and to ",
                      "the LISA covariate list. Both are spatial by construction (a site is a patch ",
                      "of the map, and density is defined from the coordinates), so they are expected ",
                      "to cluster. They act as positive controls: if the method could not find ",
                      "clustering in them, nothing else it reported would be believable."),
                    "your analysis in spatial_autocorrelation_MGM.Rmd. No article is claimed.")
                )
              ),
              div(
                guide_group("Inference (sidebar)",
                  guide_row("Permutation replicates", "How precise the p-values are",
                    tagList(
                      "Every p-value comes from reshuffling the values across households many times ",
                      "and counting how often the reshuffled statistic is as extreme as the observed ",
                      "one. More replicates give a finer p-value: the smallest two-sided p possible is ",
                      "2 / (replicates + 1), about 0.002 at 999 and 0.0002 at 9999. For the LISA, each ",
                      "household's own value is held fixed and only the other n - 1 are reshuffled. ",
                      "All replicates for the global statistics are computed as a single matrix ",
                      "product."),
                    "Anselin (1995) for holding each household's own value fixed in the LISA permutation; Amgalan et al. (2022) for evaluating the statistic as one matrix product."),
                  guide_row("Recompute", "When the global table updates",
                    tagList(
                      "The global table updates only when you press Recompute, or tick the ",
                      "site / density box. The LISA tab and the weights diagnostics respond ",
                      "straight away. The sensitivity plot always uses its own eight fixed definitions."))
                ),
                guide_group("Local Indicators (LISA) tab",
                  guide_row("Covariate", "Which variable is mapped",
                    "Chooses which variable is mapped, plotted and tabulated."),
                  guide_row("Significance", "Which households are highlighted",
                    tagList(
                      "Decides which households are coloured on the LISA layer and filled in the ",
                      "scatterplot. The two p options are unadjusted, one test per household. ",
                      tags$b("q < 0.05 (BH)"), " applies the Benjamini-Hochberg correction across all ",
                      "the local tests; in this analysis no household survives it, so the map is ",
                      "exploratory: it points to candidate neighbourhoods rather than confirming them. ",
                      "This control does not affect the raw-value layer."),
                    "Anselin (1995), who treats multiple comparisons for local statistics as an open problem. The Benjamini-Hochberg correction has no source in the reference list."),
                  guide_row("Map layer", "What the map colours show",
                    tagList(
                      tags$b("LISA quadrants"), " classify each household by its own value and its ",
                      "neighbours' average, equation (4): High-High and Low-Low are clusters of similar ",
                      "values, High-Low and Low-High are households unlike their neighbours. The two ",
                      "negative quadrants stay on the map rather than being folded into a hotspot summary. ",
                      tags$b("Raw values"), " shows the covariate itself, with no test."),
                    "Anselin (1995) for the LISA; Hu et al. (2020) for keeping the negative quadrants.")
                ),
                guide_refs(
                  guide_ref("Amgalan, A., Mujica-Parodi, L. R. and Skiena, S. S. (2022). Fast spatial ",
                            "autocorrelation. ", tags$i("Knowledge and Information Systems", .noWS = "after"), ", 64(4)."),
                  guide_ref("Anselin, L. (1995). Local indicators of spatial association -- LISA. ",
                            tags$i("Geographical Analysis", .noWS = "after"), ", 27(2), 93-115."),
                  guide_ref("Hu, L., Chun, Y. and Griffith, D. A. (2020). Uncovering a positive and ",
                            "negative spatial autocorrelation mixture pattern: a spatial analysis of ",
                            "breast cancer incidences in Broward County, Florida, 2000-2010. ",
                            tags$i("Journal of Geographical Systems", .noWS = "after"), ", 22(3)."),
                  guide_ref("Mtshawu, B., Bezuidenhout, J. and Kilel, K. K. (2023). Spatial ",
                            "autocorrelation and hotspot analysis of natural radionuclides to study ",
                            "sediment transport. ", tags$i("Journal of Environmental Radioactivity", .noWS = "after"), ", 264."),
                  guide_ref("Tobler, W. R. (1970). A computer movie simulating urban growth in the ",
                            "Detroit region. ", tags$i("Economic Geography", .noWS = "after"), ", 46(2), 234-240.")
                )
              )
            )
          )
        ),
        nav_panel(
          "Global I and C",
          icon = bsicons::bs_icon("table"),
          card_body(
            accordion(
              open = FALSE,
              accordion_panel(
                "How to read this table",
                icon = bsicons::bs_icon("info-circle"),
                p("Moran's I and Geary's C, equations (2) and (3). I > -1/(n-1) means a location tends to be connected to locations with similar values; I < -1/(n-1) means connected locations hold dissimilar values. C < 1 is positive autocorrelation and C > 1 negative, so C moves opposite to I. Both are reported because C is the more sensitive of the two to differences between immediate neighbours while I responds to the broader pattern, so agreement between them is stronger evidence than either alone.")
              )
            ),
            layout_columns(
      fill = FALSE,
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
      fill = FALSE,
              col_widths = c(4, 3, 5),
              uiOutput("sp_lvar_ui"),
              selectInput("sp_lsig", "Significance:",
                          choices = c("p < 0.05 (unadjusted)" = "p05",
                                      "p < 0.01 (unadjusted)" = "p01",
                                      "q < 0.05 (BH)"         = "q05"),
                          selected = "p05"),
              radioButtons("sp_llayer", "Map layer:", inline = TRUE,
                           choices = c("LISA quadrants" = "lisa",
                                       "Raw values"     = "raw"))
            ),
            leafletOutput("sp_lmap", height = "460px"),
            layout_columns(
      fill = FALSE,
              col_widths = c(7, 5),
              card(
                class = "mgm-card",
                card_body(plotOutput("sp_lscatter", height = "440px"))
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
      fill = FALSE,
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
    sec_head("Section 4  |  Research aim: how covariates relate",
             "Mixed Graphical Model Explorer",
             paste("Conditional dependencies between covariates of different",
                   "measurement types. Every control in the sidebar refits the",
                   "model rather than redrawing a cached one.")),
    sec_nav("MGM Explorer"),
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
            checkboxInput("mgm_rings", "Show predictability rings", TRUE),
            checkboxInput("mgm_nosite", "Hide site-to-site edges", TRUE)
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
          "Guide",
          icon = bsicons::bs_icon("compass"),
          card_body(
            class = "theory guide",
            guide_intro(
              h5("What the controls in this section do"),
              p(class = "eqnote",
                tags$b("Research aim 1. "),
                "Investigate the significance of demographic factors through the use of ",
                "mixed graphical models, and then connect those factors to both physical ",
                "location and AMR markers. Physical location enters the model as the seven ",
                "fieldwork site nodes and neighbour density; the AMR markers are the three ",
                "carriage nodes."),
              p("The model is estimated as one penalised regression per variable, and those ",
                "regressions are combined into a single graph (Methodology, Algorithm 1; ",
                "Haslbeck and Waldorp, 2020). An edge means two variables stay dependent after ",
                "conditioning on every other variable in the model. ",
                tags$b("Variables"), " and ", tags$b("Estimation"), " change the model itself and ",
                "take effect only when you press Fit Model; ", tags$b("Display"), " changes only ",
                "the drawing and applies straight away.")
            ),
            layout_columns(
              col_widths = c(6, 6), fill = FALSE,
              div(
                guide_group("Variables (sidebar)",
                  guide_row("Core set / All variables / Pick manually", "Which variables enter the model",
                    tagList(
                      "Because every edge is conditional on all the other variables, adding or ",
                      "removing variables can make an existing edge appear or disappear: it changes ",
                      "what each pair is conditioned on. ", tags$b("Core set"), " is a smaller ",
                      "selection, so fewer parameters are estimated from the same 162 households. ",
                      tags$b("All variables"), " is the full registry. ", tags$b("Pick manually"),
                      " starts from whole domains; 'Fine-tune nodes' adds individual variables on top ",
                      "of those domains but does not remove any. At least four variables are needed."),
                    "Haslbeck and Waldorp (2020) for edges as conditional dependence. The contents of the core set are a choice made in your pipeline."),
                  guide_row("Site nodes", "How to read the seven fieldwork sites",
                    tagList(
                      "Space enters the model as ", tags$b("seven binary nodes"), ", one per ",
                      "fieldwork code: Site ARUE, ARUF, ARUL, ARUO, ARUS, ARUT and ARUU. Every site ",
                      "is a node in its own right. None is held out as a reference, so no edge is a ",
                      "contrast of one site against another, and because binary nodes are coded 0/1 ",
                      "with binarySign = TRUE each carries its own ", tags$b("sign"), " -- green for ",
                      "positive, red for negative. An edge from Site ARUU to a covariate says that ",
                      "covariate is more (or less) common in ARUU than elsewhere, conditional on ",
                      "everything else in the model."),
                    "Haslbeck and Waldorp (2020) for the 0/1 coding and edge signs. The choice of one node per site is a modelling decision in your pipeline."),
                  guide_row("Hide site-to-site edges", "Why that box is ticked",
                    tagList(
                      "Every household belongs to exactly one site, so the seven indicators sum to 1 ",
                      "in every row. Each site node's own regression is therefore determined exactly ",
                      "by the other six, and the lasso returns a dense clique of strong edges among ",
                      "them. That clique is a property of the coding, not of the settlements, and ",
                      "left in it crowds out the edges this encoding exists to show. It is zeroed on ",
                      "display by default; untick the box to see it. ",
                      tags$b("Site-to-covariate edges are never touched by this control."),
                      " One further consequence to carry into the write-up: inside a covariate's own ",
                      "regression the seven dummies plus the intercept are rank-deficient, so which ",
                      "individual site carries an effect can move between bootstrap samples even ",
                      "when the effect itself is stable. Check anything you intend to report against ",
                      "the Stability tab, and against a refit with one categorical Site node ",
                      "(SITE_ENCODING <- \"categorical\" in 01b), which is identified."),
                    "The collinearity of a full dummy set is standard; the display suppression is app behaviour."),
                  guide_row("ARUF", "A level with three households",
                    tagList(
                      "ARUF contains three households. Its parameters are estimated from those three ",
                      "rows, and as a positive control its Moran's I is 0.25 against 0.71 to 0.91 for ",
                      "every other site. Treat any edge that turns on ARUF as hypothesis-generating ",
                      "rather than as a result."))
                ),
                guide_group("Estimation (sidebar)",
                  guide_row("Interaction order (k)", "Pairs only, or three-way too",
                    tagList(
                      tags$b("Pairwise (k = 2)"), " estimates edges between pairs of variables. ",
                      tags$b("Include 3-way (k = 3)"), " also estimates interactions among three ",
                      "variables at once, which means many more parameters and a longer fit. k is the ",
                      "maximum clique size, and it defines the design matrix in step 1 of Algorithm 1. ",
                      "The network and edge list show the pairwise part of a k = 3 fit, and Interaction ",
                      "Detail cannot display one."),
                    "Haslbeck and Waldorp (2020), Algorithm 1."),
                  guide_row("Select lambda by", "How the penalty is chosen",
                    tagList(
                      "Lambda is the penalty. A larger lambda shrinks more parameters to exactly zero and ",
                      "gives a sparser graph. ", tags$b("EBIC"), " keeps the lambda that minimises ",
                      "equation (10); this is the method used in the report. ",
                      tags$b("Cross-validation"), " instead picks lambda by predictive performance across ",
                      "10 folds. Because ARUF has only 3 households, the app redraws the folds (up to 10 ",
                      "times) if a fold leaves too few of them to fit."),
                    "Haslbeck and Waldorp (2020), Algorithm 1 step 2, for the role of lambda; Foygel and Drton (2010) for the EBIC. Cross-validation has no source in the reference list."),
                  guide_row("EBIC gamma", "How sparse the graph is",
                    tagList(
                      "Only with EBIC. Gamma trades sensitivity against precision: larger values penalise ",
                      "dense graphs more heavily and return fewer edges, and gamma = 0 is the ordinary BIC."),
                    "Foygel and Drton (2010)."),
                  guide_row("Combine neighbourhoods with", "When an edge is kept",
                    tagList(
                      "Each variable's regression chooses its own neighbours, so variable v can select r ",
                      "while r does not select v. ", tags$b("AND"), " keeps an edge only if both ",
                      "regressions select it; it is the conservative rule and the one used in the report. ",
                      tags$b("OR"), " keeps an edge if either regression selects it, so it returns more edges."),
                    "Haslbeck and Waldorp (2020), Algorithm 1 step 5."),
                  guide_row("Apply beta-min threshold (tau)", "Removes very small edges",
                    tagList(
                      "When ticked, estimates smaller than the threshold tau are set to zero before the ",
                      "edges are combined, which removes very small edges. When unticked, every non-zero ",
                      "estimate left by the penalty is kept, so more weak edges can appear."),
                    "Haslbeck and Waldorp (2020), Algorithm 1 step 3. The exact threshold rule mgm applies is not described in the reference list."),
                  guide_row("Fit Model", "When changes take effect",
                    "Nothing under Variables or Estimation takes effect until this is pressed.")
                )
              ),
              div(
                guide_group("Display (sidebar)",
                  guide_row("Hide edges weaker than", "Hides weak edges from view",
                    tagList(
                      "Hides edges below the chosen weight in the network and in the Edges table. The ",
                      "model is not refitted, so hidden edges are still part of the estimated graph.")),
                  guide_row("Highlight neighbourhood of", "Focus on one node's edges",
                    "Shows only the edges attached to the chosen node. The model is not refitted."),
                  guide_row("Layout", "Where the nodes are drawn",
                    "Changes only where the nodes are drawn. Node positions are not estimates."),
                  guide_row("Show predictability rings", "How well each node is explained",
                    tagList(
                      "Draws a ring around each node showing how much of that variable its neighbours in ",
                      "the network account for: R-squared for continuous and count variables, normalised ",
                      "accuracy for categorical ones (see the Predictability tab)."),
                    "none in the reference list. This row describes the app's calculation.")
                ),
                guide_refs(
                  guide_ref("Foygel, R. and Drton, M. (2010). Extended Bayesian information criteria for ",
                            "Gaussian graphical models. ", tags$i("Advances in Neural Information ",
                            "Processing Systems", .noWS = "after"), ", 23, 604-612."),
                  guide_ref("Haslbeck, J. M. B. and Waldorp, L. J. (2020). mgm: Estimating time-varying ",
                            "mixed graphical models in high-dimensional data. ",
                            tags$i("Journal of Statistical Software", .noWS = "after"), ", 93(8), 1-46.")
                )
              )
            )
          )
        ),
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
      fill = FALSE,
              col_widths = c(6, 6),
              selectInput("mgm_i1", "Node A", choices = NULL),
              selectInput("mgm_i2", "Node B", choices = NULL)
            ),
            verbatimTextOutput("mgm_intdetail")
          )
        ),
        nav_panel(
          "Node Dictionary",
          icon = bsicons::bs_icon("journal-text"),
          card_body(
            class = "theory",
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
            h5("Every node, and why it is in the model"),
            p("Putting a variable in the model commits you to three separate ",
              "claims, and a methodology section that answers only the first is ",
              "the one an examiner pushes on. Each entry below answers all three: ",
              tags$b("why it is here"), " (what the survey asked and what the ",
              "recode made of it), ", tags$b("why this distribution"), ", and ",
              tags$b("how to read a result"), " that involves it."),
            p(class = "eqnote",
              "Type, level, domain and the summary statistics are read live from ",
              "the loaded object, so this section always describes the model you ",
              "actually have in front of you rather than a remembered one. Only ",
              "the written justification is stored."),
            uiOutput("nd_status"),
            selectInput("nd_pick", "Node:",
                        choices = MGM_REG$var %|z|% character(0),
                        selected = (MGM_REG$var %|z|% "")[1], width = "320px"),
            uiOutput("nd_detail"),
            hr(),
            h5("All nodes at a glance"),
            p("Sortable and searchable. The summary column is computed from the ",
              "cleaned frame after imputation and after the two geocoding outliers ",
              "were dropped."),
            DTOutput("nd_table")
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
    sec_nav("Conclusion"),
    layout_column_wrap(
      width = 1,
      card(
        glow_header("Synthesis of Findings"),
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
      "Both are written by WST795_analysis.Rmd: the export chunk writes the bundle and ",
      "the tangle chunk writes the core script. One knit of that document produces ",
      "everything the app loads, including output/AIARMS_mgm_spatial.rds."
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
    ## Row standardisation throughout, so that the spatial lag is the MEAN of
    ## the neighbouring values and every household contributes equally however
    ## many neighbours it has. Section 2.1 of the report states this once; the
    ## binary and globally standardised alternatives were a control nothing in
    ## the analysis used, and explaining three conventions to justify one is
    ## not a good trade against a ten-page limit.
    switch(input$sp_wtype %|z|% "knn",
      knn  = list(type = "knn",  k = input$sp_k %|z|% SPB$k, style = "W"),
      dist = list(type = "dist", d = input$sp_dband %|z|% 400, style = "W"))
  })

  # Weights are always built here. make_listw() now symmetrises by averaging,
  # so the k = 8 default reproduces the matrix the cleaning pipeline builds
  # exactly; the separate "01b matrix" option was a duplicate and has gone.
  sp_Wmat <- reactive({
    req(SP_OK)
    listw_to_W(do.call(make_listw, c(list(coords = SPB$coords), sp_wcfg())))
  })

  # Geary and the LISA need a listw object.
  sp_lw <- reactive({
    req(SP_OK)
    do.call(make_listw, c(list(coords = SPB$coords), sp_wcfg()))
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
    ## Getis-Ord Gi* is not computed: the report defines Moran's I, Geary's C
    ## and the local Moran only, and a statistic the methodology never defines
    ## has no place on the map.
    list(li = li, I = moran_I(SPB$X[[v]], lw))
  })

  output$sp_lmap <- renderLeaflet({
    req(SP_OK)
    L <- sp_lisa(); li <- L$li; v <- input$sp_lvar
    dd <- data.frame(lon = SPB$lonlat[, 1], lat = SPB$lonlat[, 2],
                     site = SPB$site_group)
    if (identical(input$sp_llayer, "raw")) {
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
    ## BASEMAP. Fixed: plain OpenStreetMap tiles, darkened by a CSS filter on
    ## the tile pane (class swam-dark-tiles). OSM is community-run and needs no
    ## API key, so the "API KEY REQUIRED" watermark that CartoDB.DarkMatter
    ## started stamping in August 2026 cannot come back. The filter touches the
    ## tiles only, so the LISA colours on the markers are exactly as specified.
    m <- leaflet(dd) %>%
      addTiles(options = tileOptions(className = "swam-dark-tiles")) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = 6, stroke = TRUE,
                       weight = 1.2, color = "#EAF6FA", fillColor = cols,
                       fillOpacity = opac, popup = lab)
    if (identical(input$sp_llayer, "lisa")) {
      m <- m %>% addLegend("bottomright", colors = c(unname(PAL_LISA), PAL_NS),
                           labels = c(names(PAL_LISA), "not significant"),
                           opacity = 1, title = v)
    }
    m
  })

  ## MORAN SCATTERPLOT.
  ## Three things made the earlier version hard to read, and each is handled
  ## explicitly here. (1) Most covariates are counts or binaries, so Z(s_i) -
  ## Zbar takes a handful of distinct values and the points stack exactly on
  ## top of one another; a small deterministic jitter on x separates them
  ## while leaving y, which is continuous, untouched. (2) Significant and
  ## non-significant sites were nearly the same mark; now the non-significant
  ## ones are small and faint and the significant ones are large and solid, so
  ## the eye lands on the sites that carry the result. (3) The fitted line has
  ## slope equal to global I, which is often near zero, and it then lies on
  ## top of the horizontal reference line; the zero axes are therefore dashed
  ## grey drawn first, the fit is a heavier semi-transparent line drawn over
  ## them, and the legend names it so the two cannot be confused.
  output$sp_lscatter <- renderPlot({
    req(SP_OK)
    L <- sp_lisa(); li <- L$li; I <- L$I
    v <- input$sp_lvar
    sig_lab <- switch(input$sp_lsig %|z|% "p05",
                      q05 = "q < 0.05, BH", p01 = "p < 0.01", "p < 0.05")

    x0 <- li$Zc; y <- li$lag_Zc; n <- length(x0)
    ux <- sort(unique(x0)); disc <- length(ux) <= 12 && length(ux) > 1
    x  <- x0
    if (disc) {                       # seeded, so the plot does not twitch
      set.seed(11)                    # between redraws of the same variable
      x <- x0 + stats::runif(n, -0.19, 0.19) * min(diff(ux))
    }
    q <- li$quadrant; sg <- li$sig

    op <- par(mar = c(7.6, 4.8, 3.8, 1.6), xpd = FALSE); on.exit(par(op))
    xr <- range(x); yr <- range(y)
    xr <- xr + c(-1, 1) * 0.06 * diff(xr)
    yr <- yr + c(-1, 1) * 0.08 * diff(yr)
    plot(NA, xlim = xr, ylim = yr, xlab = "", ylab = "", las = 1,
         cex.axis = 0.95, bty = "n")
    u <- par("usr")

    ## faint quadrant tints, so a quadrant can be read without the legend
    rect(0, 0, u[2], u[4], col = adjustcolor(PAL_LISA["High-High"], 0.055), border = NA)
    rect(u[1], u[3], 0, 0, col = adjustcolor(PAL_LISA["Low-Low"],   0.055), border = NA)
    rect(0, u[3], u[2], 0, col = adjustcolor(PAL_LISA["High-Low"],  0.055), border = NA)
    rect(u[1], 0, 0, u[4], col = adjustcolor(PAL_LISA["Low-High"],  0.055), border = NA)

    pad <- c(diff(u[1:2]), diff(u[3:4])) * 0.015
    text(u[2] - pad[1], u[4] - pad[2], "High-High", adj = c(1, 1), cex = 0.78,
         font = 2, col = adjustcolor(PAL_LISA["High-High"], 0.85))
    text(u[1] + pad[1], u[3] + pad[2], "Low-Low",   adj = c(0, 0), cex = 0.78,
         font = 2, col = adjustcolor("#0E9BB8", 0.90))
    text(u[2] - pad[1], u[3] + pad[2], "High-Low",  adj = c(1, 0), cex = 0.78,
         font = 2, col = adjustcolor("#D98B55", 0.90))
    text(u[1] + pad[1], u[4] - pad[2], "Low-High",  adj = c(0, 1), cex = 0.78,
         font = 2, col = adjustcolor("#5FA9BA", 0.95))

    abline(h = 0, v = 0, col = "grey50", lty = 2, lwd = 1.2)
    abline(a = 0, b = I, col = adjustcolor("#243B47", 0.72), lwd = 3)

    points(x[!sg], y[!sg], pch = 21, cex = 0.9, lwd = 0.6,
           bg = adjustcolor(PAL_NS, 0.20), col = adjustcolor(PAL_NS, 0.55))
    points(x[sg],  y[sg],  pch = 21, cex = 1.7, lwd = 1.1,
           bg = unname(PAL_LISA[q[sg]]), col = "#0B2027")

    title(main = sprintf("Moran scatterplot: %s", v), cex.main = 1.15, line = 2.3)
    mtext(sprintf("n = %d sites  |  %d significant (%s)  |  fitted slope = global I = %.3f%s",
                  n, sum(sg), sig_lab, I,
                  if (disc) "  |  x jittered for display" else ""),
          side = 3, line = 0.7, cex = 0.82, col = "#4A5B63")
    mtext("value at site i, centred", side = 1, line = 2.6, cex = 0.95)
    mtext("mean of the k neighbours, centred (spatial lag)", side = 2,
          line = 3.1, cex = 0.95)

    ## legend sits in the bottom margin, positioned in LINE units so that it
    ## clears the axis title at every panel size (a data-range offset does not)
    legend(x = mean(u[1:2]), y = grconvertY(3.25, "lines", "user"),
           xjust = 0.5, yjust = 1, xpd = NA, bty = "n", ncol = 3,
           cex = 0.82, y.intersp = 1.15,
           legend = c(names(PAL_LISA), "not significant", "fitted slope = I"),
           pch    = c(rep(21, 5), NA),
           lty    = c(rep(NA, 5), 1), lwd = c(rep(NA, 5), 3), seg.len = 1.6,
           pt.cex = c(1.5, 1.5, 1.5, 1.5, 0.9, NA),
           col    = c(rep("#0B2027", 4), adjustcolor(PAL_NS, 0.55),
                      adjustcolor("#243B47", 0.72)),
           pt.bg  = c(unname(PAL_LISA), adjustcolor(PAL_NS, 0.20), NA),
           text.col = "#22303A", x.intersp = 0.8)
  })

  output$sp_ltab <- renderTable({
    req(SP_OK)
    li <- sp_lisa()$li
    tb <- as.data.frame.matrix(
      table(li$quadrant, ifelse(li$sig, "significant", "not significant")))
    cbind(Quadrant = rownames(tb), tb)
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
      # CV folds are random. ARUF has only 3 households, so about one draw in
      # four leaves a training fold with 0 or 1 of them and glmnet refuses the
      # site node's regression. Redrawing the folds when (and only when) that
      # happens is equivalent to stratifying the folds on the rare category.
      fit_mgm <- function() {
        for (attempt in 1:10) {
          f <- tryCatch(do.call(mgm, args), error = function(e) e)
          if (!inherits(f, "error")) return(f)
          if (!(lamSel == "CV" && grepl("1 or 0 observations", conditionMessage(f))))
            stop(f)
        }
        stop(f)
      }
      fit <- tryCatch(fit_mgm(), error = function(e)
        validate(need(FALSE, paste0(
          "mgm() failed: ", conditionMessage(e), "\n",
          if (lamSel == "CV" && grepl("1 or 0 observations", conditionMessage(e)))
            paste0("  A category with very few households (ARUF has 3) ended up with ",
                   "0 or 1 of them in a cross-validation training fold on 10 fold ",
                   "draws in a row. Select lambda by EBIC, or fit a variable set ",
                   "without that category.\n"),
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

  # --- Node Dictionary -----------------------------------------------------
  output$nd_status <- renderUI({
    if (MGM_OK) return(NULL)
    div(class = "alert note-warn",
        tags$b("No model object loaded. "),
        "The dictionary reads type, level and the summary statistics from ",
        "output/AIARMS_mgm_spatial.rds. Run the cleaning sections of the ",
        "analysis document and reopen the app.")
  })

  output$nd_detail <- renderUI({
    req(MGM_OK)
    v <- input$nd_pick %|z|% MGM_VARS[1]
    j <- match(v, MGM_VARS); req(!is.na(j))
    nt <- NODE_NOTES[[v]]
    qa <- function(k, txt) div(class = "defrow",
                               div(class = "term", k),
                               div(class = "desc", HTML(txt)))
    tagList(
      chiprow(
        chip("Node", v, MGM_LABELS[j], tone = "warm"),
        chip("Distribution", node_type_label(j), paste("Domain:", MGM_REG$group[match(v, MGM_REG$var)])),
        chip("In this sample", node_summary(v), sprintf("n = %d households", nrow(AIARMS_OBJ$data)))
      ),
      if (is.null(nt))
        div(class = "alert note-warn",
            tags$b("No written justification for this node yet. "),
            "It is in the registry and in the model, but nothing here explains ",
            "why it carries the distribution it does. Add an entry to ",
            "NODE_NOTES keyed by the variable name.")
      else
        tagList(
          qa("Why it is here",        nt$what),
          qa("Why this distribution", nt$why),
          qa("Reading a result",      nt$read),
          if (!is.null(nt$flag)) div(class = "alert note-warn", HTML(nt$flag))
        )
    )
  })

  output$nd_table <- renderDT({
    req(MGM_OK)
    datatable(node_facts(), rownames = FALSE, selection = "none",
              options = list(pageLength = 15, scrollX = TRUE,
                             order = list(list(3, "asc"))))
  })

  # Which of the displayed nodes are site indicators. Resolved by variable
  # name rather than by label, so it survives a relabelling.
  mgm_site_idx <- reactive({
    m <- mgm_model(); req(m)
    v <- MGM_VARS[match(m$labels, MGM_LABELS)]
    which(!is.na(v) & grepl("^Site_", v))
  })

  mgm_wadj_display <- reactive({
    m <- mgm_model(); req(m)
    w <- m$fit$pairwise$wadj
    w[abs(w) < (input$mgm_cut %|z|% 0)] <- 0   # display cut-off only, not a refit

    # SITE-TO-SITE EDGES. Under the dummy_full encoding every household is in
    # exactly one site, so the seven indicators sum to 1 in every row. Each
    # site node's own nodewise logistic regression is then perfectly separated
    # by the other six and glmnet returns a dense clique among them. That
    # clique is a property of the coding, not of the settlements, and it
    # crowds out the edges the encoding exists to show. Zeroed on display by
    # default; untick the box to see it. Site-to-COVARIATE edges are never
    # touched.
    if (isTRUE(input$mgm_nosite %|z|% TRUE)) {
      si <- mgm_site_idx()
      if (length(si) > 1L) w[si, si] <- 0
    }
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
    print(showInteraction(m$fit, int = c(i, j)))
    # A categorical site node is coded 1..m; show which level is which site.
    site_nm <- AIARMS_OBJ$site_vars %|z|% character(0)
    v_ij    <- MGM_VARS[match(c(input$mgm_i1, input$mgm_i2), MGM_LABELS)]
    if (identical(AIARMS_OBJ$site_encoding, "categorical") &&
        any(v_ij %in% site_nm) && length(AIARMS_OBJ$site_labels))
      cat("\nSite levels: ", paste(sprintf("%d = %s", seq_along(AIARMS_OBJ$site_labels),
                                             AIARMS_OBJ$site_labels), collapse = ",  "), "\n")
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
          ok(x), " ", tags$b(lab), tags$span(style = "color:#9DB0B8;", paste0("  ", note)))
    csv <- !is.null(find_app_file(DATA_FILE))
    tagList(
      hr(),
      row("Survey CSV",   csv,    if (csv) "loaded" else "not found"),
      row("MGM object",   MGM_OK, if (MGM_OK) paste(nrow(MGM_REG), "nodes") else "not built"),
      row("Spatial results", SP_OK,
          if (SP_OK) paste(length(SPB$cov_vars), "covariates") else "not built"),
      if (length(BUILD_LOG))
        div(style = "color:#9DB0B8; font-size:0.8rem; margin-top:10px; line-height:1.5;",
            tags$b(style = "color:#46C8D8;", "Built this session:"), tags$br(),
            HTML(paste(BUILD_LOG, collapse = "<br/>")))
    )
  })

  # The landing-page buttons move the navbar rather than duplicating content.
  observeEvent(input$jump_method,  nav_select("main_nav", "Methodology"))
  observeEvent(input$jump_spatial, nav_select("main_nav", "Spatial Autocorrelation"))
  observeEvent(input$jump_mgm,     nav_select("main_nav", "MGM Explorer"))
  observeEvent(input$jump_conclusion, nav_select("main_nav", "Conclusion"))

  ## One observer per ordered pair of sections, for the sec_nav() button rows.
  for (.a in SECTIONS) for (.b in setdiff(SECTIONS, .a)) local({
    from <- .a; to <- .b
    observeEvent(input[[paste0("go_", sec_id(from), "_", sec_id(to))]],
                 nav_select("main_nav", to), ignoreInit = TRUE)
  })

}

# --- RUN SHINY APPLICATION ---
shinyApp(ui = ui, server = server)
