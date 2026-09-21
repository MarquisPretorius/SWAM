options(shiny.sanitize.errors = FALSE)

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(mgm)
library(qgraph)

# leaflet is optional: the map falls back to a static plot without it.
HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE)

# --- REPRODUCIBILITY --------------------------------------------------------
# Cross-validation folds are drawn at random, so without a fixed seed the same
# settings return a different graph on every run. These two constants are the
# values to quote in the report; the Specification panel prints them back
# alongside the package versions actually loaded.
# --- IDENTITY ---------------------------------------------------------------
# Change these three and every heading, tab title and browser title follows.
APP_NAME  <- "CLIQUE"
APP_STRAP <- "Conditional dependencies in household antimicrobial resistance"
APP_TITLE <- paste("Variable selection in mixed graphical models:",
                   "antimicrobial resistance in KwaZulu-Natal")

MGM_SEED     <- 20260913L
MGM_CV_FOLDS <- 10L

# --- BASEMAP ----------------------------------------------------------------
# OpenStreetMap's own tile server, given as an explicit URL rather than through
# addProviderTiles(): that function resolves a name against the
# leaflet.providers registry, and any entry there whose API-key placeholder is
# unfilled serves tiles with "API key required" printed across them. OSM has no
# key, no account and no placeholder, so the failure mode cannot arise.
# The tiles are light, so they are darkened in CSS to match the theme rather
# than by switching to a dark-styled provider, since every dark provider worth
# using is keyed.
BASEMAP_URL  <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
BASEMAP_ATTR <- paste0(
  '&copy; <a href="https://www.openstreetmap.org/copyright">',
  'OpenStreetMap</a> contributors')

DATA_FILE  <- "full_AIARMS_df.csv"
MGM_OBJECT <- file.path("output", "AIARMS_mgm_spatial.rds")

find_app_file <- function(filename) {
  if (file.exists(filename)) return(filename)
  p <- file.path(getwd(), filename);            if (file.exists(p)) return(p)
  p <- file.path(getwd(), basename(filename));  if (file.exists(p)) return(p)
  p <- file.path("C:/Users/Marquis/Desktop/Honours Research", filename)
  if (file.exists(p)) return(p)
  NULL
}

# Length-safe default: a sidebar input on a panel that has not been opened yet
# can still be NULL or zero-length when the start-up fit runs.
`%|z|%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# --- SELF-BUILDING PIPELINE -------------------------------------------------
# Builds output/AIARMS_mgm_spatial.rds on first launch. Restricted to
# interactive sessions with a writable app folder; deployed copies must ship
# the .rds alongside app.R.
BUILD_ON_START <- interactive() && file.access(".", 2L) == 0L

CLEAN_SCRIPTS <- c("01_clean_AIARMS.R", "01_clean.R")
NODE_SCRIPTS  <- c("01b_add_spatial.R", "01b_spatial.R")
CLEAN_CHUNK   <- "Data Cleaning"
NODE_CHUNK    <- "Spatial component"

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

find_pipeline_rmd <- function() {
  for (f in list.files(".", pattern = "\\.Rmd$", ignore.case = TRUE))
    if (!is.null(rmd_chunk(f, CLEAN_CHUNK)) && !is.null(rmd_chunk(f, NODE_CHUNK)))
      return(f)
  NULL
}

# Run in a SEPARATE process: the first script opens with rm(list = ls()) and the
# second resolves its prerequisites against the global environment.
run_pipeline <- function() {
  if (is.null(find_app_file(DATA_FILE)))
    stop(DATA_FILE, " not found; the cleaning step needs it.")

  resolve_step <- function(script_names, chunk_label) {
    for (nm in script_names) {
      f <- find_app_file(nm)
      if (!is.null(f)) return(normalizePath(f, winslash = "/"))
    }
    rmd <- find_pipeline_rmd()
    if (is.null(rmd))
      stop("Cannot find ", paste(script_names, collapse = " or "),
           ", and no .Rmd here contains a '", chunk_label, "' chunk.")
    tmp <- file.path(tempdir(), paste0(gsub("[^A-Za-z0-9]+", "_", chunk_label), ".R"))
    writeLines(rmd_chunk(rmd, chunk_label), tmp)
    normalizePath(tmp, winslash = "/")
  }

  f1 <- resolve_step(CLEAN_SCRIPTS, CLEAN_CHUNK)
  f2 <- resolve_step(NODE_SCRIPTS,  NODE_CHUNK)

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
if (BUILD_ON_START && is.null(find_app_file(MGM_OBJECT))) {
  message("Building ", MGM_OBJECT, " ...")
  ok <- tryCatch({ run_pipeline(); TRUE },
                 error = function(e) { BUILD_LOG <<- c(BUILD_LOG,
                   paste("Cleaning pipeline failed:", conditionMessage(e))); FALSE })
  if (ok) BUILD_LOG <- c(BUILD_LOG, "Built output/AIARMS_mgm_spatial.rds from the cleaning scripts.")
  for (l in BUILD_LOG) message("  ", l)
}

# --- LOAD THE FITTED-MODEL OBJECT -------------------------------------------
# Nothing here may stop the app: anything wrong leaves MGM_OK FALSE and a
# message for the section to display.
MGM_OK <- FALSE; AIARMS_OBJ <- NULL; MGM_REG <- NULL
MGM_OBJ_PATH <- NULL; MGM_LOAD_ERR <- NULL
local({
  obj_path <- find_app_file(MGM_OBJECT)
  if (is.null(obj_path)) {
    here <- tryCatch(sort(list.files(".", recursive = TRUE, no.. = TRUE)),
                     error = function(e) character(0))
    MGM_LOAD_ERR <<- paste0(
      MGM_OBJECT, " was not found. Working directory: ", getwd(),
      ". Files deployed alongside the app: ",
      if (length(here)) paste(utils::head(here, 40), collapse = ", ") else "(none)", ".")
    return(invisible(NULL))
  }
  obj <- tryCatch(readRDS(obj_path), error = function(e) e)
  if (inherits(obj, "error")) {
    MGM_LOAD_ERR <<- paste0("Could not read ", obj_path, ": ", conditionMessage(obj))
    return(invisible(NULL))
  }
  bad <- NULL
  if (!is.list(obj)) bad <- "it is not a list"
  else if (!all(c("data", "registry") %in% names(obj)))
    bad <- paste0("it has no $", paste(setdiff(c("data", "registry"), names(obj)),
                                       collapse = " and no $"))
  else if (is.null(ncol(obj$data)) || ncol(obj$data) < 1)
    bad <- "$data is not a matrix or data frame"
  else if (is.null(nrow(obj$registry)) || nrow(obj$registry) < 1)
    bad <- "$registry is empty"
  if (!is.null(bad)) {
    MGM_LOAD_ERR <<- paste0(obj_path, " is not the object the cleaning pipeline builds: ",
                            bad, ". Rebuild it, or point MGM_OBJECT at the right file.")
    return(invisible(NULL))
  }
  AIARMS_OBJ <<- obj; MGM_REG <<- obj$registry
  MGM_OBJ_PATH <<- obj_path; MGM_OK <<- TRUE
})

# Each field is resolved with a fall-back drawn from the registry, and what was
# actually used is reported in the section's status line.
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
  drop_core <- setdiff(MGM_CORE, MGM_VARS)
  if (length(drop_core)) {
    MGM_CORE  <- intersect(MGM_CORE, MGM_VARS)
    MGM_NOTES <- c(MGM_NOTES, paste("core variables absent from the data and dropped:",
                                    paste(drop_core, collapse = ", ")))
  }
  # Duplicate labels break every match() on labels, so make them unique once.
  if (anyDuplicated(MGM_LABELS)) {
    MGM_LABELS <- make.unique(as.character(MGM_LABELS), sep = " ")
    MGM_NOTES  <- c(MGM_NOTES, "duplicate node labels were made unique for display")
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
## NODE_NOTES -- the written justification for each node. Only PROSE lives
## here; type, level, domain, label and the summary statistics are read live
## from the fitted object, so this block cannot drift out of step with the
## model that is actually loaded.
## ---------------------------------------------------------------------------
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
    read = "Edges carry a sign, so an edge to a continuous or binary partner reads directly as older or younger respondents. There is no strong prior reason for age to attach to anything here, which makes it a useful reference point: if it acquires many strong edges, look hard at the specification before believing them."
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
    read = "One grey edge per partner, no sign, with the per-level parameters recoverable through <code>showInteraction()</code>.",
    flag = "<b>The most attackable variable in the analysis.</b> The recode assigns level 3 to any response matching <i>Diploma, Certificate, degree, BTech</i> or <i>Post graduate</i> — which sweeps in \"Certificate with &lt;Std 10/Gr.12\" (18 households) and \"Diploma with &lt;Std 10/ Gr.12\" (7), whose own labels say they were obtained without matric. Whichever definition you adopt, report the other."
  ),
  `WorkStatus` = list(
    what = "Main activity of the respondent: 1 = employed in any form, 2 = unemployed, 3 = pensioner, student or other.",
    why  = "The clearest case in the whole registry for a nominal declaration. These three states have <b>no natural ordering</b> — a pensioner is not \"more\" of anything than an unemployed person — so any numeric coding would be arbitrary, and the model's answer must not depend on which arbitrary coding was used.",
    read = "Use this node as the worked example when explaining why <code>\"c\"</code> exists. Any numeric treatment would assert that unemployment sits midway between employment and retirement, which is not a claim anyone would defend."
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
    read = "Dwelling type is the most robust of the infrastructure measures: it is observable rather than reported, so it is the least sensitive of the WASH block to the cleaning decisions and to recall. An edge here is a good candidate to lead with."
  ),
  `PipedInside` = list(
    what = "Whether water is piped inside the dwelling, as against a yard tap, a public standpipe or another source. Two versions of the water question exist in the export — an older single-choice item and a newer checkbox block — and the answer is taken from the old item where present, falling back to the checkboxes.",
    why  = "Binary, 0/1, reduced to the distinction that matters for contamination risk: water inside the dwelling or not.",
    read = "Low prevalence (16%, 26 positives) means limited power, so an absent edge here is weak evidence rather than a null result."
  ),
  `FlushToilet` = list(
    what = "Flush or pour-flush toilet, to a sewer or a septic tank, as against a pit latrine, a public toilet or none.",
    why  = "Binary, 0/1. The intermediate group — pit latrine with a slab, 14 households — joins the unimproved arm, since it is too small to support a state of its own at this sample size.",
    read = "A three-level sanitation ladder would be preferable in principle; it was collapsed for power, and that collapse should be stated rather than left implicit in the coding."
  ),
  `ToiletShared` = list(
    what = "Whether the household's toilet is shared with people who are not household members.",
    why  = "Binary, 0/1.",
    read = "A shared facility is an exposure defined outside the household, so an edge to a carriage node here is mechanistically easy to argue. It is also self-reported, so read a null cautiously."
  ),
  `ToiletFloods` = list(
    what = "Whether the household's toilet ever floods.",
    why  = "Binary, 0/1.",
    read = "Directly relevant to faecal–oral transmission and to wastewater as a medium, which is why it earns a place in the model regardless of whether it produces an edge."
  ),
  `OpenDefecation` = list(
    what = "Whether any household member ever has to defecate outside the toilet — anything other than \"Never\".",
    why  = "Binary, 0/1. The frequency scale (a few times a year through most days) is collapsed to ever/never because the higher frequencies have only a handful of households each.",
    read = "At 9% prevalence this node has very little power. Its value is descriptive: reporting that open defecation is rare in this cohort is itself a finding for a WASH study, and an absent edge here should not be read as evidence of no effect."
  ),
  `StandingWater` = list(
    what = "Whether there is standing water around the dwelling.",
    why  = "Binary, 0/1.",
    read = "An environmental exposure measured at the household but determined largely by drainage, which is a property of the street rather than of the dwelling — so an edge to a site node is as plausible here as an edge to a carriage node.",
    flag = "<b>Eight households never answered this item.</b> Under the original coding those blanks were read as \"no standing water\". They are now treated as missing and imputed, which is the honest handling but does move the prevalence."
  ),
  `Flooding` = list(
    what = "Whether the area around the house floods at some time.",
    why  = "Binary, 0/1.",
    read = "Mechanistically the most interesting environmental exposure for a wastewater study: flooding mobilises faecal contamination across properties, so it is exactly the kind of exposure that should be shared between neighbouring households rather than confined to one.",
    flag = "<b>This node depends on a cleaning decision.</b> Thirty-two of 164 households never answered the flooding item. The original rule read those blanks as \"does not flood\"; they are now treated as missing and imputed. State which rule you used, because the prevalence — and therefore every edge involving this node — moves with it."
  ),
  `RefuseCollected` = list(
    what = "Whether household refuse is removed by the local authority or a private company, as against an own or communal dump.",
    why  = "Binary, 0/1.",
    read = "A refuse round is delivered street by street, so this is a service-delivery variable rather than a household choice. That makes an edge to a site node expected, and an edge to a carriage node the more interesting result."
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
    read = "A self-reported household-level measure of this kind carries obvious reporting bias, so treat any edge as a pointer to care-seeking and facility contact rather than as a statement about immune status."
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
    read = "A food-safety exposure route, and one of the few exposures in the registry that happens away from the dwelling. That makes it a useful contrast to the WASH block, which is entirely about the property itself."
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
    read = "A direct environmental route from animal or human waste to household exposure, so it is substantively interesting for a wastewater study. It is also a shared agricultural practice within a settlement, so an edge to a site node is expected.",
    flag = "<b>This node changed the most under the missing-data correction.</b> Sixty-three of 164 households left all five manure items blank, and the original rule read that as \"does not use manure\" — which took the prevalence from 71% down to 33%. Those rows are now missing and imputed. This is the clearest single example of why the blank-as-no rule mattered, and it is worth using as the illustration in the text."
  ),
  `NN_density` = list(
    what = "The number of <b>other</b> study households within 150 m. Median nearest-neighbour distance in the cohort is about 17 m, so a 150 m radius captures the immediate cluster rather than the whole settlement.",
    why  = "A count of points in a disc, so Poisson.",
    read = "This is <b>local context</b>, one of the two ways location enters the model — the other being area membership, carried by the categorical <code>Site</code> node. It answers \"how built-up are your immediate surroundings?\" and is the node most likely to connect to crowding and to the WASH block. Unlike <code>Site</code> it is an ordinary count, so its edges carry a sign and its predictability is read normally.",
    flag = "<b>Defined from the coordinates, not from the questionnaire.</b> It counts <i>study</i> households rather than all households, so it is a proxy for density conditional on the sampling design. Read it as a design-dependent measure of local context, not as a census figure."
  ),
  `Site` = list(
    what = "A single categorical node whose seven levels are the fieldwork codes ARUE, ARUF, ARUL, ARUO, ARUS, ARUT and ARUU, taken from the alphabetic prefix of each household's study code. This is how location enters every fit in this app: whatever the loaded object ships with, the seven binary indicators are replaced by this node before estimation.",
    why  = "Unordered with seven levels. Every site is a level in its own right, none is held out as a reference, and the model is <b>identified</b> -- which the seven-dummy encoding is not, because those dummies sum to 1 in every row. The cost is that a categorical node with more than two levels carries several parameters per edge, so the network shows one aggregated weight per partner and no sign.",
    read = "One grey edge per partner, and no sign — an interaction with a seven-level node is specified by several parameters and the network draws the mean of their absolute values. The per-level parameters are not lost: the Interaction Detail tab prints them along with the level key, which is where to look to say <i>which</i> settlement drives an edge and in which direction. Note that an edge here is conditional on every other covariate, so it is not a statement about raw prevalence by site; for that, use the By site panel in Data Visualisation.",
    flag = "<b>This is the encoding the app fits.</b> Whatever the object ships with, the seven indicators are replaced by this node before estimation, because the indicator set is not identified and this is."
  )
)

## Factual columns, read from whatever object is loaded rather than stored.
## The seven Site_* columns the object ships with are replaced by one
## categorical Site node before fitting, so the dictionary lists Site instead
## of them. Everything else is the registry as loaded.
DICT_VARS <- if (MGM_OK) {
  sv <- grep("^Site_", MGM_VARS, value = TRUE)
  if (length(sv)) c(setdiff(MGM_VARS, sv), "Site") else MGM_VARS
} else character(0)

SITE_LV <- if (!MGM_OK) character(0) else
  (AIARMS_OBJ$site_labels %|z|%
     sort(unique(AIARMS_OBJ$site_group %|z|% character(0))))

node_summary <- function(v) {
  if (!MGM_OK) return("")
  if (identical(v, "Site") && !("Site" %in% MGM_VARS)) {
    sg <- AIARMS_OBJ$site_group %|z|% AIARMS_OBJ$site
    if (is.null(sg)) return("")
    tb <- table(sg)
    return(paste(sprintf("%s = %d", names(tb), as.integer(tb)), collapse = ",  "))
  }
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
  if (is.na(j)) return(sprintf("\"c\" - %d levels", max(length(SITE_LV), 2L)))
  if (MGM_TYPE[j] == "g") "\"g\" - Gaussian"
  else if (MGM_TYPE[j] == "p") "\"p\" - Poisson"
  else if (MGM_LEVEL[j] == 2) "\"c\" - binary 0/1"
  else sprintf("\"c\" - %d levels", MGM_LEVEL[j])
}

node_facts <- function() {
  if (!MGM_OK) return(NULL)
  j <- match(DICT_VARS, MGM_VARS)
  data.frame(
    Variable = DICT_VARS,
    Label    = ifelse(is.na(j), DICT_VARS, MGM_LABELS[j]),
    Type     = vapply(j, node_type_label, character(1)),
    Domain   = ifelse(is.na(j), "Spatial",
                      MGM_REG$group[match(DICT_VARS, MGM_REG$var)]),
    Summary  = vapply(DICT_VARS, node_summary, character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}


# Node palette. Any domain not listed falls back to PAL_MGM_OTHER rather than
# handing qgraph an NA colour.
PAL_MGM <- c("Carriage" = "#D6455B", "Demographic" = "#F58A5E",
             "Socioeconomic" = "#FBD9C4", "WASH" = "#5AB8D4",
             "Health" = "#1F7A99", "Antibiotic" = "#8E5BC4",
             "Food/animal" = "#4FB477", "Spatial" = "#128C7E")
PAL_MGM_OTHER <- "#7E9A88"
group_colours <- function(nms) {
  cols <- unname(PAL_MGM[nms]); cols[is.na(cols)] <- PAL_MGM_OTHER; cols
}

# mgm records the sign of a pairwise interaction in fit$pairwise$signs as
# 1, -1, or 0. Zero means NO SIGN IS DEFINED -- Reg2Graph sets int_sign <- 0
# whenever the pair involves a categorical variable with more than two levels,
# and also when the majority vote across the two nodewise estimates ties. It is
# NOT a negative sign, and it is why mgm's own edgecolor matrix draws those
# edges darkgrey rather than red. NA appears only where there is no edge.
sign_label <- function(x) {
  out <- rep("undefined", length(x))
  out[!is.na(x) & x > 0] <- "positive"
  out[!is.na(x) & x < 0] <- "negative"
  out
}

# Predictability for the rings and the bar chart: R2 for continuous and count
# nodes, normalised accuracy for categorical ones (Haslbeck & Waldorp 2020,
# Section 3.1). nCC is negative when the fitted model classifies worse than the
# majority-class rule, and qgraph's pie wants a proportion, so it is clamped.
pred_vector <- function(errors, type) {
  pick <- function(nm) {
    hit <- which(colnames(errors) %in% c(nm, paste0("Error.", nm)))
    if (length(hit)) errors[[hit[1]]] else rep(NA_real_, nrow(errors))
  }
  r <- ifelse(type == "c", pick("nCC"), pick("R2"))
  r <- suppressWarnings(as.numeric(r))
  r[!is.finite(r)] <- 0
  pmin(pmax(r, 0), 1)
}

# --- VISUALISATION HELPERS ---------------------------------------------------
# One qualitative colour per fieldwork site, plus a fallback for any extra
# level the cleaning pipeline might retain.
PAL_SITE <- c("#4CC98A", "#E8734A", "#7FD1E8", "#FFC15E",
              "#B07CD8", "#E85C7A", "#2E9E6B", "#9CB5A5", "#1F7A52")

site_palette <- function(levels) {
  cols <- PAL_SITE[seq_along(levels)]
  cols[is.na(cols)] <- "#7E9A88"
  stats::setNames(cols, levels)
}

# Household-level frame: model columns plus the fields needed for the map.
# Built once; returns NULL rather than erroring if the object lacks coordinates.
VIZ <- NULL
if (MGM_OK) VIZ <- local({
  n  <- nrow(AIARMS_OBJ$data)
  ll <- AIARMS_OBJ$lonlat
  sg <- AIARMS_OBJ$site_group %|z|% AIARMS_OBJ$site
  ok_ll <- !is.null(ll) && is.matrix(ll) && nrow(ll) == n && ncol(ll) >= 2
  if (ok_ll) {
    ll <- apply(ll[, 1:2, drop = FALSE], 2, function(z) suppressWarnings(as.numeric(z)))
    ok_ll <- all(is.finite(ll))
  }
  site <- if (!is.null(sg) && length(sg) == n) as.character(sg) else rep("(unknown)", n)
  list(
    n      = n,
    lon    = if (ok_ll) ll[, 1] else NULL,
    lat    = if (ok_ll) ll[, 2] else NULL,
    has_xy = ok_ll,
    site   = site,
    site_levels = sort(unique(site))
  )
})

HAS_MAP <- MGM_OK && !is.null(VIZ) && isTRUE(VIZ$has_xy)

# Readable level labels for a categorical node. Binary nodes read No/Yes;
# the categorical Site node reads its fieldwork codes; anything else keeps
# its integer code, since the recode mapping is not carried on the object.
## Names for the levels of the multi-level categorical variables, in the
## integer order the cleaning script codes them (01_clean_AIARMS.R).
LEVEL_NAMES <- list(
  Education     = c("Primary or incomplete secondary", "Completed secondary", "Tertiary"),
  WorkStatus    = c("Employed", "Unemployed", "Pensioner, student or other"),
  DiarrSeverity = c("None", "Mild", "Moderate or severe"),
  AbxSource     = c("Does not source antibiotics", "Government facility only",
                    "Private GP or pharmacy"),
  AbxCourse     = c("Completes the course", "Stops when symptoms resolve",
                    "Does not take antibiotics"))

## Binary No/Yes: red and green. Multi-level: a colour-blind-safe qualitative
## set (Okabe-Ito). Sex is binary but not a No/Yes item, so it gets neutral
## colours rather than a red/green reading.
COL_NO  <- "#D64545"
COL_YES <- "#2E9E5B"
PAL_LEVELS <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
                "#0072B2", "#D55E00", "#F0E442")

dist_colours <- function(v, labs) {
  if (identical(v, "Sex")) return(setNames(c("#7FA7C9", "#C98FB0")[seq_along(labs)], labs))
  if (length(labs) == 2 && all(labs %in% c("No", "Yes")))
    return(setNames(ifelse(labs == "Yes", COL_YES, COL_NO), labs))
  setNames(PAL_LEVELS[seq_along(labs)], labs)
}

level_labels <- function(v) {
  j <- match(v, MGM_VARS); if (is.na(j)) return(NULL)
  x <- sort(unique(AIARMS_OBJ$data[, j]))
  if (identical(v, "Site") && length(AIARMS_OBJ$site_labels) >= length(x))
    return(AIARMS_OBJ$site_labels[seq_along(x)])
  if (identical(v, "Sex") && all(x %in% c(0, 1)))
    return(c("Male", "Female")[seq_along(x)])
  if (MGM_TYPE[j] == "c" && MGM_LEVEL[j] == 2 && all(x %in% c(0, 1)))
    return(c("No", "Yes")[seq_along(x)])
  as.character(x)
}

viz_values <- function(v) {
  j <- match(v, MGM_VARS); if (is.na(j)) return(NULL)
  AIARMS_OBJ$data[, j]
}

viz_is_cat <- function(v) {
  j <- match(v, MGM_VARS); !is.na(j) && MGM_TYPE[j] == "c"
}

viz_label <- function(v) {
  j <- match(v, MGM_VARS); if (is.na(j)) v else MGM_LABELS[j]
}

# Choices for the covariate pickers, grouped by domain so the list is navigable.
## Number of nodes a variable set becomes once fitted: every Site_* indicator
## is folded into one categorical Site node, exactly as mgm_model() does.
fitted_node_count <- function(v) {
  is_site <- grepl("^Site_", v)
  sum(!is_site) + as.integer(any(is_site))
}

## Choices for a drop-down, grouped under domain headings. Each group is
## wrapped with as.list(): shiny renders a length-1 vector as a plain option
## labelled with the GROUP name, so a domain that happens to hold a single
## variable -- common after a preset or a manual selection -- would otherwise
## lose its heading and show the domain name in place of the variable.
grouped_choices <- function(values, labels, groups) {
  groups[is.na(groups)] <- "Other"
  g <- factor(groups, levels = sort(unique(groups)))
  lapply(split(seq_along(values), g),
         function(ix) as.list(stats::setNames(values[ix], labels[ix])))
}

## The map's colour picker. The seven Site_* indicators are dropped: colouring
## by one of them only shows which households are in that settlement, which
## colouring by Site already shows for all seven at once. Site itself sits in
## the Spatial group alongside neighbour density.
map_choices <- function() {
  if (!MGM_OK) return(character(0))
  keep <- !grepl("^Site_", MGM_VARS)
  v <- MGM_VARS[keep]
  grouped_choices(c("__site__", v),
                  c("Site", MGM_LABELS[keep]),
                  c("Spatial", MGM_REG$group[match(v, MGM_REG$var)]))
}

viz_choices <- function() {
  if (!MGM_OK) return(character(0))
  grouped_choices(MGM_VARS, MGM_LABELS,
                  MGM_REG$group[match(MGM_VARS, MGM_REG$var)])
}

# Continuous colour ramp for the map when a non-categorical node is selected.
ramp_cols <- function(x, n = 9) {
  pal <- grDevices::colorRampPalette(c("#0B3A24", "#1F7A52", "#4CC98A", "#C9FFE0"))(n)
  idx <- cut(x, breaks = n, labels = FALSE, include.lowest = TRUE)
  list(cols = pal[idx], pal = pal,
       brk = seq(min(x), max(x), length.out = n + 1))
}

# A light theme for the base-R panels, so they sit on the white cards.
viz_par <- function(mar = c(4.2, 4.2, 2, 1)) {
  par(mar = mar, bg = "white", fg = "#34493D", col.axis = "#34493D",
      col.lab = "#1A202C", col.main = "#1A202C", cex.axis = 0.8,
      cex.lab = 0.9, bty = "n", las = 1)
}

# --- THEME ------------------------------------------------------------------
# "Deep water" palette. Cool teals carry the structure, so anything warm on the
# page is an epidemiological signal rather than decoration.
app_theme <- bs_theme(
  version   = 5,
  bg        = "#08160F",
  fg        = "#E4EDE6",
  primary   = "#4CC98A",
  secondary = "#2A4A38",
  success   = "#6FE0A6",
  info      = "#6FB894",
  warning   = "#FFC15E",
  danger    = "#FF6B6B"
)

# --- Presentational helpers --------------------------------------------------
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

glow_header <- function(title) card_header(span(class = "glow", title))

SECTIONS <- c("Introduction", "Methodology", "MGM Explorer", "Conclusion")
SEC_ICON <- c("Introduction" = "house-door-fill", "Methodology" = "gear-wide-connected",
              "MGM Explorer" = "diagram-3-fill",
              "Conclusion" = "check2-circle")
sec_id <- function(x) gsub("[^a-z]", "", tolower(x))

sec_nav <- function(current) {
  others <- setdiff(SECTIONS, current)
  div(class = "sec-nav",
      span(class = "sec-nav-label", "Go to"),
      lapply(others, function(t)
        actionButton(paste0("go_", sec_id(current), "_", sec_id(t)), t,
                     icon  = bsicons::bs_icon(unname(SEC_ICON[t])),
                     class = "btn-sm sec-nav-btn")))
}

sec_head <- function(eyebrow, title, lede = NULL) {
  div(class = "sec-head",
      div(class = "eyebrow", eyebrow),
      tags$h2(class = "glow", title),
      if (!is.null(lede)) p(class = "lede", lede))
}

# --- UI ----------------------------------------------------------------------
ui <- page_navbar(
  id = "main_nav",
  theme = app_theme,
  title = APP_NAME,
  window_title = paste(APP_NAME, "\u2014", APP_STRAP),
  fillable = FALSE,
  header = tagList(
    tags$head(tags$style(HTML("
      body, .bslib-page-navbar { background-color: #08160F !important; color: #E4EDE6 !important; }
      .card:not(.mgm-card), .bslib-card:not(.mgm-card), .well, .accordion-body {
        background-color: #12241A !important; color: #E4EDE6 !important; border: 1px solid #2A4A38 !important;
      }
      .accordion-button { background-color: #12241A !important; color: #4CC98A !important;
                          border-bottom: 1px solid #2A4A38 !important;
                          font-weight: 600; font-size: 0.9rem; }
      .accordion-button:not(.collapsed) { background-color: #0D1E15 !important; color: #4CC98A !important; }
      .sidebar, .bslib-sidebar-layout > .sidebar { background-color: #0D1E15 !important; border-right: 1px solid #2A4A38 !important; }
      .table, .table td, .table th, .dataTables_wrapper { color: #E4EDE6 !important; }
      .form-control, .form-select { background-color: #08160F !important; color: #E4EDE6 !important; border: 1px solid #2A4A38 !important; }
      .value-box { background-color: #12241A !important; border: 1px solid #2A4A38 !important; }

      /* --- Boxes size to their content -------------------------------
         bslib lays cards out as flex/grid fill items, so a card in a row
         stretches to the tallest and its body centres the content in the
         leftover space. These rules turn the body back into ordinary block
         flow. Cards holding a plot or a table are unaffected: those outputs
         carry an explicit pixel height of their own. */
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

      /* --- Title bar: the tab strip is hidden, so the navbar is a title bar
             and nothing else; nav_select() still drives the panels. --- */
      .navbar .navbar-nav, .navbar .nav, .navbar-toggler { display: none !important; }
      .navbar > .container-fluid, .navbar > .container,
      .bslib-page-navbar > .navbar > .container-fluid { justify-content: center !important; }
      .navbar-brand { margin: 0 auto !important; float: none !important;
                      font-size: 1.45rem !important; letter-spacing: 0.14em !important;
                      padding: 2px 0 0 0 !important; font-weight: 700 !important; }
      .navbar { border-bottom: 1px solid #2A4A38 !important;
                padding-top: 10px !important; padding-bottom: 8px !important; }

      /* --- Section navigation --- */
      .sec-nav { display: flex; flex-wrap: wrap; align-items: center; gap: 8px;
                 margin: -8px 0 22px 0; padding: 10px 14px;
                 background: #0D1E15; border: 1px solid #2A4A38; border-radius: 8px; }
      .sec-nav-label { text-transform: uppercase; letter-spacing: 0.15em;
                       font-size: 0.68rem; font-weight: 700; color: #9CB5A5; margin-right: 4px; }
      .sec-nav-btn, .jump-row .btn {
        border: 1px solid #2A4A38 !important; background: #12241A !important;
        color: #C5D8CB !important; font-weight: 600; letter-spacing: 0.01em;
        transition: background 0.15s ease, color 0.15s ease, border-color 0.15s ease; }
      .sec-nav-btn:hover, .jump-row .btn:hover {
        background: #4CC98A !important; color: #08160F !important; border-color: #4CC98A !important; }
      .jump-row { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 18px; }

      /* --- Map canvas ---------------------------------------------------
         OSM tiles are light. Inverting and rotating the hue by 180 degrees
         turns them dark while leaving road and label geometry legible; the
         markers sit in a different pane and are not filtered. */
      .leaflet-container { background: #0D1E15 !important; }
      .leaflet-tile-pane {
        filter: invert(1) hue-rotate(180deg) brightness(0.92)
                contrast(0.88) saturate(0.55); }
      .leaflet-control-attribution { background: rgba(13,30,21,0.82) !important;
        color: #9CB5A5 !important; font-size: 10px !important; }
      .leaflet-control-attribution a { color: #4CC98A !important; }

      /* --- White panels for plots --- */
      .mgm-card { background-color: #FFFFFF !important; color: #1A202C !important; border: 1px solid #CBDFD0 !important; }
      .mgm-card p, .mgm-card h5, .mgm-card div { color: #1A202C !important; }
      .mgm-card h5, .mgm-card h6 { color: #1A202C !important; }
      .mgm-card > .card-header { background-color: #FFFFFF !important;
        color: #1A202C !important; border-bottom: 1px solid #CBDFD0 !important;
        border-left: 3px solid #4CC98A !important; }
      .mgm-card .card-header span, .mgm-card .card-header .glow {
        color: #1A202C !important; -webkit-text-fill-color: #1A202C !important;
        background-image: none !important; animation: none !important; filter: none !important; }

      /* --- Landing page --- */
      .hero { background: linear-gradient(135deg, #0D1E15 0%, #08160F 70%);
              border: 1px solid #2A4A38; border-radius: 8px;
              padding: 34px 38px; margin-bottom: 22px; }
      .hero h1 { font-size: 2.15rem; font-weight: 700; letter-spacing: 0.02em;
                 margin: 0 0 6px 0; color: #FFFFFF; }
      .hero .lede { font-size: 1.05rem; color: #C5D8CB; max-width: 60em;
                    line-height: 1.55; margin-bottom: 4px; }
      .hero .eyebrow { text-transform: uppercase; letter-spacing: 0.18em;
                       font-size: 0.72rem; color: #4CC98A; margin-bottom: 10px; }
      .jump-row .btn { margin: 14px 10px 0 0; font-weight: 600; }
      .aim { border-left: 3px solid #4CC98A; padding: 2px 0 2px 14px; margin-bottom: 16px; }
      .aim b { color: #4CC98A; }
      .step-num { display: inline-block; width: 26px; height: 26px;
                  border-radius: 50%; background: #4CC98A; color: #08160F;
                  text-align: center; font-weight: 700; line-height: 26px; margin-right: 10px; }
      .theory { line-height: 1.62; }
      .theory .eqnote { color: #9CB5A5; font-size: 0.88rem; }
      .theory p { margin-bottom: 0.95rem; }
      .MathJax, .MathJax_Display { color: #E4EDE6 !important; }

      /* --- Section banners --- */
      .sec-head { border-left: 4px solid #4CC98A; padding: 2px 0 2px 18px;
                  margin: 4px 0 22px 0; }
      .eyebrow { text-transform: uppercase; letter-spacing: 0.16em;
                 font-size: 0.72rem; font-weight: 700; color: #6FB894; }
      .sec-head .eyebrow { letter-spacing: 0.16em; font-size: 0.68rem;
                           color: #4CC98A; margin-bottom: 6px; }
      .sec-head h2 { font-size: 1.55rem; font-weight: 700; color: #FFFFFF;
                     margin: 0 0 8px 0; letter-spacing: 0.01em; }
      .sec-head .lede { color: #9CBAC6; font-size: 0.97rem; line-height: 1.55;
                        max-width: 62em; margin: 0; }

      /* --- Headings --- */
      .card-header { font-size: 1.06rem !important; font-weight: 700 !important;
                     letter-spacing: 0.03em; padding: 13px 20px 12px 20px !important;
                     border-left: 3px solid #4CC98A !important;
                     background-color: #0D1E15 !important;
                     border-bottom: 1px solid #2A4A38 !important;
                     color: #4CC98A !important; }
      .card-header:has(.glow) { box-shadow: inset 0 -1px 0 rgba(76,201,138,0.35);
                                font-size: 1.12rem !important; padding: 12px 20px !important; }
      h5 { font-size: 1.06rem !important; color: #4CC98A; font-weight: 700;
           letter-spacing: 0.02em; margin-top: 4px; }
      h6 { font-size: 0.8rem !important; letter-spacing: 0.09em;
           color: #9CB5A5 !important; font-weight: 700; text-transform: uppercase; }
      .mgm-card h6 { color: #4A6B57 !important; }

      /* --- Tabs --- */
      .nav-tabs { border-bottom: 1px solid #2A4A38 !important; }
      .nav-tabs .nav-link { color: #9CB5A5 !important; padding: 10px 18px !important;
                            font-weight: 600; border: none !important; }
      .nav-tabs .nav-link:hover { color: #4CC98A !important; }
      .nav-tabs .nav-link.active { background-color: #4CC98A !important; color: #08160F !important;
                                   font-weight: bold; border-radius: 6px 6px 0 0 !important; }
      .navbar .nav-link { font-weight: 600; letter-spacing: 0.02em; }

      /* --- Value boxes and rhythm --- */
      .value-box .value-box-title { text-transform: uppercase; letter-spacing: 0.11em;
                                    font-size: 0.7rem; color: #9CB5A5 !important; }
      .value-box .value-box-value { font-weight: 700; }
      .card { margin-bottom: 16px; }
      .card-body { padding: 20px 22px; }
      hr { border-color: #2A4A38 !important; opacity: 1; margin: 18px 0; }
      .sidebar h6 { margin-top: 2px; }

      /* --- Themed notices --- */
      .note-warn { color: #FFD79A !important;
                   background: rgba(255,193,94,0.10) !important;
                   border: 1px solid rgba(255,193,94,0.45) !important;
                   border-left: 4px solid #FFC15E !important;
                   padding: 12px 16px; border-radius: 4px; }
      .note-info { color: #A9F7C9 !important;
                   background: rgba(76,201,138,0.09) !important;
                   border: 1px solid rgba(76,201,138,0.40) !important;
                   border-left: 4px solid #4CC98A !important;
                   padding: 12px 16px; border-radius: 4px; }
      .note-warn b, .note-info b { color: inherit !important; }

      /* --- Equation panels --- */
      .eqbox { background: linear-gradient(90deg, rgba(76,201,138,0.07), rgba(76,201,138,0.0));
               border-left: 3px solid #4CC98A; border-radius: 0 6px 6px 0;
               padding: 6px 20px 10px 20px; margin: 14px 0 20px 0; }
      .eqbox .eqlabel { text-transform: uppercase; letter-spacing: 0.15em;
                        font-size: 0.66rem; color: #4CC98A; font-weight: 700; margin-bottom: 2px; }
      .eqbox .eqcap { color: #9CB5A5; font-size: 0.85rem; margin-top: 2px; }

      /* --- Fact chips and definition rows --- */
      .chiprow { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 6px; }
      .chip { flex: 1 1 210px; background: #0D1E15; border: 1px solid #2A4A38;
              border-top: 3px solid #4CC98A; border-radius: 6px; padding: 12px 16px; }
      .chip .k { text-transform: uppercase; letter-spacing: 0.12em;
                 font-size: 0.64rem; color: #9CB5A5; font-weight: 700; }
      .chip .v { font-size: 1.02rem; color: #E4EDE6; font-weight: 700; margin-top: 3px; }
      .chip .d { font-size: 0.82rem; color: #9CB5A5; margin-top: 4px; line-height: 1.45; }
      .chip.warm { border-top-color: #FF6B6B; }
      .chip.cool { border-top-color: #1F7A99; }
      .defrow { display: flex; gap: 14px; padding: 9px 0;
                border-bottom: 1px solid rgba(22,87,107,0.55); }
      .defrow:last-child { border-bottom: none; }
      .defrow .term { flex: 0 0 190px; color: #4CC98A; font-weight: 700; font-size: 0.9rem; }
      .defrow .desc { flex: 1; color: #E4EDE6; font-size: 0.92rem; line-height: 1.55; }

      /* --- Glowing headings: light moving through water --- */
      .glow { display: inline-block;
              background-image: linear-gradient(100deg, #4CC98A 0%, #3DDC97 18%,
                                #C9FFE0 36%, #4CC98A 50%, #3DDC97 68%,
                                #C9FFE0 86%, #4CC98A 100%);
              background-size: 200% 100%;
              -webkit-background-clip: text; background-clip: text;
              color: transparent; -webkit-text-fill-color: transparent;
              animation: amr-flow 9s linear infinite,
                         amr-breathe 4.5s ease-in-out infinite alternate; }
      @keyframes amr-flow { from { background-position: 0% 50%; }
                             to   { background-position: 200% 50%; } }
      @keyframes amr-breathe {
        from { filter: drop-shadow(0 0 2px rgba(76,201,138,0.30)); }
        to   { filter: drop-shadow(0 0 9px rgba(76,201,138,0.75)); } }
      @media (prefers-reduced-motion: reduce) {
        .glow { animation: none; filter: drop-shadow(0 0 5px rgba(76,201,138,0.5)); }
      }
    "))),
    withMathJax()
  ),

  # SECTION 1: INTRODUCTION
  nav_panel(
    "Introduction",
    icon = bsicons::bs_icon("house-door-fill"),

    div(
      class = "hero",
      div(class = "eyebrow", "WST795 Research Report  |  University of Pretoria"),
      h1(APP_TITLE),
      div(
        class = "jump-row",
        actionButton("jump_method",  "Read the methodology",
                     icon = bsicons::bs_icon("book"), class = "btn-outline-info"),
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
      value_box(title = "Nodes Available", value = textOutput("lp_kpi_nodes"),
                showcase = bsicons::bs_icon("diagram-3"), theme = "info"),
      value_box(title = "Nodes in Core Set", value = textOutput("lp_kpi_core"),
                showcase = bsicons::bs_icon("list-ol"), theme = "success")
    ),

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
            p("Demographic-based surveillance connects the population present in a ",
              "catchment with the characteristics of that population, treated as ",
              "covariates. Elevation, for instance, was found to be associated with ",
              "the distribution of cholera during the 2008-2009 epidemic in Harare, ",
              "Zimbabwe."),
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
          glow_header("The aims"),
          card_body(
            div(class = "aim", tags$b("1. "),
                "Investigate the significance of demographic factors through the ",
                "use of mixed graphical models, and then connect those factors to ",
                "both physical location and AMR markers."),
            div(class = "aim", tags$b("2. "),
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
              "dependency network, refitted live as you change its settings, ",
              "together with the household map and the distribution of each ",
              "covariate."),
            p(span(class = "step-num", "3"),
              tags$b("Conclusion"), " answers aim 2, drawing the findings together.")
          )
        ),
        card(
          glow_header("Target markers"),
          card_body(
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
             "Methodology"),
    sec_nav("Methodology"),
    navset_card_tab(

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
          h5("From parameters to a single edge-weight"),
          p("An interaction between two continuous nodes is one parameter, and the ",
            "edge weight is its value. An interaction involving a categorical node ",
            "with \\(m\\) categories is specified by several parameters, and the ",
            "weight drawn in the network is then the ", tags$b("mean of their ",
            "absolute values"), ", so no sign is defined for it. The Interaction ",
            "Detail tab prints the individual parameters behind any such edge."),
          eqbox("Edge presence for a multi-parameter interaction",
                "$$(s,r) \\in E \\iff \\exists\\, z : |\\theta^{z}_{s,r}| > 0$$",
                "An edge exists if any one of the parameters defining it is non-zero."),
          hr(),
          h5("Reconciling the two regressions"),
          p("Since each node is regressed separately, node \\(v\\) may select node ",
            "\\(r\\) as a neighbour while \\(r\\) does not select \\(v\\)."),
          chiprow(
            chip("OR-rule", "Sensitive  |  adopted here",
                 "Retains an edge if either regression selects it.", tone = "warm"),
            chip("AND-rule", "Conservative  |  reported as sensitivity",
                 "Retains an edge only if both regressions select it.")
          ),
          p(class = "eqnote",
            "The two rules are asymptotically equivalent for graph recovery; the ",
            "difference is a finite-sample trade between sensitivity and precision. ",
            "This analysis is exploratory, and in a screening context a missed ",
            "exposure costs more than a flagged one that later fails to replicate, ",
            "so the sensitive rule is used for the primary fit and the conservative ",
            "one reported alongside it."),
          hr(),
          h5("Selecting the regularisation parameter"),
          p("Two options are available: an information criterion, or ",
            tags$b("cross-validation"), ", which selects the \\(\\lambda\\) ",
            "minimising out-of-sample prediction error across folds. The primary ",
            "fit here uses cross-validation, because \\(\\gamma = 0.25\\) is a ",
            "default calibrated on simulated data rather than on this study, ",
            "whereas cross-validation selects \\(\\lambda\\) from the data at ",
            "hand. The extended Bayesian information criterion is:"),
          eqbox("Equation (10)  |  Extended BIC",
                "$$\\mathrm{EBIC}_{\\gamma}(\\hat{\\theta}) = -2L(\\hat{\\theta}) + \\hat{s}_0 \\log n + 2\\gamma\\, \\hat{s}_0 \\log p$$",
                "The lambda minimising this is retained."),
          p("The value of \\(\\lambda\\) minimising this is retained. The ",
            "hyper-parameter \\(\\gamma\\) trades sensitivity against precision: ",
            "larger values penalise dense graphs more heavily and return fewer ",
            "edges, while \\(\\gamma = 0\\) recovers the ordinary BIC."),
          hr(),
          h5("Thresholding"),
          p("An \\(\\ell_1\\) penalty shrinks every estimate towards zero, so a ",
            "small true effect and a zero look alike. The guarantees on false and ",
            "true positive rates hold only if real effects are not arbitrarily ",
            "small, which is the ", tags$b("beta-min condition"), ". Thresholding ",
            "the estimates at \\(\\tau\\) enforces it: anything below that floor ",
            "is set to zero. This is the false-positive control in the procedure, ",
            "and it is applied in the fit reported here. Switching it off in the ",
            "sidebar removes the guarantee, so the resulting graph is a ",
            "sensitivity check rather than a result."),
          hr(),
          h5("Predictability"),
          p("An edge says a dependency exists; it does not say how much of a node ",
            "the rest of the network accounts for. Predictability answers that, and ",
            "it is what the ring around each node in the network shows. For ",
            "continuous and count nodes it is the proportion of variance explained, ",
            "\\(R^2\\). For categorical nodes it is the ", tags$b("normalised ",
            "accuracy"), ", the gain over the intercept-only model divided by the ",
            "largest gain available:"),
          eqbox("Normalised accuracy",
                "$$A_{\\mathrm{norm}} = \\frac{A - \\max\\{p_0, p_1, \\dots, p_m\\}}{1 - \\max\\{p_0, p_1, \\dots, p_m\\}}$$",
                "A is the proportion correctly classified; p_j are the marginal category probabilities."),
          p("The normalisation matters here because several nodes have a large ",
            "majority class: a node that is positive in 90% of households would ",
            "score 0.90 on raw accuracy while learning nothing from its neighbours, ",
            "and scores 0 on this measure instead."),
          div(
            class = "alert note-info",
            tags$b("In the app. "),
            "The MGM Explorer loads with the primary specification: all variables, ",
            "\\(k = 2\\), cross-validated \\(\\lambda\\), the OR-rule and the ",
            "\\(\\tau\\) threshold applied. Every one of those choices is a ",
            "control in its sidebar, so any sensitivity fit is a click or two ",
            "away, and changing any of them refits the model rather than ",
            "redrawing a cached one."
          )
        )
      )
    )
  ),

  # SECTION 3: DATA VISUALISATION
  nav_panel(
    "MGM Explorer",
    icon = bsicons::bs_icon("diagram-3-fill"),
    sec_head("Section 3  |  Research aim: how covariates relate",
             "Mixed Graphical Model Explorer"),
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
                         choices = c("All variables" = "full",
                                     "Core set" = "core",
                                     "Pick manually" = "manual"),
                         selected = "full"),
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
                         choices = c("Cross-validation" = "CV", "EBIC" = "EBIC"),
                         selected = "CV"),
            conditionalPanel(
              "input.mgm_lamSel == 'EBIC'",
              sliderInput("mgm_gamma", "EBIC gamma (higher = sparser):",
                          min = 0, max = 1, value = 0.25, step = 0.25)
            ),
            radioButtons("mgm_rule", "Combine neighbourhoods with:",
                         choices = c("OR (sensitive)" = "OR",
                                     "AND (conservative)" = "AND"),
                         selected = "OR"),
            checkboxInput("mgm_thresh", "Apply beta-min threshold (tau)", TRUE),
            actionButton("mgm_go", "Fit Model", class = "btn-primary", width = "100%")
          ),
          accordion_panel(
            "Export",
            icon = bsicons::bs_icon("download"),
            radioButtons("mgm_dl_fmt", "Figure format:",
                         choices = c("PDF" = "pdf", "PNG" = "png"),
                         selected = "pdf"),
            downloadButton("mgm_dl_net", "Network figure",
                           class = "btn-outline-info", style = "width:100%;"),
            div(style = "height:8px;"),
            downloadButton("mgm_dl_pred", "Predictability figure",
                           class = "btn-outline-info", style = "width:100%;"),
            div(style = "height:8px;"),
            downloadButton("mgm_dl_edges", "Edge list (CSV)",
                           class = "btn-outline-info", style = "width:100%;")
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
        value_box(title = "Nodes in Model", value = textOutput("mgm_kpi_nodes"),
                  showcase = bsicons::bs_icon("diagram-3"), theme = "primary"),
        value_box(title = "Edges Shown", value = textOutput("mgm_kpi_edges"),
                  showcase = bsicons::bs_icon("share"), theme = "success"),
        value_box(title = "Observations", value = textOutput("mgm_kpi_obs"),
                  showcase = bsicons::bs_icon("people"), theme = "info")
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
          "Specification",
          icon = bsicons::bs_icon("clipboard-check"),
          card_body(
            verbatimTextOutput("mgm_spec")
          )
        ),
        nav_panel(
          "Edges",
          icon = bsicons::bs_icon("list-ul"),
          card_body(
            DTOutput("mgm_edgetab")
          )
        ),
        nav_panel(
          "Predictability",
          icon = bsicons::bs_icon("bar-chart-fill"),
          card_body(
            card(
              class = "mgm-card",
              card_body(plotOutput("mgm_predplot", height = "520px"))
            ),
            DTOutput("mgm_errtab")
          )
        ),
        nav_panel(
          "Household map",
          icon = bsicons::bs_icon("geo-alt-fill"),
          card_body(
            layout_columns(
              fill = FALSE, col_widths = c(3, 3, 2, 4),
              sliderInput("viz_map_size", "Marker size:", 3, 12, 6, step = 1),
              sliderInput("viz_map_alpha", "Marker opacity:", 0.2, 1, 0.85, step = 0.05),
              div(style = "padding-top:28px;",
                  checkboxInput("viz_map_labels", "Site on hover", TRUE)),
              selectInput("viz_map_colour", "Colour households by:",
                          choices = c("Site" = "__site__"),
                          selected = "__site__")
            ),
            card(class = "mgm-card", card_body(uiOutput("viz_map_ui"))),
            layout_columns(
              fill = FALSE, col_widths = c(8, 4),
              card(card_header("Households per site"),
                   card_body(DTOutput("viz_site_counts"))),
              div(style = "padding-top:12px;",
                  downloadButton("viz_dl_map", "Map as PDF",
                                 class = "btn-outline-info", style = "width:100%;"))
            )
          )
        ),
        nav_panel(
          "Distributions",
          icon = bsicons::bs_icon("bar-chart-line-fill"),
          card_body(
            layout_columns(
              fill = FALSE, col_widths = c(5, 4, 3),
              selectInput("viz_dist_var", "Covariate:", choices = NULL),
              div(style = "padding-top:28px;",
                  checkboxInput("viz_dist_bysite", "Split by fieldwork site", FALSE)),
              div(style = "padding-top:24px;",
                  downloadButton("viz_dl_dist", "Plot as PDF",
                                 class = "btn-outline-info", style = "width:100%;"))
            ),
            card(
              class = "mgm-card",
              card_body(
                plotOutput("viz_dist_plot", height = "480px"),
                uiOutput("viz_dist_caption")
              )
            )
          )
        ),
        nav_panel(
          "Node Profile",
          icon = bsicons::bs_icon("diagram-2"),
          card_body(
            selectInput("mgm_prof_node", "Node:", choices = NULL, width = "340px"),
            verbatimTextOutput("mgm_profile")
          )
        ),
        nav_panel(
          "Interaction Detail",
          icon = bsicons::bs_icon("zoom-in"),
          card_body(
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
            uiOutput("nd_status"),
            uiOutput("nd_pick_ui"),
            uiOutput("nd_detail"),
            hr(),
            h5("All nodes at a glance"),
            DTOutput("nd_table")
          )
        )
      )
    )
  ),

  # SECTION 4: CONCLUSION
  nav_panel(
    "Conclusion",
    icon = bsicons::bs_icon("check2-circle"),
    sec_head("Section 4  |  Synthesis",
             "Conclusion"),
    sec_nav("Conclusion"),
    layout_column_wrap(
      width = 1,
      card(
        glow_header("Synthesis of Findings"),
        card_body(
          p("The mixed graphical model estimates the conditional dependencies between environmental sanitation infrastructure, socio-demographic characteristics, fieldwork site membership and pathogen colonisation, on one cleaned frame of 162 households and up to nine sampling months."),
          tags$ul(
            tags$li(tags$b("Network sparsity: "), "Regularised neighbourhood selection isolates conditional rather than marginal associations, so an edge survives only after every other covariate in the model has been conditioned on."),
            tags$li(tags$b("Location in the model: "), "Site membership and local household density enter as nodes in their own right, so between-settlement differences are estimated alongside the covariates rather than left in the residual."),
            tags$li(tags$b("Predictability: "), "An edge establishes a dependency; the ring around each node reports how much of that variable the rest of the network actually accounts for, which is the quantity that decides whether an association is worth acting on.")
          )
        )
      ),
      card(
        glow_header("Limits of the design"),
        card_body(
          tags$ul(
            tags$li(tags$b("Cross-sectional covariates. "), "The survey is measured once per household while carriage is counted over months, so no edge in this model is directional and none of it establishes causation."),
            tags$li(tags$b("n = 162. "), "Several nodes have a large majority class or, in the case of ARUF, three households. Low predictability and unstable membership follow from the sample size rather than from the estimator."),
            tags$li(tags$b("Self-report. "), "Antibiotic use, handwashing and HIV status are all reported by the respondent, so a null edge is weak evidence of no association."),
            tags$li(tags$b("Cleaning decisions are load-bearing. "), "Blank survey items are now treated as missing and imputed rather than read as \"No\". The Node Dictionary flags every variable where that decision moves the prevalence materially.")
          )
        )
      )
    )
  )
)

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

  # -----------------------------------------------------------------------
  # MGM EXPLORER
  # Every refit calls mgm() with exactly the arguments shown in the sidebar,
  # so what is displayed is always a real model, never a cached redraw.
  # -----------------------------------------------------------------------

  output$mgm_explorer_status <- renderUI({
    if (MGM_OK) {
      if (!length(MGM_NOTES)) return(NULL)
      return(div(class = "alert note-info",
                 tags$b("Object notes: "),
                 tags$ul(lapply(MGM_NOTES, tags$li))))
    }
    div(
      class = "alert note-warn",
      tags$b("MGM object not available. "),
      if (!is.null(MGM_LOAD_ERR)) tags$span(MGM_LOAD_ERR, tags$br()),
      "This section reads output/AIARMS_mgm_spatial.rds. Run 01_clean_AIARMS.R ",
      "and then 01b_add_spatial.R to create it."
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
    selectizeInput("mgm_vars", "Fine-tune nodes:", choices = viz_choices(),
                   multiple = TRUE, options = list(plugins = list("remove_button")))
  })

  mgm_selected_vars <- reactive({
    req(MGM_OK)
    switch(input$mgm_preset %|z|% "full",
           core   = MGM_CORE,
           full   = MGM_VARS,
           manual = {
             v <- MGM_REG$var[MGM_REG$group %in% input$mgm_domains]
             v <- intersect(v, MGM_VARS)
             if (length(input$mgm_vars)) union(v, input$mgm_vars) else v
           })
  })

  # eventReactive: nothing is recomputed until "Fit Model" is pressed, so
  # dragging the display sliders redraws instantly without refitting.
  mgm_model <- eventReactive(input$mgm_go, {
    req(MGM_OK)
    v <- mgm_selected_vars()
    validate(need(length(v) >= 4, paste0(
      "Only ", length(v), " variable(s) selected; mgm() needs at least four.\n",
      "Preset: ", input$mgm_preset %|z|% "full",
      "   |  core set: ", length(MGM_CORE),
      "   |  all variables: ", length(MGM_VARS),
      if (length(MGM_NOTES)) paste0("\n", paste("Note:", MGM_NOTES, collapse = "\n")) else "")))

    k_ord  <- as.numeric(input$mgm_k  %|z|% "2")
    lamSel <- input$mgm_lamSel        %|z|% "CV"
    gam    <- input$mgm_gamma         %|z|% 0.25
    rule   <- input$mgm_rule          %|z|% "OR"
    thr    <- isTRUE(input$mgm_thresh %|z|% TRUE)

    idx <- match(v, MGM_VARS)
    validate(need(!anyNA(idx), paste(
      "Not present in the data:", paste(v[is.na(idx)], collapse = ", "))))

    X     <- as.matrix(AIARMS_OBJ$data[, idx, drop = FALSE])
    type  <- MGM_TYPE[idx]
    level <- MGM_LEVEL[idx]
    labs  <- MGM_LABELS[idx]
    colnames(X) <- labs
    grp   <- MGM_REG$group[match(v, MGM_REG$var)]
    grp[is.na(grp)] <- "Other"

    # --- location encoding ---------------------------------------------------
    # Location is always a single categorical node. The seven binary indicators
    # the object ships with sum to one in every row and are therefore not
    # identified: their mutual edges are an artefact of the coding, their own
    # predictability is 1 by construction, and which indicator carries an effect
    # is unstable. One categorical node is identified. The per-household site
    # code is read from the object rather than reconstructed from the indicators.
    site_cols <- grep("^Site_", v)
    if (length(site_cols) > 0) {
      keep <- setdiff(seq_along(v), site_cols)
      v <- v[keep]; X <- X[, keep, drop = FALSE]
      type <- type[keep]; level <- level[keep]; labs <- labs[keep]; grp <- grp[keep]
      sg <- VIZ$site %|z|% character(0)
      validate(need(length(sg) == nrow(X),
                    "The object carries no per-household site code, so location cannot be encoded."))
      f     <- factor(sg)
      X     <- cbind(X, Site = as.integer(f))
      v     <- c(v, "Site")
      type  <- c(type, "c")
      level <- c(level, nlevels(f))
      labs  <- c(labs, "Site")
      grp   <- c(grp, "Spatial")
      colnames(X) <- labs
      site_levels <- levels(f)
    } else site_levels <- character(0)

    validate(
      need(nrow(X) > 0 && ncol(X) >= 4, "Not enough data to fit."),
      need(length(type) == ncol(X) && length(level) == ncol(X),
           "type / level lengths do not match the number of columns."),
      need(!anyNA(type) && !anyNA(level), "type or level contains NA."),
      need(!anyNA(X), "The selected columns contain missing values; mgm() needs a complete frame."),
      need(length(k_ord) == 1 && is.finite(k_ord), "Interaction order k is not set.")
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
    if (lamSel == "CV")   args$lambdaFolds <- MGM_CV_FOLDS

    withProgress(message = "Fitting MGM...", value = 0.5, {
      # CV folds are random. ARUF has only 3 households, so about one draw in
      # four leaves a training fold with 0 or 1 of them and glmnet refuses that
      # node's regression. Redrawing the folds only when that happens is
      # equivalent to stratifying on the rare category.
      fit_once <- function() {
        for (attempt in 1:10) {
          # Seeded per attempt: the first fit is always MGM_SEED, and a fold
          # redraw forced by a near-empty category is itself reproducible.
          set.seed(MGM_SEED + attempt - 1L)
          f <- tryCatch(do.call(mgm, args), error = function(e) e)
          if (!inherits(f, "error")) return(f)
          if (!(lamSel == "CV" && grepl("1 or 0 observations", conditionMessage(f))))
            stop(f)
        }
        stop(f)
      }
      fit <- tryCatch(fit_once(), error = function(e)
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
      pr <- tryCatch(predict(object = fit, data = X,
                             errorCon = c("RMSE", "R2"),
                             errorCat = c("CC", "nCC")),
                     error = function(e)
        validate(need(FALSE, paste("predict() failed:", conditionMessage(e)))))
    })

    gch <- grouped_choices(labs, labs, grp)
    updateSelectInput(session, "mgm_focus",
                      choices = c(list("(none)" = "(none)"), gch), selected = "(none)")
    updateSelectInput(session, "mgm_prof_node", choices = gch, selected = labs[1])
    updateSelectInput(session, "mgm_i1", choices = gch, selected = labs[1])
    updateSelectInput(session, "mgm_i2", choices = gch, selected = labs[2])

    list(fit = fit, X = X, vars = v, type = type, level = level,
         labels = labs, groups = grp, pred = pr,
         spec = list(k = k_ord, lamSel = lamSel, gamma = gam, folds = MGM_CV_FOLDS,
                     rule = rule, threshold = if (thr) "LW" else "none",
                     seed = MGM_SEED, preset = input$mgm_preset %|z|% "full"),
         site_levels = site_levels)
  }, ignoreNULL = FALSE)   # fit once on start-up with the defaults

  # --- Node Dictionary ----------------------------------------------------
  output$nd_status <- renderUI({
    if (MGM_OK) return(NULL)
    div(class = "alert note-warn",
        tags$b("No model object loaded. "),
        "The dictionary reads type, level and the summary statistics from ",
        "output/AIARMS_mgm_spatial.rds. Run the cleaning sections of the ",
        "analysis document and reopen the app.")
  })

  ## Grouped by domain like the other pickers. Site is listed first within
  ## Spatial so it sits above neighbour density.
  output$nd_pick_ui <- renderUI({
    req(MGM_OK)
    v   <- c(intersect("Site", DICT_VARS), setdiff(DICT_VARS, "Site"))
    j   <- match(v, MGM_VARS)
    lab <- ifelse(is.na(j), v, MGM_LABELS[j])
    grp <- ifelse(v == "Site", "Spatial", MGM_REG$group[match(v, MGM_REG$var)])
    selectInput("nd_pick", "Node:", choices = grouped_choices(v, lab, grp),
                selected = DICT_VARS[1], width = "340px")
  })

  output$nd_detail <- renderUI({
    req(MGM_OK)
    v <- input$nd_pick %|z|% DICT_VARS[1]
    req(v %in% DICT_VARS)
    j <- match(v, MGM_VARS)
    nt <- NODE_NOTES[[v]]
    qa <- function(k, txt) div(class = "defrow",
                               div(class = "term", k),
                               div(class = "desc", HTML(txt)))
    tagList(
      chiprow(
        chip("Node", v, if (is.na(j)) "Site" else MGM_LABELS[j], tone = "warm"),
        chip("Distribution", node_type_label(j),
             paste("Domain:", if (is.na(j)) "Spatial"
                              else MGM_REG$group[match(v, MGM_REG$var)]))
      ),
      if (!is.null(nt))
        tagList(
          qa("Why it is here",        nt$what),
          qa("Why this distribution", nt$why),
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

  ## The network drawing, factored out so the on-screen plot and the exported
  ## file are produced by exactly the same call. The layout is seeded because
  ## "spring" is randomised: without this the downloaded figure would not match
  ## the one on screen.
  draw_mgm_net <- function(legend_cex = 0.35, vsize = 4.5) {
    m  <- mgm_model()
    w  <- mgm_wadj_display()
    ec <- m$fit$pairwise$edgecolor

    if (!is.null(input$mgm_focus) && input$mgm_focus != "(none)") {
      f <- match(input$mgm_focus, m$labels)
      if (!is.na(f)) {
        keep <- matrix(FALSE, nrow(w), ncol(w))
        keep[f, ] <- TRUE; keep[, f] <- TRUE
        w[!keep] <- 0
      }
    }
    rings <- if (isTRUE(input$mgm_rings)) pred_vector(m$pred$errors, m$type) else NULL
    glist <- split(seq_along(m$labels), m$groups)

    set.seed(MGM_SEED)
    qgraph(w,
           edge.color = ec,
           layout     = input$mgm_layout %|z|% "spring",
           repulsion  = 1.1,
           pie        = rings,
           pieColor   = ifelse(m$type == "c", "#F58A5E", "#5AB8D4"),
           groups     = glist,
           color      = group_colours(names(glist)),
           nodeNames  = m$labels,
           labels     = as.character(seq_along(m$labels)),
           legend     = TRUE, legend.cex = legend_cex,
           vsize = vsize, esize = 14)
  }

  output$mgm_net <- renderPlot({ req(MGM_OK); draw_mgm_net() })

  ## --- Figure export -------------------------------------------------------
  ## PDF is vector, so it stays sharp at any size in the report; PNG at 300 dpi
  ## is there for anything that will not take a PDF.
  output$mgm_dl_net <- downloadHandler(
    filename = function()
      sprintf("figure_mgm_network.%s", input$mgm_dl_fmt %|z|% "pdf"),
    content = function(file) {
      fmt <- input$mgm_dl_fmt %|z|% "pdf"
      if (fmt == "pdf") grDevices::pdf(file, width = 11, height = 8.5)
      else grDevices::png(file, width = 11, height = 8.5, units = "in", res = 300)
      on.exit(grDevices::dev.off())
      draw_mgm_net(legend_cex = 0.30, vsize = 4.2)
    })

  output$mgm_dl_pred <- downloadHandler(
    filename = function()
      sprintf("figure_predictability.%s", input$mgm_dl_fmt %|z|% "pdf"),
    content = function(file) {
      fmt <- input$mgm_dl_fmt %|z|% "pdf"
      if (fmt == "pdf") grDevices::pdf(file, width = 7.5, height = 9)
      else grDevices::png(file, width = 7.5, height = 9, units = "in", res = 300)
      on.exit(grDevices::dev.off())
      draw_mgm_pred(cex_names = 0.62)
    })

  output$mgm_dl_edges <- downloadHandler(
    filename = function() "table_edges.csv",
    content = function(file) {
      m <- mgm_model(); w <- mgm_wadj_display()
      ut <- which(upper.tri(w) & w != 0, arr.ind = TRUE)
      sgn <- m$fit$pairwise$signs[ut]
      d <- data.frame(From = m$labels[ut[, 1]], To = m$labels[ut[, 2]],
                      Weight = round(w[ut], 4),
                      Sign = sign_label(sgn),
                      stringsAsFactors = FALSE)
      utils::write.csv(d[order(-abs(d$Weight)), ], file, row.names = FALSE)
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

  output$mgm_spec <- renderPrint({
    req(MGM_OK)
    m  <- mgm_model(); sp <- m$spec
    w  <- mgm_wadj_display()
    p_ <- ncol(m$X); n_ <- nrow(m$X)
    w_all <- m$fit$pairwise$wadj
    e_all <- sum(w_all[upper.tri(w_all)] != 0)

    line <- function(k, v) cat(sprintf("  %-26s %s\n", paste0(k, ":"), v))
    cat("SPECIFICATION\n")
    line("Variable set", switch(sp$preset, full = "all variables",
                                core = "core set", manual = "manual selection"))
    line("Observations (n)", n_)
    line("Nodes (p)", p_)
    line("Interaction order (k)", sp$k)
    line("Location encoded as",
         sprintf("one categorical Site node, %d levels (identified)",
                 length(m$site_levels)))
    line("lambda selected by",
         if (sp$lamSel == "CV")
           sprintf("%d-fold cross-validation", sp$folds)
         else sprintf("EBIC, gamma = %.2f", sp$gamma))
    line("Edge rule", sprintf("%s-rule", sp$rule))
    line("Threshold", if (sp$threshold == "none") "none (tau not applied)"
                      else sprintf("tau (%s)", sp$threshold))
    line("scale", "TRUE  (Gaussian nodes standardised)")
    line("binarySign", "TRUE  (sign defined for binary edges)")
    line("overparameterize", if (sp$k == 3) "TRUE" else "FALSE")
    line("Random seed", sp$seed)

    cat("\nRESULT\n")
    line("Possible pairs", choose(p_, 2))
    line("Edges estimated", e_all)
    line("Density", sprintf("%.1f%%", 100 * e_all / choose(p_, 2)))
    line("Edges currently shown", sum(w[upper.tri(w)] != 0))

    cat("\nENVIRONMENT\n")
    line("R", paste(R.version$major, R.version$minor, sep = "."))
    for (pkg in c("mgm", "qgraph", "glmnet", "shiny"))
      line(pkg, tryCatch(as.character(utils::packageVersion(pkg)),
                         error = function(e) "not installed"))
    line("Platform", R.version$platform)
    line("Fitted", format(Sys.time(), "%Y-%m-%d %H:%M"))

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
                    Sign = sign_label(sgn),
                    stringsAsFactors = FALSE)
    datatable(d[order(-abs(d$Weight)), ], rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })

  draw_mgm_pred <- function(cex_names = 0.7) {
    m <- mgm_model()
    r <- pred_vector(m$pred$errors, m$type)
    o <- order(r)
    op <- par(mar = c(4.4, 12, 2.2, 2)); on.exit(par(op))
    barplot(r[o], horiz = TRUE, names.arg = m$labels[o], las = 1,
            cex.names = cex_names, xlim = c(0, 1),
            col = ifelse(m$type[o] == "c", "#F58A5E", "#5AB8D4"),
            border = "white", xlab = "Predictability")
  }

  output$mgm_predplot <- renderPlot({ req(MGM_OK); draw_mgm_pred() })

  output$mgm_errtab <- renderDT({
    req(MGM_OK)
    m  <- mgm_model()
    er <- m$pred$errors
    num <- vapply(er, is.numeric, logical(1))
    er[num] <- lapply(er[num], round, 3)
    datatable(data.frame(Variable = m$labels, Type = m$type,
                         er[, -1, drop = FALSE], check.names = FALSE),
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  ## --- Node profile --------------------------------------------------------
  ## Degree on its own is misleading: a categorical node with m levels forms an
  ## edge whenever ANY of its parameters is non-zero, so it has more chances to
  ## connect than a binary node does. The domain breakdown below is therefore
  ## reported by total edge WEIGHT as well as by count, and the concentration
  ## lines show how much of that weight sits in the strongest few partners.
  output$mgm_profile <- renderPrint({
    req(MGM_OK)
    m <- mgm_model()
    v <- input$mgm_prof_node %|z|% m$labels[1]
    i <- match(v, m$labels); req(!is.na(i))
    w <- mgm_wadj_display()
    nb <- which(w[i, ] != 0)
    ww <- w[i, nb]
    o  <- order(-ww); nb <- nb[o]; ww <- ww[o]

    line <- function(k, val) cat(sprintf("  %-26s %s\n", k, val))
    cat(v, "\n"); cat(strrep("-", max(20, nchar(v))), "\n")
    line("Domain", m$groups[i])
    line("Type", if (m$type[i] == "c" && m$level[i] == 2) "binary"
                 else if (m$type[i] == "c") sprintf("categorical, %d levels", m$level[i])
                 else if (m$type[i] == "g") "Gaussian" else "Poisson")

    ## --- predictability, with the raw figure beside the normalised one
    er <- m$pred$errors
    grab <- function(nm) {
      hit <- which(colnames(er) %in% c(nm, paste0("Error.", nm)))
      if (length(hit)) suppressWarnings(as.numeric(er[[hit[1]]][i])) else NA_real_
    }
    if (m$type[i] == "c") {
      cc <- grab("CC"); nc <- grab("nCC")
      line("Accuracy", sprintf("%.1f%%", 100 * cc))
      if (is.finite(cc) && is.finite(nc) && nc < 1) {
        base <- (cc - nc) / (1 - nc)
        line("Majority baseline", sprintf("%.1f%%", 100 * base))
        line("Gain over baseline", sprintf("%.1f percentage points", 100 * (cc - base)))
      }
      line("Normalised accuracy", sprintf("%.2f", nc))
    } else {
      line("R-squared", sprintf("%.2f", grab("R2")))
      line("RMSE", sprintf("%.3f", grab("RMSE")))
    }

    if (!length(nb)) {
      cat("\n  No edges. Nothing in the network is conditionally dependent on it.\n")
      return(invisible(NULL))
    }

    tot <- sum(ww)
    cat("\nEDGES\n")
    line("Count", sprintf("%d of %d possible", length(nb), length(m$labels) - 1L))
    line("Total weight", sprintf("%.3f", tot))
    line("Mean / median weight", sprintf("%.3f / %.3f", mean(ww), stats::median(ww)))
    for (k in c(3, 5)) if (length(nb) > k)
      line(sprintf("Top %d partners carry", k),
           sprintf("%.1f%% of its edge weight", 100 * sum(ww[seq_len(k)]) / tot))

    cat("\nWHERE THE WEIGHT SITS\n")
    dm  <- m$groups[nb]
    agg <- tapply(ww, dm, sum); cnt <- table(dm)
    dsz <- table(m$groups[-i])
    for (g in names(sort(agg, decreasing = TRUE)))
      cat(sprintf("  %-16s %2d of %2d nodes   weight %.3f  (%4.1f%%)\n",
                  g, as.integer(cnt[g]), as.integer(dsz[g]), agg[g],
                  100 * agg[g] / tot))

    cat("\nNEIGHBOURS\n")
    sgn <- sign_label(m$fit$pairwise$signs[i, nb])
    for (k in seq_along(nb))
      cat(sprintf("  %6.3f  %-10s %-26s [%s]\n", ww[k], sgn[k],
                  m$labels[nb[k]], m$groups[nb[k]]))
  })

  ## showInteraction() computes the per-level parameters, but mgm's print
  ## method for the object it returns shows only the interaction, weight and
  ## sign -- the parameters themselves are never displayed. This renders them,
  ## with the integer level codes replaced by the level names, which is what
  ## makes an edge involving the seven-level Site node readable.
  output$mgm_intdetail <- renderPrint({
    req(MGM_OK)
    m <- mgm_model()
    i <- match(input$mgm_i1, m$labels); j <- match(input$mgm_i2, m$labels)
    if (is.na(i) || is.na(j) || i == j) return(cat("Choose two different nodes."))

    out <- tryCatch(showInteraction(m$fit, int = c(i, j)), error = function(e) e)
    if (inherits(out, "error")) {
      cat("showInteraction() could not report this pair:\n  ",
          conditionMessage(out), "\n")
      return(invisible(NULL))
    }

    lab <- function(k) m$labels[k]
    sgn <- if (is.null(out$sign)) 0 else out$sign
    cat(sprintf("%s  \u2014  %s\n", lab(i), lab(j)))
    cat(sprintf("  Edge weight : %.4f\n", out$edgeweight))
    cat(sprintf("  Sign        : %s\n", sign_label(sgn)))
    if (identical(out$edgeweight, 0) || isTRUE(out$edgeweight == 0)) {
      cat("\n  No edge: this interaction was estimated to zero.\n")
      return(invisible(NULL))
    }

    ## Level names for a node, as the model codes it.
    lvnames <- function(k) {
      if (identical(m$vars[k], "Site") && length(m$site_levels)) return(m$site_levels)
      if (m$type[k] == "c" && m$level[k] == 2) return(c("0", "1"))
      if (m$type[k] == "c") return(as.character(seq_len(m$level[k])))
      NULL
    }

    if (!length(out$parameters)) {
      cat("\n  No per-level parameters are stored for this interaction.\n")
      return(invisible(NULL))
    }

    for (nm in names(out$parameters)) {
      pm  <- out$parameters[[nm]]
      resp <- as.integer(sub("^Predict_", "", nm))
      pred <- setdiff(c(i, j), resp)
      rn <- lvnames(resp); cn <- lvnames(pred)
      rownames(pm) <- if (!is.null(rn) && length(rn) == nrow(pm)) rn else lab(resp)
      colnames(pm) <- if (!is.null(cn) && length(cn) == ncol(pm)) cn else lab(pred)
      cat(sprintf("\n  Regression on %s   (columns = %s)\n", lab(resp), lab(pred)))
      print(round(pm, 4))
    }

    ## Plain reading for the common case: one multi-level node against one
    ## continuous or binary partner. The parameter matrix is oriented by
    ## finding the dimension whose extent matches the number of levels; for a
    ## binary response the two rows are mirror images, so the row for state 1
    ## is the one to read.
    multi <- c(i, j)[m$type[c(i, j)] == "c" & m$level[c(i, j)] > 2]
    if (length(multi) == 1) {
      other <- setdiff(c(i, j), multi)
      nm    <- paste0("Predict_", other)
      ln    <- lvnames(multi)
      if (nm %in% names(out$parameters) && length(ln)) {
        pm <- out$parameters[[nm]]
        v  <- if (ncol(pm) == length(ln)) pm[nrow(pm), ]
              else if (nrow(pm) == length(ln)) pm[, 1]
              else NULL
        if (!is.null(v)) {
          v  <- as.numeric(v)
          ok <- which(!is.na(v) & v != 0)
          ref <- ln[is.na(v)]
          cat(sprintf("\n  Reading \u2014 %s by level of %s%s\n", lab(other), lab(multi),
                      if (length(ref)) sprintf(", relative to %s", paste(ref, collapse = ", "))
                      else ""))
          if (!length(ok)) {
            cat("    every level is at the reference or estimated to zero\n")
          } else {
            for (k in ok[order(-abs(v[ok]))])
              cat(sprintf("    %-10s %+8.4f   %s %s\n", ln[k], v[k], lab(other),
                          if (v[k] > 0) "higher" else "lower"))
            zero <- setdiff(seq_along(v), c(ok, which(is.na(v))))
            if (length(zero))
              cat(sprintf("    at the reference: %s\n",
                          paste(ln[zero], collapse = ", ")))
          }
        }
      }
    }

    if ("Site" %in% m$vars[c(i, j)] && length(m$site_levels))
      cat("\n  Site levels: ",
          paste(sprintf("%d = %s", seq_along(m$site_levels), m$site_levels),
                collapse = ",  "), "\n")
  })

  # -----------------------------------------------------------------------
  # DATA VISUALISATION
  # Everything here is marginal and descriptive. No model is fitted, so
  # nothing in this section depends on the MGM Explorer's settings.
  # -----------------------------------------------------------------------

  observe({
    req(MGM_OK)
    ch <- viz_choices()
    updateSelectInput(session, "viz_map_colour",
                      choices = map_choices(), selected = "__site__")
    first <- MGM_VARS[1]
    updateSelectInput(session, "viz_dist_var", choices = ch, selected = first)
  })

  ## --- Household map -------------------------------------------------------
  viz_map_spec <- reactive({
    req(MGM_OK, HAS_MAP)
    v <- input$viz_map_colour %|z|% "__site__"
    if (identical(v, "__site__") || !(v %in% MGM_VARS)) {
      pal <- site_palette(VIZ$site_levels)
      return(list(cols = unname(pal[VIZ$site]), legend = pal,
                  title = "Site", value = VIZ$site, continuous = FALSE))
    }
    x <- viz_values(v)
    if (viz_is_cat(v)) {
      lv <- sort(unique(x)); labs <- level_labels(v); pal <- site_palette(labs)
      return(list(cols = unname(pal[match(x, lv)]), legend = pal,
                  title = viz_label(v), value = labs[match(x, lv)], continuous = FALSE))
    }
    r <- ramp_cols(x)
    list(cols = r$cols, legend = r$pal, brk = r$brk,
         title = viz_label(v), value = x, continuous = TRUE)
  })

  output$viz_map_ui <- renderUI({
    if (!MGM_OK || !HAS_MAP)
      return(div(class = "alert note-warn",
                 "A map cannot be drawn without household coordinates."))
    if (HAS_LEAFLET) leaflet::leafletOutput("viz_map_leaflet", height = "620px")
    else plotOutput("viz_map_static", height = "620px")
  })

  if (HAS_LEAFLET) output$viz_map_leaflet <- leaflet::renderLeaflet({
    req(MGM_OK, HAS_MAP)
    sp <- viz_map_spec()
    lab <- if (isTRUE(input$viz_map_labels))
      sprintf("%s &mdash; %s: %s", VIZ$site, sp$title,
              if (sp$continuous) format(round(sp$value, 2)) else as.character(sp$value))
    else NULL
    m <- leaflet::leaflet(options = leaflet::leafletOptions(
      minZoom = 10, attributionControl = TRUE))
    m <- leaflet::addTiles(m, urlTemplate = BASEMAP_URL,
                           attribution  = BASEMAP_ATTR,
                           options = leaflet::tileOptions(maxZoom = 19))
    m <- leaflet::addCircleMarkers(
      m, lng = VIZ$lon, lat = VIZ$lat,
      radius = input$viz_map_size %|z|% 6,
      fillColor = sp$cols, fillOpacity = input$viz_map_alpha %|z|% 0.85,
      color = "#E4EDE6", weight = 0.7, opacity = 0.65,
      label = if (is.null(lab)) NULL else lapply(lab, htmltools::HTML))
    if (!sp$continuous)
      m <- leaflet::addLegend(m, "bottomright", colors = unname(sp$legend),
                              labels = names(sp$legend), title = sp$title, opacity = 0.9)
    else
      m <- leaflet::addLegend(m, "bottomright",
                              pal = leaflet::colorNumeric(sp$legend, range(sp$value)),
                              values = sp$value, title = sp$title, opacity = 0.9)
    m
  })

  ## Publication figure for the household map. Deliberately NOT a tiled
  ## basemap: geolocated households with reported health status among the
  ## covariates become identifiable dwellings once street detail is drawn
  ## underneath them.
  draw_viz_map <- function(cex_pt = 1.3) {
    req(MGM_OK, HAS_MAP)
    sp  <- viz_map_spec()
    lat <- VIZ$lat; lon <- VIZ$lon
    op <- viz_par(c(4.2, 4.6, 3, 1)); on.exit(par(op))
    plot(lon, lat, asp = 1 / cos(mean(lat) * pi / 180),
         pch = 21, bg = sp$cols, col = "#34493D", cex = cex_pt,
         xlab = "Longitude", ylab = "Latitude",
         main = paste("Households coloured by", sp$title))
    u <- par("usr")
    m_per_deg <- 111320 * cos(mean(lat) * pi / 180)
    span_m    <- (u[2] - u[1]) * m_per_deg
    nice      <- c(50, 100, 200, 250, 500, 1000, 2000, 5000)
    bar_m     <- nice[which.min(abs(nice - span_m / 4))]
    bar_deg   <- bar_m / m_per_deg
    x0 <- u[1] + 0.06 * (u[2] - u[1]); y0 <- u[3] + 0.05 * (u[4] - u[3])
    segments(x0, y0, x0 + bar_deg, y0, lwd = 3, col = "#34493D", lend = 1)
    segments(c(x0, x0 + bar_deg), y0 - 0.008 * (u[4] - u[3]),
             c(x0, x0 + bar_deg), y0 + 0.008 * (u[4] - u[3]),
             lwd = 1.5, col = "#34493D")
    text(x0 + bar_deg / 2, y0, pos = 3, offset = 0.35, cex = 0.72, col = "#34493D",
         labels = if (bar_m >= 1000) sprintf("%g km", bar_m / 1000)
                  else sprintf("%d m", bar_m))
    ax <- u[2] - 0.05 * (u[2] - u[1]); ay <- u[3] + 0.06 * (u[4] - u[3])
    ah <- 0.07 * (u[4] - u[3])
    arrows(ax, ay, ax, ay + ah, length = 0.07, lwd = 2, col = "#34493D")
    text(ax, ay + ah, "N", pos = 3, offset = 0.15, cex = 0.78, font = 2, col = "#34493D")
    if (!sp$continuous)
      legend("topright", legend = names(sp$legend), pt.bg = unname(sp$legend),
             pch = 21, col = "#34493D", bty = "n", cex = 0.78)
  }

  output$viz_map_static <- renderPlot({ req(MGM_OK); draw_viz_map() })

  output$viz_site_counts <- renderDT({
    req(MGM_OK)
    tb <- as.data.frame(table(Site = VIZ$site), stringsAsFactors = FALSE)
    names(tb) <- c("Site", "Households")
    tb$Percent <- sprintf("%.1f%%", 100 * tb$Households / sum(tb$Households))
    datatable(tb, rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 20))
  })

  ## --- Distributions -------------------------------------------------------
  draw_viz_dist <- function() {
    req(MGM_OK)
    v <- input$viz_dist_var %|z|% MGM_VARS[1]; req(v %in% MGM_VARS)
    x <- viz_values(v); ttl <- viz_label(v)
    bysite <- isTRUE(input$viz_dist_bysite)
    if (viz_is_cat(v)) {
      labs <- level_labels(v); lv <- sort(unique(x))
      cols <- dist_colours(v, labs)
      named <- LEVEL_NAMES[[v]]
      multi <- !is.null(named) && length(named) == length(labs)
      ## For a multi-level variable the axis shows the level number and the
      ## legend says what each number means.
      key <- if (multi) sprintf("%s = %s", labs, named) else labs
      f  <- factor(labs[match(x, lv)], levels = labs)
      if (bysite) {
        tb <- table(f, VIZ$site)
        op <- viz_par(c(5.5, 4.5, 3, 15)); on.exit(par(op))
        barplot(tb, beside = TRUE, col = unname(cols), border = NA,
                las = 2, ylab = "Households", main = ttl)
        u <- par("usr")
        legend(u[2] + 0.02 * (u[2] - u[1]), u[4], xjust = 0, yjust = 1,
               xpd = TRUE, legend = key, fill = unname(cols), border = NA,
               bty = "n", cex = 0.78,
               title = if (multi) "What each colour shows" else NULL,
               title.adj = 0)
      } else {
        tb <- table(f)
        op <- viz_par(c(5.5, 4.5, 3, if (multi) 16 else 1)); on.exit(par(op))
        bp <- barplot(tb, col = unname(cols), border = NA, las = 1,
                      ylab = "Households", main = ttl, ylim = c(0, max(tb) * 1.18))
        text(bp, tb, labels = tb, pos = 3, cex = 0.85, col = "#34493D")
        if (multi) {
          u <- par("usr")
          legend(u[2] + 0.02 * (u[2] - u[1]), u[4], xjust = 0, yjust = 1,
                 xpd = TRUE, legend = key, fill = unname(cols), border = NA,
                 bty = "n", cex = 0.78, title = "What each colour shows",
                 title.adj = 0)
        }
      }
    } else {
      if (bysite) {
        op <- viz_par(c(5.5, 4.5, 3, 1)); on.exit(par(op))
        boxplot(x ~ factor(VIZ$site), col = unname(site_palette(VIZ$site_levels)),
                border = "#34493D", las = 2, xlab = "", ylab = ttl, main = ttl)
      } else {
        op <- viz_par(c(4.5, 4.5, 3, 1)); on.exit(par(op))
        br <- if (length(unique(x)) <= 12)
          seq(min(x) - 0.5, max(x) + 0.5, by = 1) else "Sturges"
        hist(x, breaks = br, col = "#5AB8D4", border = "white",
             xlab = ttl, ylab = "Households", main = ttl)
        abline(v = mean(x), col = "#FF6B6B", lwd = 2, lty = 2)
      }
    }
  }

  output$viz_dist_plot <- renderPlot({ req(MGM_OK); draw_viz_dist() })

  output$viz_dist_caption <- renderUI({
    req(MGM_OK)
    v <- input$viz_dist_var %|z|% MGM_VARS[1]; req(v %in% MGM_VARS)
    j <- match(v, MGM_VARS)
    div(class = "eqnote", style = "color:#4A6B57; font-size:0.85rem; margin-top:8px;",
        tags$b(node_type_label(j)), " \u2014 ", node_summary(v),
        if (!viz_is_cat(v)) " The dashed line marks the mean." else "")
  })

  ## --- Figure export -------------------------------------------------------
  viz_device <- function(file, w, h) grDevices::pdf(file, width = w, height = h)

  viz_dl <- function(stem, w, h, drawfun)
    downloadHandler(
      filename = function() sprintf("%s.pdf", stem),
      content  = function(file) {
        viz_device(file, w, h); on.exit(grDevices::dev.off()); drawfun()
      })

  output$viz_dl_map  <- viz_dl("figure_household_map",  7.5, 7.0, function() draw_viz_map())
  output$viz_dl_dist <- viz_dl("figure_distribution",   7.5, 5.0, function() draw_viz_dist())

  # -----------------------------------------------------------------------
  # LANDING PAGE
  # -----------------------------------------------------------------------

  output$lp_kpi_hh <- renderText(
    if (MGM_OK) format(nrow(AIARMS_OBJ$data), big.mark = ",") else "-")

  output$lp_kpi_months <- renderText(
    if (is.na(N_MONTHS)) "-" else as.character(N_MONTHS))

  ## These must agree with p in the MGM Explorer. The Explorer replaces any
  ## Site_* indicators with a single categorical Site node before fitting, so
  ## the count here applies the same collapse rather than counting registry rows.
  output$lp_kpi_nodes <- renderText(if (MGM_OK) as.character(fitted_node_count(MGM_VARS)) else "-")
  output$lp_kpi_core  <- renderText(if (MGM_OK) as.character(fitted_node_count(MGM_CORE)) else "-")

  output$lp_data_status <- renderUI({
    ok  <- function(x) if (x) bsicons::bs_icon("check-circle-fill") else
                              bsicons::bs_icon("exclamation-circle-fill")
    col <- function(x) if (x) "#3DDC97" else "#FFC15E"
    row <- function(lab, x, note)
      div(style = paste0("color:", col(x), "; font-size:0.88rem; margin-top:6px;"),
          ok(x), " ", tags$b(lab), tags$span(style = "color:#9CB5A5;", paste0("  ", note)))
    csv <- !is.null(find_app_file(DATA_FILE))
    tagList(
      hr(),
      row("Survey CSV", csv,    if (csv) "loaded" else "not found"),
      row("MGM object", MGM_OK, if (MGM_OK) paste(fitted_node_count(MGM_VARS), "nodes") else "not built"),
      if (length(BUILD_LOG))
        div(style = "color:#9CB5A5; font-size:0.8rem; margin-top:10px; line-height:1.5;",
            tags$b(style = "color:#4CC98A;", "Built this session:"), tags$br(),
            HTML(paste(BUILD_LOG, collapse = "<br/>")))
    )
  })

  observeEvent(input$jump_method,     nav_select("main_nav", "Methodology"))
  observeEvent(input$jump_mgm,        nav_select("main_nav", "MGM Explorer"))
  observeEvent(input$jump_conclusion, nav_select("main_nav", "Conclusion"))

  ## One observer per ordered pair of sections, for the sec_nav() button rows.
  for (.a in SECTIONS) for (.b in setdiff(SECTIONS, .a)) local({
    from <- .a; to <- .b
    observeEvent(input[[paste0("go_", sec_id(from), "_", sec_id(to))]],
                 nav_select("main_nav", to), ignoreInit = TRUE)
  })

}

shinyApp(ui = ui, server = server)
