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
APP_TITLE <- paste("Spatial variable selection in mixed graphical models:",
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
# University of Pretoria logo, embedded so the app stays a single file
UP_LOGO_URI <- paste0(
  "data:image/png;base64,",
  "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"
)

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

# Circular University of Pretoria crest, used on the Introduction and
# Conclusion pages. The image is embedded above, so no www/ folder is needed.
up_badge <- function(size = 120) {
  div(class = "up-badge",
      img(src = UP_LOGO_URI, alt = "University of Pretoria",
          title = "University of Pretoria",
          style = sprintf("width:%dpx; height:%dpx;", size, size)))
}

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
      .hero-row { display: flex; align-items: center; gap: 28px; }
      .hero-main { flex: 1 1 auto; min-width: 0; }
      .card-subhead { color: #4CC98A; font-weight: 700; font-size: 1rem;
                      margin: 20px 0 6px 0; }
      .up-badge { flex: 0 0 auto; background: #FFFFFF; border-radius: 50%;
                  padding: 10px; border: 2px solid #2A4A38;
                  box-shadow: 0 2px 16px rgba(0,0,0,0.45); line-height: 0; }
      .up-badge img { display: block; border-radius: 50%; }
      /* Section heading with the crest sitting opposite it */
      .sec-head-row { display: flex; align-items: center; justify-content: space-between;
                      gap: 24px; margin: 4px 0 22px 0; }
      .sec-head-row .sec-head { margin: 0; flex: 1 1 auto; min-width: 0; }
      @media (max-width: 900px) {
        .hero-row { flex-direction: column-reverse; gap: 18px; }
        .hero-row .up-badge { align-self: flex-start; }
        .hero-row .up-badge img { width: 96px !important; height: 96px !important; }
        .sec-head-row .up-badge { display: none; }
      }
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
      /* Presentation legibility: equations are large by default and grow
         further on hover, without shifting the surrounding text sideways. */
      .eqbox { font-size: 1.30rem; overflow-x: auto; cursor: zoom-in;
               transition: font-size 0.18s ease, background 0.18s ease,
                           border-left-color 0.18s ease, box-shadow 0.18s ease,
                           padding 0.18s ease; }
      .eqbox p { margin: 2px 0 0 0; }
      .eqbox:hover { font-size: 1.95rem;
                     background: #0D1E15; border-left-color: #6FE0A8;
                     box-shadow: 0 8px 28px rgba(0,0,0,0.55);
                     padding: 10px 20px 14px 20px; }
      /* label and caption are fixed in rem, so only the mathematics grows */
      .eqbox::-webkit-scrollbar { height: 6px; }
      .eqbox::-webkit-scrollbar-thumb { background: #2A4A38; border-radius: 3px; }
      @media (prefers-reduced-motion: reduce) {
        .eqbox, .eqbox .MathJax, .eqbox mjx-container { transition: none; }
      }

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
      div(
        class = "hero-row",
        div(
          class = "hero-main",
          div(class = "eyebrow", "WST795 Research Report  |  University of Pretoria"),
          h1(APP_TITLE)
        ),
        up_badge(116)
      ),
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
            p("Wastewater surveillance has become a powerful tool within the modern ",
              "world of public health management, specifically after the COVID-19 ",
              "pandemic. COVID-19 showed that an effective outbreak response requires ",
              "fast and accurate detection, efficient allocation of resources to people ",
              "in need and the ability to react predictively rather than ",
              "retrospectively (Saingam et al., 2024; Meadows et al., 2025)."),
            p("Clinical surveillance and wastewater surveillance aim to solve the same ",
              "issue. During COVID-19, individual testing required the consent of an ",
              "individual before testing could begin (Zhu et al., 2025). Wastewater ",
              "surveillance does not require the consent of individuals, as it tests ",
              "raw sewage for biological markers that tell public health officials what ",
              "is happening within a community. It is possible because people shed ",
              "viruses, bacteria and parasites in faeces, and it captures asymptomatic ",
              "and untested people. The two are ", tags$b("complementary"), " tools: ",
              "wastewater surveillance is inexpensive, non-invasive and less biased than ",
              "clinical surveillance used alone, and together they improve the speed of ",
              "disease detection and mapping (Dzinamarira et al., 2022; Zheng et al., ",
              "2024; Saingam et al., 2024)."),
            p("Antimicrobial resistance (AMR) in KwaZulu-Natal is the primary focus of ",
              "this report. Mares et al. (2021) define AMR as the process in which a ",
              "microorganism survives despite the presence of an antibiotic. Murray et ",
              "al. (2022) estimate that 4.95 million deaths globally were associated ",
              "with bacterial AMR in 2019, of which 1.27 million were directly ",
              "attributed to it, placing AMR among the leading causes of death ",
              "worldwide, with sub-Saharan Africa carrying the highest burden. ",
              "Sammarro et al. (2023) suggest that factors relating to water, ",
              "sanitation and hygiene are key drivers in the presence of AMR in Malawi."),
            p("AMR is a suitable subject for wastewater surveillance, since AMR markers ",
              "are shed through waste such as faeces and can be detected in samples ",
              "from wastewater treatment plants or sewer systems. The ",
              tags$b("household stool samples"), " used in this report bridge the gap ",
              "between what individuals shed and the sewage used in wastewater ",
              "surveillance: stool samples show which households carry AMR markers, ",
              "while wastewater surveillance shows the area as a whole.")
          )
        ),
        card(
          glow_header("The data"),
          card_body(
            p("A survey was conducted in the uMgungundlovu District within ",
              "KwaZulu-Natal in 2025. Stool samples were collected for up to nine ",
              "months and tested for the three AMR markers. Before data cleaning there ",
              "were 164 households; after cleaning, 162 households and 35 covariates ",
              "remained."),
            p("The covariates were grouped into eight ", tags$b("domains"), ":"),
            tags$ul(
              tags$li(tags$b("WASH: "), "water, sanitation and hygiene."),
              tags$li(tags$b("Antibiotics: "), "antibiotic use, sourcing and course completion."),
              tags$li(tags$b("Carriage: "), "the presence of the AMR markers."),
              tags$li(tags$b("Socioeconomic: "), "the economic status of a household."),
              tags$li(tags$b("Demographics: "), "factors that describe a household."),
              tags$li(tags$b("Food/animal: "), "presence of animals and consumption of animal products."),
              tags$li(tags$b("Health: "), "the health of members within a household."),
              tags$li(tags$b("Spatial: "), "location.")
            ),
            p("The 35 covariates are of several types: 18 binary indicators, 6 ",
              "categorical, 6 counts and 5 treated as continuous. This mixture is why ",
              "a mixed graphical model is required, and why each node in this app ",
              "carries a declared type and level."),
            p("Location is treated differently from classical spatial statistics, ",
              "where measures such as Moran's I quantify spatial autocorrelation ",
              "(Moraga, 2023). Here, ", tags$b("Site"), " represents the seven study ",
              "areas within the uMgungundlovu District and enters the model as a ",
              "covariate. The importance of location for disease spread is shown, for ",
              "example, by the association between elevation and the distribution of ",
              "cholera during the 2008-2009 epidemic in Harare, Zimbabwe (Luque ",
              "Fernandez et al., 2012)."),
            uiOutput("lp_data_status")
          )
        )
      ),

      div(
        card(
          glow_header("The aims"),
          card_body(
            div(class = "aim", tags$b("1. "),
                "Perform variable selection on a set of mixed variables, to identify ",
                "which covariates remain conditionally dependent on location and the ",
                "AMR markers, using mixed graphical models (MGMs)."),
            div(class = "aim", tags$b("2. "),
                "Use the information provided by the MGM to set up a foundation to ",
                "assist public health officials in AMR intervention."),
            p(class = "eqnote", style = "color:#9CB5A5; font-size:0.88rem; margin-top:10px;",
              "MGMs have been applied to factors associated with suicidality among ",
              "university students in China (Lu et al., 2024) and to the relationships ",
              "between chronic diseases (Alvarez-Galvez and Vegas-Lozano, 2022), ",
              "showing their usefulness for understanding associations in public health."),
            p(class = "eqnote", style = "color:#9CB5A5; font-size:0.88rem;",
              "Wastewater surveillance sees AMR presence within an area as a whole. ",
              "Stool samples, together with the survey, show which households carry ",
              "AMR and which household characteristics are linked to it.")
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
          p("The dataset contains covariates of various types. Since the aim is to ",
            "understand the dependency between the covariates, a framework that ",
            "models all of them together and returns their conditional dependence is ",
            "required. Mixed graphical models (MGMs) provide this framework."),
          hr(),
          h5("Graphical models and the meaning of an edge"),
          p("Haslbeck and Waldorp (2020) define an undirected graphical model as a ",
            "family of probability distributions whose conditional dependence is ",
            "illustrated by the connections between covariates on a graph. Let ",
            "\\(G = (V,E)\\) denote an undirected graph. The set of all nodes is ",
            "\\(V = \\{1,\\dots,p\\}\\), and each node represents a covariate. The set ",
            "of edges is \\(E \\subset V \\times V\\), and an edge connects two nodes. ",
            "Here, \\(p\\) denotes the number of covariates and \\(n\\) the number of ",
            "observations. Each node \\(s \\in V\\) corresponds to a random variable ",
            "\\(X_s\\), and \\(X = (X_1,\\dots,X_p)\\) collects them all. The ",
            "neighbourhood of a node \\(s\\) is ",
            "\\(N(s) := \\{t \\in V \\mid (s,t) \\in E\\}\\)."),
          chiprow(
            chip("An edge means", "Direct dependence",
                 paste("Two covariates remain dependent after conditioning on",
                       "every other covariate in the model."), tone = "warm"),
            chip("No edge means", "Explained away",
                 paste("Any association between them is explained away by the",
                       "remaining covariates."), tone = "cool")
          ),
          p("The graph \\(G\\) shows which covariates are dependent, but not how ",
            "strong those dependencies are, nor the distribution each covariate ",
            "follows. The strength of each dependency is recorded in the estimated ",
            "weighted adjacency matrix, which is visualised through the width of the ",
            "edges in the network."),
          hr(),
          h5("Factorisation"),
          p("A clique \\(C\\) is a set of nodes in which every pair of nodes is ",
            "connected by an edge. The joint distribution factorises over the cliques ",
            "of \\(G\\) and can be written as:"),
          eqbox("Equation (1)  |  Clique factorisation",
                "$$P(X) = \\exp\\left( \\sum_{C \\in \\mathcal{C}} \\theta_C \\phi_C(X_C) - \\Phi(\\theta) \\right)$$",
                "All structural information sits in the zero pattern of theta."),
          p("where \\(\\theta_C\\) is the parameter associated with the clique ",
            "function, \\(X_C\\) denotes the covariates in clique \\(C\\), ",
            "\\(\\phi_C(X_C)\\) is the sufficient statistic function specified by the ",
            "chosen member of the exponential family, \\(\\Phi(\\theta)\\) is the ",
            "log-normalising constant and \\(\\mathcal{C}\\) is the set of all cliques ",
            "(Haslbeck and Waldorp, 2020)."),
          hr(),
          h5("Node-conditional distributions"),
          p("A node-conditional distribution is the distribution of a covariate given ",
            "all remaining covariates. Each is taken to be a univariate member of the ",
            "exponential family:"),
          eqbox("Equation (2)  |  Node-conditional distribution",
                "$$P(X_s \\mid X_{\\setminus s}) = \\exp\\left\\{ E_s(X_{\\setminus s})\\phi_s(X_s) + B_s(X_s) - \\Phi(X_{\\setminus s}) \\right\\}$$",
                NULL),
          p("where \\(X_{\\setminus s}\\) denotes all covariates excluding \\(X_s\\), ",
            "\\(E_s(X_{\\setminus s})\\) is the canonical parameter, a function of all ",
            "covariates excluding \\(X_s\\), \\(\\phi_s(\\cdot)\\) is the sufficient ",
            "statistic function, \\(B_s(\\cdot)\\) is the base measure and ",
            "\\(\\Phi(X_{\\setminus s})\\) is the log-normalising constant ",
            "(Haslbeck and Waldorp, 2020)."),
          p("A mixed graphical model allows each node-conditional distribution to come ",
            "from a different member of the exponential family, which is what allows ",
            "covariates of various types to be modelled together. The Gaussian, ",
            "Poisson, Bernoulli and categorical distributions are all members."),
          p("The node-conditional distributions combine into a single joint ",
            "distribution only when each \\(E_s(X_{\\setminus s})\\) is built from the ",
            "sufficient statistics of the neighbours of node \\(s\\), with each term ",
            "involving at most \\(k\\) covariates, including \\(X_s\\) (Haslbeck and ",
            "Waldorp, 2020). The joint distribution is then Markov with respect to ",
            "\\(G\\): two covariates that are not joined by an edge are conditionally ",
            "independent given all of the other covariates. This structure is what ",
            "allows each node to be estimated with its own regression."),
          hr(),
          h5("Parameters behind an edge"),
          p("The number of parameters that define an edge depends on the covariates it ",
            "connects. Two continuous covariates share a single parameter, while ",
            "interactions involving categorical covariates bring several parameters. ",
            "An edge is therefore present if at least one of the interaction ",
            "parameters is nonzero:"),
          eqbox("Edge rule",
                "$$(s,r) \\in E \\iff \\exists\\, z : |\\theta^{z}_{s,r}| > 0$$",
                "z runs over the R parameters of the interaction between s and r."),
          p("where \\(\\theta^{z}_{s,r}\\) is the \\(z\\)-th parameter of the ",
            "interaction between \\(s\\) and \\(r\\), for \\(z \\in \\{1,\\dots,R\\}\\). ",
            "Here, \\(R\\) is the number of parameters of the interaction, which ",
            "depends on the types of covariates involved (Haslbeck and Waldorp, 2020).")
        )
      ),

      nav_panel(
        "MGM Estimation",
        icon = bsicons::bs_icon("sliders2"),
        card_body(
          class = "theory",
          h5("Neighbourhood selection"),
          p("The model is estimated by regressing every covariate on all the others, ",
            "so \\(p\\) regressions are fitted. The covariates with nonzero ",
            "coefficients in the regression on node \\(s\\) form the estimated ",
            "neighbourhood \\(\\hat{N}(s)\\); this strategy is known as neighbourhood ",
            "selection (Haslbeck and Waldorp, 2020)."),
          p("To obtain estimates that are exactly zero, and hence a sparse and ",
            "interpretable graph, each regression carries an \\(\\ell_1\\) penalty, ",
            "giving the least absolute shrinkage and selection operator (LASSO; ",
            "Tibshirani, 1996):"),
          eqbox("Equation (3)  |  LASSO",
                "$$\\hat{\\theta}_s = \\arg\\min_{\\theta} \\left\\{ -\\mathcal{L}(\\theta, X) + \\lambda_s \\|\\theta\\|_1 \\right\\}$$",
                "The L1 penalty is what makes estimates exactly zero."),
          p("where \\(\\|\\theta\\|_1\\) is the sum of the absolute values of the ",
            "parameters, \\(\\lambda_s\\) is the regularisation parameter of the ",
            "regression on node \\(s\\), and \\(\\mathcal{L}(\\theta, X)\\) is the ",
            "log-likelihood of the node-conditional distribution of node \\(s\\). ",
            "Larger values of \\(\\lambda_s\\) shrink more parameters to zero, giving a ",
            "sparser graph (Haslbeck and Waldorp, 2020)."),
          hr(),
          h5("Thresholding"),
          p("Haslbeck and Waldorp (2020) show that, to guarantee control of false and ",
            "true positives for the \\(\\ell_1\\)-penalised estimator, a lower bound on ",
            "the size of the true effects is required. This is known as the ",
            tags$b("beta-min condition"), ". Thresholding the estimates at ",
            "\\(\\tau_s\\) approximately enforces this condition. The ",
            tags$code("mgm"), " package (version 1.2-15) in R applies the following ",
            "threshold to each nodewise regression (Haslbeck and Waldorp, 2020):"),
          eqbox("Threshold  |  mgm default",
                "$$\\tau_s = \\sqrt{d}\\, \\big\\lVert \\hat{\\theta}_s \\big\\rVert_2 \\sqrt{\\frac{\\log P_s}{n}}$$",
                "Estimates smaller than tau_s in absolute value are set to zero."),
          p("where \\(n\\) is the number of observations, \\(d = k - 1\\) with \\(k\\) ",
            "the maximum size of cliques, \\(\\hat{\\theta}_s\\) is the estimated ",
            "parameter vector of the regression on node \\(s\\) excluding the ",
            "intercept, \\(\\lVert\\hat{\\theta}_s\\rVert_2\\) is its Euclidean length, ",
            "and \\(P_s\\) is the number of columns of the design matrix for node ",
            "\\(s\\). Any estimate below \\(\\tau_s\\) in absolute value is set to zero, ",
            "separately for each category of a categorical node. Switching the ",
            "threshold off in the sidebar removes this control, so the resulting graph ",
            "is a sensitivity check rather than a result."),
          hr(),
          h5("Reconciling the two regressions"),
          p("Once the estimates are thresholded, a single weight is computed for each ",
            "interaction. Because each node is regressed separately, a \\(k\\)-order ",
            "interaction yields \\(k\\) estimates, which are reconciled in Step 2 of ",
            "Algorithm 1. Both rules take the arithmetic mean of the \\(k\\) estimates: ",
            "the OR-rule takes it unconditionally, whereas the AND-rule takes it only ",
            "if all \\(k\\) estimates are nonzero, and sets the parameter to zero ",
            "otherwise (Haslbeck and Waldorp, 2020)."),
          chiprow(
            chip("OR-rule", "Sensitive  |  used in this report",
                 "Retains an edge if either regression selects it.", tone = "warm"),
            chip("AND-rule", "Conservative",
                 "Retains an edge only if both regressions select it.")
          ),
          hr(),
          h5("Algorithm 1  |  Estimating mixed graphical models via neighbourhood regression"),
          p(class = "eqnote", "Reproduced from Haslbeck and Waldorp (2020, p. 9)."),
          tags$ol(
            tags$li("For each \\(s \\in V\\):",
              tags$ol(type = "a",
                tags$li("Construct the design matrix defined by \\(k\\), the order of the MGM."),
                tags$li("Solve the LASSO problem in Equation (3) with regularisation parameter \\(\\lambda\\)."),
                tags$li("Threshold the estimates at \\(\\tau\\)."),
                tags$li("Aggregate interactions with several parameters into a single edge weight.")
              )),
            tags$li("Combine the edge weights with the AND- or OR-rule."),
            tags$li("Define \\(G\\) based on the zero / nonzero pattern in the combined parameter vector, according to the edge rule.")
          ),
          hr(),
          h5("Selecting the regularisation parameter"),
          p("The regularisation parameter is chosen for each nodewise regression, ",
            "either by ", tags$b("cross-validation"), " (used in this report) or by ",
            "the extended Bayesian information criterion (EBIC; Haslbeck and ",
            "Waldorp, 2020):"),
          eqbox("Equation (4)  |  Extended BIC",
                "$$\\mathrm{EBIC}_{\\gamma}(\\hat{\\theta}_s) = -2\\mathcal{L}(\\hat{\\theta}_s) + \\hat{s}_0 \\log n + 2\\gamma\\, \\hat{s}_0 \\log p$$",
                "The lambda_s minimising this is retained."),
          p("where \\(\\mathcal{L}(\\hat{\\theta}_s)\\) is the log-likelihood evaluated ",
            "at \\(\\hat{\\theta}_s\\), \\(\\hat{s}_0\\) is the number of nonzero ",
            "neighbours selected in the regression on node \\(s\\), \\(p\\) is the ",
            "number of covariates and \\(\\gamma\\) is the tuning parameter. Larger ",
            "values of \\(\\gamma\\) penalise dense graphs more heavily and return ",
            "fewer edges, while \\(\\gamma = 0\\) gives the ordinary BIC. Haslbeck and ",
            "Waldorp (2020) report that raising \\(\\gamma\\) from 0 to 0.25 ",
            "considerably reduces false positives without increasing false negatives ",
            "too much."),
          hr(),
          h5("Limitations of Algorithm 1"),
          tags$ul(
            tags$li(tags$b("Larger k. "), "As \\(k\\) increases, the number of potential ",
                    "interactions increases with it (Haslbeck and Waldorp, 2020); for ",
                    "\\(k \\geq 3\\), interactions become more difficult to interpret than ",
                    "a pairwise edge."),
            tags$li(tags$b("A single penalty. "), "Each nodewise regression uses a single ",
                    "\\(\\lambda_s\\) for all its predictors, even when they are of ",
                    "different types, so it can shrink some types of edges more than ",
                    "others because the covariates are measured on different scales. ",
                    "This is partly reduced by standardising the Gaussian covariates.")
          ),
          hr(),
          h5("Predictability"),
          p("Predictability is crucial for judging the practical relevance of an edge ",
            "(Haslbeck and Waldorp, 2018), and it is what the ring around each node in ",
            "the network shows. It matters for three reasons: it shows which edges can ",
            "be considered practically meaningless, based on how much of a node's ",
            "variation is explained by its neighbours; it indicates to what extent a ",
            "node can be influenced by acting on its neighbours; and, across nodes, high ",
            "predictability shows the network is largely determined by itself, while low ",
            "predictability suggests that factors not captured in the survey play a ",
            "bigger role."),
          p("For continuous and count covariates, predictability is the proportion of ",
            "variance explained (Haslbeck and Waldorp, 2018):"),
          eqbox("Equation (5)  |  Explained variance",
                "$$R^2_s = 1 - \\frac{\\operatorname{Var}(\\hat{x}_s - x_s)}{\\operatorname{Var}(x_s)}$$",
                NULL),
          p("where \\(x_s\\) denotes the observed values of node \\(s\\) for the ",
            "\\(n\\) observations and \\(\\hat{x}_s\\) their prediction by its ",
            "neighbours. For categorical covariates, the normalised accuracy is used ",
            "(Haslbeck and Waldorp, 2018):"),
          eqbox("Equation (6)  |  Normalised accuracy",
                "$$\\mathrm{nAcc}_s = \\frac{\\mathrm{Acc}_s - \\max\\{\\pi_{s,1},\\dots,\\pi_{s,m_s}\\}}{1 - \\max\\{\\pi_{s,1},\\dots,\\pi_{s,m_s}\\}}, \\qquad \\mathrm{Acc}_s = \\frac{1}{n}\\sum_{i=1}^{n} \\mathbb{I}(x_{is} = \\hat{x}_{is})$$",
                NULL),
          p("where \\(\\mathrm{Acc}_s\\) is the proportion of correct predictions at node ",
            "\\(s\\), \\(x_{is}\\) is observation \\(i\\)'s actual category and ",
            "\\(\\hat{x}_{is}\\) its prediction by the neighbours, \\(\\mathbb{I}\\) is ",
            "the indicator function, \\(m_s\\) is the number of categories of node ",
            "\\(s\\), and \\(\\pi_{s,j}\\) is the proportion of observations in category ",
            "\\(j\\), for \\(j = 1,\\dots,m_s\\)."),
          div(
            class = "alert note-info",
            tags$b("In the app. "),
            "The MGM Explorer loads with the specification used in the report: all ",
            "covariates, \\(k = 2\\), \\(\\lambda_s\\) selected by 10-fold ",
            "cross-validation for each nodewise regression, the OR-rule and the ",
            "default threshold. Every one of those choices is a control in its ",
            "sidebar, and changing any of them refits the model rather than redrawing ",
            "a cached one."
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
    div(
      class = "sec-head-row",
      sec_head("Section 5  |  Synthesis", "Conclusion"),
      up_badge(96)
    ),
    sec_nav("Conclusion"),
    layout_column_wrap(
      width = 1,
      card(
        glow_header("Synthesis of Findings"),
        card_body(
          p("This report set out to perform spatial variable selection on a set of ",
            "mixed variables, to identify which covariates remain conditionally ",
            "dependent on location and the AMR markers, for 162 households in the ",
            "uMgungundlovu District."),
          tags$ul(
            tags$li(tags$b("Network sparsity: "), "The network contains 97 of a ",
                    "possible 595 edges, so most pairs of covariates are not ",
                    "conditionally dependent on one another."),
            tags$li(tags$b("Location: "), "Site, the covariate that introduces the ",
                    "spatial aspect into the model, has the greatest number of ",
                    "connections (21 of a possible 34). A large share of its weight lies ",
                    "with WASH covariates such as Refuse collected, Formal dwelling and ",
                    "Household flooding."),
            tags$li(tags$b("AMR markers: "), "The markers in the Carriage domain have ",
                    "very few edges, mostly within the Carriage domain itself. ",
                    tags$i("E. coli"), " is the only marker connected to location, and ",
                    "that conditional dependence is weak."),
            tags$li(tags$b("Previous findings: "), "The link between carriage and ",
                    "Toilet floods agrees with previous findings, whereas the links with ",
                    "Persons per room and Piped water indoors do not."),
            tags$li(tags$b("Overall: "), "Location is conditionally dependent mainly on ",
                    "sanitation, and not directly on the AMR markers.")
          ),
          p("CLIQUE complements the report by giving the reader the tools to explore ",
            "the network and to see how changing the estimation settings influences ",
            "the results.")
        )
      ),
      card(
        glow_header("Limitations and future work"),
        card_body(
          tags$ul(
            tags$li(tags$b("How location is treated. "), "Site enters the model as a ",
                    "seven-level categorical node rather than as a separate node per ",
                    "study code, so edges connected to Site have no sign, and a positive ",
                    "or negative conditional dependence cannot be concluded."),
            tags$li(tags$b("Single imputation. "), "Missing values were handled by single ",
                    "imputation (van Buuren, 2018), which may distort the relationships ",
                    "between covariates and could lead to edges existing where they ",
                    "should not."),
            tags$li(tags$b("Data cleaning decisions. "), "Decisions made during data ",
                    "cleaning, such as the grouping of tertiary education, affect how ",
                    "household covariates are defined. The Node Dictionary documents ",
                    "these choices for every covariate.")
          ),
          h5(class = "card-subhead", "Future work"),
          p("Future work could use spatial methods such as spatial autocorrelation to ",
            "gain further insight into the importance of location for disease ",
            "transmission.")
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
