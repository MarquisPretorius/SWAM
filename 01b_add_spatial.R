###############################################################################
# 01b_add_spatial.R
#
# Adds a SPATIAL block to the cleaned AIARMS object without touching
# 01_clean_AIARMS.R. Reads output/AIARMS_mgm.rds, joins coordinates back from
# the raw export, derives spatial covariates, and writes
# output/AIARMS_mgm_spatial.rds with the same structure plus the new nodes.
#
# Run AFTER 01_clean_AIARMS.R:
#     source("01_clean_AIARMS.R")
#     source("01b_add_spatial.R")
#
# ---------------------------------------------------------------------------
# WHAT CHANGED IN THIS VERSION
# ---------------------------------------------------------------------------
# The spatial grouping is now the FIELDWORK SITE CODE itself -- ARUE, ARUO,
# ARUT, ARUL, ARUS, ARUU, ARUF -- rather than an anonymous merged "Zone".
#
#   * Node renamed Zone -> Site, and its categories keep their real codes, so
#     the Area Profile and showInteraction() output name actual sites instead
#     of the integers 1..4.
#   * SITE_MIN_N dropped from 20 to 10. At 20 the merge loop collapsed seven
#     sites into four (ARUF -> ARUU, then ARUS -> ARUO, then ARUU -> ARUO),
#     which is exactly the site identity you wanted to keep. At 10 only ARUF
#     merges. Set SITE_MIN_N <- 0 to keep all seven codes untouched.
#   * The trend surface (East_km / North_km) is now OFF by default. See the
#     note in section 4a: with site membership in the graph the two describe
#     overlapping structure, and including both puts near-collinear nodes in
#     the same model.
#
# THE THREE NOTIONS OF SPACE, AND WHICH ONE EACH NODE ANSWERS
#
#   (a) AREA MEMBERSHIP   -- Site        [ON by default]
#       Which fieldwork area a household belongs to. Captures discrete
#       between-settlement differences: shared standpipe, shared sewer line,
#       shared refuse round. This is the one you asked for.
#
#   (b) LOCAL CONTEXT     -- NN_density  [ON by default]
#       How many other study households sit within a fixed radius. Built
#       environment density as an exposure in its own right, and the spatial
#       variable most likely to connect to crowding and WASH.
#
#   (c) LARGE-SCALE TREND -- East_km, North_km   [OFF by default]
#       A linear trend surface. An edge from East_km means a variable shifts
#       systematically along the east-west axis, conditional on the rest.
#
#   A fourth notion -- the spatially lagged OUTCOME -- is derived in section 6
#   but deliberately left out of the node set. See the note there.
###############################################################################

## Deliberately NOT rm(list = ls()) here. This script is often run a section at
## a time in the console, and wiping the workspace mid-way is how you end up
## with "object 'REG' not found" from section 7. Set RESET <- TRUE if you do
## want a clean slate.
RESET <- FALSE
if (RESET) rm(list = setdiff(ls(), "RESET"))

OUT_DIR  <- "output"
RAW_PATH <- "full_AIARMS_df.csv"

## load_aiarms() is idempotent: safe to call again from any section.
load_aiarms <- function() {
  f <- file.path(OUT_DIR, "AIARMS_mgm.rds")
  if (!file.exists(f))
    stop("Cannot find ", f, "\n",
         "Run 01_clean_AIARMS.R first -- 01b extends its output, it does not ",
         "rebuild it.")
  readRDS(f)
}

AIARMS <- load_aiarms()
REG    <- AIARMS$registry
D      <- AIARMS$clean_frame

## ---------------------------------------------------------------------------
## 0. Switches
## ---------------------------------------------------------------------------
## How the fieldwork site enters the model. All three produce valid models;
## they answer the question differently. Numbers below are from this cohort.
##
##   "categorical"     ONE node, level = m. mgm handles it natively. Per-site
##                     detail is recovered with showInteraction(). One edge per
##                     partner, no sign.
##
##   "dummy_reference" m-1 BINARY nodes, one site held out as reference.
##                     RECOMMENDED if you want per-site nodes: each site gets
##                     its own node, its own edges, and -- because binary nodes
##                     are coded 0/1 with binarySign = TRUE -- its own SIGN.
##                     Predicting each dummy from the others gives R^2 of
##                     0.18-0.27 here, so they are ordinary correlated
##                     predictors, nothing degenerate. Cost: the reference site
##                     has no node, and every effect is read relative to it.
##
##   "dummy_full"      ALL m binary nodes. This is the one that breaks. The
##                     dummies sum to 1 in every row, so predicting any one
##                     from the other m-1 gives R^2 = 1.000000 exactly. Each
##                     site node's own nodewise logistic regression is then
##                     perfectly separated: its predictors determine it with
##                     certainty. glmnet will not error -- the L1 penalty keeps
##                     a solution finite -- but the coefficients are not
##                     identified, the lasso picks arbitrarily among equivalent
##                     solutions, and you get a clique of strong artefactual
##                     edges among the site nodes that is an artefact of the
##                     coding, not of the data. Which site attaches to an
##                     outcome then flips between bootstrap samples.
##                     Provided so you can see it; not recommended.
##
## Note: mgm's own overparameterize = TRUE is NOT this. There the indicators
## are predictors inside another node's design matrix and never receive a
## conditional model of their own, which is why the L1 penalty is enough there
## and is not enough here.
SITE_ENCODING  <- "dummy_reference"
SITE_REFERENCE <- NULL   # NULL = use the largest site; or name one, e.g. "ARUE"

SITE_MIN_N     <- 10     # merge a site into its nearest neighbour below this n
#   0  -> keep all seven codes exactly as they are
#   10 -> only ARUF (n = 3) merges, leaving six sites
#   20 -> collapses to four zones (the old behaviour)
INCLUDE_TREND  <- FALSE  # add East_km / North_km alongside Site
DROP_OUTLIERS  <- TRUE
OUTLIER_KM     <- 10
RADIUS_M       <- 150    # neighbourhood radius for NN_density
K_NN           <- 8      # neighbours for the spatial weights matrix

## ---------------------------------------------------------------------------
## 1. Join coordinates back on Study_code
## ---------------------------------------------------------------------------
raw <- read.csv(RAW_PATH, stringsAsFactors = FALSE, check.names = TRUE,
                fileEncoding = "UTF-8", na.strings = c("", "NA", "na", "N/A"))
raw <- raw[!duplicated(raw$Study_code), ]      # same de-duplication as 01

geo <- raw[, c("Study_code", "Longitude.x", "Latitude.x")]
D   <- merge(D, geo, by = "Study_code", all.x = TRUE, sort = FALSE)
D   <- D[match(AIARMS$clean_frame$Study_code, D$Study_code), ]   # restore order
stopifnot(!any(is.na(D$Latitude.x)))

## Fieldwork site is encoded in the Study_code prefix.
D$Site_code <- sub("^([A-Za-z]+).*$", "\\1", D$Study_code)
cat("Fieldwork sites as recorded:\n"); print(table(D$Site_code))

## ---------------------------------------------------------------------------
## 2. Project longitude/latitude to local metres
## ---------------------------------------------------------------------------
## Over a study area a few km across, an equirectangular projection about the
## median point is accurate to well under a metre, so there is no need for sf
## or a formal CRS transformation. Distances are then ordinary Euclidean.
R_EARTH <- 6371000
lat0 <- median(D$Latitude.x); lon0 <- median(D$Longitude.x)
D$x_m <- R_EARTH * cos(lat0 * pi/180) * (D$Longitude.x - lon0) * pi/180
D$y_m <- R_EARTH * (D$Latitude.x - lat0) * pi/180

## ---------------------------------------------------------------------------
## 3. Outlying households
## ---------------------------------------------------------------------------
## Two ARUO households are recorded ~62 km from every other point, at identical
## coordinates. Whether that is a second site or a GPS/data-entry error, they
## would dominate any distance-based quantity: the trend surface would fit one
## line through them, and their nearest-neighbour density would be 1.
d_centre <- sqrt(D$x_m^2 + D$y_m^2) / 1000
is_out   <- d_centre > OUTLIER_KM
cat("\nHouseholds beyond", OUTLIER_KM, "km of the study centroid:", sum(is_out), "\n")
if (sum(is_out)) print(D[is_out, c("Study_code", "Longitude.x", "Latitude.x")])

if (DROP_OUTLIERS && any(is_out)) {
  D <- D[!is_out, ]
  cat("Dropped. Retained", nrow(D), "households for the spatial model.\n")
}

## ---------------------------------------------------------------------------
## 4. Spatial covariates
## ---------------------------------------------------------------------------

## (a) Trend surface -- OFF unless INCLUDE_TREND is TRUE.
##     Kilometres rather than metres so the coefficients sit on a scale
##     comparable to the other Gaussian nodes after scale = TRUE.
##
##     Why off by default now that Site is in the model: the sites ARE the
##     spatial partition of this cohort, and their centroids span barely 2 km.
##     A linear surface fitted across that span is largely a smoothed version
##     of site membership, so the two nodes compete to explain the same
##     variation. The lasso then splits the signal arbitrarily between them and
##     both edges look weaker than the structure actually is. Turn it on only
##     if you specifically want to ask whether anything varies smoothly ACROSS
##     sites rather than BETWEEN them.
D$East_km  <- (D$x_m - mean(D$x_m)) / 1000                            # "g"
D$North_km <- (D$y_m - mean(D$y_m)) / 1000                            # "g"

## (b) Local density: number of OTHER study households within RADIUS_M.
##     A count of points in a disc, so a Poisson node. Median nearest-neighbour
##     distance is about 17 m, so 150 m captures the immediate cluster.
dmat <- as.matrix(dist(cbind(D$x_m, D$y_m)))
D$NN_density <- rowSums(dmat < RADIUS_M) - 1                          # "p"

## (c) Site: the fieldwork area code, kept as the grouping.
##
##     Merging rule: while any site has fewer than SITE_MIN_N households, the
##     smallest is merged into the site with the NEAREST centroid. Merging by
##     proximity rather than by convenience keeps every resulting group
##     spatially coherent -- a merged group is still a contiguous patch of the
##     study area, not a scattering of unrelated households.
##
##     Why a floor exists at all: a categorical level is estimated from the
##     households in it. ARUF has three. Three observations cannot support the
##     parameters of their own multinomial level, and mgm will either drop the
##     level or produce an estimate driven by those three rows. Setting
##     SITE_MIN_N <- 0 keeps all seven codes if you would rather see that
##     happen than merge.
site <- D$Site_code
merge_log <- character(0)
repeat {
  tab <- table(site)
  if (all(tab >= SITE_MIN_N) || length(tab) <= 2) break
  small <- names(tab)[which.min(tab)]
  cent  <- t(sapply(split(seq_along(site), site),
                    function(i) c(mean(D$x_m[i]), mean(D$y_m[i]))))
  others <- setdiff(rownames(cent), small)
  dd <- sqrt((cent[others, 1] - cent[small, 1])^2 +
               (cent[others, 2] - cent[small, 2])^2)
  target <- others[which.min(dd)]
  merge_log <- c(merge_log,
                 sprintf("  %s (n = %d) -> %s  (centroids %.2f km apart)",
                         small, min(tab), target, min(dd) / 1000))
  site[site == small] <- target
}

site_f      <- factor(site)
site_labels <- levels(site_f)
n_site      <- nlevels(site_f)

## Build whichever node set SITE_ENCODING asked for.
site_vars <- character(0)

if (SITE_ENCODING == "categorical") {
  D$Site    <- as.integer(site_f)                                     # "c", m
  site_vars <- "Site"
  
} else {
  ## One binary node per site, coded 0/1 so binarySign = TRUE gives each a sign.
  all_sites <- site_labels
  if (SITE_ENCODING == "dummy_reference") {
    ref <- if (is.null(SITE_REFERENCE)) names(which.max(table(site)))
    else SITE_REFERENCE
    if (!ref %in% all_sites)
      stop("SITE_REFERENCE '", ref, "' is not one of: ",
           paste(all_sites, collapse = ", "))
    kept <- setdiff(all_sites, ref)
    cat("\nReference site (absorbed, no node):", ref,
        sprintf("(n = %d)\n", sum(site == ref)))
  } else if (SITE_ENCODING == "dummy_full") {
    kept <- all_sites
    warning("SITE_ENCODING = 'dummy_full': the site dummies sum to 1 in every ",
            "row and are exactly collinear (R^2 = 1). Nodewise regressions for ",
            "these nodes are perfectly separated and the edges among them are ",
            "artefacts of the coding. Use 'dummy_reference' unless you are ",
            "deliberately inspecting that artefact.", call. = FALSE)
  } else {
    stop("SITE_ENCODING must be 'categorical', 'dummy_reference' or 'dummy_full'.")
  }
  for (a in kept) {
    v <- paste0("Site_", a)
    D[[v]] <- as.integer(site == a)                                   # "c", 2
    site_vars <- c(site_vars, v)
  }
}
D$Site_group <- site      # merged code as text, for maps and tables

cat("\nSite grouping (SITE_MIN_N =", SITE_MIN_N, "):\n")
if (length(merge_log)) {
  cat("merges performed:\n"); cat(merge_log, sep = "\n"); cat("\n")
} else {
  cat("no merges needed; all site codes retained\n")
}
print(table(site))
cat("\nInteger code -> site:\n")
print(setNames(site_labels, seq_len(n_site)))

## (d) Distance from the centre of the study area. Correlated with the trend
##     surface by construction, so it is derived but NOT in the default set.
D$Dist_centre_km <- sqrt((D$x_m - mean(D$x_m))^2 +
                           (D$y_m - mean(D$y_m))^2) / 1000

## ---------------------------------------------------------------------------
## 5. Spatial weights matrix (k nearest neighbours, row-standardised)
## ---------------------------------------------------------------------------
## Used by the Moran's I diagnostics in 02_fit_mgm.R. Symmetrised so that
## "i is a neighbour of j" implies the reverse, then row-standardised so each
## household's neighbours carry a total weight of 1.
W <- matrix(0, nrow(D), nrow(D))
for (i in seq_len(nrow(D))) {
  nb <- order(dmat[i, ])[2:(K_NN + 1)]   # [1] is the household itself
  W[i, nb] <- 1
}
W <- (W + t(W)) / 2
W <- W / rowSums(W)

## ---------------------------------------------------------------------------
## 6. Spatially lagged carriage  (derived, but OFF by default)
## ---------------------------------------------------------------------------
## The mean carriage of a household's neighbours. As a graph node this is
## seductive but awkward, for two reasons worth being explicit about:
##
##   1. ENDOGENEITY. It is built from other rows' values of the very outcome
##      it would be joined to. In spatial econometrics this is the lagged
##      dependent variable, and it is not exogenous; the resulting edge cannot
##      be read the way the other edges are read.
##   2. INDEPENDENCE. An MGM assumes i.i.d. rows. A node constructed across
##      rows breaks that assumption directly.
##
## Moran's I on the model residuals answers the same question -- is there
## spatial structure left over? -- without either problem.
D$ESBL_lag <- as.numeric(W %*% D$ESBL_pos_n)
D$KP_lag   <- as.numeric(W %*% D$KP_pos_n)

## ---------------------------------------------------------------------------
## Prerequisite guard
## ---------------------------------------------------------------------------
## If you jumped straight here, these recover or explain what is missing rather
## than failing on an unhelpful "object not found".
if (!exists("AIARMS")) AIARMS <- load_aiarms()
if (!exists("REG"))    REG    <- AIARMS$registry
need <- c("D", "n_site", "site_labels", "site_vars", "RADIUS_M", "dmat", "W")
gone <- need[!vapply(need, exists, logical(1))]
if (length(gone))
  stop("Section 7 needs objects built earlier in this script: ",
       paste(gone, collapse = ", "),
       "\nRun the whole file instead:  source(\"01b_add_spatial.R\")")

## ---------------------------------------------------------------------------
## 7. Extend the registry
## ---------------------------------------------------------------------------
## NN_density is an ADDITION to the site nodes, not a replacement for them.
## They answer different questions: site membership is which settlement you are
## in, density is how built-up your immediate surroundings are. Set
## INCLUDE_DENSITY <- FALSE if you want the site nodes alone.
INCLUDE_DENSITY <- TRUE

site_reg <- if (SITE_ENCODING == "categorical") {
  data.frame(var = "Site", type = "c", level = as.integer(n_site),
             group = "Spatial", label = "Fieldwork site",
             stringsAsFactors = FALSE)
} else {
  data.frame(var = site_vars, type = "c", level = 2L, group = "Spatial",
             label = paste("Site", sub("^Site_", "", site_vars)),
             stringsAsFactors = FALSE)
}

spatial_reg <- site_reg
if (INCLUDE_DENSITY)
  spatial_reg <- rbind(
    data.frame(var = "NN_density", type = "p", level = 1L, group = "Spatial",
               label = paste0("Neighbours within ", RADIUS_M, "m"),
               stringsAsFactors = FALSE),
    spatial_reg)
if (INCLUDE_TREND) {
  spatial_reg <- rbind(
    data.frame(var = c("East_km", "North_km"), type = "g", level = 1L,
               group = "Spatial", label = c("Easting (km)", "Northing (km)"),
               stringsAsFactors = FALSE),
    spatial_reg)
}
REG2 <- rbind(as.data.frame(REG), spatial_reg)

## Same validation as in 01: "p" non-negative integers, "c" consecutive from
## 0 (binary) or 1 (m > 2).
for (i in seq_len(nrow(spatial_reg))) {
  v <- spatial_reg$var[i]; ty <- spatial_reg$type[i]; lv <- spatial_reg$level[i]
  x <- D[[v]]
  if (anyNA(x)) stop("Spatial variable ", v, " has missing values.")
  if (ty == "p") stopifnot(all(x >= 0), all(x == round(x)))
  if (ty == "c") {
    u <- sort(unique(x))
    ok <- if (lv == 2) identical(as.numeric(u), c(0, 1)) else
      identical(as.numeric(u), as.numeric(seq_len(lv)))
    if (!ok) stop("Categorical ", v, " badly coded: ", paste(u, collapse = ","))
  }
}
cat("\nSpatial coding validation passed.\n")

## ---------------------------------------------------------------------------
## 8. Build the model matrix, with a guard
## ---------------------------------------------------------------------------
## mgm() rejects anything that is not integer or numeric with the message
##   "Only integer and numeric values permitted."
## which does not say WHICH column is at fault. as.matrix() on a data.frame
## containing even one character or factor column silently coerces EVERY
## column to character, so a single stray column poisons the whole matrix and
## the error surfaces much later, inside mgm(). This guard names the offender
## instead, and coerces the cases that are safe to coerce.
build_model_matrix <- function(df, vars) {
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars))
    stop("Columns missing from the frame: ", paste(missing_vars, collapse = ", "))
  
  cls <- vapply(df[vars], function(z) class(z)[1], character(1))
  bad <- vars[!cls %in% c("numeric", "integer", "logical", "factor")]
  if (length(bad)) {
    stop("These columns are not numeric and cannot be coerced safely:\n",
         paste(sprintf("  %s  (class %s, e.g. %s)", bad, cls[bad],
                       vapply(df[bad], function(z)
                         paste(utils::head(unique(as.character(z)), 3),
                               collapse = " / "), character(1))),
               collapse = "\n"),
         "\nRemove them from the registry, or convert them upstream.")
  }
  ## logical -> 0/1, factor -> its integer codes; both are legitimate here.
  for (v in vars) {
    if (is.logical(df[[v]])) df[[v]] <- as.integer(df[[v]])
    if (is.factor(df[[v]]))  df[[v]] <- as.integer(df[[v]])
  }
  M <- as.matrix(df[, vars, drop = FALSE])
  storage.mode(M) <- "double"
  if (anyNA(M)) {
    na_cols <- vars[colSums(is.na(M)) > 0]
    stop("NA values remain in: ", paste(na_cols, collapse = ", "),
         " -- mgm() cannot take missing data.")
  }
  M
}

Xall <- build_model_matrix(D, REG2$var)
cat("Model matrix built:", nrow(Xall), "x", ncol(Xall),
    "| storage mode:", storage.mode(Xall), "\n")

## Core set gains the spatial block, so the default fit in 02 answers the
## question this script was added for.
core_vars2 <- c(AIARMS$core_vars, spatial_reg$var)

group_cols <- c(Carriage = "#B2182B", Demographic = "#EF8A62",
                Socioeconomic = "#FDDBC7", WASH = "#67A9CF",
                Health = "#2166AC", Antibiotic = "#7B3294",
                `Food/animal` = "#5AAE61", Spatial = "#1B7837")

AIARMS_SP <- list(
  data         = Xall,
  type         = REG2$type,
  level        = REG2$level,
  labels       = REG2$label,
  vars         = REG2$var,
  groups       = split(seq_len(nrow(REG2)), REG2$group),
  groups_color = unname(group_cols[names(split(seq_len(nrow(REG2)), REG2$group))]),
  registry     = REG2,
  core_vars    = core_vars2,
  clean_frame  = D,
  ## spatial extras carried alongside for the diagnostics and the map
  coords       = as.matrix(D[, c("x_m", "y_m")]),
  lonlat       = as.matrix(D[, c("Longitude.x", "Latitude.x")]),
  W            = W,
  K_NN         = K_NN,
  radius_m     = RADIUS_M,
  site_labels  = site_labels,      # every retained site code
  site_vars    = site_vars,        # the node name(s) carrying site information
  site_encoding = SITE_ENCODING,
  site_group   = D$Site_group,     # merged code per household, as text
  site         = D$Site_code       # original, unmerged code per household
)

saveRDS(AIARMS_SP, file.path(OUT_DIR, "AIARMS_mgm_spatial.rds"))
write.csv(D, file.path(OUT_DIR, "AIARMS_clean_spatial.csv"), row.names = FALSE)

cat("\nSaved:", file.path(OUT_DIR, "AIARMS_mgm_spatial.rds"), "\n")
cat("Model matrix:", nrow(Xall), "rows x", ncol(Xall), "nodes",
    "(", nrow(spatial_reg), "spatial )\n")
print(table(AIARMS_SP$type))

## ---------------------------------------------------------------------------
## 9. Reading the Site node afterwards
## ---------------------------------------------------------------------------
## Site is one categorical node, so the network shows ONE edge weight per
## partner: the mean absolute value of the several parameters behind it, and
## no sign. The per-site parameters are still in the fit. After running
## 02_fit_mgm.R:
##
##   j <- which(AIARMS_SP$vars == "Site")
##   showInteraction(fit_mgm, int = c(j, which(AIARMS_SP$vars == "ESBL_pos_n")))
##
## and map the integer codes back with AIARMS_SP$site_labels.
if (SITE_ENCODING == "categorical") {
  cat("\nSite codes in the fitted model:",
      paste(sprintf("%d=%s", seq_along(site_labels), site_labels), collapse = "  "),
      "\n")
} else {
  cat("\nSite nodes in the fitted model:", paste(site_vars, collapse = ", "), "\n")
  cat("Each is binary 0/1, so each carries its own SIGN in the network.\n")
}