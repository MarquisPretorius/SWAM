###############################################################################
# 01_clean_AIARMS.R
#
# Purpose : Turn the raw AIARMS household survey + monthly culture results into
#           a MIXED GRAPHICAL MODEL (MGM) ready object, following the data
#           requirements of Haslbeck & Waldorp (2020, JSS 93(8)), the `mgm`
#           package paper.
#
# What mgm() needs (Haslbeck & Waldorp 2020, Section 3.1):
#   1. `data`  : a fully NUMERIC matrix/data.frame, NO missing values.
#   2. `type`  : character vector, one entry per column:
#                  "g" = conditional Gaussian
#                  "p" = conditional Poisson (non-negative integer counts)
#                  "c" = conditional categorical (multinomial)
#   3. `level` : integer vector, number of categories for each variable.
#                By convention level = 1 for every "g" and "p" variable.
#   4. Categorical variables must be coded as consecutive integers.
#      - Binary variables are coded 0/1 so that `binarySign = TRUE` can define
#        a SIGN for binary-binary and binary-continuous edges.
#      - Variables with m > 2 categories are coded 1..m (no sign is definable
#        for these; the paper explains why in Section 2.2).
#
# Output: ./output/AIARMS_mgm.rds  -- a list with $data, $type, $level,
#         $labels, $groups, $groups_color, plus the untouched cleaned frame.
#
# Author: prepared for the AIARMS analysis
###############################################################################

## ---------------------------------------------------------------------------
## 0. Setup
## ---------------------------------------------------------------------------
rm(list = ls())

pkgs <- c("dplyr", "stringr", "tidyr", "tibble")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

RAW_PATH <- "full_AIARMS_df.csv"   # <-- edit if the file lives elsewhere
OUT_DIR  <- "output"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

## Read with explicit UTF-8 so the R-currency / en-dash characters in the
## income bands do not turn into mojibake ("R1601 - R3200 (R19201 - R38400)").
raw <- read.csv(RAW_PATH, stringsAsFactors = FALSE, check.names = TRUE,
                fileEncoding = "UTF-8", na.strings = c("", "NA", "na", "N/A"))

cat("Raw dimensions:", dim(raw), "\n")

## ---------------------------------------------------------------------------
## 1. Small helper functions
## ---------------------------------------------------------------------------

## The survey column names are enormous ("X118..D010a..Toilet.Facilities...").
## `col()` finds the single column whose name starts with a short stem, so the
## script stays readable and survives minor re-exports of the questionnaire.
col <- function(stem) {
  hit <- grep(paste0("^", stem), names(raw), value = TRUE)
  if (length(hit) == 0) stop("No column starting with: ", stem)
  raw[[hit[1]]]
}

## Yes/No -> 1/0
yn <- function(x) ifelse(x == "Yes", 1L, ifelse(x == "No", 0L, NA_integer_))

## "Select all that apply" checkbox blocks: a blank cell means "not ticked",
## not "unknown", so NA is legitimately recoded to 0.
yn0 <- function(x) ifelse(!is.na(x) & x == "Yes", 1L, 0L)

## Grab every column of a checkbox block, e.g. block("X123") returns
## X123.1..Before.eating ... X123.8..When.your.hands.appear.dirty
block <- function(stem, idx) {
  nm <- paste0("^", stem, "\\.", idx, "\\.")
  do.call(cbind, lapply(nm, function(p) {
    hit <- grep(p, names(raw), value = TRUE)
    if (length(hit) == 0) rep(NA_character_, nrow(raw)) else raw[[hit[1]]]
  }))
}

## ---------------------------------------------------------------------------
## 2. De-duplicate participants
## ---------------------------------------------------------------------------
## Study_code has 164 unique values across 171 rows (repeat survey submissions).
## An MGM assumes i.i.d. rows, so duplicated participants would inflate the
## effective sample size and bias the lasso towards keeping their edges.
dup <- duplicated(raw$Study_code)
cat("Dropping", sum(dup), "duplicate Study_code rows.\n")
raw <- raw[!dup, ]

## ---------------------------------------------------------------------------
## 3. OUTCOMES: monthly stool/rectal culture results -> carriage counts
## ---------------------------------------------------------------------------
## Each participant has up to 9 monthly results per organism. Free-text entry
## produced variants: "EC_Poss", "E.coli", "No Growth", "KP_NEg",
## "ESBL_ Ecoli", "ESBL _EC + KP", ... We normalise case/underscores/spaces and
## then apply a positive / negative rule per organism.

months <- c("March","May","June","July","August","September",
            "October","November","December")

norm <- function(x) tolower(gsub("[_[:space:]]+", " ", trimws(x)))

ec_pos <- function(x) {
  s <- norm(x)
  out <- rep(NA_integer_, length(s))
  out[grepl("no growth|neg", s)]            <- 0L
  out[is.na(out) & grepl("ec|coli", s)]     <- 1L
  out
}
kp_pos <- function(x) {
  s <- norm(x)
  out <- rep(NA_integer_, length(s))
  out[grepl("neg", s)]                      <- 0L
  out[is.na(out) & grepl("kp", s)]          <- 1L
  ## cells reading only "E. coli" are E. coli results mis-filed in a KP column
  out
}
esbl_pos <- function(x) {
  s <- norm(x)
  out <- rep(NA_integer_, length(s))
  out[grepl("neg", s)]                      <- 0L
  out[is.na(out) & grepl("esbl", s)]        <- 1L
  out
}

EC <- sapply(months, function(m) ec_pos(raw[[paste0("EC_",  m)]]))
KP <- sapply(months, function(m) kp_pos(raw[[paste0("KP_",  m)]]))
ES <- sapply(months, function(m) esbl_pos(raw[[paste0("ESBL_", m)]]))

## Number of positive months (a count -> Poisson node) and number of months
## actually cultured. Because participants were sampled an unequal number of
## times, `Months_tested` is carried into the model as a Gaussian covariate:
## mgm has no offset term, so conditioning on the exposure is the closest
## available equivalent to a rate model.
D <- data.frame(
  Study_code    = raw$Study_code,
  EC_pos_n      = rowSums(EC, na.rm = TRUE),
  KP_pos_n      = rowSums(KP, na.rm = TRUE),
  ESBL_pos_n    = rowSums(ES, na.rm = TRUE),
  Months_tested = rowSums(!is.na(ES)),
  stringsAsFactors = FALSE
)

## Convenience binaries (not used in the default model, handy for checks)
D$ESBL_ever <- as.integer(D$ESBL_pos_n > 0)
D$KP_ever   <- as.integer(D$KP_pos_n   > 0)

## NOTE: E. coli carriage is near-saturated (~97% of cultured months positive),
## so EC_pos_n carries almost no variance and is excluded from the default
## variable set. It is kept in the frame in case you want to model it.

## ---------------------------------------------------------------------------
## 4. DEMOGRAPHY AND SOCIO-ECONOMIC POSITION
## ---------------------------------------------------------------------------
D$Age <- as.numeric(col("X4\\.\\."))                                   # "g"

D$Sex <- ifelse(col("X5\\.\\.") == "Female", 1L, 0L)                   # "c", 2

## X8 asks for people EXCLUDING the respondent, so add 1 for household size.
D$HHsize <- as.numeric(col("X8\\.\\.")) + 1                            # "p"

rooms_map <- c("One"=1,"Two"=2,"Three"=3,"Four"=4,"Five"=5,
               "Six"=6,"Seven"=7,"More than seven"=8)
D$Rooms <- unname(rooms_map[col("X99\\.\\.")])                         # "p"

## Crowding = persons per room. A classic transmission-relevant exposure and
## a genuinely continuous quantity, so it enters as Gaussian.
D$Crowding <- D$HHsize / D$Rooms                                       # "g"

## Highest household education, collapsed to 3 ordered-but-nominal levels.
## MGM treats "c" variables as unordered; collapsing avoids 18 sparse states.
edu <- col("X52\\.\\.")
D$Education <- ifelse(
  grepl("Diploma|Certificate|degree|BTech|Post graduate", edu), 3L,
  ifelse(grepl("Grade 12|Grade 13", edu) & !grepl("Attend Grade 12", edu), 2L,
         ifelse(!is.na(edu), 1L, NA_integer_)))                        # "c", 3
# 1 = primary / incomplete secondary, 2 = completed secondary, 3 = tertiary

ws <- col("X53\\.\\.")
D$WorkStatus <- ifelse(ws %in% c("Employer","Paid employee","Self-employed",
                                 "Paid domestic worker"), 1L,
                       ifelse(ws == "Unemployed", 2L, 3L))                    # "c", 3
# 1 = employed, 2 = unemployed, 3 = pensioner / student / other

## Income bands are ordered, so they are far more informative as a single
## ordinal score (Gaussian) than as 11 categorical states we cannot estimate.
inc_levels <- c("No income","R1-R400","R401 - R800","R801 - R1600",
                "R1601 - R3200","R3201 - R6400","R6401 - R12800",
                "R12801 - R25600","R25601 - R51200","R51201 - R102400")
inc_raw <- col("X61\\.\\.")
D$IncomeBand <- sapply(inc_raw, function(s) {
  if (is.na(s) || grepl("Prefer not", s)) return(NA_real_)
  hit <- which(startsWith(s, inc_levels))
  if (length(hit)) hit[1] - 1 else NA_real_
}, USE.NAMES = FALSE)                                                  # "g"

D$Grant <- yn(col("X59\\.\\."))                                        # "c", 2

## ---------------------------------------------------------------------------
## 5. HOUSING, WATER, SANITATION, HYGIENE (the WASH block)
## ---------------------------------------------------------------------------
dw <- col("X95\\.\\.")
D$FormalDwelling <- as.integer(grepl("^House|^Room/flatlet", dw))      # "c", 2

## Two versions of the water question exist in the export: an old single-choice
## item (X110) and a newer checkbox block (X112.*). We take the old item where
## present and fall back to the checkboxes, then reduce to the distinction that
## actually matters for contamination risk: water inside the dwelling or not.
w_old <- col("X110\\.\\.")
w_in  <- as.integer(yn0(block("X112", 1)))
w_yd  <- as.integer(yn0(block("X112", 2)))
D$PipedInside <- ifelse(
  !is.na(w_old) & w_old == "Piped water inside the dwelling", 1L,
  ifelse(!is.na(w_old) & w_old %in% c("Piped water outside the dwelling (e.g., yard)",
                                      "Public tap/standpipe","Other"), 0L,
         ifelse(w_in == 1, 1L, ifelse(w_yd == 1, 0L, NA_integer_))))   # "c", 2

## Toilet: flush (to sewer or septic) versus pit latrine / public / none.
## The intermediate "pit with slab" group has only 14 members, too few to
## support its own categorical state at n = 164, so it joins the unimproved arm.
tl <- col("X118\\.\\.")
D$FlushToilet    <- as.integer(grepl("flush", tl, ignore.case = TRUE)) # "c", 2
D$ToiletShared   <- yn(col("X121\\.\\."))                              # "c", 2
D$ToiletFloods   <- yn(col("X120\\.\\."))                              # "c", 2
D$OpenDefecation <- as.integer(col("X122\\.\\.") != "Never")           # "c", 2

D$StandingWater  <- as.integer(yn0(block("X126", 1)))                              # "c", 2
D$Flooding       <- as.integer(yn0(block("X128", 1)))                              # "c", 2
D$RefuseCollected <- as.integer(startsWith(
  ifelse(is.na(col("X108\\.\\.")), "", col("X108\\.\\.")), "Removed")) # "c", 2

## Handwashing: 8 checkboxes summed into a 0-8 practice score. A sum of
## indicators is exactly the kind of variable the Poisson node is for.
D$HandwashScore <- rowSums(block("X123", 1:8) == "Yes", na.rm = TRUE)  # "p"

## ---------------------------------------------------------------------------
## 6. HEALTH SERVICE USE AND ANTIBIOTIC BEHAVIOUR
## ---------------------------------------------------------------------------
cf <- col("X62\\.\\.")
## "Never" has only 2 observations, so it is merged with "Yearly".
D$CareMonthly <- as.integer(cf %in% c("Weekly", "Montly"))             # "c", 2

D$HIVinHH <- yn(col("X82\\.\\."))                                      # "c", 2

ch <- col("X76\\.\\.")
D$ChronicAny <- ifelse(is.na(ch), NA_integer_,
                       as.integer(ch != "No chronic conditions"))      # "c", 2

sev <- col("X81\\.\\.")
D$DiarrSeverity <- ifelse(sev == "No one experiences diarrhoea", 1L,
                          ifelse(sev == "Mild (resolves in 1-2 days)",   2L,
                                 ifelse(!is.na(sev), 3L, NA_integer_)))              # "c", 3
# 1 = no diarrhoea, 2 = mild, 3 = moderate or severe

D$AbxNoRx <- yn(col("X90\\.\\."))                                      # "c", 2

src <- col("X86\\.\\.")
D$AbxSource <- ifelse(is.na(src), NA_integer_,
                      ifelse(src == "Not applicable", 1L,
                             ifelse(src == "Government facility (Clinic; Community Heath Centre, Hospital)",
                                    2L, 3L)))                                        # "c", 3
# 1 = does not source antibiotics, 2 = government facility only,
# 3 = private GP and/or pharmacy (with or without government)

crs <- col("X92\\.\\.")
## "Until the package is empty" (n = 2) is merged with "As advised", since both
## describe completing a full prescribed course.
D$AbxCourse <- ifelse(crs %in% c("As advised by a healthcare provider",
                                 "Until the package is empty"), 1L,
                      ifelse(crs == "Until symptoms are resolved or cured", 2L,
                             ifelse(crs == "I do not take antibiotics", 3L, NA_integer_)))
# "c", 3
# 1 = completes course, 2 = stops when symptoms resolve, 3 = does not take

## ---------------------------------------------------------------------------
## 7. FOOD, ANIMALS, ENVIRONMENTAL EXPOSURE
## ---------------------------------------------------------------------------
D$StreetFood <- as.integer(col("X135\\.\\.") != "Don't consume street food")

## Meat frequency: the questionnaire changed mid-study (X129 old, X131 new) and
## both are checkbox grids of frequency x meat type. We take the HIGHEST
## frequency ticked for any meat type: 1 = never ... 6 = daily, minus 1 so the
## score starts at 0.
meat_blocks <- cbind(block("X129", 1:6), block("X131", 1:6))
meat_level  <- rep(NA_real_, nrow(raw))
for (j in 1:6) {
  ticked <- !is.na(meat_blocks[, j]) | !is.na(meat_blocks[, j + 6])
  meat_level[ticked] <- j
}
D$MeatFreq <- meat_level - 1                                           # "g"

D$AnimalsKept <- yn(col("X157\\.\\."))                                 # "c", 2
D$ManureUse   <- as.integer(rowSums(block("X150", 1:5) == "Yes",
                                    na.rm = TRUE) > 0)                 # "c", 2

## ---------------------------------------------------------------------------
## 8. Variable registry: name -> type, level, group
## ---------------------------------------------------------------------------
## This registry IS the contract with mgm(). Everything downstream (fitting,
## plotting, the Shiny app) reads `type` and `level` from here, so a variable
## can never drift out of sync with its declared distribution.

registry <- tibble::tribble(
  ~var,             ~type, ~level, ~group,        ~label,
  # --- outcomes -------------------------------------------------------------
  "ESBL_pos_n",     "p",   1L,     "Carriage",    "ESBL+ months",
  "KP_pos_n",       "p",   1L,     "Carriage",    "K. pneumoniae+ months",
  "Months_tested",  "g",   1L,     "Carriage",    "Months cultured",
  # --- demography / SES -----------------------------------------------------
  "Age",            "g",   1L,     "Demographic", "Age",
  "Sex",            "c",   2L,     "Demographic", "Sex (1=F)",
  "HHsize",         "p",   1L,     "Demographic", "Household size",
  "Crowding",       "g",   1L,     "Demographic", "Persons per room",
  "Education",      "c",   3L,     "Socioeconomic","Highest education",
  "WorkStatus",     "c",   3L,     "Socioeconomic","Work status",
  "IncomeBand",     "g",   1L,     "Socioeconomic","Income band",
  "Grant",          "c",   2L,     "Socioeconomic","Receives grant",
  # --- WASH -----------------------------------------------------------------
  "FormalDwelling", "c",   2L,     "WASH",        "Formal dwelling",
  "PipedInside",    "c",   2L,     "WASH",        "Piped water indoors",
  "FlushToilet",    "c",   2L,     "WASH",        "Flush toilet",
  "ToiletShared",   "c",   2L,     "WASH",        "Shared toilet",
  "ToiletFloods",   "c",   2L,     "WASH",        "Toilet floods",
  "OpenDefecation", "c",   2L,     "WASH",        "Open defecation",
  "StandingWater",  "c",   2L,     "WASH",        "Standing water",
  "Flooding",       "c",   2L,     "WASH",        "Household flooding",
  "RefuseCollected","c",   2L,     "WASH",        "Refuse collected",
  "HandwashScore",  "p",   1L,     "WASH",        "Handwashing score (0-8)",
  # --- health service use / antibiotics -------------------------------------
  "CareMonthly",    "c",   2L,     "Health",      "Care sought monthly+",
  "HIVinHH",        "c",   2L,     "Health",      "HIV in household",
  "ChronicAny",     "c",   2L,     "Health",      "Any chronic condition",
  "DiarrSeverity",  "c",   3L,     "Health",      "Diarrhoea severity",
  "AbxNoRx",        "c",   2L,     "Antibiotic",  "Antibiotics w/o Rx",
  "AbxSource",      "c",   3L,     "Antibiotic",  "Antibiotic source",
  "AbxCourse",      "c",   3L,     "Antibiotic",  "Course completion",
  # --- food / animals -------------------------------------------------------
  "StreetFood",     "c",   2L,     "Food/animal", "Eats street food",
  "MeatFreq",       "g",   1L,     "Food/animal", "Meat frequency",
  "AnimalsKept",    "c",   2L,     "Food/animal", "Keeps animals",
  "ManureUse",      "c",   2L,     "Food/animal", "Uses manure"
)

## A smaller "core" set for when you want more power per parameter. With
## n = 164 the full 32-node model is a genuinely high-dimensional problem
## (the l1 penalty keeps it identified, but sensitivity is low).
core_vars <- c("ESBL_pos_n","KP_pos_n","Months_tested","Age","Sex","HHsize",
               "Crowding","Education","IncomeBand","PipedInside","FlushToilet",
               "ToiletShared","OpenDefecation","StandingWater","HandwashScore",
               "HIVinHH","AbxNoRx","AbxSource","AbxCourse","AnimalsKept")

## ---------------------------------------------------------------------------
## 9. Missing data
## ---------------------------------------------------------------------------
## mgm() cannot take NA. Missingness here is light (<= 6 cells per variable),
## so median (continuous / count) or modal (categorical) single imputation is
## used by default. Set IMPUTE <- FALSE for complete-case analysis instead.
IMPUTE <- TRUE

miss_report <- sapply(registry$var, function(v) sum(is.na(D[[v]])))
cat("\nMissing cells per model variable:\n"); print(miss_report[miss_report > 0])

if (IMPUTE) {
  for (i in seq_len(nrow(registry))) {
    v <- registry$var[i]
    x <- D[[v]]
    if (!any(is.na(x))) next
    fill <- if (registry$type[i] == "c") {
      as.numeric(names(sort(table(x), decreasing = TRUE))[1])   # mode
    } else {
      median(x, na.rm = TRUE)                                    # median
    }
    D[[v]][is.na(x)] <- fill
  }
} else {
  D <- D[complete.cases(D[, registry$var]), ]
}
cat("Rows retained:", nrow(D), "\n")

## ---------------------------------------------------------------------------
## 10. Validate the coding against mgm's requirements
## ---------------------------------------------------------------------------
## Any violation caught here would otherwise surface as an obscure glmnet error
## deep inside mgm(), so we check explicitly:
##   - "p" variables are non-negative integers
##   - "c" variables have exactly `level` distinct consecutive values,
##     starting at 0 (binary, so binarySign works) or 1 (m > 2)
for (i in seq_len(nrow(registry))) {
  v <- registry$var[i]; ty <- registry$type[i]; lv <- registry$level[i]
  x <- D[[v]]
  if (ty == "p") {
    stopifnot(all(x >= 0), all(x == round(x)))
  }
  if (ty == "c") {
    u <- sort(unique(x))
    if (lv == 2) {
      if (!identical(as.numeric(u), c(0, 1)))
        stop("Binary variable ", v, " is not coded 0/1: ", paste(u, collapse = ","))
    } else {
      if (!identical(as.numeric(u), as.numeric(seq_len(lv))))
        stop("Categorical ", v, " must be coded 1..", lv, "; found: ",
             paste(u, collapse = ","))
    }
  }
}
cat("Coding validation passed for all", nrow(registry), "model variables.\n")

## ---------------------------------------------------------------------------
## 11. Assemble and save the MGM object
## ---------------------------------------------------------------------------
group_cols <- c(Carriage = "#B2182B", Demographic = "#EF8A62",
                Socioeconomic = "#FDDBC7", WASH = "#67A9CF",
                Health = "#2166AC", Antibiotic = "#7B3294",
                `Food/animal` = "#5AAE61")

AIARMS <- list(
  data         = as.matrix(D[, registry$var]),
  type         = registry$type,
  level        = registry$level,
  labels       = registry$label,
  vars         = registry$var,
  groups       = split(seq_len(nrow(registry)), registry$group),
  groups_color = unname(group_cols[names(split(seq_len(nrow(registry)),
                                               registry$group))]),
  registry     = registry,
  core_vars    = core_vars,
  clean_frame  = D
)

saveRDS(AIARMS, file.path(OUT_DIR, "AIARMS_mgm.rds"))
write.csv(D, file.path(OUT_DIR, "AIARMS_clean.csv"), row.names = FALSE)

cat("\nSaved:", file.path(OUT_DIR, "AIARMS_mgm.rds"), "\n")
cat("Model matrix:", nrow(AIARMS$data), "rows x", ncol(AIARMS$data), "nodes\n")
print(table(AIARMS$type))