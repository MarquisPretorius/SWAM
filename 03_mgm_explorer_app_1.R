###############################################################################
# 03_mgm_explorer_app.R
#
# An interactive Shiny explorer for the AIARMS mixed graphical model.
# Run with:   shiny::runApp("03_mgm_explorer_app.R")
#   (or open the file in RStudio and click "Run App")
#
# What you can change live:
#   * which variables are in the model (by domain, or hand-picked)
#   * k (pairwise vs 3-way), the EBIC gamma, AND vs OR rule, thresholding
#   * an edge-weight cut-off for display, so weak edges can be hidden without
#     refitting
#   * which node you want the neighbourhood of
#
# Every refit calls mgm() with exactly the arguments shown in the sidebar, so
# what you see is always a real model, never a redrawing of a cached one.
###############################################################################

library(shiny)
library(mgm)
library(qgraph)
library(DT)

## Prefer the spatial object when 01b_add_spatial.R has been run; fall back to
## the original otherwise, so the app works either way with no edits.
sp_path <- file.path("output", "AIARMS_mgm_spatial.rds")
HAS_SPATIAL <- file.exists(sp_path)
AIARMS <- readRDS(if (HAS_SPATIAL) sp_path else file.path("output", "AIARMS_mgm.rds"))
REG    <- AIARMS$registry

PAL <- c("Carriage"="#B2182B","Demographic"="#EF8A62","Socioeconomic"="#FDDBC7",
         "WASH"="#67A9CF","Health"="#2166AC","Antibiotic"="#7B3294",
         "Food/animal"="#5AAE61","Spatial"="#1B7837")

## ---------------------------------------------------------------------------
## UI
## ---------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("AIARMS mixed graphical model explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Variables"),
      radioButtons("preset", NULL,
                   c("Core set"        = "core",
                     "All variables"   = "full",
                     "Pick manually"   = "manual"),
                   selected = "core"),
      conditionalPanel(
        "input.preset == 'manual'",
        checkboxGroupInput("domains", "Domains to include",
                           choices  = sort(unique(REG$group)),
                           selected = sort(unique(REG$group))),
        selectizeInput("vars", "Fine-tune nodes", choices = REG$var,
                       multiple = TRUE, options = list(plugins = list("remove_button")))
      ),

      hr(), h4("Estimation"),
      radioButtons("k", "Interaction order (k)",
                   c("Pairwise (k = 2)" = "2", "Include 3-way (k = 3)" = "3"),
                   selected = "2"),
      radioButtons("lamSel", "Select lambda by",
                   c("EBIC" = "EBIC", "Cross-validation" = "CV"), selected = "EBIC"),
      conditionalPanel("input.lamSel == 'EBIC'",
        sliderInput("gamma", "EBIC gamma (higher = sparser)",
                    min = 0, max = 1, value = 0.25, step = 0.25)),
      radioButtons("rule", "Combine neighbourhoods with",
                   c("AND (conservative)" = "AND", "OR (sensitive)" = "OR"),
                   selected = "AND"),
      checkboxInput("thresh", "Apply beta-min threshold (tau)", TRUE),

      hr(), h4("Display"),
      sliderInput("cut", "Hide edges weaker than", 0, 0.5, 0, step = 0.01),
      selectInput("focus", "Highlight neighbourhood of", choices = c("(none)")),
      selectInput("layout", "Layout", c("spring", "circle"), "spring"),
      checkboxInput("rings", "Show predictability rings", TRUE),

      hr(),
      actionButton("go", "Fit model", class = "btn-primary btn-block"),
      helpText("A 20-node pairwise fit takes a few seconds; k = 3 or CV takes longer.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Network", plotOutput("net", height = "700px"),
                 verbatimTextOutput("summary")),
        tabPanel("Edges", DTOutput("edgetab")),
        tabPanel("Predictability", plotOutput("predplot", height = "500px"),
                 DTOutput("errtab")),
        tabPanel("Interaction detail",
                 fluidRow(column(6, selectInput("i1", "Node A", NULL)),
                          column(6, selectInput("i2", "Node B", NULL))),
                 verbatimTextOutput("intdetail"),
                 helpText("For an edge involving a variable with more than two ",
                          "categories, the weight shown in the network is the mean ",
                          "absolute value of several parameters. This tab prints them all.")),
        tabPanel("Map",
                 conditionalPanel("!output.hasSpatial",
                   helpText("Run 01b_add_spatial.R to enable the map.")),
                 fluidRow(
                   column(5, selectInput("mapvar", "Colour households by", NULL)),
                   column(4, checkboxInput("mapsize", "Size by local density", TRUE)),
                   column(3, checkboxInput("maplag", "Show neighbour average instead", FALSE))),
                 plotOutput("map", height = "560px"),
                 verbatimTextOutput("moran"),
                 helpText("Moran's I is the correlation between a household's value and ",
                          "the average of its k nearest neighbours. Positive means nearby ",
                          "households resemble each other; near zero means location carries ",
                          "no information about this variable. The p-value comes from 499 ",
                          "random permutations of the values across locations.")),
        tabPanel("Variable dictionary", DTOutput("dict"))
      )
    )
  )
)

## ---------------------------------------------------------------------------
## Server
## ---------------------------------------------------------------------------
server <- function(input, output, session) {

  ## --- which columns are in play -------------------------------------------
  selected_vars <- reactive({
    switch(input$preset,
           core   = AIARMS$core_vars,
           full   = REG$var,
           manual = {
             v <- REG$var[REG$group %in% input$domains]
             if (length(input$vars)) union(v, input$vars) else v
           })
  })

  ## --- fit ------------------------------------------------------------------
  ## eventReactive means nothing is recomputed until "Fit model" is pressed:
  ## dragging the display sliders redraws instantly without refitting.
  model <- eventReactive(input$go, {
    v <- selected_vars()
    validate(need(length(v) >= 4, "Select at least four variables."))

    idx   <- match(v, AIARMS$vars)
    X     <- AIARMS$data[, idx, drop = FALSE]
    type  <- AIARMS$type[idx]
    level <- AIARMS$level[idx]
    labs  <- AIARMS$labels[idx]
    grp   <- REG$group[idx]

    args <- list(data = X, type = type, level = level,
                 k = as.numeric(input$k),
                 lambdaSel = input$lamSel,
                 ruleReg = input$rule,
                 threshold = if (input$thresh) "LW" else "none",
                 binarySign = TRUE, scale = TRUE,
                 overparameterize = (input$k == "3"),
                 pbar = FALSE)
    if (input$lamSel == "EBIC") args$lambdaGam   <- input$gamma
    if (input$lamSel == "CV")   args$lambdaFolds <- 10

    withProgress(message = "Fitting MGM...", value = 0.5, {
      fit <- do.call(mgm, args)
      pr  <- predict(fit, X, errorCon = c("RMSE","R2"), errorCat = c("CC","nCC"))
    })

    ## keep node choosers in sync with the fitted model
    updateSelectInput(session, "focus", choices = c("(none)", labs))
    updateSelectInput(session, "i1", choices = labs)
    updateSelectInput(session, "i2", choices = labs, selected = labs[2])

    list(fit = fit, X = X, type = type, level = level, labels = labs,
         groups = grp, pred = pr)
  }, ignoreNULL = FALSE)   # fit once on start-up with the defaults

  ## --- adjacency matrix as displayed ---------------------------------------
  wadj_display <- reactive({
    m <- model()
    w <- m$fit$pairwise$wadj
    w[abs(w) < input$cut] <- 0          # display cut-off only, not a refit
    dimnames(w) <- list(m$labels, m$labels)
    w
  })

  output$net <- renderPlot({
    m   <- model()
    w   <- wadj_display()
    ec  <- m$fit$pairwise$edgecolor
    ec[w == 0] <- NA

    ## fade everything that is not attached to the focus node
    if (!is.null(input$focus) && input$focus != "(none)") {
      f <- match(input$focus, m$labels)
      keep <- matrix(FALSE, nrow(w), ncol(w))
      keep[f, ] <- TRUE; keep[, f] <- TRUE
      w[!keep] <- 0
    }

    rings <- if (isTRUE(input$rings)) {
      r <- ifelse(m$type == "c", m$pred$errors[, "nCC"], m$pred$errors[, "R2"])
      r[is.na(r)] <- 0; r
    } else NULL

    glist <- split(seq_along(m$labels), m$groups)
    qgraph(w,
           edge.color = ec,
           layout     = input$layout,
           repulsion  = 1.1,
           pie        = rings,
           pieColor   = ifelse(m$type == "c", "tomato", "lightblue"),
           groups     = glist,
           color      = unname(PAL[names(glist)]),
           nodeNames  = m$labels,
           labels     = seq_along(m$labels),
           legend     = TRUE, legend.cex = 0.35,
           vsize = 4.5, esize = 14)
  })

  output$summary <- renderPrint({
    m <- model(); w <- wadj_display()
    cat("Nodes:", ncol(m$X), " Observations:", nrow(m$X), "\n")
    cat("Edges shown:", sum(w[upper.tri(w)] != 0),
        "of", choose(ncol(m$X), 2), "possible\n")
    cat("Node types: ", paste(names(table(m$type)), table(m$type),
                              sep = " = ", collapse = ",  "), "\n")
  })

  output$edgetab <- renderDT({
    m <- model(); w <- wadj_display()
    ut <- which(upper.tri(w) & w != 0, arr.ind = TRUE)
    if (!nrow(ut)) return(datatable(data.frame(Message = "No edges above cut-off.")))
    sgn <- m$fit$pairwise$signs[ut]
    d <- data.frame(From = m$labels[ut[,1]], To = m$labels[ut[,2]],
                    Weight = round(w[ut], 3),
                    Sign = ifelse(is.na(sgn), "undefined",
                                  ifelse(sgn > 0, "positive", "negative")))
    datatable(d[order(-abs(d$Weight)), ], rownames = FALSE,
              options = list(pageLength = 20))
  })

  output$predplot <- renderPlot({
    m <- model()
    r <- ifelse(m$type == "c", m$pred$errors[, "nCC"], m$pred$errors[, "R2"])
    r[is.na(r)] <- 0
    o <- order(r)
    par(mar = c(4, 12, 2, 2))
    barplot(r[o], horiz = TRUE, names.arg = m$labels[o], las = 1,
            cex.names = 0.7, xlim = c(0, 1),
            col = ifelse(m$type[o] == "c", "tomato", "lightblue"),
            xlab = "Predictability  (R2 for continuous, normalised accuracy for categorical)")
  })

  output$errtab <- renderDT({
    m <- model()
    datatable(data.frame(Variable = m$labels, Type = m$type,
                         round(m$pred$errors[, -1, drop = FALSE], 3)),
              rownames = FALSE, options = list(pageLength = 20))
  })

  output$intdetail <- renderPrint({
    m <- model()
    i <- match(input$i1, m$labels); j <- match(input$i2, m$labels)
    if (is.na(i) || is.na(j) || i == j) return(cat("Choose two different nodes."))
    showInteraction(m$fit, int = c(i, j))
  })

  ## --- map ------------------------------------------------------------------
  output$hasSpatial <- reactive(HAS_SPATIAL)
  outputOptions(output, "hasSpatial", suspendWhenHidden = FALSE)

  observe({
    m <- model()
    updateSelectInput(session, "mapvar", choices = m$labels,
                      selected = if ("ESBL+ months" %in% m$labels) "ESBL+ months"
                                 else m$labels[1])
  })

  ## Moran's I with a permutation test. Kept inline so the app needs no extra
  ## package; identical to the function used in 02_fit_mgm.R.
  moran_test <- function(v, W, nperm = 499) {
    v <- as.numeric(v)
    if (sd(v) == 0) return(list(I = NA, p = NA))
    n <- length(v); z <- v - mean(v)
    Ifun <- function(z) (n / sum(W)) * as.numeric(t(z) %*% W %*% z) /
                        as.numeric(t(z) %*% z)
    I_obs <- Ifun(z)
    I_perm <- replicate(nperm, Ifun(sample(z)))
    list(I = I_obs, p = (sum(abs(I_perm) >= abs(I_obs)) + 1) / (nperm + 1))
  }

  map_values <- reactive({
    req(HAS_SPATIAL); m <- model()
    j <- match(input$mapvar, m$labels); req(!is.na(j))
    v <- m$X[, j]
    ## The neighbour average is the spatial lag: each household replaced by the
    ## mean of its neighbours. Smoothing it this way makes clustering visible
    ## that individual points can hide.
    if (isTRUE(input$maplag)) v <- as.numeric(AIARMS$W %*% v)
    v
  })

  output$map <- renderPlot({
    req(HAS_SPATIAL)
    m  <- model(); xy <- AIARMS$coords; v <- map_values()
    pal_v <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(10)
    cols  <- pal_v[cut(v, breaks = 10, labels = FALSE)]
    dj <- grep("^Neighbours within", m$labels)[1]
    cx <- if (isTRUE(input$mapsize) && !is.na(dj))
            0.7 + 1.8 * m$X[, dj] / max(m$X[, dj]) else 1.3
    par(mar = c(4, 4, 3, 1))
    plot(xy[, 1], xy[, 2], asp = 1, pch = 21, bg = cols, col = "grey30", cex = cx,
         xlab = "Easting (m from centroid)", ylab = "Northing (m from centroid)",
         main = paste0(input$mapvar,
                       if (isTRUE(input$maplag)) "  (neighbour average)" else ""))
    legend("topleft", legend = c("low", "", "", "", "high"),
           pt.bg = pal_v[c(1, 3, 5, 7, 10)], pch = 21, bty = "n", cex = 0.85)
  })

  output$moran <- renderPrint({
    req(HAS_SPATIAL)
    m <- model(); j <- match(input$mapvar, m$labels); req(!is.na(j))
    r  <- moran_test(m$X[, j], AIARMS$W)
    rj <- if (m$type[j] %in% c("g", "p"))
            moran_test(m$X[, j] - m$pred$predicted[, j], AIARMS$W) else NULL
    cat("Variable:", input$mapvar, "  (k =", AIARMS$K_NN, "neighbours)\n")
    cat(sprintf("  raw       Moran's I = %+.3f   p = %.3f\n", r$I, r$p))
    if (!is.null(rj))
      cat(sprintf("  residual  Moran's I = %+.3f   p = %.3f\n", rj$I, rj$p))
    cat(sprintf("  expected under no autocorrelation: %+.3f\n",
                -1 / (nrow(m$X) - 1)))
  })

  output$dict <- renderDT({
    datatable(REG[, c("var","label","type","level","group")], rownames = FALSE,
              colnames = c("Column","Label","mgm type","Levels","Domain"),
              options = list(pageLength = 35))
  })
}

shinyApp(ui, server)
