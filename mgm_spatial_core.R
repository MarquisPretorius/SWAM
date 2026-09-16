## ---------------------------------------------------------------------------
## mgm_spatial_core.R
##
## Tangled from WST795_analysis.Rmd on 2026-09-15.
## Edit the Rmd, not this file.
## ---------------------------------------------------------------------------

## ---- libs ----
## Only these two are needed. Every figure is base graphics, so the tangled
## core -- and therefore the Shiny app -- carries no plotting dependency.
suppressPackageStartupMessages({
  library(sf)      # only for the projection cross-check in Section 3
  library(spdep)   # neighbours, weights, reference implementations
})

## ---- expand-core ----
## Their equirectangular x_m / y_m are already in metres, so coordinates are
## taken straight from the object rather than re-projected.
mgm_coords <- function(A) A$coords

## Site is a categorical node whose integer codes stand for ARUE, ARUF, ARUL,
## ARUO, ARUS, ARUT, ARUU. level_name() turns the code back into the code the
## fieldwork used, so nothing downstream is ever labelled "Site = 4".
level_name <- function(A, v, l) {
  if (identical(v, "Site") && !is.null(A$site_labels) &&
      l >= 1 && l <= length(A$site_labels)) A$site_labels[l] else as.character(l)
}

expand_registry <- function(A) {
  R <- A$registry; D <- A$clean_frame
  out <- list(); meta <- list()
  for (i in seq_len(nrow(R))) {
    v <- R$var[i]; ty <- R$type[i]; lv <- R$level[i]
    if (ty == "c" && lv > 2) {
      for (l in sort(unique(D[[v]]))) {
        ln <- level_name(A, v, l)
        nm <- sprintf("%s_%s", v, ln)
        out[[nm]]  <- as.numeric(D[[v]] == l)
        meta[[nm]] <- data.frame(var = nm, parent = v, kind = "indicator",
                                 group = R$group[i],
                                 label = sprintf("%s = %s", R$label[i], ln),
                                 stringsAsFactors = FALSE)
      }
    } else {
      out[[v]]  <- as.numeric(D[[v]])
      meta[[v]] <- data.frame(var = v, parent = NA_character_,
                              kind = if (ty == "c") "binary" else ty,
                              group = R$group[i], label = R$label[i],
                              stringsAsFactors = FALSE)
    }
  }
  list(X = as.data.frame(out), meta = do.call(rbind, meta))
}

## ---- weights-core ----
AIARMS_CRS <- 32736   # only used if lon/lat are supplied instead of metric coords

as_points <- function(d, crs = AIARMS_CRS) {
  p <- sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sf::st_transform(p, crs)
}

## Accepts a coordinate matrix in metres, or a frame with lon/lat columns.
coords_of <- function(d, coords = NULL) {
  if (!is.null(coords)) return(as.matrix(coords))
  sf::st_coordinates(as_points(d))
}

make_listw <- function(coords, type = c("knn", "dist", "idw"),
                       k = 6, d = 400, alpha = 1, style = "W", d0 = 1) {
  type <- match.arg(type)
  n <- nrow(coords)
  if (type == "knn") {
    k  <- min(k, n - 1L)
    ## SYMMETRISATION. A k-nearest-neighbour graph is directed: j can be among
    ## the k nearest to i without i being among the k nearest to j. Section 2.1
    ## of the report defines W as the average of the indicator matrix with its
    ## transpose, (B + B')/2, row-standardised, so that is what is built here.
    ## make.sym.nb() gives the neighbour set of that matrix (a cell is non-zero
    ## if EITHER direction had it); the glist then carries 1 on a reciprocated
    ## link and 0.5 on a one-directional one, and style = "W" standardises the
    ## rows. Verified to reproduce (B + B')/2 to 0 at 1662 non-zero cells, and
    ## to reproduce the W built independently in the model-fitting section --
    ## before this change those two disagreed by up to 0.024 on Moran's I.
    nbk <- spdep::knn2nb(spdep::knearneigh(coords, k = k), sym = FALSE)
    nbu <- spdep::make.sym.nb(nbk)
    gl  <- lapply(seq_along(nbu), function(i) {
      j   <- nbu[[i]]
      rec <- j %in% nbk[[i]] & vapply(j, function(jj) i %in% nbk[[jj]], TRUE)
      ifelse(rec, 1, 0.5)
    })
    lw <- spdep::nb2listw(nbu, glist = gl, style = style, zero.policy = TRUE)
  } else if (type == "dist") {
    nb <- spdep::dnearneigh(coords, 0, d)
    lw <- spdep::nb2listw(nb, style = style, zero.policy = TRUE)
  } else {
    nb    <- spdep::dnearneigh(coords, 0, d)
    dl    <- spdep::nbdists(nb, coords)
    ## d0 is a distance floor: some households share exact coordinates and
    ## 1/0 would be Inf.
    glist <- lapply(dl, function(x) 1 / (pmax(x, d0)^alpha))
    lw    <- spdep::nb2listw(nb, glist = glist, style = style, zero.policy = TRUE)
  }
  attr(lw, "n_isolated") <- sum(spdep::card(lw$neighbours) == 0L)
  lw
}

## ---- estimators-core ----
## Report equation (2). n / [(n-1) S^2 w..] * sum_i sum_j w_ij (Z_i - Zbar)(Z_j - Zbar),
## evaluated using (n-1) S^2 = sum_i (Z_i - Zbar)^2.
moran_I <- function(Z, lw) {
  n     <- length(Z)
  Zc    <- Z - mean(Z)                                   # Z_i - Zbar
  w_dd  <- spdep::Szero(lw)                              # w..
  lagZc <- spdep::lag.listw(lw, Zc, zero.policy = TRUE)  # sum_j w_ij (Z_j - Zbar)
  (n / w_dd) * sum(Zc * lagZc) / sum(Zc^2)
}

## Report equation (3). 1 / [2 S^2 w..] * sum_i sum_j w_ij (Z_i - Z_j)^2,
## with the double sum expanded into three lag operations:
##   sum_i [ Z_i^2 sum_j w_ij  -  2 Z_i sum_j w_ij Z_j  +  sum_j w_ij Z_j^2 ]
geary_C <- function(Z, lw) {
  n     <- length(Z)
  Zc    <- Z - mean(Z)
  w_dd  <- spdep::Szero(lw)
  w_i   <- spdep::lag.listw(lw, rep(1, n), zero.policy = TRUE)  # sum_j w_ij
  lagZ  <- spdep::lag.listw(lw, Z,         zero.policy = TRUE)  # sum_j w_ij Z_j
  lagZ2 <- spdep::lag.listw(lw, Z^2,       zero.policy = TRUE)  # sum_j w_ij Z_j^2
  ((n - 1) * sum(w_i * Z^2 - 2 * Z * lagZ + lagZ2)) / (2 * w_dd * sum(Zc^2))
}

## Report equation (4). I_i = n (Z_i - Zbar) sum_j w_ij (Z_j - Zbar) / sum_i (Z_i - Zbar)^2.
## Quadrants classify each site by the signs of (Z_i - Zbar) and its spatial lag:
## High-High and Low-Low are positive local association (clusters), High-Low and
## Low-High are negative local association (the checkerboard case).
local_moran <- function(Z, lw) {
  n     <- length(Z)
  Zc    <- Z - mean(Z)
  lagZc <- spdep::lag.listw(lw, Zc, zero.policy = TRUE)
  Ii    <- n * Zc * lagZc / sum(Zc^2)
  quad  <- ifelse(Zc >= 0 & lagZc >= 0, "High-High",
           ifelse(Zc <  0 & lagZc <  0, "Low-Low",
           ifelse(Zc >= 0 & lagZc <  0, "High-Low", "Low-High")))
  data.frame(Zc = Zc, lag_Zc = lagZc, Ii = Ii,
             quadrant = quad, stringsAsFactors = FALSE)
}

## ---- perm-core ----
mc_pack <- function(obs, sim) {
  nsim <- length(sim)
  hi <- (1 + sum(sim >= obs)) / (1 + nsim)
  lo <- (1 + sum(sim <= obs)) / (1 + nsim)
  list(statistic = obs, sim = sim, nsim = nsim,
       p_greater = hi, p_less = lo, p_two = min(1, 2 * min(hi, lo)),
       E_sim = mean(sim), sd_sim = stats::sd(sim),
       z_sim = (obs - mean(sim)) / stats::sd(sim))
}

perm_moran <- function(x, lw, nsim = 999, seed = 1) {
  set.seed(seed)
  mc_pack(moran_I(x, lw), replicate(nsim, moran_I(sample(x), lw)))
}


## ---- localperm-core ----
local_moran_perm <- function(Z, lw, nsim = 999, seed = 1) {
  set.seed(seed)
  n     <- length(Z); Zc <- Z - mean(Z); SS <- sum(Zc^2)
  nb    <- lw$neighbours; wt <- lw$weights
  lagZc <- spdep::lag.listw(lw, Zc, zero.policy = TRUE)
  Ii    <- n * Zc * lagZc / SS                       # report equation (4)
  p     <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    ki <- length(nb[[i]]); if (ki == 0L || is.na(Ii[i])) next
    pool  <- Zc[-i]                                  # Z_i is held out
    ## WITHOUT replacement: the conditional null of Anselin (1995) draws k_i
    ## of the remaining n-1 values, it does not resample them. With
    ## replacement (the previous version) the draw variance is inflated by
    ## a factor (n-1)/(n-1-k_i+1) and the local p-values come out slightly
    ## conservative.
    draws <- vapply(seq_len(nsim), function(b) pool[sample.int(n - 1L, ki)],
                    numeric(ki))
    if (ki == 1L) draws <- matrix(draws, 1L, nsim)
    sim   <- n * Zc[i] * colSums(draws * wt[[i]]) / SS
    hi <- (1 + sum(sim >= Ii[i])) / (1 + nsim)
    lo <- (1 + sum(sim <= Ii[i])) / (1 + nsim)
    p[i] <- min(1, 2 * min(hi, lo))
  }
  quad <- ifelse(Zc >= 0 & lagZc >= 0, "High-High",
          ifelse(Zc <  0 & lagZc <  0, "Low-Low",
          ifelse(Zc >= 0 & lagZc <  0, "High-Low", "Low-High")))
  data.frame(Zc = Zc, lag_Zc = lagZc, Ii = Ii, p = p,
             q_BH = stats::p.adjust(p, "BH"), quadrant = quad,
             stringsAsFactors = FALSE)
}

## Getis-Ord Gi*, following Mtshawu et al. (2023), who pair I with Gi* maps.
getis_gstar <- function(x, lw, nsim = 999, seed = 1) {
  lwS <- spdep::nb2listw(spdep::include.self(lw$neighbours),
                         style = "W", zero.policy = TRUE)
  g <- spdep::localG_perm(x, lwS, nsim = nsim, iseed = seed)
  data.frame(Gstar = as.numeric(g), p = attr(g, "internals")[, "Pr(z != E(Gi))"])
}

## Whole-variable test for an UNORDERED categorical: the multi-colour join
## count (Cliff & Ord), permuted. Moran's I on the integer code is not a
## coherent alternative; this is.
## NOTE ON WEIGHTS. A join count counts joins, so it is defined on BINARY
## weights. Passing a row-standardised listw runs without error but returns a
## weighted sum, not a count -- the "same_colour" column then reads 10.24
## rather than 81.96 for the same configuration. The p-values are identical
## (the permutation ranks are unchanged by the row scaling), but the estimate
## column is not a count and must not be reported as one. binary_listw()
## rebuilds the same neighbour graph with style = "B".
binary_listw <- function(lw)
  spdep::nb2listw(spdep::make.sym.nb(lw$neighbours), style = "B",
                  zero.policy = TRUE)

joincount_cat <- function(x, lw, nsim = 999, seed = 1) {
  set.seed(seed)
  f  <- factor(x)
  lw <- binary_listw(lw)
  jc <- spdep::joincount.mc(f, lw, nsim = nsim, zero.policy = TRUE)
  ## BUG FIX. joincount.mc() returns an htest/mc.sim object in which
  ##     $statistic   is the OBSERVED same-colour join count
  ##     $estimate[1] is "mean of simulation"
  ##     $estimate[2] is "variance of simulation"
  ## The previous version read $estimate[1] into the "same_colour" column and
  ## $estimate[2] into "expected", so the table printed the permutation mean
  ## as the observed count and the permutation VARIANCE as the expected
  ## count. For Education level 1 that is 98.4 and 55.8 where the true values
  ## are 96 observed against 98.4 expected -- i.e. the old table showed a
  ## level with FEWER same-colour joins than chance as though it had more.
  ## The p-values were never affected. Cross-checked against
  ## joincount.test(): observed 96.00, expectation 98.13, variance 57.18.
  ##
  ## Note also that joincount.mc is one-sided ("greater"), which is the
  ## convention for a join count: clustering means MORE same-colour joins
  ## than chance. Say so in the write-up; it is not the two-sided rule used
  ## for Moran's I elsewhere in this document.
  do.call(rbind, lapply(seq_along(jc), function(i)
    data.frame(level       = levels(f)[i],
               same_colour = unname(jc[[i]]$statistic),
               expected    = unname(jc[[i]]$estimate[1]),
               sd_sim      = sqrt(unname(jc[[i]]$estimate[2])),
               p_greater   = jc[[i]]$p.value, stringsAsFactors = FALSE)))
}

## ---- fastcore ----
listw_to_W <- function(lw) as.matrix(spdep::listw2mat(lw))

moran_I_W <- function(x, W) {
  n <- length(x); z <- x - mean(x)
  (n / sum(W)) * sum(z * (W %*% z)) / sum(z^2)
}

perm_moran_W <- function(x, W, nsim = 999, seed = 1) {
  set.seed(seed); n <- length(x); z <- x - mean(x)
  obs <- (n / sum(W)) * sum(z * (W %*% z)) / sum(z^2)
  Z   <- matrix(z[replicate(nsim, sample.int(n))], n, nsim)
  mc_pack(obs, (n / sum(W)) * colSums(Z * (W %*% Z)) / sum(z^2))
}

bh_matrix <- function(P) {
  ## BH over the upper triangle only: p.adjust on the whole symmetric matrix
  ## would count each pair twice and treat the NA diagonal as a test.
  ut <- upper.tri(P)
  q  <- matrix(NA_real_, nrow(P), ncol(P), dimnames = dimnames(P))
  q[ut] <- stats::p.adjust(P[ut], "BH")
  q[lower.tri(q)] <- t(q)[lower.tri(q)]
  q
}

## ---- global-core ----
global_table <- function(X, coords, vars = names(X),
                         wcfg = list(type = "knn", k = 8, style = "W"),
                         nsim = 9999, seed = 1) {
  lw <- do.call(make_listw, c(list(coords = as.matrix(coords)), wcfg))
  W  <- listw_to_W(lw)
  out <- do.call(rbind, lapply(vars, function(v) {
    pm <- perm_moran_W(X[[v]], W, nsim = nsim, seed = seed)
    data.frame(variable = v, moran_I = pm$statistic, EI = -1 / (nrow(X) - 1),
               sd_perm = pm$sd_sim, z_perm = pm$z_sim, p_perm = pm$p_two,
               geary_C = geary_C(X[[v]], lw), stringsAsFactors = FALSE)
  }))
  out$q_BH <- stats::p.adjust(out$p_perm, "BH")
  out[order(-out$moran_I), ]
}
