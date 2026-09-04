## ---------------------------------------------------------------------------
## mgm_spatial_core.R
##
## Tangled from spatial_autocorrelation_MGM.Rmd on 2026-09-04.
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

expand_registry <- function(A) {
  R <- A$registry; D <- A$clean_frame
  out <- list(); meta <- list()
  for (i in seq_len(nrow(R))) {
    v <- R$var[i]; ty <- R$type[i]; lv <- R$level[i]
    if (ty == "c" && lv > 2) {
      for (l in sort(unique(D[[v]]))) {
        nm <- sprintf("%s_%d", v, l)
        out[[nm]]  <- as.numeric(D[[v]] == l)
        meta[[nm]] <- data.frame(var = nm, parent = v, kind = "indicator",
                                 group = R$group[i],
                                 label = sprintf("%s = %d", R$label[i], l),
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
    nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k), sym = FALSE)
    lw <- spdep::nb2listw(nb, style = style, zero.policy = TRUE)
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
## Report equation (1). n / [(n-1) S^2 w..] * sum_i sum_j w_ij (Z_i - Zbar)(Z_j - Zbar),
## evaluated using (n-1) S^2 = sum_i (Z_i - Zbar)^2.
moran_I <- function(Z, lw) {
  n     <- length(Z)
  Zc    <- Z - mean(Z)                                   # Z_i - Zbar
  w_dd  <- spdep::Szero(lw)                              # w..
  lagZc <- spdep::lag.listw(lw, Zc, zero.policy = TRUE)  # sum_j w_ij (Z_j - Zbar)
  (n / w_dd) * sum(Zc * lagZc) / sum(Zc^2)
}

## Report equation (2). 1 / [2 S^2 w..] * sum_i sum_j w_ij (Z_i - Z_j)^2,
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

## Report equation (3). I_i = n (Z_i - Zbar) sum_j w_ij (Z_j - Zbar) / sum_i (Z_i - Zbar)^2.
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

## Lee's L, the bivariate extension used in the supplementary section. Not part
## of equations (1)-(3); see the note there before reporting it.
lee_L <- function(x, y, lw) {
  n   <- length(x); zx <- x - mean(x); zy <- y - mean(y)
  wi  <- spdep::lag.listw(lw, rep(1, n), zero.policy = TRUE)
  lzx <- spdep::lag.listw(lw, zx, zero.policy = TRUE)
  lzy <- spdep::lag.listw(lw, zy, zero.policy = TRUE)
  (n / sum(wi^2)) * sum(lzx * lzy) / (sqrt(sum(zx^2)) * sqrt(sum(zy^2)))
}

local_lee <- function(x, y, lw) {
  n   <- length(x); zx <- x - mean(x); zy <- y - mean(y)
  wi  <- spdep::lag.listw(lw, rep(1, n), zero.policy = TRUE)
  lzx <- spdep::lag.listw(lw, zx, zero.policy = TRUE)
  lzy <- spdep::lag.listw(lw, zy, zero.policy = TRUE)
  ## spdep's convention is mean(local L) == global L, hence the extra factor n
  (n^2 / sum(wi^2)) * (lzx * lzy) / (sqrt(sum(zx^2)) * sqrt(sum(zy^2)))
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

perm_lee <- function(x, y, lw, nsim = 999, seed = 1, joint = TRUE) {
  set.seed(seed)
  obs <- lee_L(x, y, lw)
  sim <- replicate(nsim, {
    i <- sample.int(length(x))
    if (joint) lee_L(x[i], y[i], lw) else lee_L(x[i], y, lw)
  })
  mc_pack(obs, sim)
}

## ---- localperm-core ----
local_moran_perm <- function(Z, lw, nsim = 999, seed = 1) {
  set.seed(seed)
  n     <- length(Z); Zc <- Z - mean(Z); SS <- sum(Zc^2)
  nb    <- lw$neighbours; wt <- lw$weights
  lagZc <- spdep::lag.listw(lw, Zc, zero.policy = TRUE)
  Ii    <- n * Zc * lagZc / SS                       # report equation (3)
  p     <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    ki <- length(nb[[i]]); if (ki == 0L || is.na(Ii[i])) next
    pool  <- Zc[-i]                                  # Z_i is held out
    draws <- matrix(pool[sample.int(n - 1L, ki * nsim, replace = TRUE)], ki, nsim)
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
joincount_cat <- function(x, lw, nsim = 999, seed = 1) {
  set.seed(seed)
  f  <- factor(x)
  jc <- spdep::joincount.mc(f, lw, nsim = nsim, zero.policy = TRUE)
  do.call(rbind, lapply(seq_along(jc), function(i)
    data.frame(level = levels(f)[i],
               same_colour = unname(jc[[i]]$estimate[1]),
               expected    = unname(jc[[i]]$estimate[2]),
               p = jc[[i]]$p.value, stringsAsFactors = FALSE)))
}

## ---- fastcore ----
listw_to_W <- function(lw) as.matrix(spdep::listw2mat(lw))

moran_I_W <- function(x, W) {
  n <- length(x); z <- x - mean(x)
  (n / sum(W)) * sum(z * (W %*% z)) / sum(z^2)
}

lee_L_W <- function(x, y, W) {
  n <- length(x); zx <- x - mean(x); zy <- y - mean(y)
  (n / sum(rowSums(W)^2)) * sum((W %*% zx) * (W %*% zy)) /
    sqrt(sum(zx^2) * sum(zy^2))
}

perm_moran_W <- function(x, W, nsim = 999, seed = 1) {
  set.seed(seed); n <- length(x); z <- x - mean(x)
  obs <- (n / sum(W)) * sum(z * (W %*% z)) / sum(z^2)
  Z   <- matrix(z[replicate(nsim, sample.int(n))], n, nsim)
  mc_pack(obs, (n / sum(W)) * colSums(Z * (W %*% Z)) / sum(z^2))
}

perm_lee_W <- function(x, y, W, nsim = 999, seed = 1, joint = TRUE, idx = NULL) {
  n  <- length(x); zx <- x - mean(x); zy <- y - mean(y)
  k  <- (n / sum(rowSums(W)^2)) / sqrt(sum(zx^2) * sum(zy^2))
  obs <- k * sum((W %*% zx) * (W %*% zy))
  if (is.null(idx)) { set.seed(seed); idx <- replicate(nsim, sample.int(n)) }
  nsim <- ncol(idx)
  A <- W %*% matrix(zx[idx], n, nsim)
  B <- if (joint) W %*% matrix(zy[idx], n, nsim)
       else matrix(rep(W %*% zy, nsim), n, nsim)
  mc_pack(obs, k * colSums(A * B))
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

## ---- leematrix-core ----
lee_matrix_fast <- function(X, coords, vars = names(X),
                            wcfg = list(type = "knn", k = 8, style = "W"),
                            nsim = 19999, block = 1000, seed = 1) {
  p  <- length(vars); n <- nrow(X)
  L  <- P <- matrix(NA_real_, p, p, dimnames = list(vars, vars))
  W  <- listw_to_W(do.call(make_listw, c(list(coords = as.matrix(coords)), wcfg)))
  kn <- n / sum(rowSums(W)^2)
  Zc <- lapply(vars, function(v) X[[v]] - mean(X[[v]])); names(Zc) <- vars
  lag0 <- lapply(Zc, function(z) as.vector(W %*% z))
  ss   <- vapply(Zc, function(z) sqrt(sum(z^2)), numeric(1))
  pr   <- t(utils::combn(p, 2))

  obs <- apply(pr, 1, function(ij)
    kn * sum(lag0[[ij[1]]] * lag0[[ij[2]]]) / (ss[ij[1]] * ss[ij[2]]))

  set.seed(seed)
  hi <- lo <- integer(nrow(pr)); tot <- 0L
  while (tot < nsim) {                 # permute in blocks to bound memory
    m   <- min(block, nsim - tot)
    idx <- matrix(replicate(m, sample.int(n)), n, m)
    LZ  <- lapply(Zc, function(z) W %*% matrix(z[idx], n, m))
    for (r in seq_len(nrow(pr))) {
      i <- pr[r, 1]; j <- pr[r, 2]
      s <- kn * colSums(LZ[[i]] * LZ[[j]]) / (ss[i] * ss[j])
      hi[r] <- hi[r] + sum(s >= obs[r]); lo[r] <- lo[r] + sum(s <= obs[r])
    }
    tot <- tot + m
  }
  for (r in seq_len(nrow(pr))) {
    i <- pr[r, 1]; j <- pr[r, 2]
    L[i, j] <- L[j, i] <- obs[r]
    P[i, j] <- P[j, i] <- min(1, 2 * min((1 + hi[r]) / (1 + nsim),
                                         (1 + lo[r]) / (1 + nsim)))
  }
  for (v in seq_len(p))                # diagonal = spatial smoothing scalar
    L[v, v] <- kn * sum(lag0[[v]]^2) / (ss[v]^2)
  list(L = L, P = P, n = n, nsim = nsim)
}
