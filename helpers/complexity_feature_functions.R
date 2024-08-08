# -----------------------------------------------------------------------------
# MODULE
# Functions for defining complexity features in choices_13k data
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Use: compl <- modules::use("helpers/complexity_feature_functions.R")
# -----------------------------------------------------------------------------

# Setup -----------------------------------------------------------------------
import(stats)
import(testit)
import(magrittr)
import(dplyr) # mutate()
import(tidyr) # replae_na()
import(glue)
import(purrr) # map2
import(OneR) # bin()

# Build Complexity Features ----------------------------------------------------
build_features <- function(df, lots = c("a", "b")) {
  # input:
  # - df with columns x_a, x_b, p_a, p_b, x_a_cor, x_b_cor, p_ij_cor
  # - indicator incl_tiles; if TRUE, builds in quartile vars for continuous vals

  # output: df with complexity features
  orig_cols <- names(df)
  if (!("compound" %in% names(df))) {
    df <- df %>%
      mutate(
        compound = 0,
        compound_range = 0
      )
  }

  for (i in 1:length(lots)) {
    lot_i <- lots[i]

    x_i <- df[[glue("x_{lot_i}")]]
    p_i <- df[[glue("p_{lot_i}")]]

    df[[glue("ev__{lot_i}")]] <- map2(
      .x = x_i, .y = p_i, ~ (.x %*% .y)
    ) %>% unlist()
  }

  for (i in 1:(length(lots))) {
    lot_i <- lots[i]

    x_i <- df[[glue("x_{lot_i}")]]
    p_i <- df[[glue("p_{lot_i}")]]
    x_i_cor <- df[[glue("x_{lot_i}_cor")]]

    if (i < length(lots)) {
      for (j in (i + 1):length(lots)) {
        lot_j <- lots[j]

        x_j <- df[[glue("x_{lot_j}")]]
        p_j <- df[[glue("p_{lot_j}")]]
        x_j_cor <- df[[glue("x_{lot_j}_cor")]]
        p_ij_cor <- df[[glue("p_{lot_i}{lot_j}_cor")]]

        # ev diff
        ev_i <- df[[glue("ev__{lot_i}")]]
        ev_j <- df[[glue("ev__{lot_j}")]]
        ev_diff_ij <- glue("ev_diff__{lot_i}{lot_j}")
        df[[ev_diff_ij]] <- ev_i - ev_j

        # ev diff transformations
        abs_ev_diff <- glue("abs_{ev_diff_ij}")
        df[[abs_ev_diff]] <- abs(df[[ev_diff_ij]])

        abs_ev_diff_sq <- glue("{abs_ev_diff}_sq")
        df[[abs_ev_diff_sq]] <- df[[abs_ev_diff]]^2

        abs_ev_diff_cu <- glue("{abs_ev_diff}_cu")
        df[[abs_ev_diff_cu]] <- df[[abs_ev_diff]]^3

        sgn_ev_diff_sq <- glue("sgn_{ev_diff_ij}_sq")
        df[[sgn_ev_diff_sq]] <- df[[abs_ev_diff]] * df[[ev_diff_ij]]

        ev_diff_cu <- glue("{ev_diff_ij}_cu")
        df[[ev_diff_cu]] <- df[[ev_diff_ij]]^3

        # dominance
        df[[glue("dom__{lot_i}{lot_j}")]] <- pmap(
          .l = list(x_i_cor, x_j_cor), .f = dominated
        ) %>% unlist()

        cdf_diff_abs_val <- pmap(
          .l = list(x_i_cor, x_j_cor, p_ij_cor), .f = CDF_diff_abs
        ) %>% unlist()
        ev_diff_val <- df[[abs_ev_diff]]

        cdf_diff_abs <- glue("cdf_diff_abs__{lot_i}{lot_j}")
        df[[cdf_diff_abs]] <- (cdf_diff_abs_val - ev_diff_val) %>%
          round(digits = 2)

        # add log versions of cardinal features ----------------------------
        card_feats <- c(
          "abs_ev_diff", "cdf_diff_abs"
        )

        for (feat in card_feats) {
          feat_i <- glue("{feat}__{lot_i}{lot_j}")
          log_i <- glue("ln_{feat_i}")
          sq_i <- glue("sq_{feat_i}")
          df[[log_i]] <- log(df[[feat_i]] + 1)
          df[[sq_i]] <- df[[feat_i]]^2
        }
      }
    }

    # generate indiv features for lottery i
    df[[glue("mixed__{lot_i}")]] <- map(
      x_i, ~ mixed_sign(xs = .x)
    ) %>% unlist()

    df[[glue("gains__{lot_i}")]] <- !df[[glue("mixed__{lot_i}")]] & # nolint
      df[[glue("ev__{lot_i}")]] >= 0

    df[[glue("losses__{lot_i}")]] <- !df[[glue("mixed__{lot_i}")]] & # nolint
      df[[glue("ev__{lot_i}")]] < 0

    df[[glue("evil_probs__{lot_i}")]] <- map(
      p_i, ~ evil_probabilities(probs = .x)
    ) %>% unlist()

    df[[glue("scale__{lot_i}")]] <- map(
      x_i, ~ x_scale(xs = .x)
    ) %>% unlist()

    df[[glue("var__{lot_i}")]] <- map2(
      x_i, p_i, ~ var_lottery(xs = .x, probs = .y)
    ) %>% unlist()

    df[[glue("var_norm__{lot_i}")]] <- map2(
      x_i, p_i, ~ var_norm(xs = .x, probs = .y)
    ) %>% unlist()

    df[[glue("cdf_diff_self__{lot_i}")]] <- map2(
      x_i, p_i, ~ CDF_diff_self(xs = .x, probs = .y)
    ) %>% unlist()

    df[[glue("range__{lot_i}")]] <- map(
      x_i, ~ x_range(xs = .x)
    ) %>% unlist()

    df[[glue("payoff_var__{lot_i}")]] <- map(
      x_i, ~ var_general(xs = .x)
    ) %>% unlist()

    df[[glue("prob_var__{lot_i}")]] <- map(
      p_i, ~ var_general(xs = .x)
    ) %>% unlist()

    df[[glue("entropy__{lot_i}")]] <- map(
      p_i, ~ entropy(probs = .x)
    ) %>% unlist()

    df[[glue("adb__{lot_i}")]] <- map(
      p_i, ~ avg_dist_from_boundary(probs = .x)
    ) %>% unlist()

    df[[glue("payoff_wtd_db__{lot_i}")]] <- map2(
      x_i, p_i, ~ payout_wtd_dist_from_boundary(xs = .x, probs = .y)
    ) %>% unlist()

    df[[glue("nstates__{lot_i}")]] <- map(
      x_i, ~ num_states(xs = .x)
    ) %>% unlist()

    df[[glue("payoff_dispersion__{lot_i}")]] <- map(
      x_i, ~ payout_dispersion(xs = .x)
    ) %>% unlist()

    df[[glue("certain__{lot_i}")]] <- ifelse(
      df[[glue("nstates__{lot_i}")]] == 1, TRUE, FALSE
    )

    many_safe <- mean(df[[glue("certain__{lot_i}")]]) > 0.25

    # add log, square versions of cardinal features ----------------------------
    card_feats <- c(
      "scale", "range", "var",
      "payoff_var", "nstates", "adb", "payoff_wtd_db", "entropy", "prob_var",
      "payoff_dispersion", "var_norm", "cdf_diff_self"
    )

    for (feat in card_feats) {
      log_i <- glue("ln_{feat}__{lot_i}")
      sq_i <- glue("sq_{feat}__{lot_i}")
      feat_i <- glue("{feat}__{lot_i}")
      df[[log_i]] <- log(df[[feat_i]] + 1)
      df[[sq_i]] <- df[[feat_i]]^2
    }
  }

  # add aves of feat_i, feat_j -------------------------------------------------
  ave_feats <- c(
    card_feats, glue("ln_{card_feats}"), glue("sq_{card_feats}"),
    "gains", "evil_probs"
  )
  lot_i <- lots[1]
  lot_j <- lots[2]

  for (ave_feat in ave_feats) {
    ave_name <- glue("ave_{ave_feat}__{lot_i}{lot_j}")
    feat_i <- glue("{ave_feat}__{lots[1]}")
    feat_j <- glue("{ave_feat}__{lots[2]}")

    df[[ave_name]] <- map2(
      .x = df[[feat_i]], .y = df[[feat_j]], .f = ~ mean(c(.x, .y))
    ) %>% unlist()
  }

  return(df)
}

correlate_states <- function(x_a, x_b, p_a, p_b, ...) {
  a_df <- data.frame(x = x_a, p = p_a)
  a_df <- a_df[order(a_df$x), ] %>%
    mutate(cdf = cumsum(p) %>% round(digits = 9))

  b_df <- data.frame(x = x_b, p = p_b)
  b_df <- b_df[order(b_df$x), ] %>%
    mutate(cdf = cumsum(p) %>% round(digits = 9))

  cdf_vals <- c(a_df$cdf, b_df$cdf) %>%
    round(digits = 9) %>%
    unique()
  cdf_vals <- cdf_vals[order(cdf_vals)]

  pay_a <- c()
  pay_b <- c()

  for (i in 1:length(cdf_vals)) {
    cdf_i <- cdf_vals[i]

    a_better <- filter(a_df, cdf >= cdf_i)
    b_better <- filter(b_df, cdf >= cdf_i)
    assert(length(a_better$x) > 0 & length(b_better$x) > 0)
    assert(!(any(is.na(a_better$x))) & !(any(is.na(a_better$x))))

    pay_a_i <- min(a_better$x)
    pay_b_i <- min(b_better$x)

    pay_a <- c(pay_a, pay_a_i)
    pay_b <- c(pay_b, pay_b_i)
  }

  cor_states <- data.frame(
    state = 1:length(cdf_vals), cdf_vals, pay_a, pay_b
  ) %>%
    mutate(
      p_ab_cor = c(cdf_vals[1], diff(cdf_vals))
    )
  return(cor_states)
}

dominated <- function(x_a_cor, x_b_cor, ...) {
  if (any(is.na(c(x_a_cor, x_b_cor)))) {
    return(NA)
  }

  x_a <- unlist(x_a_cor)
  x_b <- unlist(x_b_cor)

  dom <- all(x_a >= x_b) |
    all(x_b >= x_a)

  equal <- all(x_a == x_b)
  dom <- dom & !equal

  return(dom)
}

any_certain <- function(probs) {
  if (any(is.na(probs))) {
    return(NA)
  }
  certain <- any(unlist(probs) == 1)
  return(certain)
}

mixed_sign <- function(xs) {
  if (any(is.na(xs))) {
    return(NA)
  }
  xs <- unlist(xs)
  same_sign <- all(xs >= 0) | all(xs <= 0)
  mixed_sign <- !same_sign
  return(mixed_sign)
}

entropy <- function(probs) {
  if (any(is.na(probs))) {
    return(NA)
  }
  probs <- unlist(probs) %>%
    setdiff(c(0))
  entropy <- (probs * (-log(probs))) %>% sum()
  return(entropy)
}

x_scale <- function(xs) {
  if (any(is.na(xs))) {
    return(NA)
  }
  xs <- unlist(xs)
  scale <- mean(abs(xs))
  return(scale)
}

num_states <- function(xs) {
  xs <- unlist(xs)

  if (all(is.na(xs))) {
    return(NA)
  }
  num_states <- length(unique(xs))
  return(num_states)
}

avg_dist_from_boundary <- function(probs) {
  probs <- unlist(probs)

  if (all(is.na(probs))) {
    return(NA)
  }

  total_dist <- 0
  n <- length(probs[!is.na(probs)])
  for (i in 1:n) {
    total_dist <- total_dist + min(abs(probs[i]), abs(1 - probs[i]))
  }

  avg_dist <- total_dist / n
  return(avg_dist)
}

payout_wtd_dist_from_boundary <- function(xs, probs) {
  probs <- unlist(probs)
  xs <- unlist(xs)

  if (all(is.na(probs))) {
    return(NA)
  }
  total_wtd_dist <- 0

  n <- length(probs[!is.na(probs)])
  for (i in seq(n)) {
    dist_i <- abs(xs[i]) * min(abs(probs[i]), abs(1 - probs[i]))
    total_wtd_dist <- total_wtd_dist + dist_i
  }

  avg_dist <- total_wtd_dist / n
  return(avg_dist)
}

var_norm <- function(xs, probs) {
  if (any(is.na(c(xs, probs)))) {
    return(NA)
  }
  xs <- unlist(xs)
  probs <- unlist(probs)

  scale_x <- mean(abs(xs))

  if (scale_x == 0) {
    var_norm <- 0
  } else {
    exp_of_square <- probs %*% xs^2
    square_of_exp <- (probs %*% xs)^2

    var_norm <- (exp_of_square - square_of_exp) / scale_x^2
  }

  return(var_norm)
}

var_general <- function(xs) {
  if (any(is.na(xs))) {
    return(NA)
  }
  xs <- unlist(xs)

  var <- sum((xs - mean(xs))^2 / length(xs))

  return(var)
}

var_lottery <- function(xs, probs) {
  if (any(is.na(c(xs, probs)))) {
    return(NA)
  }
  xs <- unlist(xs)
  probs <- unlist(probs)

  exp_of_square <- probs %*% xs^2
  square_of_exp <- (probs %*% xs)^2

  var <- exp_of_square - square_of_exp
  return(var)
}

CDF_diff_abs <- function(x_a_cor, x_b_cor, p_ij_cor, ...) {
  # calculates total absolute area of difference between CDFs for two lotteries

  if (any(is.na(c(x_a_cor, x_b_cor, p_ij_cor)))) {
    return(NA)
  }
  x_a <- unlist(x_a_cor)
  x_b <- unlist(x_b_cor)
  p <- unlist(p_ij_cor)

  df <- data.frame(x_a, x_b, p) %>%
    mutate(
      val_diff = abs(x_a - x_b),
      area_i = val_diff * p,
    )

  area <- sum(df$area_i)

  return(area)
}

CDF_diff_self <- function(xs, probs, ...) {
  x <- unlist(xs)
  p <- unlist(probs)

  ev <- x %*% p

  cdf_diff <- 0

  for (i in seq_len(length(x))) {
    x_i <- x[i]
    p_i <- p[i]

    cdf_diff_i <- abs(x_i - ev) * p_i
    cdf_diff <- cdf_diff + cdf_diff_i
  }

  return(cdf_diff)
}

x_range <- function(xs) {
  if (any(is.na(xs))) {
    return(NA)
  }
  xs <- unlist(xs)
  range <- max(xs) - min(xs)

  return(range)
}

payout_dispersion <- function(xs) {
  if (any(is.na(xs))) {
    return(NA)
  }
  xs <- unlist(xs) %>% unique()

  n <- length(xs)
  xbar <- mean(abs(xs))

  if (xbar == 0) {
    return(0)
  }

  norm_dispersion <- abs(xs - xbar) / xbar
  avg_dispersion <- (1 / n) * sum(norm_dispersion)

  return(avg_dispersion)
}

evil_probabilities <- function(probs) {
  probs <- unlist(probs)

  nice_probs <- c(0, 0.01, seq(0.05, 0.95, 0.05), 0.99, 1) %>% round(2)

  probs_nice <- all(round(probs, 2) %in% nice_probs)

  evil <- !probs_nice

  return(evil)
}
