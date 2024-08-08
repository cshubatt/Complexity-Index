# -----------------------------------------------------------------------------
# MODULE
# Functions for defining complexity features in choices_13k data
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Use: u <- modules::use(here("lib", "util.R"))
# -----------------------------------------------------------------------------
import(dplyr) # %>%
import(purrr) # pmap
import(glue) # glue()

data_fp <- function(fn) {
  fp <- file.path("../data", fn)
  return(fp)
}

unpack_column <- function(
    unpack_col, name_scheme, df, ncol = 9) {
  n <- nrow(df)
  mat <- matrix(NA, nrow = n, ncol = ncol)
  for (i in 1:n) {
    ktemp <- length(unlist(df[[unpack_col]][i]))
    mat[i, 1:ktemp] <- unlist(df[[unpack_col]][i])
  }
  df <- data.frame(mat)
  names(df) <- glue("{name_scheme}_{1:ncol}")

  return(df)
}

# Repack payoffs/probabilities from wide to long
repack_column <- function(
    repacked_col, name_scheme, df, ncol = 9) {
  df <- df %>%
    mutate(
      repacked = pmap(
        select(., starts_with(name_scheme)), c
      )
    )
  df[[repacked_col]] <- df$repacked

  drop_cols <- c(
    "repacked", glue("{name_scheme}{1:ncol}")
  )

  df <- df %>% select(-all_of(drop_cols))
  return(df)
}
