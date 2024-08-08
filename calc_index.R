# -----------------------------------------------------------------------------
# Purpose: Calculated Complexity Indices of Choice Problems and Lotteries
# Author: Benjamin Enke & Cassidy Shubatt
# Output: CSV File with Indices and Features
# Last updated: Aug, 2024 by Cassidy Shubatt
# -----------------------------------------------------------------------------

# Load package environment and set directory in R Console:
if (FALSE) {
  install.packages("renv")
  library(renv)
  script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(script_dir) # or set path for working directory manually
  renv::restore()
  renv::activate()
}

library("tidyverse")
library("glue")
library("testit")
library("optparse")
library("xtable")
# library("pracma")
library("OneR")
# library("assert")
# library("usethis")

# Load -------------------------------------------------------------------------
message("Loading features and response...")

util <- modules::use("helpers/util.R")
compl <- modules::use("helpers/complexity_feature_functions.R")

df <- read.csv("sample_data/sample_all_indices_calculation_1.csv")
# df <- read.csv("sample_data/sample_just_OLC_SLC_calculation_1.csv")
# df <- read.csv("sample_data/jpe_randomization.csv")

# Replace empty strings with NA in character columns
char_columns <- sapply(df, is.character)
df[char_columns] <- lapply(df[char_columns], function(x) ifelse(x == "", NA, x))

# make dataframe numeric
# Identify columns with mostly numeric values
numeric_columns <- sapply(df, function(col) {
  all(grepl("^\\d*\\.?\\d+$", col, perl = TRUE)) # Check if all entries are numeric
})

# Convert identified numeric columns to numeric
df[, numeric_columns] <- lapply(df[, numeric_columns], as.numeric)

# safe for later
df_fundamentals <- df

# max numer of states
num_st_max <- 9

message("Checking if more than one lottery is supplied")

if (
  (length(grep("^x_b", names(df))) > 0) &&
    (length(grep("^p_b", names(df))) > 0)
) {
  message("Two lotteries per problem found!")
  single_lottery <- FALSE
} else {
  message("Just one lottery per problem found!")
  single_lottery <- TRUE
  for (i in 1:num_st_max) {
    if (glue("x_{i}") %in% names(df)) {
      df <- df %>%
        rename(!!glue("x_a_{i}") := glue("x_{i}"))
    }
    if (glue("p_{i}") %in% names(df)) {
      df <- df %>%
        rename(!!glue("p_a_{i}") := glue("p_{i}"))
    }
  }
  # Later, adjust x_b_1 = EV(A)
  df[glue("x_b_1")] <- 0
  df[glue("p_b_1")] <- 1
}

# "check if one lottery has at least more than 1 state"
testit::assert(
  "check if one lottery has at least more than 1 state per problem",
  sum(grepl("x_a", names(df))) > 1 || sum(grepl("x_b", names(df))) > 1
)

# add missing columns
x_a_c <- c()
x_b_c <- c()
p_a_c <- c()
p_b_c <- c()

for (i in 1:num_st_max) {
  if (!(glue("x_a_{i}") %in% names(df))) {
    df[glue("x_a_{i}")] <- NA
  }
  if (!(glue("p_a_{i}") %in% names(df))) {
    df[glue("p_a_{i}")] <- NA
  }
  if (!(glue("x_b_{i}") %in% names(df))) {
    df[glue("x_b_{i}")] <- NA
  }
  if (!(glue("p_b_{i}") %in% names(df))) {
    df[glue("p_b_{i}")] <- NA
  }

  # if probability is 0, remove state
  condition_a <- df[[glue("p_a_{i}")]] == 0
  condition_b <- df[[glue("p_b_{i}")]] == 0
  condition_a <- ifelse(is.na(condition_a), FALSE, condition_a)
  condition_b <- ifelse(is.na(condition_b), FALSE, condition_b)
  df[condition_a, c(glue("p_a_{i}"), glue("x_a_{i}"))] <- NA
  df[condition_b, c(glue("p_b_{i}"), glue("x_b_{i}"))] <- NA

  # check if each probaility matches as state and vice versa for
  print(glue("x_a_{i} and p_a_{i}"))
  testit::assert(
    "check if each probaility matches as state and vice versa in lottery A",
    !any((is.na(df[glue("p_a_{i}")]) != is.na(df[glue("x_a_{i}")])))
  )
  print(glue("x_b_{i} and p_b_{i}"))
  testit::assert(
    "check if each probaility matches as state and vice versa in lottery B",
    !any((is.na(df[glue("p_b_{i}")]) != is.na(df[glue("x_b_{i}")])))
  )
  x_a_c <- c(x_a_c, glue("x_a_{i}"))
  x_b_c <- c(x_b_c, glue("x_b_{i}"))
  p_a_c <- c(p_a_c, glue("p_a_{i}"))
  p_b_c <- c(p_b_c, glue("p_b_{i}"))
}

# Reorder each row thus numbers are first
# Function to reorder each row with numbers first and then NA
reorder_row <- function(row) {
  non_na_values <- row[!is.na(row)]
  na_values <- row[is.na(row)]
  reordered_row <- c(non_na_values, na_values)
  return(reordered_row)
}

order_c <- c(x_a_c, p_a_c, x_b_c, p_b_c)

compound <- FALSE
df_compound <- data.frame()
if ("compound" %in% names(df)) {
  compound <- TRUE
  df_compound <- df %>%
    select(compound)
}

df <- df %>%
  select(all_of(order_c))

df[x_a_c] <- as.data.frame(t(apply(df[x_a_c] %>%
  select(starts_with("x_a")), 1, reorder_row)))
df[p_a_c] <- as.data.frame(t(apply(df[p_a_c] %>%
  select(starts_with("p_a")), 1, reorder_row)))
df[x_b_c] <- as.data.frame(t(apply(df[x_b_c] %>%
  select(starts_with("x_b")), 1, reorder_row)))
df[p_b_c] <- as.data.frame(t(apply(df[p_b_c] %>%
  select(starts_with("p_b")), 1, reorder_row)))

# Pack p/x columns
message("Packing p,x columns into lists...")
types <- c("p", "x")
lots <- c("a", "b")

for (type in types) {
  for (lot in lots) {
    repacked <- glue("{type}_{lot}")
    name_scheme <- glue("{repacked}_")
    df <- util$repack_column(repacked, name_scheme, df)
    for (i in 1:nrow(df)) {
      li_val <- df[[repacked]][i] %>% unlist()
      if (all(is.na(li_val))) {
        df[[repacked]][i] <- NA
      } else {
        df[[repacked]][i] <- list(li_val[!is.na(li_val)])
      }
    }
  }
}

df$ev__a <- map2(
  .x = df$x_a, .y = df$p_a, ~ (.x %*% .y)
) %>% unlist()

if (single_lottery) {
  df$x_b <- map(df$ev__a, ~ list(.x)) %>% unlist(recursive = FALSE)
  df$x_b_1 <- map(df$ev__a, ~ list(.x)) %>% unlist(recursive = TRUE)
}

# Generate correlated state variables ------------------------------------------
message("Constructing correlated state versions of x, p...")
x_a_cor <- c()
x_b_cor <- c()
p_ab_cor <- c()
nstates_cor <- c()

for (i in seq(nrow(df))) {
  x_a_i <- df$x_a[i] %>% unlist()
  x_b_i <- df$x_b[i] %>% unlist()
  p_a_i <- df$p_a[i] %>% unlist()
  p_b_i <- df$p_b[i] %>% unlist()
  cor_df <- compl$correlate_states(x_a_i, x_b_i, p_a_i, p_b_i)
  x_a_cor_i <- cor_df$pay_a
  x_b_cor_i <- cor_df$pay_b
  p_ab_cor_i <- cor_df$p_ab_cor
  nstates_cor_i <- nrow(cor_df)

  x_a_cor <- c(x_a_cor, list(x_a_cor_i))
  x_b_cor <- c(x_b_cor, list(x_b_cor_i))
  p_ab_cor <- c(p_ab_cor, list(p_ab_cor_i))
  nstates_cor <- c(nstates_cor, nstates_cor_i)
}

df$x_a_cor <- x_a_cor
df$x_b_cor <- x_b_cor
df$p_ab_cor <- p_ab_cor
df$nstates_cor <- nstates_cor

if (compound) {
  df <- cbind(df, df_compound)
}

# Construct features -----------------------------------------------------------
message("Constructing features for lotteries...")
old_names <- names(df)
df <- compl$build_features(df)

# ------------------------------------------------------------------------------
#              Generating complexity index using features
# ------------------------------------------------------------------------------
message("Calculating Complexity Indices")
# df <- df %>%
#   rename_with(~ gsub("(__ab)", "", .), matches("(__ab)")) %>%
#   rename_with(~ gsub("(__a)", "_a", .), matches("(__a)")) %>%
#   rename_with(~ gsub("(__b)", "_b", .), matches("(__b)"))

# features for problem complexity
features_ci <- c(
  "ln_abs_ev_diff__ab",
  "ave_ln_scale__ab",
  "ave_ln_nstates__ab",
  "compound",
  "ave_gains__ab",
  "ln_cdf_diff_abs__ab"
)

# features for aggregation complexity
features_ac <- c(
  "ave_ln_scale__ab",
  "ave_ln_nstates__ab",
  "compound",
  "ave_gains__ab",
  "ln_cdf_diff_abs__ab"
)

# Only do this if not single lottery
# define indices to calculate
calc_indices <- c("OCI", "SCI", "OAC", "SAC")

index_tmp <- read.csv("coef/all_coef.csv")

for (index in calc_indices) {
  features_used <- c()
  datafile <- ""
  if ((index == "OCI") || (index == "SCI")) {
    features_used <- features_ci
  } else if ((index == "OAC") || (index == "SAC")) {
    features_used <- features_ac
  }
  df[index] <- 0

  for (row in 1:nrow(df)) {
    # add intercept
    df[row, index] <- index_tmp[index_tmp[, "features"] == "_cons", index]

    # Calculate Convex Combination
    for (cur_feat in features_used) {
      print(cur_feat)
      feat_coef <- cur_feat
      print(df[[row, feat_coef]])

      df[row, index] <- df[[row, index]] + index_tmp[index_tmp[, "features"] == cur_feat, index] * df[[row, feat_coef]]
      print(df[row, index])
    }
  }
  # Winsorize index at 0
  df[[index]] <- pmax(df[[index]], 0)

  if ((index == "OCI") || (index == "SCI")) {
    df[[index]] <- pmin(df[[index]], 0.5)
  }
}

# If single lottery, rename indices
if (single_lottery) {
  df <- df %>%
    rename(
      OLCI = OCI,
      SLCI = SCI
    )
  calc_indices <- c("OLCI", "SLCI")
}

# Save  ------------------------------------------------------------------------
# Renaming and Rearranging -----------------------------------------------------
df <- df %>%
  select(all_of(calc_indices))

# add indices and features to original data
df <- cbind(df_fundamentals, df)

write_csv(df, "output/index_calculated_R.csv")

message("Done.")
