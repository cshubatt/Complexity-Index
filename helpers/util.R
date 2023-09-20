# -----------------------------------------------------------------------------
# MODULE
# Functions for defining complexity features in choices_13k data
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Use: u <- modules::use(here("lib", "util.R"))
# -----------------------------------------------------------------------------
import(xtable)
import(broom) # tidy() regression output
import(testit) # assert
import(dplyr) # %>%
import(data.table) # setnames()
import(purrr) # pmap
import(glue) # glue()

data_fp <- function(fn){
  fp <- file.path("../data", fn)
  return(fp)
}

tidy_plus <- function(fit, var_labels = NA, alt_names = c("Variable", "Estimate", "Std. Error", "t", "p"), alt_var_labels = NA, drop_t = TRUE, save = FALSE, save_fp = NA){
  # takes in lm() object, cleans and (optionally) saves output tbl
  
  assert(
    "Alt names is length 5", length(alt_names) == 5
  )
  
  tidy_tbl <- tidy(fit)
  if(drop_t){
    tidy_tbl<- tidy_tbl %>% select(-statistic)
    alt_names <- alt_names[c(1,2,3,5)]
  }
  tidy_tbl <- tidy_tbl %>%
    setnames(alt_names)
  
  if(!is.na(alt_var_labels)){
    assert(
      "Num variables equals num supplied var labels",
      length(alt_var_labels == nrow(tidy_tbl))
    )
    tidy_tbl$Variable <- alt_var_labels
  }
  
  if(save){
    assert("non-NA save_fp", !is.na(save_fp))
    xt <- xtable(tidy_tbl)
    print.xtable(xt, type = "latex", file = save_fp, floating = FALSE)
  }
}

unpack_column <- function(
    unpack_col, name_scheme, df, ncol = 9
){
  n <- nrow(df)
  mat <- matrix(NA, nrow = n, ncol = ncol)
  for (i in 1:n){
    ktemp <- length(unlist(df[[unpack_col]][i]))
    mat[i, 1:ktemp] <- unlist(df[[unpack_col]][i])
  }
  df <- data.frame(mat)
  names(df) <- glue("{name_scheme}_{1:ncol}")
  
  return(df)
}

# Repack payoffs/probabilities from wide to long
repack_column <- function(
    repacked_col, name_scheme, df, ncol = 9
) {
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
