# -----------------------------------------------------------------------------
# Purpose: Calculated Complexity Indices of Choice Problems and Lotteries
# Author: Benjamin Enke & Cassidy Shubatt
# Output: CSV File with Indices and Features
# Last updated: 8 Sept, 2023 by Sebasitan Redl
# -----------------------------------------------------------------------------

install.packages("renv")
library(renv)
renv::restore()

library("tidyverse")
library("glue")
library("testit")
library("optparse")
library("xtable")
library("pracma")
library("OneR")
library("assert")
library("usethis")

# Load -------------------------------------------------------------------------
message("Loading features and response...")

util <- modules::use("helpers/util.R")
compl <- modules::use("helpers/complexity_feature_functions.R")
df <- read.csv("sample_data/sample_all_indices_calculation_2.csv")

#which indices will be calculated
indices <- "PC"

#Replace empty strings with NA in character columns
char_columns <- sapply(df, is.character)
df[char_columns] <- lapply(df[char_columns], function(x) ifelse(x == "", NA, x))

#make dataframe numeric
df[] <- lapply(df, as.numeric)

#max numer of states
num_st_max <- 7

message("Checking if more than one lotterie in the data set is supplied 
        in order to calculated choice complexity indices too (OPC/SPC/OAC/SAC)")

if((length(grep("^x_b", names(df))) > 0) && 
   (length(grep("^p_b", names(df))) > 0)){
  message("Two lotteries per problem found!")
}else{
  message("Just one lottery per problem found!")
  indices <- "LC"
  for (i in 1:num_st_max) {
    if (glue("x_{i}") %in% names(df)){
         df <- df %>%
           rename(!!glue("x_a_{i}") := glue("x_{i}"))
    }
    if (glue("p_{i}") %in% names(df)){
      df <- df %>%
        rename(!!glue("p_a_{i}") := glue("p_{i}"))
    }
  }
  df[glue("x_b_1")] <- 0
  df[glue("p_b_1")] <- 1 
  
}

#"check if one lottery has at least more than 1 state"
testit::assert("check if one lottery has at least more than 1 state per problem",
       sum(grepl("x_a", names(df))) > 1|| sum(grepl("x_b", names(df))) > 1)

#add missing columns 
x_a_c <- c()
x_b_c <- c()
p_a_c <- c()
p_b_c <- c()

for (i in 1:num_st_max) {
  if (!(glue("x_a_{i}") %in% names(df))){
    df[glue("x_a_{i}")] <- NA
  }
  if (!(glue("p_a_{i}") %in% names(df))){
    df[glue("p_a_{i}")] <- NA
  }
  if (!(glue("x_b_{i}") %in% names(df))){
    df[glue("x_b_{i}")] <- NA
  }
  if (!(glue("p_b_{i}") %in% names(df))){
    df[glue("p_b_{i}")] <- NA
  }
  
  #if probability is 0, remove state
  condition_a <- df[[glue("p_a_{i}")]] == 0
  condition_b <- df[[glue("p_b_{i}")]] == 0
  condition_a <- ifelse(is.na(condition_a), FALSE, condition_a)
  condition_b <- ifelse(is.na(condition_b), FALSE, condition_b)
  df[condition_a, c(glue("p_a_{i}"), glue("x_a_{i}"))] <- NA
  df[condition_b, c(glue("p_b_{i}"), glue("x_b_{i}"))] <- NA
  
  #check if each probaility matches as state and vice versa for 
  print(glue("x_a_{i} and p_a_{i}"))
  testit::assert("check if each probaility matches as state and vice versa in lottery A",
         !any((is.na(df[glue("p_a_{i}")]) != is.na(df[glue("x_a_{i}")]))))
  print(glue("x_b_{i} and p_b_{i}"))
  testit::assert("check if each probaility matches as state and vice versa in lottery B",
         !any((is.na(df[glue("p_b_{i}")]) != is.na(df[glue("x_b_{i}")]))))
  x_a_c <- c(x_a_c, glue("x_a_{i}"))
  x_b_c <- c(x_b_c, glue("x_b_{i}"))
  p_a_c <- c(p_a_c, glue("p_a_{i}"))
  p_b_c <- c(p_b_c, glue("p_b_{i}"))
}

#Reorder each row thus numbers are first
# Function to reorder each row with numbers first and then NA
reorder_row <- function(row) {
  non_na_values <- row[!is.na(row)]
  na_values <- row[is.na(row)]
  reordered_row <- c(non_na_values, na_values)
  return(reordered_row)
}


order_c <- c(x_a_c, p_a_c, x_b_c, p_b_c)

if("problem" %in% names(df)){
  order_c<- c("problem", order_c)
}

#saving lottery fundamentals for later
df_fundamentals <- df[order_c]
if (indices == "LC"){
  order_lc <- c(x_a_c, p_a_c)
  if("problem" %in% names(df)){
    order_lc <- c("problem", order_lc)
  }
  df_fundamentals <- df[order_c]
}

compound <- FALSE
df_compound <- data.frame()
if("compound" %in% names(df)){
  compound <- TRUE
  df_compound <- df %>% select(compound)
}
df <- df %>% 
  select(all_of(order_c))


df[x_a_c]<- as.data.frame(t(apply(df[x_a_c]%>% 
                                    select(starts_with("x_a")), 1, reorder_row)))
df[p_a_c]<- as.data.frame(t(apply(df[p_a_c]%>% 
                                    select(starts_with("p_a")), 1, reorder_row)))
df[x_b_c]<- as.data.frame(t(apply(df[x_b_c]%>% 
                                    select(starts_with("x_b")), 1, reorder_row)))
df[p_b_c]<- as.data.frame(t(apply(df[p_b_c]%>% 
                                    select(starts_with("p_b")), 1, reorder_row)))

# Repack p/x columns
message("Repacking p,x columns into lists...")
types <- c("p", "x")
lots <- c("a", "b")

for (type in types) {
  for (lot in lots) {
    repacked <- glue ("{type}_{lot}")
    name_scheme <- glue("{repacked}_")
    df <- util$repack_column(repacked, name_scheme, df)
    for(i in 1:nrow(df)) {
      li_val <- df[[repacked]][i] %>% unlist
      if(all(is.na(li_val))) {
        df[[repacked]][i] <- NA
      } else {
        df[[repacked]][i] <- list(li_val[!is.na(li_val)])
      }
    }
  }
}


# Generate correlated state variables ------------------------------------------
message("Constructing correlated state versions of x, p...")
x_a_cor <- c()
x_b_cor <- c()
p_ab_cor <- c()
nstates_cor <- c()

for (i in seq(nrow(df))) {
  x_a_i <- df$x_a[i] %>% unlist
  x_b_i <- df$x_b[i] %>% unlist
  p_a_i <- df$p_a[i] %>% unlist
  p_b_i <- df$p_b[i] %>% unlist
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

if(compound){
df <- cbind(df, df_compound)
}

# Construct features -----------------------------------------------------------
message("Constructing features for lotteries...")
old_names <- names(df)
df <- compl$build_features(df)

unpack_column <- function(
    unpack_col, name_scheme, df, max_states = 7
) {
  n <- nrow(df)
  mat <- matrix(0, nrow = n, ncol = max_states)
  for (i in 1:n){
    ktemp <- length(unlist(df[[unpack_col]][i]))
    mat[i, 1:ktemp] <- unlist(df[[unpack_col]][i])
  }
  df <- data.frame(mat)
  names(df) <- glue("{name_scheme}_{1:max_states}")
  
  return(df)
}

A_prob_df <- unpack_column(
  "p_a", name_scheme = "p_a", df, max_states = num_st_max
)
A_pay_df <- unpack_column(
  "x_a", name_scheme = "x_a", df, max_states = num_st_max
)
B_prob_df <- unpack_column(
  "p_b", name_scheme = "p_b", df, max_states = num_st_max
)
B_pay_df <- unpack_column(
  "x_b", name_scheme = "x_b", df, max_states = num_st_max
)

df <- cbind(A_prob_df, A_pay_df, B_prob_df, B_pay_df, df)

#replace 0 probabilites if NA in states and probabilities
for (i in 1:num_st_max) {
  #if probability is 0, remove state
  condition_a <- df[[glue("p_a_{i}")]] == 0
  condition_b <- df[[glue("p_b_{i}")]] == 0
  condition_a <- ifelse(is.na(condition_a), FALSE, condition_a)
  condition_b <- ifelse(is.na(condition_b), FALSE, condition_b)
  df[condition_a, c(glue("p_a_{i}"), glue("x_a_{i}"))] <- NA
  df[condition_b, c(glue("p_b_{i}"), glue("x_b_{i}"))] <- NA
  
}



# ------------------------------------------------------------------------------
#              Generating complexity index using features
# ------------------------------------------------------------------------------

df <- df %>%
  rename_with(~gsub("(__ab)", "", .), matches("(__ab)")) %>% 
  rename_with(~gsub("(__a)", "_a", .), matches("(__a)")) %>% 
  rename_with(~gsub("(__b)", "_b", .), matches("(__b)"))

#features for problem complexity
features_pc <- c(
  "abs_ev_diff", 
  "abs_ev_diff_sq",
   "ave_ln_scale",
   "ave_ln_nstates",
   "compound",
   "nodom",
   "ave_not_gains",
   "ln_cdf_diff_abs"
) 

#features for aggregation complexity
features_ac <- c(
  "ave_ln_scale",
  "ave_ln_nstates",
  "compound",
  "nodom",
  "ave_not_gains",
  "ln_cdf_diff_abs"
) 

#features for lottery complexity of lottery a
features_lc_a <- c(
  "ln_scale_a",
  "ln_nstates_a",
  "compound",
  "not_gains_a",
  "ln_var_a"
) 

#features for lottery complexity of lottery b
features_lc_b <- c(
  "ln_scale_b",
  "ln_nstates_b",
  "compound",
  "not_gains_b",
  "ln_var_b"
) 

#define indices to calculate
calc_indices <- c("OPC", "SPC", "OAC", "SAC", "OLC_a", "OLC_b", "SLC_a", "SLC_b")
if (indices == "LC"){
  calc_indices <- c("OLC_a", "SLC_a")
}

index_tmp <- read.csv("coef/all_coef.csv")

for (index in calc_indices) {
  features_used <- c()
  datafile <- ""
  index_name <- index
  if(index == "OPC"){
    features_used <-  features_pc
  }
  if(index == "SPC"){
    features_used <-  features_pc
  }
  if(index == "OAC"){
    features_used <-  features_ac
  }
  if(index == "SAC"){
    features_used <-  features_ac
  }
  if(index == "OLC_a"){
    features_used <-  features_lc_a
    index_name <- "OLC"
  }
  if(index == "OLC_b"){
    features_used <-  features_lc_b
    index_name <- "OLC"
  }
  if(index == "SLC_a"){
    features_used <-  features_lc_a
    index_name <- "SLC"
  }
  if(index == "SLC_b"){
    features_used <-  features_lc_b
    index_name <- "SLC"
  }
  
  for (row in 1:nrow(df)) {
    #add intercept
    df[row, index] <- index_tmp[index_tmp[,"features"] == "_cons" , index_name]
    
    #Calculate Convex Combination
    for (cur_feat in features_used) {
      feat_coef <- cur_feat
      #take feature coefficient from OLC_a for OLC_b
      if(index %in% c("OLC_b", "SLC_b")){
        cur_feat <-  sub("_b$", "_a", cur_feat)
      }
      df[row, index] <- df[[row, index]] + index_tmp[index_tmp[,"features"] == cur_feat , index_name] * df[[row, feat_coef]]
    }
  }
  #Winsorize index at 0
  df[df[, index] < 0,index] <- 0
}

# Save  -----------------------------------------------------------

# Construct features -----------------------------------------------------------
keep_features <- union(c(calc_indices, features_pc, features_ac), c(features_lc_a, features_lc_b))

if (indices == "LC"){
  keep_features <- c(calc_indices, features_lc_a)
}

df <- df %>% 
  select( all_of(keep_features))

df <- cbind(df_fundamentals, df)

write_csv(df, "output/index_calculated_R.csv")


message("Done.")

