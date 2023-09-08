# -----------------------------------------------------------------------------
# MODULE
# Functions for operationalizing utility functions and working with lotteries
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# -----------------------------------------------------------------------------

# Setup -----------------------------------------------------------------------
import(testit) # assert
import(purrr) # map_2
import(dplyr) # mutate()
import(knitr)

sample_pwf <- function(p_list, chi = 1, gamma = 1){
  # Input: p_list, a list of all probabilities p
  # Output: weighted_prob, a vector of weighted probabilities from a fixed probability weighting function
  
  p <- unlist(p_list)
  weighted_prob <- (chi*p^gamma)/(chi*p^gamma + (1-p)^gamma)
  return(weighted_prob)
}

sample_utility <- function(x_list, alpha = 1, beta = 1, lambda = 1){
  # Input: x_list, a list of all possible outcomes x
  # Output: utility, a vector of utilities associated with
  # the values of x_vector, according to a fixed utility function
  x <- unlist(x_list)
  utility <- ifelse(
    x > 0, x^alpha, -lambda * (-x)^beta
  )
  return(utility)
}

cdf <- function (x, x_list, p_list) {
  x_vec <- unlist(x_list)
  p_vec <- unlist(p_list)
  
  x_vec_sort <- x_vec[order(x_vec)]
  p_vec_sort <- p_vec[order(x_vec)]
  
  p_cum <- cumsum(p_vec_sort)
  
  closest_x <- max(x_vec[which(x_vec <= x)])
  
  cdf_x <- p_cum[which(x_vec_sort == closest_x)]
  
  return(cdf_x)
}

quantile <- function(p, x_list, p_list, min_geq = FALSE) {
  # warning this is only kind of a quantile; used for creating dominance lots
  # returns max x s.t. Pr(X >= x) <= q
  # if plus_one, returns min x s.t. Pr(X >= x) >= q
  # min x s.t. Pr(X >= x) = q
  
  x_vec <- unlist(x_list)
  p_vec <- unlist(p_list)
  
  x_vec_sort <- x_vec[order(x_vec)]
  p_vec_sort <- p_vec[order(x_vec)]
  
  p_cum <- cumsum(p_vec_sort)
  
  if(min_geq){
    p_cum_larger <- p_cum[which(p_cum >= p)]
    if (length(p_cum_larger) == 0) {
      quant <- max(x_vec)
    } else {
      closest_p <- min(p_cum_larger)
      quant_index <- which(p_cum == closest_p)
      quant <- x_vec_sort[quant_index]
    }
  } else {
    p_cum_larger <- p_cum[which(p_cum > p)]
    if (length(p_cum_larger) == 0) {
      quant <- max(x_vec)
    } else {
      closest_p <- min(p_cum_larger)
      quant_index <- which(p_cum == closest_p)
      quant <- x_vec_sort[quant_index]
    }
  }
  return(quant)
}

lottery_value <- function (
    p_list, x_list, chi = 1, gamma = 1, alpha = 1, beta = 1, lambda = 1
) {
  assert(
    "x_list and p_list same length",
    length(x_list) == length(p_list)
  )
  x <- sample_utility(x_list, alpha, beta, lambda)
  p <- sample_pwf(p_list, gamma, chi)
  value <- x %*% p
  return(value)
}

display_problem <- function (Problem, bRate, n, EV_A, EV_B, lotb_var_lottery, A_xs, A_probabilities, B_xs, B_probabilities, ...) {
  A_df <- data.frame(X = unlist(A_xs), Wts = unlist(A_probabilities)) %>%
    filter(Wts != 0)
  B_df <- data.frame(X = unlist(B_xs), Wts = unlist(B_probabilities)) %>%
    filter(Wts != 0)
  
  message("=================================")
  message("Problem ", Problem)
  message("Rate of choosing B: ", bRate)
  message("Number of subjects: ", n)
  message("EV(B) - EV(A) = ", EV_B - EV_A)
  message("Variance of B = ", lotb_var_lottery)
  message("---------------------------------")
  message("Lottery A")
  print(kable(A_df, "rst"))
  message("")
  message("---------------------------------")
  message("Lottery B")
  print(kable(B_df, "rst"))
  message("")
  message("=================================")
  
  return(NA)
}
