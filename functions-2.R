# Ex. 1

randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]])
  return(d)
}

###

difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- mean(d_1[[var]]) - mean(d_2[[var]])
  return(result)
}

###

permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    permutation <- randomize(d, var)
    permutation_statistics[i] <- statistic(permutation,
                                           var,
                                           grouping_var,
                                           group1,
                                           group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

###

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}
permutation_pvalue_twosided <- function(p) {
  return(2*min(permutation_pvalue_left(p),
               permutation_pvalue_right(p)))
}