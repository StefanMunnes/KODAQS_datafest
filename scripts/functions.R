top_code_income <- function(income_vector, percentile = 0.99) {
  threshold <- quantile(income_vector, probs = percentile, na.rm = TRUE)
  income_vector[income_vector > threshold] <- threshold
  return(income_vector)
}
