

uvmeasures <- function(ir){
  # ir is a vector of returns for a given time period (or, if historical, all observations)
  n <- length(ir)
  
  min <- base::min(ir)
  max <- base::max(ir)
  mean <- base::mean(ir)
  sd <- stats::sd(ir) # per stats::sd documentation, "Like var this uses denominator n - 1"
  
  skew1 <- e1071::skewness(ir, type=1) # typical older textbook measure
  # skew2 <- e1071::skewness(ir, type=2) # SAS and SPSS measure, unbiased under normality; subtracts3
  # skew3 <- e1071::skewness(ir, type=3) # default; minitab and bmdp formula
  # all 3 skewness measures are unbiased under normality; they don't seem to be much different
  
  # xkurt0 <- moments::kurtosis(ir) - 3 # the estimator of Pearson's measure of kurtosis, adjusted by me with -3
  xkurt1 <- e1071::kurtosis(ir, type=1) # typical older textbook, subtracts 3, appears to be same as kurt0
  # xkurt2 <- e1071::kurtosis(ir, type=2) # SAS and SPSS measure, unbiased under normality; subtracts3
  # xkurt3 <- e1071::kurtosis(ir, type=3) # default; minitab and bmdp formula
  
  tibble(n, min, max, mean, sd, skew=skew1, xkurt=xkurt1)
  # x <- rnorm(100)
  # uvmeasures(x)
}


# x <- rnorm(100)
# uvmeasures(x)

