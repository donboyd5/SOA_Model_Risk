

# utility functions -------------------------------------------------------

iclosest <- function(value, vec){
  # get the first index of the value in a vector that is closest to the argument: value
  which.min(abs(value - vec))[1]
}


# probability distributions -----------------------------------------------

scale_dist <- function(x, μ_y, σ_y){
  # Perform a linear transformation on a random variable x:
  
  #  y = a + b * x
  
  # The transformation shifts the mean of the distribution by a units and scales
  # it by b, and increases the SD proportionately.
  
  #   μ_y = a + b * μ_x
  #   σ_y = |b| * σ_x
  
  # Given x, μ_y, σ_y, we can solve for b and a:
  #   b = σ_y / σ_x
  #   a = μ_y - b * μ_x
  
  #  y will follow the same probability distribution and have the same shape as x, but some parameters
  #    of the distribution might change due to the transformation.
  # See Casella, G., & Berger, R. L. (2002). Statistical Inference (2nd ed.). Pacific Grove, CA: Duxbury.
  # Chapters 1 and 2.
  
  μ_x <- mean(x)
  σ_x <- sd(x)
  
  b <- σ_y / σ_x
  a <- μ_y - b * μ_x
  a + b * x
}


# t_dfreedom <- function(xkurt){
#   # calculate degrees of freedom for the Student's t distribution, given a desired excess kurtosis
#   # ν = (12 + 6γ2_excess + 2γ2_excess\^2) / (3 + γ2_excess) for ν > 4
#   # t_dfreedom(3)
#   v <- (12 + 6 * xkurt + 2 * xkurt^2) / (3 + xkurt)
#   if(v <= 4) print("CAUTION: v is <= 4")
#   v
# }


t_dfreedom <- function(xkurt){
  # calculate degrees of freedom for the Student's t distribution, given a desired excess kurtosis
  # Kurtosis (γ2) = 3 + 6/(ν-4), for ν > 4
  # xkurt =  6/(ν-4)
  # v = 6 / xk + 4
  # t_dfreedom(4)
  v <- 6 / xkurt + 4
  if(v <= 4) print("CAUTION: v is <= 4")
  v
}


# statistical -------------------------------------------------------------

ar1 <- function(vec){
  rts <- ts(vec, start = 1)
  rts_ar1 <- Arima(rts, order = c(1, 0, 0))  # fit AR(1) model
  stats <- coeftest(rts_ar1)
  list(phi=stats["ar1", 1], pval=stats["ar1", 4])
}

fphi <- function(vec) {ar(vec, aic=FALSE, order.max = 1)$ar}


# functions related to investment returns ---------------------------------


# gmat2 <- function(seed, nsims, nyears, ir_mean_target, ir_sd_target, phi){
#   set.seed(seed)
#   m <- matrix(nrow=nsims, ncol=nyears)
#   y1 <- rnorm(nsims, mean=ir_mean_target, sd=ir_sd_target) # year 1
#   # y1 <- scale_dist(y1, mu, sd) # scale year 1
#   m[, 1] <- y1
#   
#   for (t in 2:nyears) {
#     e <- rnorm(nsims, mean = 0, sd = ir_sd_target)
#     # e <- scale_dist(e, 0, sd)
#     m[, t] <- ir_mean_target + phi * (m[, t-1] - ir_mean_target) + e
#   }
#   m
# }



mr_mat <- function(seed, nyears=50, nsims=1000, ir_phi_target,
                   ir_mean_target, ir_sd_target){
  # generate a matrix of mean-reverting sims, for a given seed
  set.seed(seed)
  # generate the innovations for the matrix all at once
  # m_innov <- matrix(rnorm(nyears*nsims, ir_mean_target, ir_sd_target), nrow=nsims)
  f <- function(i){
    # y0 <- arima.sim(n = nyears, list(ar = phi), innov = m_innov[i, ])
    y0 <- arima.sim(n = nyears, list(ar = ir_phi_target))
    # c(scale_dist(y0, ir_mean_target, ir_sd_target))
    y0
  }
  ml <- map(1:nsims, f)
  m <- matrix(unlist(ml), nrow = length(ml), byrow = TRUE)
  m <- scale_dist(m, ir_mean_target, ir_sd_target)
  m
}



frontier_tbl <- function(frontobj){
  # convert an efficient-frontier object, from Portfolio Analytics package
  # to a tibble
  cnames <- colnames(frontobj$frontier)
  matrix(frontobj$frontier, nrow=nrow(frontobj$frontier)) |> 
    as_tibble() |> 
    setNames(cnames) |> 
    rename(sd=StdDev) |> 
    mutate(obs=row_number()) |> 
    relocate(obs)
}


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
  # CAUTION: the e1071 measures already subtract 3, so don't do it again!
  xkurt1 <- e1071::kurtosis(ir, type=1) # typical older textbook, subtracts 3, appears to be same as kurt0
  # xkurt2 <- e1071::kurtosis(ir, type=2) # SAS and SPSS measure, unbiased under normality; subtracts3
  # xkurt3 <- e1071::kurtosis(ir, type=3) # default; minitab and bmdp formula
  
  tibble(n, min, max, mean, sd, skew=skew1, xkurt=xkurt1)
  # x <- rnorm(100)
  # uvmeasures(x)
}


# x <- rnorm(100)
# uvmeasures(x)

