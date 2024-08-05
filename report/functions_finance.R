

lrport <- function(lrassets, weights){
  # lrassets is a matrix of log returns of assets where columns are asset classes, rows are timepoints
  # weights is a vector of weights that sum to 1
  
  # argument checking
  if (abs(sum(weights) - 1) > .Machine$double.eps^0.5) {
    stop("Error: The weights must sum to 1.")
  }
  
  lrassets <- as.matrix(lrassets)
  port_aret <- (exp(lrassets) -1) %*% weights # arithmetic return of portfolio
  log(1 + port_aret) |> as.vector()  # log returns of the portfolio
}


runslv <- function(data, start, end, modtype="leverage"){
  # get the demeaned log return from a data subset and estimate an slv model
  # data must have date and lr columns
  
  vec <- data |> 
    filter(date >= as.Date(start), date <= as.Date(end)) |> 
    filter(!is.na(lr)) |> 
    mutate(lrc=lr - mean(lr)) |> 
    pull(lrc)
  
  mod <- estimate_parameters(vec, model = modtype, silent = TRUE)
  mod
}
