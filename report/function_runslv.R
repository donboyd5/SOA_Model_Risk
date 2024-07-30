
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
