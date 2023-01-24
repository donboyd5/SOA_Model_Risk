
get_parameters <- function(data, params, start_date){
  # data and params are defaults from the AIRG generator
  runparams <- list()
  
  runparams$start_date <- as.Date(start_date)
  runparams$hist_curves <- data$hist_curves
  runparams$init_curve <- get_init_curve(hist_curves=data$hist_curves)
  runparams$mrp <- update_mrp(hist_curves=data$hist_curves, start_date)
  runparams$ir_generator <- get_ir_generator_parameters(params)
  runparams$bond_index <- get_bond_index_parameters(params)
  runparams$equity_fund <- get_equity_index_parameters(params)
  runparams$equity_cormat <- get_equity_correlation_matrix(params)
  
  runparams
}


get_bond_index_parameters <- function(params){
  # get default bond index parameters as previously read-in from the AIRG spreadsheet
  
  # get maturity, monthlyFactor, monthlySpread, duration, and volatility
  #  for each of
  # MoneyFund, IntGovtFund, LongCorpFund
  
  # names(params)
  # params$bond_index
  
  ## vba names and model names for bond index variables
  # m      MoneyFund.maturity = .Cells(7, 9).Value
  # beta0  MoneyFund.monthlyFactor = .Cells(8, 9).Value
  # kappa  MoneyFund.monthlySpread = .Cells(9, 9).Value
  # beta1  MoneyFund.duration = .Cells(10, 9).Value
  # sigma  MoneyFund.volatility = .Cells(11, 9).Value
  
  # convert symbol to lower case and save to runparams
  # maybe change format later
  bond_index <- params$bond_index |> 
    dplyr::mutate(symbol=str_to_lower(symbol))
  
  bond_index
}

get_equity_index_parameters <- function(params){
  # get default equity index parameters as previously read-in from the AIRG spreadsheet
  
  # For DiversifiedFund, InternationalFund, IntermediateRiskFund, AggressiveFund
  # we need parameter values
  
  # params$equity_fund
  ## vba names and model names for equity index variables
  
  # tau       DiversifiedFund.targetVol = .Cells(18, 9).Value
  # phi       DiversifiedFund.meanRevStrength = .Cells(19, 9).Value
  # sigma_v  DiversifiedFund.volStdDev = .Cells(20, 9).Value
  # a         DiversifiedFund.a = .Cells(22, 9).Value
  # b         DiversifiedFund.b = .Cells(23, 9).Value
  # c         DiversifiedFund.C = .Cells(24, 9).Value
  # sigma_0  DiversifiedFund.currentVol = .Cells(25, 9).Value
  # sigma_min    DiversifiedFund.minVol = .Cells(26, 9).Value
  # sigma_max    DiversifiedFund.maxVolBefore = .Cells(27, 9).Value
  # sigma_maxrand    DiversifiedFund.maxVolAfter = .Cells(28, 9).Value
  # NA        DiversifiedFund.SETmedianReturn = .Cells(38, 9).Value
  # NA        DiversifiedFund.SETvolatility = .Cells(39, 9).Value
  
  equity_fund <- params$equity_fund |> 
    dplyr::mutate(symbol=str_to_lower(symbol),
                  symbol=case_when(symbol=="sigma(v)" ~ "sigma_v",
                  symbol=="sigma(0)" ~ "sigma_0",
                  symbol=="sigma-" ~ "sigma_min",
                  symbol=="sigma+" ~ "sigma_max",
                  symbol=="sigma*" ~ "sigma_maxrand",
                  TRUE ~ symbol))
  equity_fund
}


get_init_curve <- function(hist_curves){
  init_curve <- hist_curves |> 
    dplyr::filter(date %in% c(as.Date(start_date), max(date))) |> 
    dplyr::arrange(date) |> 
    dplyr::filter(row_number()==1)
  init_curve
}

get_ir_generator_parameters <- function(params){
  # get default interest rate generator parameters as previously read-in from
  # the AIRG spreadsheet
  
  # params has the parameters exactly as provided in the AIRG
  # this function just cleans them up a bit and calculates constants
  
  # model gets the following - all are in params$ir_generator:
  #   tau1, beta1
  #   tau2, beta2, sigma2
  #   tau3, beta3, sigma3 
  #   initialVol
  #   phi, psi, theta, kappa
  #   correl12, correl13, correl23
  
  # note:in VB, # after a number indicates a long integer
  # we need to calculate several constants
  # const1 = (1 - correl12 * correl12) ^ 0.5
  # const2 = (correl23 - correl12 * correl13) / ((1# - correl12 ^ 2) ^ 0.5)
  #                                               const3 = (1# - ((correl23 - correl12 * correl13) ^ 2) / (1 - correl12 ^ 2) - correl13 ^ 2) ^ 0.5
  #                                                         const4 = beta3 * Log(tau3)
  #                                                         const5 = beta1 * Log(tau1)
  
  df <- params$ir_generator |>
    dplyr::filter(!is.na(range_name)) |>
    dplyr::mutate(range_name=str_to_lower(range_name))
  
  ir_generator <- list_from_df(df, "range_name", "default")
  
  ir_generator <- within(ir_generator, {
    const1 <- (1 - correl12 * correl12) ^ 0.5
    const2 <- (correl23 - correl12 * correl13) / ((1 - correl12 ^ 2) ^ 0.5)
    const3 <- (1 - ((correl23 - correl12 * correl13) ^ 2) / (1 - correl12 ^ 2) - correl13 ^ 2) ^ 0.5
    const4 <- beta3 * log(tau3)
    const5 <- beta1 * log(tau1)
  })
  ir_generator
}

get_equity_correlation_matrix <- function(params){
  # vba name CorrelationMatrix
  # params
  equity_cormat <- params$stock_cormat
  equity_cormat
}


update_mrp <- function(hist_curves, start_date){
  # calculate mean reversion point
  
  # data$hist_curves
  
  # Mean Reversion Point (MRP) for Long Bond Yield
  # The mean reversion point is a weighted average of historical 20-year yields.
  # we need 20-year bond yields through latest data
  
  # The AAA MRP at any given date is equal to the following weighted average, rounded to the nearest 0.25%			
  # Weight	Rate	
  # 50%	600 months median less 0.25%	
  # 50%	36 months average	
  
  # The NAIC MRP at any given date is equal to the following weighted average, 
  # rounded to the nearest 0.25%				
  # Weight	Rate		
  # 20%	600 months median (with no adjustment)		
  # 30%	120 months average		
  # 50%	36 months average		
  
  # data$hist_curves
  
  # RcppRoll does not have partial implemented so use zoo rollapplyr
  # zoo::rollapplyr(price, n(), mean, na.rm = TRUE, partial = TRUE)
  # mdn600=RcppRoll::roll_median(UST_20, n = curvenum[row_number()], align = "right", fill = NA)
  # library(zoo)
  # params$mrp
  
  # The NAIC MRP changes only once per year, in January.
  mrp_df <- hist_curves |> 
    dplyr::select(date, curvenum, UST_20) |> 
    dplyr::arrange(date) |> 
    dplyr::mutate(mdn600=zoo::rollapplyr(UST_20, 600, median,
                                  align="right", na.rm = TRUE, partial = TRUE),
           mean36=zoo::rollapplyr(UST_20, 36, mean,
                                  align="right", na.rm = TRUE, partial = TRUE),
           mean120=zoo::rollapplyr(UST_20, 120, mean,
                                   align="right", na.rm = TRUE, partial = TRUE),
           mrp_aaa_step1=.5*(mdn600 - .0025) + .5*mean36,
           mrp_aaa_round=round_nearest(mrp_aaa_step1, .0025),
           mrp_naic_step1=.2*mdn600 + .3*mean120 + .5*mean36,
           mrp_naic_jan=ifelse(month(date)==1, mrp_naic_step1, NA_real_)) |> 
    tidyr::fill(mrp_naic_jan, .direction = "down") |> 
    dplyr::mutate(mrp_naic_jan_round=round_nearest(mrp_naic_jan, .0025),
           mrp_aaa=mrp_aaa_round, mrp_naic=mrp_naic_jan_round)
  tail(mrp_df, 14)
  # the spreadsheet continues naic calculations for a year beyond the last data year -- do I need that??
  
  # take the mrp_naic for the start date, or the last mrp if start date is after last date
  # runparams$start_date <- "2020-06-01"
  # runparams$start_date <- "2021-12-01"
  mrp_naic <- mrp_df |> 
    dplyr::filter(date %in% c(as.Date(start_date), max(date))) |> 
    dplyr::arrange(date) |> 
    dplyr::filter(row_number()==1) |> 
    dplyr::pull(mrp_naic)
  
  return(mrp_naic)
}




