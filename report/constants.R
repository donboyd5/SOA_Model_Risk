
rptdir <- here::here("report")

# ddir <- r"(E:\data\soa\acli)"
# base_dir <- "s_baseline"
# base_fn <- "baseline_ACLI_v1.46 Alt3 12-2020 100yrs.xlsm"

ddir <- r"(E:\data\soa\acli_report)"
base_dir <- "baseline"
base_fn <- "baseline_ACLI_v1.46 Alt3 12-2020 100yrs.xlsm"



constants <- list()
constants$seed <- 1234
constants$nyears <- 50
constants$nsims <- 1000
constants$ir_mean_target <- .07
constants$ir_sd_target <- .10