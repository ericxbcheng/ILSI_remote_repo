# Run the simulations based on the factorial design in 1/7/19 Sampling: Analysis 2

source("Sampling_libraries.R")
source("Sampling_contamination.R")
source("Sampling_plan.R")
source("Sampling_assay.R")
source("Sampling_outcome.R")
source("Sampling_iteration.R")
source("Sampling_analysis.R")
source("Sampling_visualization.R")

## We choose "n_contam" to iterate on.
n_contam = rpois(n = 1, lambda = 3)

## Other fixed parameters
## Contamination
x_lim = c(0, 10)
y_lim = c(0, 10)
cont_level = c(7, 1)
spread = "continuous"

### Mode 1
n_affected = rpois(n = 1, lambda = 5)
covar_mat = matrix(data = c(0.25, 0, 0, 0.25), nrow = 2, ncol = 2)

### Mode 2
spread_radius = 1
LOC = 10^(-3)
fun = "exp"

## Sampling plan
method_sp = "srs"
n_sp = 15
sp_radius = 1
n_strata = 5
by = "row"
m_kbar = 0.3
m_sp = 25
conc_good = 0.1
case = 12
m = 0
M = 0
Mc = 20

## Assay
method_det = "enrichment"

## Sampling outcome
n_iter = 100

# Set the tuning values for n_contam
param_name = "n_contam"
vals = c(1,2,3,4,5,6)

# Run the following
n_sp_list = c(5, 10, 15, 20, 30, 60)
case_list = c(10, 11, 13, 12, 14, 15) # According to the attribute plan
strategy_list = c("srs", "strs", "ss")

# Set the default arguments list
ArgList = list(n_contam = n_contam, xlim = x_lim, ylim = y_lim, n_affected = n_affected, 
               covar_mat = covar_mat, spread_radius = spread_radius, method_sp = method_sp, 
               n_sp = n_sp, sp_radius = sp_radius, spread = spread, n_strata = n_strata, by = by, 
               cont_level = cont_level, LOC = LOC, fun = fun, m_kbar = m_kbar, m_sp = m_sp, 
               conc_good = conc_good, case = case, m = m, M = M, Mc = Mc, method_det = method_det)

Set_ArgList = function(n_sp_new, case_new, Args_default){
  
  # Change the n_sp and case accordingly
  Args_default$n_sp = n_sp_new
  Args_default$case = case_new
  
  ArgList_srs = ArgList_strs = ArgList_ss = Args_default
  
  # SRS
  ArgList_srs$method_sp = "srs"
  
  # STRS
  ArgList_strs$method_sp = "strs"
  
  # SS
  ArgList_ss$method_sp = "ss"
  
  return(list("srs" = ArgList_srs, "strs" = ArgList_strs, "ss" = ArgList_ss))
}

ArgList_all = map2(.x = n_sp_list, .y = case_list, .f = Set_ArgList, Args_default = ArgList)
names(ArgList_all) = as.character(n_sp_list)

Name_rds = function(contam, sp, strategy, case){
  paste0(contam, "_", sp, "_", strategy, "_", case, ".rds")
}

### ATTENTION: this function only works when n_contam = 1:6
output_rds = function(seed, n_iter, val, name, Args_default, sp, case){
  # SRS
  set.seed(seed)
  result_srs = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$srs, n_iter = n_iter)
  saveRDS(object = result_srs, file = Name_rds(contam = "1to6", sp = sp, strategy = "srs", case = case))
  
  # STRS
  set.seed(seed)
  result_strs = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$strs, n_iter = n_iter)
  saveRDS(object = result_strs, file = Name_rds(contam = "1to6", sp = sp, strategy = "strs", case = case))
  
  # SS
  set.seed(seed)
  result_ss = map(.x = 1:n_iter, .f = tune_param2, val = val, name = name, Args_default = Args_default$ss, n_iter = n_iter)
  saveRDS(object = result_ss, file = Name_rds(contam = "1to6", sp = sp, strategy = "ss", case = case))
}

pmap(.l = list(Args_default = ArgList_all, sp = n_sp_list, case = case_list), .f = output_rds, seed = 123, n_iter = n_iter, val = vals, name = param_name)

