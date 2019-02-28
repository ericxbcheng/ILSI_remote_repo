# Run the simulations based on the factorial design in 1/7/19 Sampling: Analysis 2

source("Sampling_libraries.R")
source("Sampling_contamination.R")
source("Sampling_plan.R")
source("Sampling_assay.R")
source("Sampling_outcome.R")
source("Sampling_iteration.R")
source("Sampling_analysis.R")
source("Simulation_data.R")
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
spread_radius = 0.2
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

ArgList_all = map2(.x = n_sp_list, .y = case_list, .f = Set_ArgList, Args_default = ArgList)
names(ArgList_all) = as.character(n_sp_list)

pmap(.l = list(Args_default = ArgList_all, sp = n_sp_list, case = case_list), .f = output_rds, 
     seed = 123, n_iter = n_iter, val = vals, name = param_name, contam = "1to80")

# Find all files with the suffix ".rds"
rds_files = dir(pattern = ".rds")

# Read all the RDS files and save them into rds2var
rds2var = map(.x = rds_files, .f = readRDS)
names(rds2var) = rds_files

cleaned_data = map2(.x = rds2var, .y = names(rds2var), .f = clean_rds)
full_data = bind_rows(cleaned_data)

write.csv(x = full_data, file = "sim_data_1to80_5to60_3_10to15.csv")



