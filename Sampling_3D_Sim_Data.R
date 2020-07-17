source(file = "Sampling_libraries.R")
source(file = "Sampling_contamination.R")
source(file = "Sampling_contamination_3d.R")
source(file = "Sampling_visualization.R")
source(file = "Sampling_assay_prep.R")
source(file = "Sampling_plan.R")
source(file = "Sampling_plan_3d.R")
source(file = "Sampling_assay_3d.R")
source(file = "Sampling_assay.R")
source(file = "Sampling_outcome_3d.R")
source(file = "Sampling_outcome.R")
source(file = "Sampling_iteration.R")
source(file = "Sampling_tuning_3d.R")
source(file = "Sampling_analysis.R")

######## Parameter ############
set.seed(123)
# Pre-generate healthy kernel concentrations to save time
conc_neg = rpert(n = 10^6, min = 0, mode = 0.7, max = 19.99, shape = 80)

## Contamination
c_hat = 5
x_lim = c(0, 1)
y_lim = c(0, 1)
z_lim = c(0, 1)
lims = list(xlim = x_lim, ylim = y_lim, zlim = z_lim)

dis_level = make_dis_level(type = "constant", args = 40000)
# dis_level = make_dis_level(type = "Gamma", args = c(40000, 20))
spread = "discrete"
n_affected = 1
covar_mat = make_covar_mat(spread = spread, varx = 0.0004, 
                           vary = 0.0004, varz = 0.0004, 
                           covxy = 0, covxz = 0, covyz = 0)

# Sampling
method_sp = "srs"
n_sp = 9
n_strata = 9 
by = "column"
d = 0.04
sp_radius = d / 2
L = z_lim[2]
m_kbar = 0.3 
rho = 1.28
homogeneity = 0.1

### SS
container = "truck"
compartment = NA
type = NA

# Assaying
method_det = "ELISA aflatoxin"
tox = "AF"
Mc = 20

# Iteration
n_iter = 100
n_seed = 100

# Parameter tuning
c_hat_vec = c(5, 10, 20, 40, 80, 100)
n_affected_vec = c(1, 10, 100)
n_sp_vec = c(5, 10, 100)
method_sp_vec = c("srs", "strs", "ss")

########################## Experiment 1: SRS/STRS/SS + n_affected(1/10/100/1000) ############
Args_default_Ex1 = list(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat,
                       n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, 
                       sp_radius = sp_radius, n_sp = 9, n_strata = n_strata, by = by, L = L, 
                       rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, tox = tox, Mc = Mc, 
                       method_det = method_det, verbose = FALSE, homogeneity = 0.1, 
                       compartment = compartment, type = type, container = container)


# Generate 3 argument lists
Args_list_Ex1 = list()
Args_list_Ex1 = c(Args_list_Ex1, map(.x = n_affected_vec, .f = update_arg,
                                     param = "n_affected", Args = Args_default_Ex1))

# 2 tuning parameters
for(i in 1:length(Args_list_Ex1)){
  
  sim_data_Ex1 = tune_param_sec(Args = Args_default_Ex1, var_prim = "c_hat", vals_prim = c_hat_vec, 
                              var_sec = "method_sp", vals_sec = method_sp_vec, n_seed = n_seed, n_iter = n_iter)
  
  saveRDS(object = sim_data_Ex1, file = sprintf(fmt = "sim_3D_9_0.1_srsxstrsxss_%d_%dx%d.rds", n_affected_vec[i], n_seed, n_iter))  
}

rm(Args_default_Ex1, Args_list_Ex1, sim_data_Ex1)

###################### Experiment 2: n_sp (5/10/100) + n_affected (1/10/100/1000) #############
Args_default_Ex2 = list(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat,
                        n_affected = n_affected, dis_level = dis_level, method_sp = "srs", 
                        sp_radius = sp_radius, n_sp = n_sp, n_strata = n_strata, by = by, L = L, 
                        rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, tox = tox, Mc = Mc, 
                        method_det = method_det, verbose = FALSE, homogeneity = 0.1, 
                        compartment = compartment, type = type, container = container)

# Generate 3 argument lists
Args_list_Ex2 = list()
Args_list_Ex2 = c(Args_list_Ex2, map(.x = n_affected_vec, .f = update_arg,
                                     param = "n_affected", Args = Args_default_Ex2))

# 2 tuning parameters
for(i in 1:length(Args_list_Ex2)){
  
  sim_data_Ex2 = tune_param_sec(Args = Args_default_Ex2, var_prim = "c_hat", vals_prim = c_hat_vec, 
                              var_sec = "n_sp", vals_sec = n_sp_vec, n_seed = n_seed, n_iter = n_iter)
  
  saveRDS(object = sim_data_Ex2, file = sprintf(fmt = "sim_3D_5x10x100_0.1_srs_%d_%dx%d.rds", n_affected_vec[i], n_seed, n_iter))  
}

rm(Args_default_Ex2, Args_list_Ex2, sim_data_Ex2)
