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
source(file = "Validation.R")
library(lme4)

# Pre-generate healthy kernel concentrations to save time
set.seed(123)
conc_neg = rpert(n = 10^6, min = 0, mode = 0.7, max = 19.99, shape = 80)

## Contamination
m_kbar = 0.3 
rho = 1.28
cube_side = get_cube_side(pound = 100, rho = rho)
x_lim = c(0, cube_side)
y_lim = c(0, cube_side)
z_lim = c(0, cube_side)
lims = list(xlim = x_lim, ylim = y_lim, zlim = z_lim)

c_hat = 10
dis_level = list(type = "constant", args = 40000)
spread = "discrete"
n_affected = 0
covar_mat = make_covar_mat(spread = spread, varx = 0.0009, 
                           vary = 0.0009, varz = 0.0009, 
                           covxy = 0, covxz = 0, covyz = 0)

# Subsampling scheme
n = 32
n_sub = 2
homogeneity = expr(runif(n = 1, min = 0, max = 1))
m_sp = 50
unbalanced = FALSE
Mc = 20

n_seed = 100
n_iter = 100

c_hat_vec = c(5.8, 6.4, 6.7, 8.6, 11.8, 15.9, 18.2, 
              25.6, 27.3, 32.9, 56.7, 57.1, 94.7,
              95.6, 113.8, 276.9, 298.9, 676.6)

# Default arg list
ArgList_default = list(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat,
                       n_affected = n_affected, dis_level = dis_level, n = n, 
                       rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, homogeneity = homogeneity, 
                       m_sp = m_sp, n_sub = n_sub, unbalanced = unbalanced, Mc = Mc)

for(i in 1:length(c_hat_vec)){

  result = tune_param_val(Args = ArgList_default, n_seed = n_seed, n_iter = n_iter, param = "c_hat", val = c_hat_vec[i])
  saveRDS(object = result, file = paste0("val_100lb_32x2_", c_hat_vec[i], "_100x100.rds"))

}

