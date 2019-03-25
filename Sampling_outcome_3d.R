# Create intermediate datasets for discrete case
sim_intmed_dis = function(n_contam, lims, spread, covar_mat, n_affected, spread_radius, dis_level, 
                          method_sp, d, container, compartment, type, L, rho, m_kbar, conc_neg, tox){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(n_contam = n_contam, lims = lims, spread = spread, covar = covar_mat, 
                             n_affected = n_affected, spread_radius = NA, dis_level = dis_level) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan_new(method_sp = method_sp, spread = spread, lims = lims, radius = d/2, 
                       container = container, compartment = compartment, type = type)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy, spread = spread, method_sp = method_sp)
  
  # Generate combined dataset
  contam_sp_xy = gen_sim_data_new(df_contam = contam_xy, df_sp = sp_xy, dist = dist_contam_sp, 
                                  spread = spread, d = d, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg)
  
  # Create work portion and test portion
  sample_dis = get_sample_dis(data = contam_sp_xy, container = container, m_kbar = m_kbar, tox = tox)
  
  return(list(contam_sp_xy = contam_sp_xy, dist = dist_contam_sp, sample = sample_dis))
}

# Create intermediate datasets for continuous case
sim_intmed_cont = function(n_contam, lims, spread, spread_radius, cont_level, 
                           method_sp, n_sp, n_strata, by, LOC, fun){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(n_contam = n_contam, lims = lims, spread = spread, 
                             spread_radius = spread_radius, cont_level = cont_level) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan_new(method_sp = method_sp, spread = spread, lims = lims, n_sp = n_sp, 
                       n_strata = n_strata, by = by, radius = NA)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy, spread = spread, method_sp = method_sp)
  
  # Generate combined dataset
  contam_sp_xy = gen_sim_data_new(df_contam = contam_xy, df_sp = sp_xy, dist = dist_contam_sp, 
                                  spread = spread, spread_radius = spread_radius, LOC = LOC, 
                                  fun = fun, cont_level = cont_level)
  
  return(list(contam_sp_xy = contam_sp_xy, dist = dist_contam_sp))
}

# Create intermediate datasets
sim_intmed = function(n_contam, lims, spread, covar_mat, n_affected, spread_radius, dis_level,
                      method_sp, d, container, compartment, type, L, rho, m_kbar, conc_neg, tox, 
                      n_sp, sp_radius, n_strata, by, LOC, fun){
  
  # Check point
  stopifnot(spread %in% c("discrete", "continuous"))
  
  if(spread == "discrete"){
    
    sim_intmed_dis(n_contam = n_contam, lims = lims, spread = spread, covar_mat = covar_mat, 
                   n_affected = n_affected, dis_level = dis_level, 
                   method_sp = method_sp, d = d, container = container, compartment = compartment, 
                   type = type, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, tox = tox)
  } else {
    
    sim_intmed_cont(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, 
                    cont_level = cont_level, method_sp = method_sp, n_sp = n_sp, 
                    n_strata = n_strata, by = by, LOC = LOC, fun = fun)
  }
}
