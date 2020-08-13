# Create intermediate datasets for discrete case
sim_intmed_dis = function(c_hat, lims, spread, covar_mat, n_affected, dis_level, 
                          method_sp, sp_radius, container, compartment, type, L, 
                          rho, m_kbar, conc_neg, tox, seed, homogeneity, n_sp, n_strata, by){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(c_hat = c_hat, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, 
                             lims = lims, spread = spread, n_affected = n_affected, 
                             covar = covar_mat, dis_level = dis_level, seed = seed) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan_new(method_sp = method_sp, spread = spread, lims = lims, radius = sp_radius, 
                       container = container, compartment = compartment, type = type, 
                       n_sp = n_sp, n_strata = n_strata, by = by)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy, spread = spread, method_sp = method_sp)
  
  # Generate combined dataset
  contam_sp_xy = gen_sim_data_new(df_contam = contam_xy, df_sp = sp_xy, dist = dist_contam_sp, 
                                  spread = spread, sp_radius = sp_radius, L = L, rho = rho, 
                                  m_kbar = m_kbar, conc_neg = conc_neg, method_sp = method_sp, lims = lims)
  
  # Create work portion and test portion
  sample_dis = get_sample_dis(data = contam_sp_xy$raw, m_kbar = m_kbar, tox = tox, homogeneity = homogeneity)
  
  return(list(contam_sp_xy = contam_sp_xy, dist = dist_contam_sp, sample = sample_dis))
}

# Create intermediate datasets for continuous case
sim_intmed_cont = function(n_contam, lims, spread, spread_radius, cont_level, 
                           method_sp, n_sp, n_strata, by, LOC, fun, bg_level, geom, seed){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(n_contam = n_contam, lims = lims, spread = spread, 
                             spread_radius = spread_radius, cont_level = cont_level, geom = geom, seed = seed) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan_new(method_sp = method_sp, spread = spread, lims = lims, n_sp = n_sp, 
                       n_strata = n_strata, by = by, radius = NA)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy, spread = spread, method_sp = method_sp)
  
  # Generate combined dataset
  contam_sp_xy = gen_sim_data_new(df_contam = contam_xy, df_sp = sp_xy, dist = dist_contam_sp, 
                                  spread = spread, spread_radius = spread_radius, LOC = LOC, 
                                  fun = fun, bg_level = bg_level, geom = geom)
  
  return(list(contam_sp_xy = contam_sp_xy, dist = dist_contam_sp))
}

# Create intermediate datasets
sim_intmed = function(n_contam, c_hat, lims, spread, covar_mat, n_affected, spread_radius, dis_level, cont_level,
                      method_sp, sp_radius, container, compartment, type, L, rho, m_kbar, conc_neg, tox, 
                      n_sp, n_strata, by, LOC, fun, bg_level, geom, seed, homogeneity){
  
  # Check point
  stopifnot(spread %in% c("discrete", "continuous"))
  
  if(spread == "discrete"){
    
    sim_intmed_dis(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat, 
                   n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, 
                   sp_radius = sp_radius, container = container, compartment = compartment,
                   type = type, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, 
                   tox = tox, seed = seed, homogeneity = homogeneity, n_sp = n_sp, n_strata = n_strata, by = by)
  } else {
    
    sim_intmed_cont(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, 
                    cont_level = cont_level, method_sp = method_sp, n_sp = n_sp, bg_level = bg_level,
                    n_strata = n_strata, by = by, LOC = LOC, fun = fun, geom = geom, seed = seed)
  }
}

# Outcome simulation for continuous case
sim_outcome_cont = function(n_contam, lims, spread, spread_radius, cont_level, bg_level, method_sp, 
                            n_sp, n_strata, by, LOC, fun, case, m, M, m_sp, method_det, geom, seed){
  
  # Create intermediate datasets
  a = sim_intmed(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, 
                 bg_level = bg_level, method_sp = method_sp, n_sp = n_sp, n_strata = n_strata, 
                 by = by, LOC = LOC, fun = fun, cont_level = cont_level,geom = geom, seed = seed)
  
  # Extract intermediate datasets
  contam_sp_xy = a$contam_sp_xy
  dist_contam_sp = a$dist
  
  # Determine which contamination points are detected or which sample points detect contamination
  cover = calc_cover(df_dist = dist_contam_sp, spread_radius = spread_radius, 
                     sp_radius = sp_radius, spread = spread, geom = geom, df_contam_sp = contam_sp_xy)
  
  ## Determine whether the contamination is detected or not. If detected, I_det = 1; if not detected, I_det = 0.
  I_det = {nrow(cover) > 0} %>% as.integer()
  
  ## Determine lot decision
  decision = lot_decision_new(data = contam_sp_xy, case = case, m = m, M = M, m_sp = m_sp, spread = spread, method_det = method_det)
  
  # Output
  list(I_det, decision)
}

# Outcome simulation for discrete case
sim_outcome_dis = function(c_hat, lims, spread, covar_mat, n_affected, dis_level, 
                           method_sp, sp_radius, container, L, rho, m_kbar, conc_neg, 
                           tox, Mc, method_det, verbose = FALSE, seed, homogeneity, n_sp,
                           n_strata, by, compartment, type){
  
  # Create intermediate datasets
  a = sim_intmed(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat, 
                 n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, 
                 sp_radius = sp_radius, container = container, L = L, rho = rho, 
                 m_kbar = m_kbar, conc_neg = conc_neg, tox = tox, seed = seed, 
                 homogeneity = homogeneity, n_sp = n_sp, n_strata = n_strata, 
                 by = by, compartment = compartment, type = type)
  
  # Extract intermediate datasets
  c_true = a$contam_sp_xy$c_true
  test = a$sample$test
  
  ## Determine lot decision
  decision = lot_decision_new(data = test, Mc = Mc, spread = spread, method_det = method_det)
  
  # Create diagnostics option
  if(verbose == FALSE){
    
    return(list(c_true, decision))
    
  } else {
    
    raw = a$contam_sp_xy$raw
    work = a$sample$work
    
    # Calculate the means
    mean_raw = mean(raw)
    mean_work = mean(work)
    mean_test = mean(test)
    
    # output
    return(list(c_true, decision, mean_raw, mean_work, mean_test))
  }
}

# Outcome simulation
sim_outcome_new = function(n_contam, c_hat, lims, spread, spread_radius, method_sp, n_sp, 
                           n_strata, by, LOC, fun, case, m, M, m_sp, method_det, 
                           covar_mat, n_affected, dis_level, cont_level, bg_level, 
                           sp_radius, container, L, rho, m_kbar, conc_neg, tox, 
                           Mc, verbose = FALSE, geom, seed, homogeneity, compartment, type){
  
  if(spread == "discrete"){
    sim_outcome_dis(c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat, 
                    n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, 
                    sp_radius = sp_radius, container = container, L = L, rho = rho, m_kbar = m_kbar, 
                    conc_neg = conc_neg, tox = tox, Mc = Mc, method_det = method_det, verbose = verbose, 
                    seed = seed, homogeneity = homogeneity, n_sp = n_sp, n_strata = n_strata, 
                    by = by, compartment = compartment, type = type)
    
  } else {
    sim_outcome_cont(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, cont_level = cont_level,
                     bg_level = bg_level, method_sp = method_sp, n_sp = n_sp, n_strata = n_strata, by = by, 
                     LOC = LOC, fun = fun, case = case, m = m, M = M, m_sp = m_sp, method_det = method_det, geom = geom, seed = seed)
  }
  
}
