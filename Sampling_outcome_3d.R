# Create intermediate datasets for discrete case
sim_intmed_dis = function(n_contam, lims, spread, covar_mat, n_affected, dis_level, 
                          method_sp, sp_radius, container, compartment, type, L, rho, m_kbar, conc_neg, tox){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(n_contam = n_contam, lims = lims, spread = spread, covar = covar_mat, 
                             n_affected = n_affected, spread_radius = NA, dis_level = dis_level) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan_new(method_sp = method_sp, spread = spread, lims = lims, radius = sp_radius, 
                       container = container, compartment = compartment, type = type)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy, spread = spread, method_sp = method_sp)
  
  # Generate combined dataset
  contam_sp_xy = gen_sim_data_new(df_contam = contam_xy, df_sp = sp_xy, dist = dist_contam_sp, 
                                  spread = spread, sp_radius = sp_radius, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg)
  
  # Create work portion and test portion
  sample_dis = get_sample_dis(data = contam_sp_xy$raw$c_pooled, container = container, m_kbar = m_kbar, tox = tox)
  
  return(list(contam_sp_xy = contam_sp_xy, dist = dist_contam_sp, sample = sample_dis))
}

# Create intermediate datasets for continuous case
sim_intmed_cont = function(n_contam, lims, spread, spread_radius, cont_level, 
                           method_sp, n_sp, n_strata, by, LOC, fun, bg_level, geom){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam_new(n_contam = n_contam, lims = lims, spread = spread, 
                             spread_radius = spread_radius, cont_level = cont_level, geom = geom) 
  
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
sim_intmed = function(n_contam, lims, spread, covar_mat, n_affected, spread_radius, dis_level, cont_level,
                      method_sp, sp_radius, container, compartment, type, L, rho, m_kbar, conc_neg, tox, 
                      n_sp, n_strata, by, LOC, fun, bg_level, geom){
  
  # Check point
  stopifnot(spread %in% c("discrete", "continuous"))
  
  if(spread == "discrete"){
    
    sim_intmed_dis(n_contam = n_contam, lims = lims, spread = spread, covar_mat = covar_mat, 
                   n_affected = n_affected, dis_level = dis_level, 
                   method_sp = method_sp, sp_radius = sp_radius, container = container, compartment = compartment, 
                   type = type, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, tox = tox)
  } else {
    
    sim_intmed_cont(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, 
                    cont_level = cont_level, method_sp = method_sp, n_sp = n_sp, bg_level = bg_level,
                    n_strata = n_strata, by = by, LOC = LOC, fun = fun, geom)
  }
}

# Outcome simulation for continuous case
sim_outcome_cont = function(n_contam, lims, spread, spread_radius, cont_level, bg_level, method_sp, 
                            n_sp, n_strata, by, LOC, fun, case, m, M, m_sp, method_det, geom){
  
  # Create intermediate datasets
  a = sim_intmed(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, bg_level = bg_level,
                 method_sp = method_sp, n_sp = n_sp, n_strata = n_strata, by = by, LOC = LOC, fun = fun, cont_level = cont_level,geom = geom)
  
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
sim_outcome_dis = function(n_contam, lims, spread, covar_mat, n_affected, dis_level, 
                           method_sp, sp_radius, container, L, rho, m_kbar, conc_neg, tox, Mc, method_det, diag){
  
  # Create intermediate datasets
  a = sim_intmed(n_contam = n_contam, lims = lims, spread = spread, covar_mat = covar_mat, 
                 n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, sp_radius = sp_radius, 
                 container = container, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, tox = tox)
  
  # Extract intermediate datasets
  raw = a$contam_sp_xy$raw$c_pooled
  c_true = a$contam_sp_xy$c_true
  work = a$sample$work
  test = a$sample$test
  
  ## Determine lot decision
  decision = lot_decision_new(data = test, Mc = Mc, spread = spread, method_det = method_det)
  
  # Create diagnostics option
  if(diag == FALSE){
    
    # Output
    list(c_true, decision)
    
  } else {
    
    # Calculate the means
    mean_raw = mean(raw)
    mean_work = mean(work)
    mean_test = mean(test)
    
    # output
    list(c_true, decision, mean_raw, mean_work, mean_test)
  }
}

# Outcome simulation
sim_outcome_new = function(n_contam, lims, spread, spread_radius, method_sp, n_sp, n_strata, 
                       by, LOC, fun, case, m, M, m_sp, method_det, covar_mat, n_affected, dis_level, cont_level,
                       bg_level, sp_radius, container, L, rho, m_kbar, conc_neg, tox, Mc, diag, geom){
  
  if(spread == "discrete"){
    sim_outcome_dis(n_contam = n_contam, lims = lims, spread = spread, covar_mat = covar_mat, 
                    n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, 
                    sp_radius = sp_radius, container = container, L = L, rho = rho, m_kbar = m_kbar, 
                    conc_neg = conc_neg, tox = tox, Mc = Mc, method_det = method_det, diag = diag)
    
  } else {
    sim_outcome_cont(n_contam = n_contam, lims = lims, spread = spread, spread_radius = spread_radius, cont_level = cont_level,
                     bg_level = bg_level, method_sp = method_sp, n_sp = n_sp, n_strata = n_strata, by = by, 
                     LOC = LOC, fun = fun, case = case, m = m, M = M, m_sp = m_sp, method_det = method_det, geom = geom)
  }
  
}
