

# Create a function to idenfity points that falls within a certain distance from another point
## Determine which sample points fall within the spread_radius in the continuous spread case.
calc_cover_cont = function(df_dist, r){
  a = subset(x = df_dist, subset = label == "spot", drop = FALSE)
  b = subset(x = a, subset = Distance <= r)
  return(b)
}

## Determine which spot and spread points fall within the sp_radius in the discrete spread case.
calc_cover_dis = function(df_dist, r){
  a = subset(x = df_dist, subset = Distance <= r)
  return(a)
}

## Wrap up function
calc_cover = function(df_dist, spread_radius, sp_radius, spread){
  
  if(spread == "discrete"){
    calc_cover_dis(df_dist = df_dist, r = sp_radius)
  } else if (spread == "continuous"){
    calc_cover_cont(df_dist = df_dist, r = spread_radius)
  } else {
    stop("Unknown spread type.")
  }
}

# Create a function that calculates the rate of detection
calc_ROD = function(df_cover, n_sp, df_contam, spread){
  if(spread == "continuous"){
    length(unique(df_cover$ID_sp)) / n_sp
  } else if(spread == "discrete"){
    length(unique(df_cover$ID_contam)) / nrow(df_contam)
  }
}

# Create a function that runs the simulation once and gives an ROD
sim_outcome = function(n_contam, xlim, ylim, n_affected, covar_mat, spread_radius, method, n_sp, sp_radius, spread, n_strata, by, cont_level, LOC, fun, m_kbar, m_sp, conc_good, case, m, M, Mc, method_det){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam(n_contam = n_contam, xlim = xlim, ylim = ylim, covariance = covar_mat, n_affected = n_affected, radius = spread_radius, cont_level = cont_level) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan(method_sp = method_sp, n_sp = n_sp, xlim = xlim, ylim = ylim, radius = sp_radius, by = by)
  
  # Generate the distance matrix
  dist_contam_sp = calc_dist(df_contam = contam_xy, df_sp = sp_xy)
  
  # Combine contam_xy and sp_xy
  contam_sp_xy = gen_sim_data(df_contam = contam_xy, df_sp = sp_xy, spread_radius = spread_radius, LOC = LOC, fun = fun, dist = dist_contam_sp, sp_radius = sp_radius, m_kbar = m_kbar, m_sp = m_sp, conc_good = conc_good, cont_level = cont_level)
  
  # Determine which contamination points are detected or which sample points detect contamination
  cover = calc_cover(df_dist = dist_contam_sp, spread_radius = spread_radius, sp_radius = sp_radius, spread = spread)
  
  ## Determine whether the contamination is detected or not. If detected, I_det = 1; if not detected, I_det = 0.
  I_det = {nrow(cover) > 0} %>% as.integer()
  
  ## Calculate ROD per iteration
  ROD = calc_ROD(df_cover = cover, n_sp = n_sp, df_contam = contam_xy, spread = spread)
  
  ## Determine lot decision
  decision = lot_decision(data = contam_sp_xy, case = case, m = m, M = M, Mc = Mc, spread = spread, method_det = method_det)
  
  # Output
  list(I_det, ROD, decision)
}
