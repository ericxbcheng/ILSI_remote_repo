

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

# Create a function that calculates the probability of detection
calc_POD = function(df_cover, n_sp, df_contam, spread){
  if(spread == "continuous"){
    length(unique(df_cover$ID_sp)) / n_sp
  } else if(spread == "discrete"){
    length(unique(df_cover$ID_contam)) / nrow(df_contam)
  }
}

# Create a function that runs the simulation once and gives an ROD
sim_outcome = function(n_contam, xlim, ylim, n_affected, covar_mat, spread_radius, method, n_sp, sp_radius, spread, n_strata, by, cont_level, LOC, fun){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam(n_contam = n_contam, xlim = xlim, ylim = ylim, covariance = covar_mat, n_affected = n_affected, radius = spread_radius, cont_level = cont_level) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan(method = method, n_sp = n_sp, xlim = xlim, ylim = ylim, radius = sp_radius, n_strata = n_strata, by = by)
  
  # Generate the combined coordinates of contamination and sample points
  contam_sp_xy = gen_sim_data(df_contam = contam_xy, df_sp = sp_xy, spread_radius = spread_radius, LOC = LOC, fun = fun)
  
  # Calculate the distance between sample points and contamination points
  dist_contam_sp = calc_dist(df = contam_sp_xy, spotONLY = FALSE)
  
  # Determine whether contamination is detected and calculate ROD
  if(spread == "discrete"){
    
    cover_dis = cover(df_dist = dist_contam_sp, df_coord = contam_sp_xy, r = sp_radius, spread = spread)
    calc_ROD(df_cover = cover_dis, df_contam = contam_xy, n_sp = n_sp, spread = spread)
    
  } else if (spread == "continuous"){
    
    cover_cont = cover(df_dist = dist_contam_sp, df_coord = contam_sp_xy, r = spread_radius, spread = spread)
    calc_ROD(df_cover = cover_cont, df_contam = contam_xy, n_sp = n_sp, spread = spread)
    
  }
}