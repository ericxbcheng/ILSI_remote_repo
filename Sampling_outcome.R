

# Create a function to idenfity points that falls within a certain distance from another point
cover = function(df_dist, df_coord, r, spread){
  
  ## Find the points that meet the distance criterion
  a = df_dist %>%
    filter(Distance <= r)
  
  ## Match the points with corresponding metadata
  b = df_coord[a$row_contam, c("X", "Y","ID", "label", "cont_level"), drop = FALSE]
  
  sp_ID = df_coord$ID[a$row_sp]
  
  sp_cont_level = df_coord$cont_level[a$row_sp]
  
  c = cbind(a, b, sp_ID, sp_cont_level)
  
  ## Create output based on the type of spread.
  if(spread == "discrete") {
    d = c %>%
      arrange(.data = ., ID)
  } else if (spread == "continuous") {
    d = c %>%
      dplyr::filter(label == "spot") %>%
      arrange(.data = ., ID)
  }
  
  return(d)
}

# Create a function that calculates the rate of detection
calc_ROD = function(df_cover, df_contam, n_sp, spread){
  if(spread == "discrete"){
    length(unique(df_cover$ID)) / nrow(df_contam)
  } else if (spread == "continuous"){
    length(unique(df_cover$sp_ID)) / n_sp
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