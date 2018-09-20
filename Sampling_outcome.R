# Create a function that calculates the Euclidean distance between points and only outputs the distances between sample points and contamination points.
calc_dist = function(df){
  
  a = dist(x = df[ ,1:2], method = "euclidean") %>% as.matrix()
  
  sp_ind = which(df$label == "sample point")
  
  b = a[-sp_ind, sp_ind] %>%
    melt(data = ., varnames = c("row_contam", "row_sp"), value.name = "Distance")
  
  return(b)
}

# Create a function to idenfity points that falls within a certain distance from another point
cover = function(df_dist, df_coord, r, spread){
  
  ## Find the points that meet the distance criterion
  a = df_dist %>%
    filter(Distance <= r)
  
  ## Create columns that identify each observation
  contam_ID = df_coord$ID[a$row_contam]
  
  b = substr(x = contam_ID, start = 3, stop = 3)
  contam_type = ifelse(test = b == "0", yes = "spot", no = "spread")
  
  sp_ID = df_coord$ID[a$row_sp]
  
  c = cbind(a, contam_ID, sp_ID, contam_type)
  
  ## Create output based on the type of spread.
  if(spread == "discrete") {
    d = c %>%
      arrange(.data = ., contam_ID)
  } else if (spread == "continuous") {
    d = c %>%
      dplyr::filter(contam_type == "spot") %>%
      arrange(.data = ., contam_ID)
  }
  
  return(d)
}

# Create a function that calculates the rate of detection
calc_ROD = function(df_cover, df_contam, n_sp, spread){
  if(spread == "discrete"){
    length(unique(df_cover$contam_ID)) / nrow(df_contam)
  } else if (spread == "continuous"){
    length(unique(df_cover$sp_ID)) / n_sp
  }
}

# Create a function that runs the simulation once and gives an ROD
sim_outcome = function(n_contam, xlim, ylim, n_affected, covar_mat, spread_radius, n_sp, sp_radius, spread){
  
  # Generate the coordinates of contamination points
  contam_xy = sim_contam(n_contam = n_contam, xlim = xlim, ylim = ylim, covariance = covar_mat, n_affected = n_affected, radius = spread_radius) 
  
  # Generate the coordinates of sample points
  sp_xy = sim_plan(n_sp = n_sp, xlim = xlim, ylim = ylim, r = sp_radius)
  
  # Generate the combined coordinates of contamination and sample points
  contam_sp_xy = rbind(contam_xy, sp_xy)
  rownames(contam_sp_xy) = NULL
  
  # Calculate the distance between sample points and contamination points
  dist_contam_sp = calc_dist(contam_sp_xy)
  
  # Determine whether contamination is detected and calculate ROD
  if(spread == "discrete"){
    
    cover_dis = cover(df_dist = dist_contam_sp, df_coord = contam_sp_xy, r = sp_radius, spread = spread)
    calc_ROD(df_cover = cover_dis, df_contam = contam_xy, n_sp = n_sp, spread = spread)
    
  } else if (spread == "continuous"){
    
    cover_cont = cover(df_dist = dist_contam_sp, df_coord = contam_sp_xy, r = spread_radius, spread = spread)
    calc_ROD(df_cover = cover_cont, df_contam = contam_xy, n_sp = n_sp, spread = spread)
    
  }
}