# Create a helper function that gives unique identifers (3D)
naming_sp_3d = function(n_sp, x_sp, y_sp, z_sp, radius){
  
  # Generate a unique identifier for each sample point
  a = rep("sp", times = n_sp)
  b = 1:n_sp %>% as.character()
  ID = paste(a, "-", b, sep = "")
  
  ## Generate a column for sample radius
  r = rep(radius, times = n_sp)
  
  df = data.frame(X = x_sp,
                  Y = y_sp,
                  Z = z_sp,
                  ID = ID,
                  label = "sample point",
                  r = r,
                  cont_level = NA,
                  dis_level = NA)
  
  df$ID = as.character(df$ID)
  
  return(df)
}

# 3D: simple random sampling
sim_plan_srs_3d = function(n_sp, lims, radius){
  
  # Check point: make sure the dimension is 3
  stopifnot(length(lims) == 3)
  
  ## Generate a data frame that contains the coordinates of the sampling points
  x_sp = runif(n = n_sp, min = lims$xlim[1], max = lims$xlim[2])
  y_sp = runif(n = n_sp, min = lims$ylim[1], max = lims$ylim[2])
  z_sp = runif(n = n_sp, min = lims$zlim[1], max = lims$zlim[2])
  
  naming_sp_3d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}


# Create a function that calculates the boundaries of each stratum on z-axis
calc_bounds_3d = function(lims, n_strata){
  seq(from = lims$zlim[1], to = lims$zlim[2], by = (lims$zlim[2] - lims$zlim[1])/n_strata)
}

# 3D: Create a function that generates a stratified random sampling plan
sim_plan_strs_3d = function(n_sp, n_strata, lims, radius){
  
  # Check points
  stopifnot(length(lims) == 3)
  
  if (n_sp %% n_strata != 0) {
    stop("n_sp is not a multiple of n_strata.")
  } else {
    bounds = calc_bounds_3d(lims = lims, n_strata = n_strata)
    
    x_sp = runif(n = n_sp, min = lims$xlim[1], max = lims$xlim[2])
    y_sp = runif(n = n_sp, min = lims$ylim[1], max = lims$ylim[2])
    z_sp = runif(n = n_sp, min = bounds[1:length(bounds) - 1], max = bounds[2:length(bounds)])
  }
  
  naming_sp_3d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# systematic sampling for 3D
sim_plan_ss_3d = function(container, lims, depth_ft, radius, compartment, type){
  
  # Check points
  stopifnot(length(lims) == 3)
  stopifnot(container %in% c("truck", "barge","hopper"))
  
  switch(container, 
         "truck" = ss_truck(lims = lims, depth_ft = depth_ft, radius = radius),
         "barge" = ss_barge(lims = lims, radius = radius),
         "hopper" = ss_hopper(lims = lims, compartment = compartment, type = type, radius = radius))
}

sim_plan_3d = function(method_sp, n_sp, lims, radius, container, compartment, type, depth_ft){
  
  # Check points
  stopifnot(method_sp %in% c("srs", "strs", "ss"))
  
  if(method_sp == "srs"){
    sim_plan_srs_3d(n_sp = n_sp, lims = lims, radius = radius)
  } else if (method_sp == "strs"){
    sim_plan_strs_3d(n_sp = n_sp, n_strata = n_strata, lims = lims, radius = radius)
  } else if (method_sp == "ss"){
    sim_plan_ss_3d(container = container, lims = lims, depth_ft = depth_ft, radius = radius, compartment = compartment, type = type)
  } else {
    stop("Sampling method does not exist. Try 'srs', 'strs', or 'ss'.")
  }
}

# A function that includes all kinds of sampling plan
sim_plan_new = function(method_sp, spread, n_sp, lims, radius, n_strata, by, compartment, type, depth_ft, container){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
  if(spread == "continuous"){
    sim_plan_2d(method_sp = method_sp, n_sp = n_sp, xlim = lims$xlim, ylim = lims$ylim, radius = radius, n_strata = n_strata, by = by)
  } else if(spread == "discrete"){
    sim_plan_3d(method_sp = method_sp, n_sp, lims = lims, radius = radius, container = container, compartment = compartment, type = type, depth_ft = depth_ft)
  }
}

# Convert foot to meter
ft2m = function(x){
  0.3048*x
}

# Probe sampling pattern for trucks
ss_truck = function(lims, depth_ft, radius){
  
  # Check point
  ## depth should be between 0 and zlim
  stopifnot(depth_ft >=0 & ft2m(depth_ft) <= lims$zlim)
  
  if(depth_ft >= 4){
    x_sp = c(ft2m(2), lims$xlim[2] / 4, 3/4 * lims$xlim[2]/2, lims$xlim[2]/2, 5/8*lims$xlim[2], 3/4*lims$xlim[2], lims$xlim[2] - ft2m(2))
    y_sp = c(lims$ylim[2] - ft2m(2), ft2m(2), lims$ylim[2] - ft2m(2), lims$ylim[2]/2, ft2m(2), lims$ylim[2] - ft2m(2), ft2m(2))
    z_sp = 0
    
  } else {
    x_sp = c(ft2m(2), ft2m(2), 3/4*lims$xlim[2]/2, 3/4*lims$xlim[2]/2, lims$xlim[2]/2, 
             5/8*lims$xlim[2], 5/8*lims$xlim[2], lims$xlim[2] - ft2m(2), lims$xlim[2] - ft2m(2))
    y_sp = c(lims$ylim[2] - ft2m(2), ft2m(2), lims$ylim[2] - ft2m(2), ft2m(2), lims$ylim[2]/2, 
             lims$ylim[2] - ft2m(2), ft2m(2), lims$ylim[2] - ft2m(2), ft2m(2))
    z_sp = 0
    
  }
  
  # Check point: Make sure the length of x_sp and y_sp are equal
  stopifnot(length(x_sp) == length(y_sp))
  
  naming_sp_3d(n_sp = length(x_sp), x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# Probe sampling pattern for barges
ss_barge = function(lims, radius){
  
  # Calculate the maximum number of sample points in a row
  ## ft2m(4) + ft2m(15) * (n_max - 1) <= xlim[2] - ft2m(3)
  ## ft2m(3) allows some wiggle room at the end of the barge
  n_max = floor((lims$xlim[2] - ft2m(4) - ft2m(3))/ft2m(15) + 1)
  
  # List all the coordinates
  x_coord = seq(from = ft2m(4), by = ft2m(15), length.out = n_max)
  y_coord = c(ft2m(7), lims$ylim[2] - ft2m(7))
  
  x_sp = rep(x_coord, times = 2)
  y_sp = rep(y_coord, each = n_max)
  z_sp = 0
  
  # Check point: Make sure the length of x_sp and y_sp are equal
  stopifnot(length(x_sp) == length(y_sp))
  
  naming_sp_3d(n_sp = length(x_sp), x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# Probe sampling pattern for hopper cars
ss_hopper = function(lims, compartment, type, radius){
  
  # Check points
  stopifnot(compartment %in% c(2, 3, 4))
  stopifnot(type %in% c("trough", "open_top", "8_hatch", "10_hatch", "12_hatch"))
  
  if(compartment == 3 & type == "trough"){
    ss_hopper_3_trough(lims = lims, radius = radius)
  } else if(compartment == 2 & type == "open_top"){
    ss_hopper_2_open(lims = lims, radius = radius)
  } else {
    stop("Undefined combination of compartment and type (yet).")
  }
  
}

# Probe sampling pattern for 3-compartment trough type hopper car
ss_hopper_3_trough = function(lims, radius){
  
  # 0.5 is for shifting the sample point
  x_sp = c(lims$xlim[2]/4 - 0.5, lims$xlim[2]/2, 3/4*lims$xlim[2] + 0.5)
  y_sp = lims$ylim[2]/2
  z_sp = 0
  
  naming_sp_3d(n_sp = length(x_sp), x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# Probe sampling pattern for 2-compartment open top type hopper car
ss_hopper_2_open = function(lims, radius){
  
  x_sp = c(1/16*lims$xlim[2], 7/16*lims$xlim[2], 9/16*lims$xlim[2],15/16*lims$xlim[2])
  y_sp = c(ft2m(4), lims$ylim[2] - ft2m(4), lims$ylim[2] - ft2m(4), ft2m(4))
  z_sp = 0
  
  # Check point: Make sure the length of x_sp and y_sp are equal
  stopifnot(length(x_sp) == length(y_sp))
  
  naming_sp_3d(n_sp = length(x_sp), x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# 3D: Create a function that calculates the Euclidean distance between points and only outputs the distances between sample points and contamination points. If spotONLY == TRUE, then only calculate the distance between spots and sample points
calc_dist_3d = function(df_contam, df_sp){
  
  df = rbind(df_contam, df_sp)
  df$label = as.character(df$label)
  
  # Calculate the Euclidean distance
  a = dist(x = df[ ,1:3], method = "euclidean") %>% as.matrix()
  attr(x = a, which = "dimnames") = list(df$ID, df$ID)
  
  sp_ind = which(df$label == "sample point")
  cont_ind = which(df$label %in% c("spot", "spread"))
  
  # Subset the matrix to keep the distances between sample points and contamination points (spot + spread)
  # Gather the matrix into a long format
  b = a[cont_ind, sp_ind, drop = FALSE] %>%
    melt(data = ., varnames = c("ID_contam", "ID_sp"), value.name = "Distance")
  
  b$ID_contam = as.character(b$ID_contam)
  
  # Attach the labels for each contamination point
  c = b %>%
    left_join(x = ., y = df_contam[, c("ID", "label")], by = c("ID_contam" = "ID"))
  
  return(c)
}

calc_dist = function(df_contam, df_sp, spread){
  
  # Check point
  stopifnot(spread %in% c("continuous", "discrete"))
  
  if(spread == "continuous"){
    calc_dist_2d(df_contam = df_contam, df_sp = df_sp)
  } else if(spread == "discrete"){
    calc_dist_3d(df_contam = df_contam, df_sp = df_sp)
  }
}

# Calculate sample concentration for continuous case
calc_level_cont = function(df_contam, dist, spread_radius, LOC, fun, cont_level){
  
  if(length(levels(df_contam$label)) > 1){
    warning("df_contam contains both contamination spots and spreads. 
            For continuous spread, contamination spread points should not exist. 
            Check 'spread' in upstream functions.")
  }
  
  # Subset the dist_contam_sp to keep the rows that show distance between spots and sample points
  # Calculate the percent contribution based on distance
  # Attach the source contamination level
  # Calculate the contamination level at each sample point, which is source level * percent contribution
  #Sum up the source_contri for each sample point to represent the actual contamination level at that sample point
  
  dist %>%
    dplyr::filter(.data = ., label == "spot") %>%
    mutate(perc_contri = calc_perc_contam(df_dist = ., r = spread_radius, LOC = LOC, fun = fun, cont_level = cont_level),
           source_level = df_contam$cont_level[match(x = .$ID_contam, table = df_contam$ID)],
           source_contri = source_level * perc_contri) %>%
    group_by(ID_sp) %>%
    summarise(cont_level = sum(source_contri))
  }

# Calculate sample concentration for discrete case
calc_level_dis = function(df_contam, sp_radius, dist, m_sp, m_kbar){
  
  # Estimate the number of kernels in each sample
  n_k = round(x = m_sp/m_kbar, digits = 0)
  
  # Subset the dist_contam_sp to keep rows where the contamination points fall within the sampling region
  # Attach the source contamination level
  # Calculate the sum of contamination point levels in each sample, and record the number of points in each sample
  # Calculate the dis_level in each sample
  
  dist %>%
    dplyr::filter(Distance <= sp_radius) %>%
    mutate(source_level = df_contam$dis_level[match(x = .$ID_contam, table = df_contam$ID)]) %>%
    group_by(ID_sp) %>%
    summarise(obs = n(),
              sum_level = sum(source_level)) %>%
    mutate(dis_level = m_kbar/m_sp*(sum_level + (n_k - obs) * conc_good))
}

# Create a function that calculates contamination levels for each sample point and combine "contam_xy" and "sp_xy"
gen_sim_data_new = function(df_contam, df_sp, dist, spread, spread_radius, sp_radius, LOC, fun, m_kbar, m_sp, conc_good, cont_level){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
  ### Combine everything, fill the NAs with the corresponding contamination level.
  df = rbind(df_contam, df_sp)
  
  if(spread == "continuous"){
    
    # Calculate the sample concentration in a continuous case
    a = calc_level_cont(df_contam = df_contam, dist = dist, spread_radius = spread_radius, LOC = LOC, fun = fun, cont_level = cont_level)
    
    # Update the cont_level column for the sample points.
    df$cont_level[match(x = a$ID_sp, table = df$ID)] = a$cont_level 
    
  } else if (spread == "discrete") {
    
    # Calculate the sample concentration in a discrete case
    b = calc_level_dis(df_contam = df_contam, sp_radius = sp_radius, dist = dist, m_sp = m_sp, m_kbar = m_kbar)
    
    # Update the dis_level column for the sample points.
    df$dis_level[match(x = b$ID_sp, table = df$ID)] = b$dis_level
    
    # NA's in dis_level indicate there is no contaminated kernel in those samples. 
    # Assign conc_good to those samples
    if(anyNA(df$dis_level)){
      df$dis_level[is.na(df$dis_level)] = conc_good
    }
  }
  
  return(df)
}

