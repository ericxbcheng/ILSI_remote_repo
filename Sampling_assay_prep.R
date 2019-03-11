########### 2D

# Create a function that calculates the Euclidean distance between points and only outputs the distances between sample points and contamination points. If spotONLY == TRUE, then only calculate the distance between spots and sample points
calc_dist_2d = function(df_contam, df_sp, probe = FALSE){
  
  df = rbind(df_contam, df_sp)
  df$label = as.character(df$label)
  
  # Calculate the Euclidean distance
  a = dist(x = df[ ,1:2], method = "euclidean") %>% as.matrix()
  attr(x = a, which = "dimnames") = list(df$ID, df$ID)
  
  sp_ind = which(df$label == "sample point")
  cont_ind = which(df$label %in% c("spot", "spread"))
  
  # Subset the matrix to keep the distances between sample points and contamination points (spot + spread)
  # Gather the matrix into a long format
  b = a[cont_ind, sp_ind, drop = FALSE] %>%
    melt(data = ., varnames = c("ID_contam", "ID_sp"), value.name = "Distance")
  
  b$ID_contam = as.character(b$ID_contam)
  
  # Attach the labels for each contamination point
  # If we are using probes for kernels, we need to have the Z-coordinates
  if(probe == FALSE){
    c = b %>%
      left_join(x = ., y = df_contam[, c("ID", "label")], by = c("ID_contam" = "ID"))
  } else {
    c = b %>%
      left_join(x = ., y = df_contam[, c("Z","ID", "label")], by = c("ID_contam" = "ID"))
  }
  
  return(c)
}

# Create a function that calculates contamination levels for each sample point and combine "contam_xy" and "sp_xy"
gen_sim_data = function(df_contam, df_sp, dist, spread_radius, sp_radius, LOC, fun, m_kbar, m_sp, conc_good, cont_level){
  
  ### Calculate the cont_level (continuous spread)
  # Subset the dist_contam_sp to keep the rows that show distance between spots and sample points
  # Calculate the percent contribution based on distance
  # Attach the source contamination level
  # Calculate the contamination level at each sample point, which is source level * percent contribution
  #Sum up the source_contri for each sample point to represent the actual contamination level at that sample point
  
  a = dist %>%
    dplyr::filter(.data = ., label == "spot") %>%
    mutate(perc_contri = calc_perc_contam(df_dist = ., r = spread_radius, LOC = LOC, fun = fun, cont_level = cont_level),
           source_level = df_contam$cont_level[match(x = .$ID_contam, table = df_contam$ID)],
           source_contri = source_level * perc_contri) %>%
    group_by(ID_sp) %>%
    summarise(cont_level = sum(source_contri))
  
  ### Calculate the dis_level (discrete spread)
  # Estimate the number of kernels in each sample
  
  n_k = round(x = m_sp/m_kbar, digits = 0)
  
  # Subset the dist_contam_sp to keep rows where the contamination points fall within the sampling region
  # Attach the source contamination level
  # Calculate the sum of contamination point levels in each sample, and record the number of points in each sample
  # Calculate the dis_level in each sample
  
  b = dist %>%
    dplyr::filter(Distance <= sp_radius) %>%
    mutate(source_level = df_contam$dis_level[match(x = .$ID_contam, table = df_contam$ID)]) %>%
    group_by(ID_sp) %>%
    summarise(obs = n(),
              sum_level = sum(source_level)) %>%
    mutate(dis_level = m_kbar/m_sp*(sum_level + (n_k - obs) * conc_good))
  
  ### Combine everything, fill the NAs with the corresponding contamination level.
  df = rbind(df_contam, df_sp)
  
  # Update the cont_level column for the sample points.
  df$cont_level[match(x = a$ID_sp, table = df$ID)] = a$cont_level 
  
  # Update the dis_level column for the sample points.
  df$dis_level[match(x = b$ID_sp, table = df$ID)] = b$dis_level
  
  if(anyNA(df$dis_level)){
    df$dis_level[is.na(df$dis_level)] = conc_good
  }
  
  return(df)
}


######################################## 3D #######################################

# Find the length of the probe
get_Lprobe = function(container, lims){
  
  stopifnot(container %in% c("barge", "hopper", "boxcar", "truck", "hopper_bottom"))
  
  # Find all possible probe lengths for a specific container type
  a = switch(EXPR = container,
             "barge" = ft2m(12),
             "hopper" = c(ft2m(10), ft2m(12)),
             "boxcar" = ft2m(6),
             "truck" = c(ft2m(5), ft2m(6)),
             "hopper_bottom" = c(ft2m(6), ft2m(8), ft2m(10)))
  
  # Select one length where the probe can reach the bottom of the corn
  # We assume the probe is fully inserted into the corn
  b = a <= lims$zlim[2]
  c = max(a[b])
  
  return(c)
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

calc_dist = function(df_contam, df_sp, spread, method_sp){
  
  # Check point
  stopifnot(spread %in% c("continuous", "discrete"))
  stopifnot(method_sp %in% c("srs", "strs", "ss"))
  
  if(spread == "continuous"){
    calc_dist_2d(df_contam = df_contam, df_sp = df_sp, probe = FALSE)
  } else if(spread == "discrete"){
    
    # When method_sp == 'ss', we only calculate 2D Euclidean distance as we are using probes
    # When method_sp == 'srs', 'strs', we calculate 3D Euclidean distance as we are using a spherical sampelr
    
    if(method_sp == "ss"){
      calc_dist_2d(df_contam = df_contam, df_sp = df_sp, probe = TRUE)
    } else {
      calc_dist_3d(df_contam = df_contam, df_sp = df_sp, probe = FALSE)
    }
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

# # Calculate sample concentration for discrete case
# calc_level_dis = function(df_contam, sp_radius, dist, m_sp, m_kbar){
#   
#   # Estimate the number of kernels in each sample
#   n_k = round(x = m_sp/m_kbar, digits = 0)
#   
#   # Subset the dist_contam_sp to keep rows where the contamination points fall within the sampling region
#   # Attach the source contamination level
#   # Calculate the sum of contamination point levels in each sample, and record the number of points in each sample
#   # Calculate the dis_level in each sample
#   
#   dist %>%
#     dplyr::filter(Distance <= sp_radius) %>%
#     mutate(source_level = df_contam$dis_level[match(x = .$ID_contam, table = df_contam$ID)]) %>%
#     group_by(ID_sp) %>%
#     summarise(obs = n(),
#               sum_level = sum(source_level)) %>%
#     mutate(dis_level = m_kbar/m_sp*(sum_level + (n_k - obs) * conc_good))
# }

# Define how a kernel is captured
capture_kernel = function(method_sp, df_contam, dist, sp_radius, d, lims, L){
  
  # Check points
  stopifnot(lims$zlim[2] >= L)
  stopifnot(method_sp %in% c("srs", "strs", "ss"))
  
  # When method_sp == ss, we use a probe
    ## We assume zlim[2] >= L
  # Otherwise, we use a spherical sampler
  if(method_sp == "ss"){
    
    dist %>%
      dplyr::filter(Distance <= d / 2 & Z >= lims$zlim[2] - L)  %>%
      mutate(source_level = df_contam$dis_level[match(x = .$ID_contam, table = df_contam$ID)])
    
  } else {
    dist %>%
      dplyr::filter(Distance <= sp_radius)  %>%
      mutate(source_level = df_contam$dis_level[match(x = .$ID_contam, table = df_contam$ID)])
    
  }
}

# Calculate the number of kernels in a sampler
calc_k_num = function(method_sp, d, L, rho, m_kbar, sp_radius){
  
  # Check point
  stopifnot(method_sp %in% c("srs", "strs", "ss"))
  
  if(method_sp == "ss"){
    # Estimate the number of kernels in each probe
    ## V = pi * r^2 * L
    ## m = rho * V, remember rho's unit = g/cm3, and V's unit is m3
    V_probe = pi * (d/2) ^ 2 * L
    m_probe = rho * V_probe * 10 ^ 6
    n_k = round(x = m_probe/m_kbar, digits = 0)
    
  } else {
    
    # Estimate the number of kernels in a sphere
    ## V = 4/3 * pi * r^3
    ## m = rho * V, remember rho's unit = g/cm3, and V's unit is m3
    V_probe = 4/3 * pi * sp_radius ^ 3
    m_probe = rho * V_probe * 10 ^ 6
    n_k = round(x = m_probe/m_kbar, digits = 0)
  }
  
  return(n_k)
}

# Generate negative kernel concentrations
gen_conc_neg = function(n, conc_neg){
  
  # Check point
  stopifnot(n >= 0)
  
  if(is.null(conc_neg) == FALSE){
    
    # When conc_neg is longer than n, vector a either takes a subset of values from conc_neg
    # Otherwise, vector a = conc_neg + values sampled from conc_neg with replacement so that the total length of a = n
    if(n <= length(conc_neg)){
      a = conc_neg[1:n]
      
    } else {
      a = c(conc_neg, sample(x = conc_neg, size = n - length(conc_neg), replace = TRUE))
      
    }
  } else {
    
    warning("c_neg is not provided. Random numbers will be generated on the fly.")
    
    a = rpert(n = n, min = 0, mode = 0.7, max = 19.99, shape = 80)
  }
  
  return(a)
}

# Form a pooled sample
get_pooled_sample = function(df_contam, df_sp, dist, method_sp, d, L, rho, m_kbar, sp_radius, conc_neg){
  
  # Find the number of kernels for each sampler
  n_k = calc_k_num(method_sp = method_sp, d = d, L = L, rho = rho, m_kbar = m_kbar, sp_radius = sp_radius)
  
  # Find captured kernels
  kcap = capture_kernel(method_sp = method_sp, df_contam = df_contam, dist = dist, sp_radius = sp_radius,d  = d, lims = lims, L = L)
  
  # Find the number of healthy kernels in total
  num_neg = n_k * nrow(df_sp) - nrow(kcap)
  
  c_pos = kcap[["source_level"]]
  c_neg = gen_conc_neg(n = num_neg, conc_neg = conc_neg)
  
  c_pooled = c(c_pos, c_neg)
  
  return(list(kcap, c_pooled))
}

# Create a function that calculates contamination levels for each sample point and combine "contam_xy" and "sp_xy"
gen_sim_data_new = function(df_contam, df_sp, dist, spread, spread_radius, LOC, fun, cont_level, d, L, rho, m_kbar, sp_radius, conc_neg){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
  ### Combine everything, fill the NAs with the corresponding contamination level.
  df = rbind(df_contam, df_sp)
  
  if(spread == "continuous"){
    
    # Calculate the sample concentration in a continuous case
    a = calc_level_cont(df_contam = df_contam, dist = dist, spread_radius = spread_radius, 
                        LOC = LOC, fun = fun, cont_level = cont_level)
    
    # Update the cont_level column for the sample points.
    df$cont_level[match(x = a$ID_sp, table = df$ID)] = a$cont_level 
    
    return(df)
    
  } else if (spread == "discrete") {
    
    b = get_pooled_sample(df_contam = df_contam, df_sp = df_sp, dist = dist, 
                          method_sp = method_sp, d = d, L = L, rho = rho, 
                          m_kbar = m_kbar, sp_radius = sp_radius, conc_neg = conc_neg)
    
    return(list(combined = df, samples = b)) 
  }
}

