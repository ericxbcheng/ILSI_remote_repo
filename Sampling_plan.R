# Create a helper function that gives unique identifers
naming_sp = function(n_sp, x_sp, y_sp, radius){
  
  # Generate a unique identifier for each sample point
  a = rep("sp", times = n_sp)
  b = 1:n_sp %>% as.character()
  ID = paste(a, "-", b, sep = "")
  
  ## Generate a column for sample radius
  r = rep(radius, times = n_sp)
  
  df = data.frame(X = x_sp,
             Y = y_sp,
             ID = ID,
             label = "sample point",
             r = r,
             cont_level = NA,
             dis_level = NA)
  
  df$ID = as.character(df$ID)
  
  return(df)
}

## Calculate the percent of contamination a source contributes to a sample point
calc_perc_contam = function(df_dist, r, LOC, fun){
  
  if(fun == "exp"){
    f_chosen = f_exp
  } else if(fun == "norm"){
    f_chosen = f_norm
  } else {
    stop("Decay function is undefined. Choose 'exp' or 'norm'. ")
  }
  
  f_chosen(x = df_dist[["Distance"]], spread_radius = r, LOC = LOC)
}

# Create a function that calculates the Euclidean distance between points and only outputs the distances between sample points and contamination points. If spotONLY == TRUE, then only calculate the distance between spots and sample points
calc_dist = function(df_contam, df_sp){
  
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
  c = b %>%
    left_join(x = ., y = df_contam[, c("ID", "label")], by = c("ID_contam" = "ID"))
  
  return(c)
}

# Create a function that calculates contamination levels for each sample point and combine "contam_xy" and "sp_xy"
gen_sim_data = function(df_contam, df_sp, dist, spread_radius, sp_radius, LOC, fun, m_kbar, m_sp, conc_good){
  
  ### Calculate the cont_level (continuous spread)
  # Subset the dist_contam_sp to keep the rows that show distance between spots and sample points
  # Calculate the percent contribution based on distance
  # Attach the source contamination level
  # Calculate the contamination level at each sample point, which is source level * percent contribution
  #Sum up the source_contri for each sample point to represent the actual contamination level at that sample point
  
  a = dist %>%
    dplyr::filter(.data = ., label == "spot") %>%
    mutate(perc_contri = calc_perc_contam(df_dist = ., r = spread_radius, LOC = LOC, fun = fun),
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
  
  ### Define sampling error
  c = rlnorm(n = nrow(df_sp), meanlog = 0, sdlog = 1)
  
  ### Combine everything, fill the NAs with the corresponding contamination level.
  df = rbind(df_contam, df_sp)
  
  # Add sampling error 
  df$cont_level[match(x = a$ID_sp, table = df$ID)] = a$cont_level + c
  df$dis_level[match(x = b$ID_sp, table = df$ID)] = b$dis_level + c[length(b$dis_level)]
  
  if(anyNA(df$dis_level)){
    df$dis_level[is.na(df$dis_level)] = conc_good
  }
  
  return(df)
}

# Create a function that calculates the boundaries of each stratum
calc_bounds = function(xlim, ylim, n_strata, by){
  if(by == "row"){
    seq(from = ylim[1], to = ylim[2], by = (ylim[2] - ylim[1])/n_strata)
  } else if(by == "column"){
    seq(from = xlim[1], to = xlim[2], by = (xlim[2] - xlim[1])/n_strata)
  }
}

# Create a function that generates a simple random sampling plan
sim_plan_srs = function(n_sp, xlim, ylim, radius){
  
  ## Generate a data frame that contains the coordinates of the sampling points
  x_sp = runif(n = n_sp, min = xlim[1], max = xlim[2])
  y_sp = runif(n = n_sp, min = ylim[1], max = ylim[2])
  
  naming_sp(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
}

# Create a function that generates a stratified random sampling plan
sim_plan_strs = function(n_sp, n_strata, by, xlim, ylim, radius){
  
  if (n_sp %% n_strata != 0) {
    stop("n_sp is not a multiple of n_strata.")
  } else {
    bounds = calc_bounds(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
    
    if (by == "row") {
      x_sp = runif(n = n_sp, min = xlim[1], max = xlim[2])
      y_sp = runif(n = n_sp, min = bounds[1:length(bounds)-1], max = bounds[2:length(bounds)])
    } else if (by == "column") {
      x_sp = runif(n = n_sp, min = bounds[1:length(bounds)-1], max = bounds[2:length(bounds)])
      y_sp = runif(n = n_sp, min = ylim[1], max = ylim[2])
    }
    
    naming_sp(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
  }
}

# Create a function that generates a systematic sampling plan
sim_plan_ss = function(xlim, ylim, n_sp, radius, by){
  
  # k = sampling interval
  k = xlim[2]*ylim[2]/n_sp
  
  # Randomly select 1 sample point in the first basic unit
  x0 = runif(n = 1, min = 0, max = 1)
  y0 = runif(n = 1, min = 0, max = 1)
  
  if(by == "row"){
    
    # Assume all samples are selected on a 1-dimensional space. Pick a sample at every k steps.
    x_sp_raw = seq(from = x0, by = k, length.out = n_sp)
    y_sp_raw = rep(y0, times = n_sp)
    
    # Calculate how much do sample points exceed the x boundary and determine how many sections should y boundary be divided into.
    exceed_xlim = floor(x_sp_raw / xlim[2])
    n_strata_y = max(exceed_xlim) + 1
    delta_y = ylim[2] / n_strata_y
    
    # Rearrange sample points so that they are within the x and y boundaries
    x_sp = x_sp_raw - xlim[2] * exceed_xlim
    y_sp = y_sp_raw + delta_y * exceed_xlim
    
  } else if(by == "column"){
    
    # Assume all samples are selected on a 1-dimensional space. Pick a sample at every k steps.
    x_sp_raw = rep(x0, times = n_sp)
    y_sp_raw = seq(from = y0, by = k, length.out = n_sp)
    
    # Calculate how much do sample points exceed the y boundary and determine how many sections should x boundary be divided into.
    exceed_ylim = floor(y_sp_raw / ylim[2])
    n_strata_x = max(exceed_ylim) + 1
    delta_x = xlim[2] / n_strata_x
    
    # Rearrange sample points so that they are within the x and y boundaries
    x_sp = x_sp_raw + delta_x * exceed_ylim
    y_sp = y_sp_raw - ylim[2] * exceed_ylim
    
  } else {
    stop("Please select either row or column by which samples are taken every kth step.")
  }

  naming_sp(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
}

# A function that includes all kinds of sampling plan
sim_plan = function(method_sp, n_sp, xlim, ylim, radius, n_strata, by){
  if(method_sp == "srs"){
    sim_plan_srs(n_sp = n_sp, xlim = xlim, ylim = ylim, radius = radius)
  } else if (method_sp == "strs"){
    sim_plan_strs(n_sp = n_sp, n_strata = n_strata, by = by, xlim = xlim, ylim = ylim, radius = radius)
  } else if (method_sp == "ss"){
    sim_plan_ss(xlim = xlim, ylim = ylim, n_sp = n_sp, radius = radius, by = by)
  } else {
    stop("Sampling method does not exist. Try 'srs', 'strs', or 'ss'.")
  }
}

