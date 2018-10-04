# Create a helper function that gives unique identifers
naming_sp = function(n_sp, x_sp, y_sp, radius){
  
  # Generate a unique identifier for each sample point
  a = rep("sp", times = n_sp)
  b = 1:n_sp %>% as.character()
  ID = paste(a, "-", b, sep = "")
  
  ## Generate a column for sample radius
  r = rep(radius, times = n_sp)
  
  data.frame(X = x_sp,
             Y = y_sp,
             ID = ID,
             label = "sample point",
             r = r)
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

# Create a function that calculates the Euclidean distance between points and only outputs the distances between sample points and contamination points.
calc_dist = function(df){
  
  a = dist(x = df[ ,1:2], method = "euclidean") %>% as.matrix()
  
  sp_ind = which(df$label == "sample point")
  
  b = a[-sp_ind, sp_ind] %>%
    melt(data = ., varnames = c("row_contam", "row_sp"), value.name = "Distance")
  
  return(b)
}

# Create a function that calculates contamination levels for each sample point and combine "contam_xy" and "sp_xy"
gen_sim_data = function(df_contam, df_sp, spread_radius, LOC, fun){
  
  ## Add the "cont_level" column to df_sp
  df_sp[["cont_level"]] = NA
  
  ## Generate the combined coordinates of contamination and sample points
  a = rbind(df_contam, df_sp)
  rownames(a) = NULL
  
  ## Calculate the Euclidean distance between sample points and contamination points.
  dist_contam_sp = calc_dist(a)
  
  ## Create a column that describes the contamination contribution of the source. Then match the row_contam with rows in contam_xy and show the corresponding cont_level. Calculate contamination contribution.
  b = dist_contam_sp %>%
    mutate(perc_contam = calc_perc_contam(df_dist = ., r = spread_radius, LOC = LOC, fun = fun)) %>%
    mutate(source_level = contam_xy$cont_level[.$row_contam],
           source_contri = source_level * perc_contam)
  
  ## Sum up the source_contri for each sample point to represent the contamination level at that sample point
  c = b %>%
    group_by(row_sp) %>%
    summarise(cont_level = sum(source_contri))
  
  ## Update the contamination level for each sample point
  a$cont_level[c$row_sp] = c$cont_level
  
  return(a)
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
sim_plan = function(method, n_sp, xlim, ylim, radius, n_strata, by){
  if(method == "srs"){
    sim_plan_srs(n_sp = n_sp, xlim = x_lim, ylim = y_lim, radius = radius)
  } else if (method == "strs"){
    sim_plan_strs(n_sp = n_sp, n_strata = n_strata, by = by, xlim = x_lim, ylim = y_lim, radius = radius)
  } else if (method == "ss"){
    sim_plan_ss(xlim = xlim, ylim = ylim, n_sp = n_sp, radius = radius, by = by)
  } else {
    stop("Sampling method does not exist. Try 'srs', 'strs', or 'ss'.")
  }
}