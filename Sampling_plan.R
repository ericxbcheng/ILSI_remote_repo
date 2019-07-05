# Create a helper function that gives unique identifers
naming_sp_2d = function(n_sp, x_sp, y_sp, radius){
  
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
             cont_level = NaN,
             dis_level = NaN)
  
  df$ID = as.character(df$ID)
  
  return(df)
}

# Create a function that calculates the boundaries of each stratum
calc_bounds_2d = function(xlim, ylim, n_strata, by){
  if(by == "row"){
    seq(from = ylim[1], to = ylim[2], by = (ylim[2] - ylim[1])/n_strata)
  } else if(by == "column"){
    seq(from = xlim[1], to = xlim[2], by = (xlim[2] - xlim[1])/n_strata)
  }
}

# Create a function that generates a simple random sampling plan
sim_plan_srs_2d = function(n_sp, xlim, ylim, radius){
  
  ## Generate a data frame that contains the coordinates of the sampling points
  x_sp = runif(n = n_sp, min = xlim[1], max = xlim[2])
  y_sp = runif(n = n_sp, min = ylim[1], max = ylim[2])
  
  naming_sp_2d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
}

# Create a function that generates a stratified random sampling plan
sim_plan_strs_2d = function(n_sp, n_strata, by, xlim, ylim, radius){
  
  # by %in% c("row", "column", "2d")
  if(by == "2d"){
    
    # Checkpoint: Make sure n_x and n_y are integers > 0
    stopifnot(length(n_strata) == 2 & 
                n_strata[1] == trunc(n_strata[1]) & 
                n_strata[2] == trunc(n_strata[2]) &
                n_strata[1] > 0 &
                n_strata[2] > 0)
    if(n_sp %% (n_strata[1] * n_strata[2]) != 0){
      stop("n_sp is not a multiple of n_strata[1] * n_strata[2].")
    }
    
    n_x = n_strata[1]
    n_y = n_strata[2]
    
    xbounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_x, by = "column")
    ybounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_y, by = "row")
    
    x_sp = runif(n = n_sp, min = xbounds[1:length(xbounds) - 1], max = xbounds[2:length(xbounds)])
    y_sp = runif(n = n_sp, min = ybounds[1:length(ybounds) - 1], max = ybounds[2:length(ybounds)]) %>%
      sample(x = ., size = length(.), replace = FALSE)
    
    naming_sp_2d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
    
  } else {
    
    # Checkpoint
    if (n_sp %% n_strata != 0) {
      stop("n_sp is not a multiple of n_strata.")
    } 
    
    bounds = calc_bounds_2d(xlim = xlim, ylim = ylim, n_strata = n_strata, by = by)
    
    if (by == "row") {
      x_sp = runif(n = n_sp, min = xlim[1], max = xlim[2])
      y_sp = runif(n = n_sp, min = bounds[1:length(bounds)-1], max = bounds[2:length(bounds)])
    } else if (by == "column") {
      x_sp = runif(n = n_sp, min = bounds[1:length(bounds)-1], max = bounds[2:length(bounds)])
      y_sp = runif(n = n_sp, min = ylim[1], max = ylim[2])
    }
    
    naming_sp_2d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
  }
}

# Create a function that generates a systematic sampling plan
sim_plan_ss_2d = function(xlim, ylim, n_sp, radius, by){
  
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

  naming_sp_2d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, radius = radius)
}

# A function that includes all kinds of sampling plan
sim_plan_2d = function(method_sp, n_sp, xlim, ylim, radius, n_strata, by){
  if(method_sp == "srs"){
    sim_plan_srs_2d(n_sp = n_sp, xlim = xlim, ylim = ylim, radius = radius)
  } else if (method_sp == "strs"){
    sim_plan_strs_2d(n_sp = n_sp, n_strata = n_strata, by = by, xlim = xlim, ylim = ylim, radius = radius)
  } else if (method_sp == "ss"){
    sim_plan_ss_2d(xlim = xlim, ylim = ylim, n_sp = n_sp, radius = radius, by = by)
  } else {
    stop("Sampling method does not exist. Try 'srs', 'strs', or 'ss'.")
  }
}

########################### Upgrade to 3D ######################################

