################################## NEW ##############################################
# A function that generates the covariance matrix
make_covar_mat = function(spread, varx, vary, varz, covxy, covxz, covyz){
  if(spread == "discrete"){
    matrix(data = c(varx, covxy, covxz, covxy, vary, covyz, covxz, covyz, varz), nrow = 3, ncol = 3)
  } else if (spread == "continuous"){
    matrix(data = c(varx, covxy, covxy, vary), nrow = 2, ncol = 2)
  } else {
    stop("Unknown type of spread. Choose either 'discrete' or 'continuous'.")
  }
}

# Continuous contamination
contam_cont = function(spot_coord, n_contam, spread, spread_radius, cont_level){
  
  # Create labels for spots
  label = c(rep("spot", times = n_contam))
  
  df = naming_total(n_contam = n_contam, spot_coord = spot_coord, spread_coord = NULL, spread = spread,
                    label = label, spread_radius = spread_radius, cont_level = cont_level)
  
  return(df)
}

# Discrete contamination
contam_dis = function(spot_coord, n_contam, n_affected, covar, spread, dis_level){
  
  # Create labels for spots and spreads
  label = c(rep("spot", times = n_contam), rep("spread", times = n_contam * n_affected))
  
  if(n_affected == 0){
    
    df = naming_total(spot_coord = spot_coord, spread_coord = NULL, spread = spread, 
                      label = label, spread_radius = NaN, dis_level = dis_level, n_contam = n_contam)
  } else {

    # Checkpoint: make sure the covariance matrix is 3 by 3
    stopifnot(sum(dim(covar)) == 6)
    
    # Create spread contaminations. Use multivariate normal distribution.
    spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = mvrnorm, n = n_affected, Sigma = covar) %>%
      split(x = ., f = col(.)) %>%
      map(.x = ., .f = function(x) {matrix(x, ncol = ncol(spot_coord))}) %>%
      do.call(what = rbind, args = .) %>%
      as.data.frame(., stringsAsFactors = FALSE) %>%
      naming_spread(df = ., spot = n_contam, spread = n_affected)
    
    df = naming_total(spot_coord = spot_coord, spread_coord = spread_coord, n_contam = n_contam, spread = spread, 
                      label = label, spread_radius = NaN, dis_level = dis_level)
  }
  return(df)
}

# Create point-source contamination spots
point_contam = function(n_contam, lims, spread){
  
  # Check point: make sure the dimension is either 2D or 3D
  if(spread == "continuous"){
    stopifnot(length(lims) == 2)
  } else if (spread == "discrete"){
    stopifnot(length(lims) == 3)
  } else {
    stop("Unknown spread type.")
  }
  
  x = runif(n = n_contam, min = lims$xlim[1], max = lims$xlim[2])
  y = runif(n = n_contam, min = lims$ylim[1], max = lims$ylim[2])
  
  if(length(lims) == 2){
    matrix(data = c(x, y), ncol = 2)
    
  } else {
    z = runif(n = n_contam, min = lims$zlim[1], max = lims$zlim[2])
    matrix(data = c(x, y, z), ncol = 3)
    
  } 
}

# Combine spots and spreads, create contamination levels, and add headers.
naming_total = function(spot_coord, spread_coord, n_contam, spread, label, spread_radius, cont_level, dis_level){
  
  # Create the header for data frame
  if(spread == "continuous"){
    header = c("X", "Y", "ID")
  } else {
    header = c("X", "Y", "Z", "ID")
  }
  
  # Give a name column to spot_coord
  spot_coord = naming_spot(df = spot_coord, spot = n_contam)
  
  # Combine spot and spread by row
  df = rbind(spot_coord, spread_coord, stringsAsFactors = FALSE)
  colnames(df) =  header
  df2 = cbind(df, label, r = spread_radius, stringsAsFactors = FALSE) 
  
  # Assume contamination in continuous case follows a log normal dist
  # Assume contamination in discrete case follows 20 + Gamma dist
  if(spread == "continuous"){
    df3 = df2 %>%
      mutate(cont_level = f_cont_level(n = nrow(.), param = cont_level),
             dis_level = NaN)
    
  } else {
    df3 = df2 %>%
      mutate(cont_level = NaN,
             dis_level = rgamma_lim(n = nrow(.), alpha = 2, mode = dis_level[["mode"]], lb = dis_level[["lb"]], ub = NULL))
  }
  
  return(df3)
}

# Remove contamination points that fall beyond perimeters
rm_outlier = function(df, lims){
  if(length(lims) == 2){
    
    out = df$X < lims$xlim[1] |
      df$X > lims$xlim[2] |
      df$Y < lims$ylim[1] |
      df$Y > lims$ylim[2]
    
    df2 = df[!out, ]
    
    return(df2)
    
  } else if(length(lims) == 3){
    
    out = df$X < lims$xlim[1] |
      df$X > lims$xlim[2] |
      df$Y < lims$ylim[1] |
      df$Y > lims$ylim[2] |
      df$Z < lims$zlim[1] |
      df$Z > lims$zlim[2]
    
    df2 = df[!out, ]
    
    return(df2)
    
  } else {
    stop("Wrong dimension. Choose either 2 or 3.")
  }
}

# Simulate contamination in 2D (continuous) or 3D (discrete) scenarios
sim_contam_new = function(n_contam, lims, spread, covar, n_affected, spread_radius, cont_level, dis_level){
  
  # Checkpoints
  stopifnot(n_contam > 0 & length(lims) %in% c(2,3))
  stopifnot(spread %in% c("continuous", "discrete"))
  
  # Create spot contaminations
  spot_coord = point_contam(n_contam = n_contam, lims = lims, spread = spread) %>%
    as.data.frame()
  
  # Generate all the data based on the spread type  
  if(spread == "continuous"){
    
    stopifnot(spread_radius >= 0)
    
    df = contam_cont(spot_coord = spot_coord, n_contam = n_contam, spread = spread, 
                     spread_radius = spread_radius, cont_level = cont_level)
  } else {
    
    stopifnot(n_affected >= 0)
    
    df = contam_dis(spot_coord = spot_coord, n_contam = n_contam, n_affected = n_affected, 
                    covar = covar, spread = spread, dis_level = dis_level)
  }
  
  # Remove outliers
  df2 = rm_outlier(df = df, lims = lims)
  
  # Final adjustments
  rownames(df2) = NULL
  #df2$ID = as.character(df2$ID)
  
  return(df2)
}

## Generate concentration levels
rgamma_lim = function(n, alpha = 2, mode, lb, ub){
  # Alpha = 2 by default, theta is calculated by mode
  # Include lower bound and upper bound
  a = lb + rgamma(n = n, shape = alpha, scale = (mode-lb)/(alpha-1))
  # When a number is > upper bound, replace it with the mode
  if(!is.null(ub)){
    a[a >= ub] = mode
  } 
  return(a)
}
