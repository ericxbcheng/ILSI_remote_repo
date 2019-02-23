## Some helper functions for creating unique identifiers for each observation.
naming_spread = function(df, spot, spread){
  a = rep(x = 1:spot, each = spread) %>% as.character()
  b = rep(x = 1:spread, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c)
}

naming_spot = function(df, spot){
  a = 1:spot %>% as.character()
  b = rep(0, times = spot) %>% as.character()
  c = paste(a, "-", b, sep = "")
  cbind(df, c)
}

## Create a function to generate contamination levels. Remember that param contains log10(mean) and log10(sd). And log() in R means log(..., base = exp(1)).
f_cont_level = function(n, param){
  rlnorm(n = n, meanlog = log(10^param[1]), sdlog = log(10^param[2]))
}

## Define a function that calculates contamination contribution when a point is within spread_radius and reduces to almost 0 when beyond spread_radius
f_decay = function(x, fun, spread_radius, LOC, cont_level){
  if(x <= spread_radius){
    fun(x = x, spread_radius = spread_radius, LOC = LOC)
  } else {
    fun(x = x, spread_radius = spread_radius, LOC = LOC)* 1/(10^cont_level[1])
  }
}

## Calculate the contamination contribution using an exponential function.
f_exp = function(x, spread_radius, LOC){
  theta = - spread_radius/log(LOC)
  exp(-x/theta)
}

## Calculate the contamination contribution using a normal distribution-like function
f_norm = function(x, spread_radius, LOC){
  sigma = sqrt(-spread_radius^2/log(LOC))
  exp(-x^2/sigma^2)
} 

## Calculate the contamination contribution on a 2D plane
f_density = function(method, x, y, x0, y0, spread_radius, LOC){
  
  d = sqrt((x-x0)^2 + (y-y0)^2)
  
  if(method == "exp"){
    
    f_exp(x = d, spread_radius = spread_radius, LOC = LOC)
    
  } else if (method == "norm"){
    
    f_norm(x = d, spread_radius = spread_radius, LOC = LOC)
    
  } else {
    stop("Method is undefined. Please choose 'exp' or 'norm'. ")
  }
}


## (OLD) The simulation function for contamination spot and its spread
sim_contam = function(n_contam, xlim, ylim, covariance, n_affected, radius, cont_level){
  
  if(n_contam == 0){
    stop("n_contam = 0. Please select another n_contam.")
  }
  
  ## Generate a matrix that contains contamination coordinates
  x = runif(n = n_contam, min = xlim[1], max = xlim[2])
  y = runif(n = n_contam, min = ylim[1], max = ylim[2])
  
  spot_coord = matrix(data = c(x, y), ncol = 2) %>% 
    as.data.frame()
  
  # Create a factor column.
  label = c(rep("spot", times = n_contam), rep("spread", times = n_contam * n_affected))
  
  # Create a spread radius column
  r = radius
  
  if(n_affected != 0){
    # Each column of spread_coord contains 2*n_affected elements, the first n_affected elements are random numbers generated from mat_1[1,1] and the second n_affected elements are random numbers generated from mat_1[1,2]
    spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = mvrnorm, n = n_affected, Sigma = covar_mat)
    
    # Split the spread_coord by columns, rearrange the vector of each list into an nx2 matrix, then combine these matrices by row.
    spread_coord_2 = spread_coord %>%
      split(x = ., f = col(.)) %>%
      map(.x = ., .f = function(x) {matrix(x, ncol = 2)}) %>%
      do.call(what = rbind, args = .) %>%
      as.data.frame()
    
    # Give a unique identifier to each observation.
    spread_coord_3 = naming_spread(spread_coord_2, n_contam, n_affected)
    spot_coord_2 = naming_spot(spot_coord, n_contam)
    
    # Combine the contamination spot coordinates and the spread coordinates, and then combine the factor column and the spread radius column.
    df_1 = rbind(spot_coord_2, spread_coord_3)
    colnames(df_1) = c("X", "Y", "ID")
    df_2 = cbind(df_1, label, r)
    df_2[["cont_level"]] = f_cont_level(n = nrow(df_2), param = cont_level)
    
  } else {
    
    # Give a unique identifier to each observation.  
    spot_coord_2 = naming_spot(spot_coord, n_contam)
    
    # Give the spot_coord_2 labels and radius column
    df_1 = spot_coord_2
    colnames(df_1) = c("X", "Y", "ID")
    df_2 = cbind(df_1, label, r)
    df_2[["cont_level"]] = f_cont_level(n = nrow(df_2), param = cont_level)
    
  }
  
  # Remove points that are outside the perimeter.
  out = df_2$X < xlim[1] |
    df_2$X > xlim[2] |
    df_2$Y < ylim[1] |
    df_2$Y > ylim[2]
  
  df_3 = df_2[!out, ]
  
  # Reset the row names
  rownames(df_3) = NULL
  
  # Add a column for contamination level in discrete spread mode. For spots and spreads, the two types of level are identical.
  df_3[["dis_level"]] = df_3[["cont_level"]]
  
  df_3$ID = as.character(df_3$ID)
  
  return(df_3)
}

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
  
  df = naming_total(spot_coord = spot_coord, spread_coord = NULL, spread = spread,
                    label = label, spread_radius = spread_radius, cont_level = cont_level)
  
  return(df)
}

# Discrete contamination
contam_dis = function(spot_coord, n_contam, n_affected, covar, spread, spread_radius, dis_level){
  
  # Create labels for spots and spreads
  label = c(rep("spot", times = n_contam), rep("spread", times = n_contam * n_affected))
  
  if(n_affected == 0){
    
    df = naming_total(spot_coord = spot_coord, spread_coord = NULL, spread = spread, 
                      label = label, spread_radius = spread_radius, dis_level = dis_level)
  } else {
    
    # Checkpoint: make sure the covariance matrix is 3 by 3
    stopifnot(sum(dim(covar)) == 6)
    
    # Create spread contaminations. Use multivariate normal distribution.
    spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = mvrnorm, n = n_affected, Sigma = covar) %>%
      split(x = ., f = col(.)) %>%
      map(.x = ., .f = function(x) {matrix(x, ncol = ncol(spot_coord))}) %>%
      do.call(what = rbind, args = .) %>%
      as.data.frame() %>%
      naming_spread(df = ., spot = n_contam, spread = n_affected)
    
    df = naming_total(spot_coord = spot_coord, spread_coord = spread_coord, spread = spread, 
                      label = label, spread_radius = spread_radius, dis_level = dis_level)
  }
  return(df)
}

# Create uniformly distributed contamination spots
unif_contam = function(n_contam, lims, spread){
  
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
naming_total = function(spot_coord, spread_coord, spread, label, spread_radius, cont_level, dis_level){
  
  # Create the header for data frame
  if(spread == "continuous"){
    header = c("X", "Y", "ID")
  } else if(spread == "discrete"){
    header = c("X", "Y", "Z", "ID")
  } else {
    stop("Unknown spread type.")
  }
  
  # Give a name column to spot_coord
  spot_coord = naming_spot(df = spot_coord, spot = n_contam)
  
  # Combine spot and spread by row
  df = rbind(spot_coord, spread_coord)
  colnames(df) =  header
  df2 = cbind(df, label, r = spread_radius) 
  
  # Assume contamination in continuous case follows a log normal dist
  # Assume contamination in discrete case follows 20 + Gamma dist
  if(spread == "continuous"){
    df3 = df2 %>%
      mutate(cont_level = f_cont_level(n = nrow(.), param = cont_level),
           dis_level = NA)
    
  } else {
    df3 = df2 %>%
      mutate(cont_level = NA,
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
  stopifnot(n_contam > 0 & spread_radius >= 0 & n_affected >= 0 & length(lims) %in% c(2,3))
  stopifnot(spread %in% c("continuous", "discrete"))
  
  # Create spot contaminations
  spot_coord = unif_contam(n_contam = n_contam, lims = lims, spread = spread) %>%
    as.data.frame()
  
  # Generate all the data based on the spread type  
  if(spread == "continuous"){
    df = contam_cont(spot_coord = spot_coord, n_contam = n_contam, spread = spread, 
                     spread_radius = spread_radius, cont_level = cont_level)
  } else {
    df = contam_dis(spot_coord = spot_coord, n_contam = n_contam, n_affected = n_affected, 
                    covar = covar, spread = spread, spread_radius = spread_radius, dis_level = dis_level)
  }
  
  # Remove outliers
  df2 = rm_outlier(df = df, lims = lims)
  
  # Final adjustments
  rownames(df2) = NULL
  df2$ID = as.character(df2$ID)
  
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
