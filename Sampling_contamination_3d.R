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

# Create systematic and sporadic contamination
area_contam = function(lims){
  
  # Check point: only continuous case has area-based contamination
  stopifnot(length(lims) == 2)
  
  x = 0
  y = 0
  
  matrix(data = c(x, y), ncol = 2)
}

# A function that returns a vector of concentrations for contaminated kernels
# param must a list
f_dis_level = function(n, param){
  
  # Check point
  stopifnot(param[["type"]] %in% c("constant", "Gamma"))
  
  if(param[["type"]] == "constant"){
    
    # concentrations are constant
    stopifnot(length(param[["args"]]) == 1)
    return(as.numeric(param[["args"]]))
    
  } else {
    
    # concentrations follow 20 + Gamma(alpha = 2, mode)
    stopifnot(length(param[["args"]]) == 2)
    a = rgamma_lim(n = n, alpha = 2, mode = param[["args"]][["mode"]], lb = param[["args"]][["lb"]])
    return(a)
    
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
             dis_level = f_dis_level(n = nrow(.), param = dis_level))
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

# Generate contamination spots for either point-source or area-based scenario in continuous case
gen_contam_cont = function(geom, n_contam, lims, spread, spread_radius){
  if(geom == "point"){
    spot_coord = point_contam(n_contam = n_contam, lims = lims, spread = spread) %>%
      as.data.frame()
    
  } else {
    spot_coord = area_contam(lims = lims) %>%
      as.data.frame()
    
    # Assume the spread_radius is big enough to cover the whole field
    # Assume there is only 1 contamination spot
    n_contam = 1
    spread_radius = sqrt(lims$xlim[2] ^ 2 + lims$ylim[2] ^ 2)
  }
  return(list(spot_coord = spot_coord, n_contam = n_contam, spread_radius = spread_radius))
}

# Calculate the expected value of dis_level 
## Note: for Gamma dist, we don't calculate the expected value because Gamma dist is skewed. We use the mode instead.
calc_E_dis_level = function(dis_level){
  
  # Checkpoint
  stopifnot(dis_level[["type"]] %in% c("constant", "Gamma"))
  
  if(dis_level[["type"]] == "constant"){
    return(as.numeric(dis_level[["args"]]))
    
  } else {
    # Assume alpha = 2
    # X ~ Gamma(alpha, theta) ==> E(X) = alpha * theta
    # mode = (alpha - 1) * theta
    # E(C_pos) = lb + mode
    return(dis_level[["args"]][["lb"]] + dis_level[["args"]][["mode"]])
  }
  
}

# Calculate the number of contaminated kernels given an estimated aflatoxin concentration in the bin
calc_n_contam = function(c_hat, lims, rho, m_kbar, dis_level, conc_neg){
  
  # Find the number of kernels in the container
  n_k = calc_k_num(rho = rho, m_kbar = m_kbar, sampler = FALSE, lims = lims)
  
  # Calculate expected value of concentration for contaminated kernels
  mean_c_contam = calc_E_dis_level(dis_level = dis_level)
  
  # Calculate number of contaminated kernels based on c_hat
  n_contam = ceiling(n_k * (c_hat - mean(conc_neg)) / (mean_c_contam - mean(conc_neg))) 
  
  return(n_contam)
}

# Created contamination spots for discrete case
gen_contam_dis = function(n_contam, lims, spread){
  spot_coord = point_contam(n_contam = n_contam, lims = lims, spread = spread) %>%
    as.data.frame()
  return(list(spot_coord = spot_coord, n_contam = n_contam))
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
    spread_coord = apply(X = spot_coord, MARGIN = 1, FUN = rmvn, n = n_affected, 
                         sigma = covar, isChol = FALSE, ncores = 1) %>%
      split(x = ., f = col(.)) %>%
      lapply(X = ., FUN = function(x) {matrix(x, ncol = ncol(spot_coord))}) %>%
      do.call(what = rbind, args = .) %>%
      as.data.frame(., stringsAsFactors = FALSE) %>%
      naming_spread(df = ., spot = n_contam, spread = n_affected)
    
    df = naming_total(spot_coord = spot_coord, spread_coord = spread_coord, n_contam = n_contam, spread = spread, 
                      label = label, spread_radius = NaN, dis_level = dis_level)
  }
  return(df)
}

# Simulate contamination in 2D (continuous) or 3D (discrete) scenarios
sim_contam_new = function(geom, n_contam, c_hat, rho, m_kbar, conc_neg, lims, spread, 
                          covar, n_affected, spread_radius, cont_level, dis_level, seed){

  # Checkpoints
  stopifnot(spread %in% c("continuous", "discrete"))
  
  # Maintain the old seed and reassign the current seed with the old seed when this function ends
  # If there is no user-defined seed, the system-generated seed will be used
  old <- .Random.seed
  on.exit(expr = {.Random.seed <<- old})
  
  if(is.na(seed)){
    seed = old
    warning("Seed is not set. Contamination points won't be reproducible.")
  } else {
    set.seed(seed)
  }
  
  # Create contamination for either continuous or discrete case
  if(spread == "continuous"){
    df = sim_contam_cont(geom = geom, lims = lims, n_contam = n_contam, spread = spread, 
                         spread_radius = spread_radius, cont_level = cont_level)
    
  } else {
    df = sim_contam_dis(c_hat = c_hat, lims = lims, rho = rho, m_kbar = m_kbar, 
                        dis_level = dis_level, conc_neg = conc_neg, spread = spread, 
                        n_affected = n_affected, covar = covar)
      
  }
  
  # Remove outliers
  df2 = rm_outlier(df = df, lims = lims)
  
  # Final adjustments
  rownames(df2) = NULL
  #df2$ID = as.character(df2$ID)
  
  return(df2)
}

# A sub-function for simulation contamination for continuous case
sim_contam_cont = function(geom, lims, n_contam, spread, spread_radius, cont_level){
  
  #check point
  stopifnot(n_contam > 0 & geom %in% c("point", "area") & length(lims) == 2 & spread_radius >= 0)
  
  spot_temp = gen_contam_cont(geom = geom, n_contam = n_contam, lims = lims, 
                              spread = spread, spread_radius = spread_radius)
  df = contam_cont(spot_coord = spot_temp$spot_coord, n_contam = spot_temp$n_contam, spread = spread, 
                   spread_radius = spot_temp$spread_radius, cont_level = cont_level)
  
  return(df)
}

# A sub-function for simulation contamination for discrete case
sim_contam_dis = function(c_hat, lims, rho, m_kbar, dis_level, conc_neg, spread, n_affected, covar){
  
  # Check point
  stopifnot(length(lims) == 3 & n_affected >= 0)
  
  # n_contam_total = n_spot + n_spot * n_affected
  n_contam_total = calc_n_contam(c_hat = c_hat, lims = lims, rho = rho, m_kbar = m_kbar, dis_level = dis_level, conc_neg = conc_neg)
  
  # Calculate n_spot
  # Set the boundary for n_affected: 0 <= n_affected <= n_contam_total - 1
  if(n_affected > n_contam_total - 1){
    stop(cat("n_affected must be <=", n_contam_total - 1))
  }
  
  n_spot = ceiling(n_contam_total / (1 + n_affected))
  
  # Generate contamination spots
  spot_temp = gen_contam_dis(n_contam = n_spot, lims = lims, spread = spread)
  
  df = contam_dis(spot_coord = spot_temp$spot_coord, n_contam = spot_temp$n_contam, n_affected = n_affected, 
                  covar = covar, spread = spread, dis_level = dis_level)  
  
  return(df)
}

## Generate concentration levels
rgamma_lim = function(n, alpha = 2, mode, lb){
  # Alpha = 2 by default, theta is calculated by mode
  # Include lower bound
  a = lb + rgamma(n = n, shape = alpha, scale = (mode-lb)/(alpha-1))

  return(a)
}

# A wrapper for discrete level inputs
make_dis_level = function(type, args){
  
  if(type == "constant"){
    dis_level = list(type = "constant", args = args)
    
  } else if (type == "Gamma"){
    dis_level = list(type = "Gamma", args = list("mode"= args[1], "lb" = args[2]))
    
  } else {
    stop("Unknown discrete level type. Choose 'constant' or 'Gamma'.")
  }
  return(dis_level)
}
