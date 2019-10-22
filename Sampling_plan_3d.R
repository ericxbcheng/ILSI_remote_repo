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
                  cont_level = NaN,
                  dis_level = NaN, 
                  stringsAsFactors = FALSE)
  
  return(df)
}

# 3D: simple random sampling (probe)
sim_plan_srs_3d = function(n_sp, lims, radius){
  
  # Check point: make sure the dimension is 3
  stopifnot(length(lims) == 3)
  
  ## Generate a data frame that contains the coordinates of the sampling points
  x_sp = runif(n = n_sp, min = lims$xlim[1], max = lims$xlim[2])
  y_sp = runif(n = n_sp, min = lims$ylim[1], max = lims$ylim[2])
  z_sp = 0
  
  naming_sp_3d(n_sp = n_sp, x_sp = x_sp, y_sp = y_sp, z_sp = z_sp, radius = radius)
}

# 3D: Create a function that generates a stratified random sampling plan (probe)
sim_plan_strs_3d = function(n_sp, n_strata, by, lims, radius){
  
  # Check points
  stopifnot(length(lims) == 3)
  
  a = sim_plan_strs_2d(n_sp = n_sp, n_strata = n_strata, by = by, 
                       xlim = lims$xlim, ylim = lims$ylim, radius = radius) %>%
    mutate(Z = 0) %>%
    dplyr::select(X, Y, Z, ID, label, r, cont_level, dis_level)
  return(a)
}

# systematic sampling for 3D
sim_plan_ss_3d = function(container, lims, radius, compartment, type){
  
  # Check points
  stopifnot(length(lims) == 3)
  stopifnot(container %in% c("truck", "barge","hopper"))
  
  switch(container, 
         "truck" = ss_truck(lims = lims, radius = radius),
         "barge" = ss_barge(lims = lims, radius = radius),
         "hopper" = ss_hopper(lims = lims, compartment = compartment, type = type, radius = radius))
}

sim_plan_3d = function(method_sp, n_sp, n_strata, by, lims, radius, container, compartment, type){
  
  # Check points
  stopifnot(method_sp %in% c("srs", "strs", "ss"))
  
  if(method_sp == "srs"){
    sim_plan_srs_3d(n_sp = n_sp, lims = lims, radius = radius)
  } else if (method_sp == "strs"){
    sim_plan_strs_3d(n_sp = n_sp, n_strata = n_strata, lims = lims, radius = radius, by = by)
  } else if (method_sp == "ss"){
    sim_plan_ss_3d(container = container, lims = lims, radius = radius, compartment = compartment, type = type)
  } else {
    stop("Sampling method does not exist. Try 'srs', 'strs', or 'ss'.")
  }
}

# A function that includes all kinds of sampling plan
sim_plan_new = function(method_sp, spread, n_sp, lims, radius, n_strata, by, compartment, type, container){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
  if(spread == "continuous"){
    sim_plan_2d(method_sp = method_sp, n_sp = n_sp, xlim = lims$xlim, ylim = lims$ylim, 
                radius = radius, n_strata = n_strata, by = by)
  } else if(spread == "discrete"){
    sim_plan_3d(method_sp = method_sp, n_sp = n_sp, lims = lims, radius = radius, 
                container = container, compartment = compartment, type = type, n_strata = n_strata, by = by)
  }
}

# Convert foot to meter
ft2m = function(x){
  0.3048*x
}

# Probe sampling pattern for trucks
ss_truck = function(lims, radius){
  
  # Check point
  ## depth should be > 0
  stopifnot(lims$zlim[2] >=0)
  
  if(lims$zlim[2] >= ft2m(4)){
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

