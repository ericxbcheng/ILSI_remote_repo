# A function for determining the sample size for aflatoxin in different containers
spsize_af = function(container){
  
  # Check point
  stopifnot(container %in% c("truck", "hopper", "barge"))
  
  a = switch(EXPR = container,
             "truck" = 908,
             "hopper" = 1362,
             "barge" = 4540)
  b = 4540
  
  return(c(min = a, max = b))
}

# Sample divider for aflatoxin
sample_divider_af = function(data, container, m_kbar){
  
  # Calculate the mass of sample
  mass = m_kbar * length(data)
  
  # Find the min and max of sample size for a particular container
  a = spsize_af(container = container)
  
  if(mass < a[["min"]]){
    stop(paste0("Sample size is less than the required minimum:", a[["min"]], " g."))
    
  } else if (mass > a[["max"]]){
    
    # How many kernels constitute the recommended sample size?
    k_num = round(a[["max"]] / m_kbar)
    
    # Use the divider to select a subset of samples
    b = sample(x = data, size = k_num, replace = FALSE)
    
    return(b)
  }
}

# Sample divider in general
sample_divider = function(data, container, m_kbar, tox = "AF"){
  
  # Check points
  stopifnot(tox %in% c("AF", "DON", "ZEN", "FM", "OTA"))
  
  switch(EXPR = tox,
         "AF" = sample_divider_af(data = data, container = container, m_kbar = m_kbar),
         message("Under construction"))
}

# Kernel grinder: homogeneity ranges from 0 to 1
grinder = function(data, homogeneity){
  
  # Checkpoint
  stopifnot(homogeneity >= 0 & homogeneity <= 1)
  
  # Grind the kernels based on homogeneity
  if(homogeneity == 0){
    return(data)
    
  } else {
    
    # Determine window length based on homogeneity
    n = length(data) * homogeneity
    ground = runmean(x = data, k = n, alg = "fast", endrule = "constant", align = "right")
    return(ground)
  }
}

# Get the work portion for aflatoxin
get_work_portion_af = function(data, m_kbar){
  
  # Find the number of kernels that constitute 500g work portion
  k_num = round(500 / m_kbar)
  
  a = sample(x = data, size = k_num, replace = FALSE)
  return(a)
}

# Get the work portion in general
get_work_portion = function(data, m_kbar, tox = "AF"){
  
  # Check point
  stopifnot(tox %in% c("AF", "DON", "ZEN", "FM", "OTA"))
  
  switch(EXPR = tox, 
         "AF" = get_work_portion_af(data = data, m_kbar = m_kbar),
         message("Under construction"))
  
}

# Get the test portion
get_test_portion = function(data, m_kbar){
  
  k_num = round(50 / m_kbar)
  
  a = sample(x = data, size = k_num, replace = FALSE)
  
  return(a)
}

# Wrapper function for getting work and test samples from raw data in the discrete case
get_sample_dis = function(data, container, m_kbar, tox, homogeneity){
  
  # Get raw sample that meet the sample size requirements
  #raw = sample_divider(data = a, container = container, m_kbar = m_kbar, tox = tox)
  
  # Get ground raw sample
  ground = grinder(data = data, homogeneity = homogeneity)
  
  # get work portion
  work = get_work_portion(data = ground, m_kbar = m_kbar, tox = tox)
  
  # get test portion
  test = get_test_portion(data = work, m_kbar = m_kbar)
  
  return(list(work = work, test = test))
}

# A function that decides lot rejection for discrete case
decision_dis_new = function(data, LOD, Mc){
  
  ## Calculate the mean concentration of the test samples
  c_bar = mean(data)
  
  ## Determine whether c_bar is above LOD. If it is >= LOD, then determine if it exceeds Mc.
  if(c_bar < LOD){
    return(5)
  } else {
    if(c_bar >= Mc){
      return(6)
    } else {
      return(7)
    }
  }
}

# Create a function that decides whether to accept or reject the lot
lot_decision_new = function(data, case, m, M, Mc, m_sp, spread, method_det){
  
  ## Get LOD for the chosen method of detection
  LOD = get_LOD(method_det = method_det)
  
  ## Make decision based on the spread type
  if(spread == "continuous"){
    
    ## data = contam_sp_xy
    ## Subset out the contamination levels at the sample points
    conc = subset(x = data, subset = label == "sample point", select = cont_level, drop = TRUE)
    
    decision_cont_new(conc = conc, LOD = LOD, case = case, m = m, M = M, m_sp = m_sp, method_det = method_det)
    
  } else if (spread == "discrete"){
    
    ## data = test portion
    decision_dis_new(data = data, LOD = LOD, Mc = Mc)
    
  } else {
    stop("Unknown type of spread. Choose either 'continuous' or 'discrete'.")
  }
}
