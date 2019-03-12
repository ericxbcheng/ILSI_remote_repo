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

# Wrapper function for getting samples in the discrete case
get_sample_dis = function(data, container, m_kbar, tox){
  
  # Extract the kernel concentrations from the sample
  a = data$samples[[2]]
  
  # Get raw sample that meet the sample size requirements
  raw = sample_divider(data = a, container = container, m_kbar = m_kbar, tox = tox)
  
  # get work portion
  work = get_work_portion(data = raw, m_kbar = m_kbar, tox = tox)
  
  # get test portion
  test = get_test_portion(data = work, m_kbar = m_kbar)
  
  return(list(raw = raw, work = work, test = test))
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
lot_decision_new = function(data, case, m, M, Mc, spread, method_det){
  
  ## Get LOD for the chosen method of detection
  LOD = get_LOD(method_det = method_det)
  
  ## Make decision based on the spread type
  if(spread == "continuous"){
    
    ## Subset out the contamination levels at the sample points
    a = subset(x = data, subset = label == "sample point", select = cont_level, drop = FALSE)
    
    decision_cont(df = a, LOD = LOD, case = case, M = M, m = m)
    
  } else if (spread == "discrete"){
    
    b = data$test
    
    decision_dis_new(data = b, LOD = LOD, Mc = Mc)
    
  } else {
    stop("Unknown type of spread. Choose either 'continuous' or 'discrete'.")
  }
}