# A function factory
gen_sim_outcome_new = function(n_contam, c_hat, lims, spread, spread_radius, method_sp, n_sp, n_strata, 
                               by, LOC, fun, case, m, M, m_sp, method_det, covar_mat, n_affected, dis_level, cont_level, 
                               sp_radius, container, L, rho, m_kbar, conc_neg, tox, Mc, verbose = FALSE, 
                               bg_level, geom, seed, homogeneity, compartment, type){
  function(...){
    sim_outcome_new(n_contam = n_contam, c_hat = c_hat, lims = lims, spread = spread, covar_mat = covar_mat,
                    n_affected = n_affected, dis_level = dis_level, method_sp = method_sp, sp_radius = sp_radius,
                    container = container, L = L, rho = rho, m_kbar = m_kbar, conc_neg = conc_neg, 
                    tox = tox, Mc = Mc, m_sp = m_sp, method_det = method_det, verbose = verbose, spread_radius = spread_radius, 
                    n_sp = n_sp, n_strata = n_strata, by = by, LOC = LOC, fun = fun, case = case, m = m, 
                    M = M, cont_level = cont_level, bg_level = bg_level, geom = geom, seed = seed, 
                    homogeneity = homogeneity, compartment = compartment, type = type)
  }
}

# Organize the results
clean_new = function(spread, data, verbose = FALSE){
  
  stopifnot(spread %in% c("continuous", "discrete"))
  
  if(spread == "discrete"){
    clean_dis(data = data, verbose = verbose)
    
  } else {
    clean_cont(data = data)
    
  }
}

# Organize the results for discrete case
clean_dis = function(data, verbose = FALSE){
  
  # Convert the list into a vector
  a = unlist(data)
  
  if(verbose == FALSE){
    
    # Produce a sequence of c(1,2) for element extraction
    ind = rep(x = c(1,2), length.out = length(a))
    
    c_true = a[ind == 1]
    decision = a[ind == 2]
    
    # Output
    return(list("c_true" = c_true, "decision" = decision))
    
  } else {
    
    # Produce a sequence of c(1,2,3,4,5) for element extraction
    ind = rep(x = c(1,2,3,4,5), length.out = length(a))
    
    c_true = a[ind == 1]
    decision = a[ind == 2]
    mean_raw = a[ind == 3]
    mean_work = a[ind == 4]
    mean_test = a[ind == 5]
    
    # Output
    return(list("c_true" = c_true, "decision" = decision, 
                "mean_raw" = mean_raw, "mean_work" = mean_work, "mean_test" = mean_test))
  }
}

# Organize the results for continuous case
clean_cont = function(data){
  # Convert the data into a vector
  a = unlist(data)
  
  #Produce a sequence of indices for element extraction
  ind = rep(x = c(1,2), length.out = length(a))
  
  # Extract elements respectively
  I_det = a[ind == 1]
  decision = a[ind == 2]
  
  # Output
  b = list("I_det" = I_det, "decision" = decision)
  
  return(b)
}

# First layer of iteration
sim_iterate = function(n_iter, Args, seed){
  
  # Check point: Is n_iter >= 1?
  stopifnot(n_iter >= 1)
  
  # Include seed into arguments list
  Args$seed = seed
  
  # Generate a sim_outcome_new() with loaded arguments
  a = do.call(what = gen_sim_outcome_new, args = Args)
  
  # Iterate that manufactured function for n_iter times
  b = map(.x = 1:n_iter, .f = a)
  
  return(b)
}

# Second layer iteration
sim_iterate2 = function(n_seed, n_iter, Args, ...){
  
  # Run the model for n_iter times under each seed
  a = map(.x = 1:n_seed, .f = sim_iterate, Args = Args, n_iter = n_iter)
  
  # Clean the data
  b = clean_new(spread = Args$spread, data = a, verbose = Args$verbose)
  
  # Add a vector for the seeds
  b$seed = rep(x = 1:n_seed, each = n_iter)
  
  return(b)
}
