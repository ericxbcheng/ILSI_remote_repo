# A function factory that produces sim_outcome()'s given all the input parameters
gen_sim_outcome = function(n_contam, xlim, ylim, n_affected, covar_mat, spread_radius,
                           method_sp, n_sp, sp_radius, spread, n_strata, by, cont_level, 
                           LOC, fun, m_kbar, m_sp, conc_good, case, m, M, Mc, method_det){
  function(...){
    sim_outcome(n_contam = n_contam, xlim = xlim, ylim = ylim, n_affected = n_affected,
                covar_mat = covar_mat, spread_radius = spread_radius, method_sp = method_sp,
                n_sp = n_sp, sp_radius = sp_radius, spread = spread, n_strata = n_strata, 
                by = by, cont_level = cont_level, LOC = LOC, fun = fun, m_kbar = m_kbar, 
                m_sp = m_sp, conc_good = conc_good, case = case, m = m, M = M, 
                Mc = Mc, method_det = method_det)
  }
}

# Rearrange the data in the nested list so that data of the same category are grouped together.
clean = function(data){
  # Convert the data into a vector
  a = unlist(data)
  
  #Produce a sequence of indices for element extraction
  ind = seq(from = 1, by = 3, length.out = length(a)/3)
  
  # Extract elements respectively
  I_det = a[ind]
  ROD = a[ind + 1]
  decision = a[ind + 2]
  
  # Output
  b = list("I_det" = I_det, "ROD" = ROD, "decision" = decision)
  
  return(b)
}

# Calculate the probability of rejection
calc_Prej = function(decision){
  
  # Check point
  stopifnot(decision %in% 1:7)

  # Based on the lookup table, numbers 2,3,6 mean rejection while the others mean acceptance. 
  # Convert the decision vector into a logical vector. 1 = rejection, 0 = acceptance.
  a = {decision %in% c(2, 3, 6)}
  
  # Calculate the probability of rejection
  b = mean(a)
  
  return(b)
}

# Calculate the probability of detection (whether any of the contamination is detected)
calc_Pdet = function(I_det){
  
  # Check point
  stopifnot(I_det %in% c(0,1))
  
  # Calculate the probability of detection
  mean(I_det)
  
}

# Create a function that can iterate the simulation for n_iter times
sim_iterate = function(n_iter, fun){
  
  # Check point: Is n_iter >= 1 and is it an integer?
  if(! any(n_iter >=1, is.integer(integer))){
    stop("n_iter is either not >= 1 or not an integer.")
  }
  
  # Iterate the function for n_iter times
  map(.x = 1:n_iter, .f = fun)
}
