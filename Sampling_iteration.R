# Determine if we accept or reject a lot
do_rej = function(decision){
  # Check point
  stopifnot(decision %in% 1:7)
  
  # Based on the lookup table, numbers 2,3,6 mean rejection while the others mean acceptance. 
  # Convert the decision vector into a logical vector. 1 = rejection, 0 = acceptance.
  a = {decision %in% c(2, 3, 6)}
  
  return(a)
}

# Calculate the probability of rejection
calc_Prej = function(decision){
  
  # Determine whether to reject or accept a lot
  a = do_rej(decision = decision)
  
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
  
  # Check point: Is n_iter >= 1?
  stopifnot(n_iter >= 1)
  
  # Iterate the function for n_iter times
  map(.x = 1:n_iter, .f = fun)
}
