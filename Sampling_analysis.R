# Calculate detection probability for simulations with each tuning value
calc_Pdet_one = function(data){
  split(x = data$I_det, f = data$param) %>%
    map(.x = ., .f = calc_Pdet) %>%
    unlist()
}

# Calculate probability of rejection for simulations with each tuning value
calc_Prej_one = function(data){
  split(x = data$decision, f = data$param) %>%
    map(.x = ., .f = calc_Prej) %>%
    unlist()
}

# Clean up the probability of detection. "data" takes a list of n lists.
calc_Pdet_n = function(data){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  map(.x = data, .f = calc_Pdet_one) %>%
    unlist() %>%
    tibble(param = as.integer(names(.)), P_det = .)
}

# Clean up the probability of acceptance. "data" takes a list of n lists.
calc_Paccept_n = function(data){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  map(.x = data, .f = calc_Prej_one) %>%
    unlist() %>%
    tibble(param = as.integer(names(.)), P_rej = .) %>%
    mutate(Paccept = 1 - P_rej)
}

# Calculate P(detection) and P(acceptance) in the continuous case for a list of n lists
metrics_cont_n = function(data){
  
  # Checkpoint
  if(!is.null(names(data))){
    stop("Data should be a list of lists, not a single list.")
  }
  
  # Get Pdet
  a = calc_Pdet_n(data)
  
  # Get Paccept
  b = calc_Paccept_n(data)
  
  # Form output
  c = cbind(a, Paccept = b$Paccept)
  
  return(c)
}
