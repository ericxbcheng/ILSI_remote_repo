# Iterate the simulation on different values of a single parameter
tune_param = function(val, n_iter, ...){
  
  # For each value of the parameter we want to tune, generate an f_outcome() loaded with arguments
  a = map(.x = val, .f = gen_sim_outcome, ...)
  
  # Iterate each f_outcome() n_iter times and produce a series of lists containing I_det, ROD, decision
  b = map(.x = a, .f = sim_iterate, n_iter = n_iter)
  
  # Organize the outcomes into one single list with "I_det", "ROD", "Decision"
  c = clean(b)
  
  # Add a vector of parameter values to the list
  c$param = rep(x = val, each = n_iter) %>% as.factor()
  
  return(c)
}

## Create a function that removes one of the arguments. This is important for iterating the simulation on different values of a single parameter.
one_arg_less = function(name, Args_default){
  
  # Check point: Is the parame_name included in the list below?
  if(! any(name %in% c("n_contam", "xlim", "ylim", "n_affected", 
                       "covar_mat", "spread_radius", "method_sp", 
                       "n_sp", "sp_radius", "spread", "n_strata", 
                       "by", "cont_level", "LOC", "fun", "m_kbar", 
                       "m_sp", "conc_good", "case", "m", "M", "Mc", "method_det"))){
    stop("Unknown tuning parameter.")
  }
  
  a = Args_default
  
  # Set the chosen parameter to NULL
  a[[name]] = NULL
  return(a)
}

## Create a function that can tune a single parameter given the parameter name and values
tune_param2 = function(val, name, Args_default, n_iter, ...){
  
  # Produce a list of arguments with the tuning parameter removed
  a = one_arg_less(name = name, Args_default = Args_default)
  
  # Tune the chosen parameter for n_iter times. !!!a means splicing the list "a" and releasing all the remaining arguments.
  b = exec(f = tune_param, val = val, n_iter = n_iter, !!!a)
  
  return(b)
}

# Calculate detection probability for simulations with each tuning value
calc_Pdet2 = function(data){
  split(x = data$I_det, f = data$param) %>%
    map(.x = ., .f = calc_Pdet) %>%
    unlist()
}

# Calculate probability of rejection for simulations with each tuning value
calc_Prej2 = function(data){
  split(x = data$decision, f = data$param) %>%
    map(.x = ., .f = calc_Prej) %>%
    unlist()
}

# Calculate mean rate of detection for simulations with each tuning value
calc_mean_ROD = function(data){
  split(x = data$ROD, f = data$param) %>%
    map(.x = ., .f = mean) %>%
    unlist()
}

# Clean up the probability of detection. "data" takes a list.
clean_Pdet = function(data, method_sp){
  map(.x = data, .f = calc_Pdet2) %>%
    unlist() %>%
    data.frame(param = as.integer(names(.)), P_det = ., method_sp = method_sp, stringsAsFactors = FALSE)
}

# Clean up the probability of acceptance. "data" takes a list.
clean_Paccept = function(data, method_sp){
  map(.x = data, .f = calc_Prej2) %>%
    unlist() %>%
    data.frame(param = as.integer(names(.)), P_rej = ., method_sp = method_sp, stringsAsFactors = FALSE) %>%
    mutate(Paccept = 1 - P_rej)
}