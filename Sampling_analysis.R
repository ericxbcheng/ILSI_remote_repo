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
                       "covar_mat", "spread_radius", "method", 
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
tune_param2 = function(val, name, Args_default, n_iter){
  
  # Produce a list of arguments with the tuning parameter removed
  a = one_arg_less(name = name, Args_default = Args_default)
  
  # Tune the chosen parameter for n_iter times. !!!a means splicing the list "a" and releasing all the remaining arguments.
  b = exec(f = tune_param, val = val, n_iter = n_iter, !!!a)
  
  return(b)
}