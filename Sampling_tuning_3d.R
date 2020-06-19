# This function makes a list of arguments from the default arguments
update_arg = function(Args, param, val){
  
  # Checkpoint
  stopifnot(param %in% names(Args))
  
  Args[[param]] = val
  return(Args)
}

# Create a function that passes one tuning parameter value to sim_iterate2()
tune_param = function(Args, n_seed, n_iter, param, val, ...){
  
  # Get tuning parameters
  a = update_arg(Args = Args, param = param, val = val)
  
  # 2 layers of iteration
  b = sim_iterate2(n_seed = n_seed, n_iter = n_iter, Args = a)

  # Add a vector of tuning parameter value
  b$param = rep.int(x = val, times = n_seed * n_iter)
  
  return(b)
}

# A function that tunes over many values of the primary tuning parameter
tune_param_n = function(vals, Args, n_seed, n_iter, var_prim){
  return(map(.x = vals, .f = tune_param, Args = Args, n_seed = n_seed, n_iter = n_iter, param = var_prim))
}

# A function that tunes over a crossed combination of the primary and secondary tuning parameter
tune_param_sec = function(Args, var_prim, vals_prim, var_sec, n_seed, n_iter, vals_sec){
  
  # Create a list of argument lists, each argument list corresponding to one secondary tuning value
  Args_sec = map(.x = vals_sec, .f = update_arg, Args = Args, param = var_sec)
  
  # For each argument list in Args_sec, do iterate_tune1()
  sim_data = list()
  for(i in 1:length(vals_sec)){
    
    sim_data[[i]] = map(.x = vals_prim, .f = tune_param, 
                        Args = Args_sec[[i]], n_seed = n_seed, n_iter = n_iter,param = var_prim)
    
  }
  return(sim_data)
}

# Create a function that summarize the sens, spec and mean c_true
summary_calc_metrics = function(df, Mc){
  a = mean(df$c_true)
  b = calc_metrics(c_true = df$c_true, decision = df$decision, Mc = Mc)
  return(c(mean_c_true = a,b))
}